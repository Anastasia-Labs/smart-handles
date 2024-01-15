<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Smart Beacons](#smart-beacons)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [What problems do Smart Beacons solve?](#what-problems-do-smart-beacons-solve)
    - [Details](#details)
      - [Basic Routing Script](#basic-routing-script)
      - [Advanced Routing Script](#advanced-routing-script)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [Using Routing Contract](#using-routing-contract)
    - [Sending Swap Order](#sending-swap-order)
    - [Routing Swap Order to Minswap](#routing-swap-order-to-minswap)
  - [License](#license)

<!-- markdown-toc end -->

# Smart Beacons

## Introduction

Smart Beacons is a framework for routing transactions. It is essentially AdaHandles for Smart Contract addresses. 

Through the use of Smart Beacons & router scripts, users will be able to interact with DApps without going through their front-end or building the smart contract transactions themselves. 

For example, swapping tokens on a dex will be as simple as sending funds to `@ada-to-min` (or whatever token pair you want). It doesn’t need to be a swap, this can be done for any arbitrary smart contract interaction. For instance, sending funds to `@offer-spacebudz` (which resolves to a router smart contract for SpaceBudz collection offers) could create a collection offer for SpaceBudz. Each smart beacon requires a custom routing smart contract. 

Here we demonstrates the use of Smart Beacons for swapping $ADA for $MIN through Minswap exchange by implementing the routing script for `@ada-to-min`.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-smart-beacons-router-nfts).

## Documentation

### What problems do Smart Beacons solve?

Right now, most interaction with smart contract protocols is done through centralized front-end services where the transactions are built and submitted through centralized backend infrastructure. In addition to the negative impact this has on decentralization, it also hampers adoption due to the restrictions it imposes. For instance, users with mobile wallets have severely limited options when it comes to interacting with DApps. Also, regular users will be unable to interact with most DApps if the DApp front-ends were to become unavailable for any reason or if the backend was down.

There are a few attempts to address this problem, such as [DApp Schemas](https://cardano.ideascale.com/c/idea/64468) or [Smart Contract Blueprints](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0057/); however, all of these solutions rely on offchain infrastructure to specify how to interpret a DApp's datums, redeemers, and other onchain data in order to build transactions. Smart Beacons differ from these other approaches in that it is a fully onchain solution that does not rely on offchain infrastructure.

A Smart Beacon is an NFT that lives at a UTxO locked in a spending validator; the datum of the UTxO specifies how to interact with the spending validator that locks it. The datum is in essence an onchain Schema describing how to interact with the associated DApp (or at-least with the spending validator that locks the associated UTxO).

### Details

#### Basic Routing Script

A user sends $ADA to the routing contract with a datum containing his address. This helps convey ownership of the UTxO and destination for receiving $MIN in exchange. Keep in mind that a single UTxO corresponds to a single swap order.

```haskell
data SmartHandleDatum = SmartHandleDatum
  { owner :: Address
  }
```

That's all that needs to be done by the user. Post which the routing agents take responsibility of sending this UTxO from the contract address to Minswap's swap address. They do so by initiating a spending transaction (with a `SmartHandleRedeemer`) and sending the output UTxO (with the correct `MinswapRequestDatum` as below). They can take a 1 ADA routing fee (from the owner) upon building a successful transaction.

```haskell
data MinswapRequestDatum = MinswapRequestDatum
  { sender :: Address                               => Owner
  , receiver :: Address                             => Owner
  , receiverDatumHash :: Maybe DatumHash            => Constr 1 []
  , step :: OrderType                               => Constr 0 [Constr 0 [policyId, tokenName], minAmount]
  , batcherFee :: Integer                           => 2_000_000
  , outputAda :: Integer                            => 2_000_000
  }

data SmartHandleRedeemer
  = Swap
      { ownIndex :: Integer
      , routerIndex :: Integer
      }
  | Reclaim
```

The `Swap` tells the contract which output UTxO (outputs[ routerIndex ]) is created from the spending of provided script input UTxO (inputs[ ownIndex ]) to facilitate validations. The owner can choose to reclaim his locked funds back (if they haven't been spent yet) by initiating a spending transaction with `Reclaim` redeemer.

```haskell
psmartHandleValidator :: Term s (PAddress :--> PSmartHandleDatum :--> PSmartHandleRedeemer :--> PScriptContext :--> PUnit)
```

The routing contract (`psmartHandleValidator`, compiled script at `./compiled/smartHandleSimple.json`) mentioned here, is the standalone validator (takes Minswap's swap address as a parameter) which gets the job done. However, it has a limitation. It only allows one script input to be spent in a single transaction, severely limiting the routing throughput per transaction.

Allowing more than one script input to be spent within it, could result in a critical vulnerability in the form of [Double Satisfaction Attack](https://plutus.readthedocs.io/en/latest/reference/writing-scripts/common-weaknesses/double-satisfaction.html?highlight=double#unique-outputs). This contract serves as a starting point for understanding and working with Smart Beacons. For those looking to carry out routing of multiple script inputs in a single transaction, the next section provides the required details.

#### Advanced Routing Script

Here a single transaction can fulfill multiple routings, whether all of them are correct or not, is validated only once at the tx level using the [Zero ADA Withdrawal Trick](https://github.com/cardano-foundation/CIPs/pull/418#issuecomment-1366605115) from a Staking validator.

```haskell
smartHandleStakeValidatorW :: Term s (PAddress :--> PStakeValidator)
```

The Staking validator (`smartHandleStakeValidatorW`, compiled script at `./compiled/smartHandleStake.json`) takes care of validating all script inputs against their corresponding swap outputs in a given tx. It takes Minswap's swap address as a parameter.

This Staking validator's credential is used as a parameter to a Spending Validator (`smartHandleRouteValidatorW`, compiled script at `./compiled/smartHandleRouter.json`), the advanced routing contract which locks the user's UTxOs. Spending validator ensures that the Staking validator is executed in the tx thereby confirming that no script input avoids validation. A successful validation from both spending and staking validator is essentail for spending UTxOs.

```haskell
smartHandleRouteValidatorW :: Term s (PStakingCredential :--> PValidator)
```

For carrying out this tx level validation, the Staking Validator requires a redeemer containing one-to-one correlation between script input UTxOs (user UTxOs) and swap output UTxOs (sent to Minswap by routing agents). This is provided via ordered lists of input/output indices of inputs/ouputs present in the Script Context.

```haskell
data RouterRedeemer = RouterRedeemer
  { inputIdxs :: [Integer]
  , outputIdxs :: [Integer]
  }
```

For e.g.

```
Inputs     :  [swapOrderA, swapOrderC, randomInput3, swapOrderB, randomInput1, randomInput2]          // random inputs are not routing script inputs
Outputs    :  [swapOutputA, swapOutputB, swapOutputC, randomOuput1, randomOutput2, randomOutput3]
InputIdxs  :  [0, 1, 3]
OutputIdxs :  [0, 2, 1]
```

While its easy to understand and declare indices of outputs (the order in which outputs appear in the tx builder), we cannot control the order of inputs as seen by the script. As inputs are sorted lexicographically based on their output reference, first by Tx#Id and then by Tx#Idx.

Note: Staking validator need not be invoked if user wishes to cancel his swap order and reclaim funds.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org) installed on your system. Nix is used for package management and to provide a consistent development environment. If you don't have Nix installed, you can do so by running the following command:

#### Official option

[Nix](https://nixos.org/download.html)

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

#### Preferred option

[Determinate Systems](https://zero-to-nix.com/concepts/nix-installer)

```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches by adding additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target which will be used with the `nix develop --accept-flake-config` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/smart-handles.git
```

Navigate to the repository directory:

```sh
cd smart-handles
```

Activate the development environment with Nix:

```sh
nix develop --accept-flake-config
```
Or
```sh
make shell
```

Please be patient when building nix development environment for the first time, as it may take a very long time. Subsequent builds should be faster. Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
make build
```

Execute the test suite:

```sh
make test
```

Compile and export Plutarch scripts:

```sh
make export
```

![smart-handles](/assets/gifs/smart-handles.gif)

### Using Routing Contract

This section explains how to interact with Basic Routing Contract using [Lucid](https://lucid.spacebudz.io/) based offchain scripts. Please install [Deno](https://deno.land/ "A modern runtime for Javascript & Typescript") before proceeding further.

#### Setup

Move into the offchain scripts directory:

```sh
cd offchain/
```

Please configure your preprod wallet details and Blockfrost API Key in `offchain/config.ts` before executing scripts.

#### Sending Swap Order

Inorder to create a swap order at the routing contract, a UTxO needs to be sent to it containing $ADA to be exchanged for $MIN along with a datum of type SmartHandleDatum.

```hs
data SmartHandleDatum = SmartHandleDatum
  { owner :: Address
  }
```

Run:

```sh
deno run --allow-all create-swap-order.ts
```

Note: Script execution may take couple of minutes, as it waits for tx confirmation. Upon successful execution, the script will output a detailed message with a tx hash.

#### Routing Swap Order to Minswap

A routing agent spends a UTxO at routing contract address (the one locked above) by passing a `Swap` redeemer and sending the output UTxO with the correct `MinswapRequestDatum` to Minswap's swap address.

Run:

```sh
deno run --allow-all route-swap-order.ts
```

Note: Script execution may take couple of minutes, as it waits for tx confirmation. Upon successful execution, the script will output a detailed message with a tx hash.

## License

© 2023 Anastasia Labs.

All code is licensed under MIT License. See [LICENSE](./LICENSE) file
for details.