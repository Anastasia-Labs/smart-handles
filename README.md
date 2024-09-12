<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Smart Beacons](#smart-beacons)
    - [Introduction](#introduction)
    - [Documentation](#documentation)
        - [What problems do Smart Beacons solve?](#what-problems-do-smart-beacons-solve)
    - [Contract Logic](#contract-logic)
    - [Getting Started](#getting-started)
        - [Prerequisites](#prerequisites)
            - [Official option](#official-option)
            - [Preferred option](#preferred-option)
        - [Building and developing](#building-and-developing)
        - [Using Routing Contract](#using-routing-contract)
    - [License](#license)

<!-- markdown-toc end -->

# Smart Beacons

## Introduction

Smart Handles is an abstract Cardano contract generator for instantiating
validators that are meant to be carried out by incentivised agents to handle
the requests in a decentralized and trustless manner.

One of the simplest use-cases can be an instance for swapping one specific token
to another without going through a DEX UI directly. This instance will have its
unique address, which can be complemented with AdaHandles (e.g. `@ada-to-min`)
and therefore any wallet that supports Smart Handles configurations can send
some $ADA directly to this address to be swapped for $MIN.

This is not limited to swaps of course. For instance, sending funds to
`@offer-spacebudz` (which resolves to a router smart contract for SpaceBudz
collection offers) could create a collection offer for SpaceBudz.

The example here uses Smart Handles for swapping any token pairs through Minswap
exchange.

This project is funded by the Cardano Treasury
in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-smart-beacons-router-nfts).

## Documentation

### What problems do Smart Beacons solve?

Right now, most interaction with smart contract protocols is done through
centralized front-end services where the transactions are built and submitted
through centralized backend infrastructure. In addition to the negative impact
this has on decentralization, it also hampers adoption due to the restrictions
it imposes. For instance, users with mobile wallets have severely limited
options when it comes to interacting with DApps. Also, regular users will be
unable to interact with most DApps if the DApp front-ends were to become
unavailable for any reason or if the backend was down.

There are a few attempts to address this problem, such as
[DApp Schemas](https://cardano.ideascale.com/c/idea/64468) or
[Smart Contract Blueprints](https://developers.cardano.org/docs/governance/cardano-improvement-proposals/cip-0057/); however,
all of these solutions rely on off-chain infrastructure to specify how to
interpret a DApp's datums, redeemers, and other on-chain data in order to build
transactions. Smart Beacons differ from these other approaches in that it is a
fully onchain solution that does not rely on offchain infrastructure.

UTxOs locked at a Smart Handles instance can carry datums that determine how a
routing agent should reproduce them at their specified destination addresses
(or route addresses).

In case of simple datums however, the interaction logic is hard-coded in the
instance itself, and their corresponding off-chain agents should provide routers
with all they need to handle requests.

## Contract Logic

Smart Handles framework offers two datums:
```hs
data PSmartHandleDatum (s :: S)
  = PSimple (Term s (PDataRecord '["owner" ':= PAddress]))
  | PAdvanced
      ( Term
          s
          ( PDataRecord
              '[ "mOwner" ':= PMaybeData PAddress
               , "routerFee" ':= PInteger
               , "reclaimRouterFee" ':= PInteger
               , "routeRequiredMint" ':= PRequiredMint
               , "reclaimRequiredMint" ':= PRequiredMint
               , "extraInfo" ':= PData
               ]
          )
      )
```
- `Simple`, which only carries an owner address
- `Advanced`, which allows for a more involved routing/reclaim scenarios:
  - `mOwner` is an optional owner (if this is set to `Nothing` it makes the UTxO
    un-reclaimable)
  - `routerFee` specifies how much an agent must be compensated for routing a
    request
  - `reclaimRouterFee` is similar to `routerFee` for invoking the advanced
    reclaim endpoint (this helps with balancing of the incentive structure)
  - `routeRequiredMint` is a value (isomorphic with
    `Maybe (PolicyID, TokenName)`) that specifies whether a route output must
    append the same quantity of mints/burns present in the transaction
  - `reclaimRequiredMint` is similar to `routeRequiredMint`, but for the
    advanced reclaim endpoint
  - `extraInfo` is an arbitrary `PData` provided for instances

On top of that, each instance can also support a "single" variant and a "batch"
variant: Single will be a spending script that only supports a single
route/reclaim per transaction. Batch, on the other hand, is a staking script for
handling multiple requests in single transactions.

Single variants will have 3 redeemers: routing, simple reclaims, and advanced
reclaims:
```hs
data PSmartHandleRedeemer (s :: S)
  = PRoute (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
  | PReclaim (Term s (PDataRecord '[]))
  | PAdvancedReclaim (Term s (PDataRecord '["ownIndex" ':= PInteger, "routerIndex" ':= PInteger]))
```
Simple reclaim only applies to simple datums, and the only requirement is
imposes on withdrawals is the signature of the owner. Advanced reclaim passes
the spending UTxO to instance's underlying validator, and therefore has a
redeemer similar to the routing endpoint.

Batch variants' scripts (spending and staking) will have 2 redeemers each:
```hs
-- for the staking script
data PRouterRedeemer (s :: S)
  = PRouterRedeemer
      ( Term
          s
          ( PDataRecord
              '[ "inputIdxs" ':= PBuiltinList (PAsData PInteger)
               , "outputIdxs" ':= PBuiltinList (PAsData PInteger)
               ]
          )
      )

-- for the batch spend script
data PSmartRedeemer (s :: S)
  = PRouteSmart (Term s (PDataRecord '[]))
  | PReclaimSmart (Term s (PDataRecord '[]))
```
The underlying logic of an instance is shared between the two variants, and
therefore utilizing either one will be very similar.

You may have noticed that both redeemers are using the [UTxO indexer pattern](https://github.com/Anastasia-Labs/design-patterns/blob/main/utxo-indexers/UTXO-INDEXERS.md)
for a more optimized performance.


## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org) installed on your
system. Nix is used for package management and to provide a consistent
development environment. If you don't have Nix installed, you can do so by
running the following command:

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

Optionally, to improve build speed, it is possible to set up binary caches by
adding additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://cache.iog.io https://cache.zw3rk.com
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=
```

To facilitate seamlessly moving between directories and associated Nix
development shells we use [direnv](https://direnv.net) and
[nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different
directories and prepare the environment accordingly. Use `direnv allow` to
enable the direnv environment and `direnv reload` to reload it when necessary.
Otherwise, the `.envrc` file contains a proper Nix target which will be used
with the `nix develop --accept-flake-config` command.

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

Please be patient when building nix development environment for the first time,
as it may take a very long time. Subsequent builds should be faster.
Additionally, when you run `nix run .#help` you'll get a list of scripts you can
run, the Github CI (nix flake check) is setup in a way where it checks the
project builds successfully, haskell format is done correctly, and commit
message follows conventional commits. Before pushing you should
run `cabal run`, `nix run .#haskellFormat` (automatically formats all haskell
files, including cabal), if you want to commit a correct format message you can
run `cz commit`.

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

Head over to the [off-chain SDK of smart handles](https://github.com/Anastasia-Labs/smart-handles-offchain) to
learn how to define your instance's off-chain, or look
through [an example with Minswap V1](https://github.com/Anastasia-Labs/smart-handles-offchain/tree/main/example).

## License

© 2024 Anastasia Labs.

All code is licensed under MIT License. See [LICENSE](./LICENSE) file
for details.
