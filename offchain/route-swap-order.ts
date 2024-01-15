import { Address, Constr, Data, Network } from "lucid";
import {
  blockFrostKey,
  lucid,
  minSwapAddress,
  routingContract,
  routingContractAddress,
  userAddress,
} from "./config.ts";
import {
  BlockfrostAdapter,
  calculateSwapExactIn,
  MetadataMessage,
  NetworkId,
  OrderDatum,
  OrderStepType,
  PoolDatum,
  PoolState,
} from "minswap";
import { BlockFrostAPI } from "blockfrost";
import { AddressD, SmartHandleDatum } from "./common.ts";

// ID of ADA-MIN Pool on Testnet Preprod
const poolId =
  "3bb0079303c57812462dec9de8fb867cef8fd3768de7f12c77f6f0dd80381d0d";

const blockfrostAdapter = new BlockfrostAdapter({
  blockFrost: new BlockFrostAPI({
    projectId: blockFrostKey,
    network: "preprod",
  }),
});

const { poolState, poolDatum } = await getPoolById(
  "Preprod",
  blockfrostAdapter,
  poolId,
);

// After adjusting 2 ADA for Batcher fee and Deposit each from 50 ADA
const swapAmountADA = 46_000_000n;

const { amountOut } = calculateSwapExactIn({
  amountIn: swapAmountADA,
  reserveIn: poolState.reserveA,
  reserveOut: poolState.reserveB,
});

// Because pool is always fluctuating, so you should determine the impact of amount which you will receive
const slippageTolerance = 20n;
const acceptedAmount = (amountOut * (100n - slippageTolerance)) / 100n;

const scriptUTxOs = await lucid.utxosAt(routingContractAddress);
// console.log(JSON.stringify(scriptUTxOs, bigIntReplacer));
const utxo = scriptUTxOs[0];

let ownerAddress: string;
try {
  const datum = Data.from<SmartHandleDatum>(utxo.datum!, SmartHandleDatum);
  ownerAddress = toAddress(datum?.owner);
} catch (error) {
  console.error(
    `Error occured while deserializing Script UTxO datum. 
    Datum: ${utxo.datum}` + error.message,
  );
}

console.log(`Routing Script UTxO: ${JSON.stringify(utxo, bigIntReplacer)}
ownerAddress: ${ownerAddress!}`);

const datum: OrderDatum = {
  sender: ownerAddress!,
  receiver: ownerAddress!,
  receiverDatumHash: undefined,
  step: {
    type: OrderStepType.SWAP_EXACT_IN,
    desiredAsset: poolDatum.assetB,
    minimumReceived: acceptedAmount,
  },
  batcherFee: 2_000_000n,
  depositADA: 2_000_000n,
};
const datumCbor = Data.to(OrderDatum.toPlutusData(datum));

// Swap ( { ownIndex, routerIndex } )
const routerRedeemer = Data.to(new Constr(0, [0n, 0n]));

const tx = await lucid
  .newTx()
  .collectFrom([utxo], routerRedeemer)
  .payToContract(
    minSwapAddress,
    datumCbor,
    { lovelace: 49_000_000n },
  )
  .attachSpendingValidator(routingContract)
  .attachMetadata(674, { msg: [MetadataMessage.SWAP_EXACT_IN_ORDER] })
  .complete({ change: { address: userAddress } }); // (routing fee - tx costs) to user wallet as user is the router here

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
await lucid.awaitTx(txHash);

console.log(`Successfully sent swap order with 49 ADA
from routingContract: ${routingContractAddress},
to Minswap address: ${minSwapAddress}
in tx: ${txHash}`);

async function getPoolById(
  network: Network,
  blockfrostAdapter: BlockfrostAdapter,
  poolId: string,
): Promise<{ poolState: PoolState; poolDatum: PoolDatum }> {
  const pool = await blockfrostAdapter.getPoolById({
    id: poolId,
  });
  if (!pool) {
    throw new Error(`Not found PoolState of ID: ${poolId}`);
  }

  const rawRoolDatum = await blockfrostAdapter.getDatumByDatumHash(
    pool.datumHash,
  );
  const poolDatum = PoolDatum.fromPlutusData(
    network === "Mainnet" ? NetworkId.MAINNET : NetworkId.TESTNET,
    Data.from(rawRoolDatum) as Constr<Data>,
  );
  return {
    poolState: pool,
    poolDatum: poolDatum,
  };
}

export function toAddress(address: AddressD): Address {
  const paymentCredential = (() => {
    if ("PublicKeyCredential" in address.paymentCredential) {
      return lucid.utils.keyHashToCredential(
        address.paymentCredential.PublicKeyCredential[0],
      );
    } else {
      return lucid.utils.scriptHashToCredential(
        address.paymentCredential.ScriptCredential[0],
      );
    }
  })();
  const stakeCredential = (() => {
    if (!address.stakeCredential) return undefined;
    if ("Inline" in address.stakeCredential) {
      if ("PublicKeyCredential" in address.stakeCredential.Inline[0]) {
        return lucid.utils.keyHashToCredential(
          address.stakeCredential.Inline[0].PublicKeyCredential[0],
        );
      } else {
        return lucid.utils.scriptHashToCredential(
          address.stakeCredential.Inline[0].ScriptCredential[0],
        );
      }
    } else {
      return undefined;
    }
  })();
  return lucid.utils.credentialToAddress(paymentCredential, stakeCredential);
}

// Parameter to 'JSON.stringify()' to help with bigint conversion
export function bigIntReplacer(_k: any, v: any) {
  return typeof v === "bigint" ? v.toString() : v;
}
