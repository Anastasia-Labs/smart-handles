import { Data } from "lucid";
import { lucid, routingContractAddress, userAddress } from "./config.ts";
import { fromAddress, SmartHandleDatum } from "./common.ts";

const datum = Data.to({ owner: fromAddress(userAddress) }, SmartHandleDatum);

const tx = await lucid
  .newTx()
  .payToContract(routingContractAddress, { inline: datum }, {
    lovelace: BigInt(50_000_000),
  })
  .complete();

const signedTx = await tx.sign().complete();
const txHash = await signedTx.submit();
await lucid.awaitTx(txHash);

console.log(`Successfully sent swap order with 50 ADA
to routingContract: ${routingContractAddress},
in tx: ${txHash}`);
