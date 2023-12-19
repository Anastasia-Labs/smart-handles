import {
  applyParamsToScript,
  Blockfrost,
  Lucid,
  Script,
} from "lucid";
import { fromAddressToData, script } from "./common.ts";

export const blockFrostKey = "blockfrost api key";

export const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    blockFrostKey,
  ),
  "Preprod",
);

// your secret key here e.g. "eed25519_.."
lucid.selectWalletFromPrivateKey(
  "your secret key here",
);
export const userAddress =
  "address belonging to secret key";

export const minSwapAddress =
  "addr_test1zzn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uwurajt8r8wqtygrfduwgukk73m5gcnplmztc5tl5ngy0upq932hcy";

const appliedScript = applyParamsToScript(script, [
  fromAddressToData(minSwapAddress),
]);

export const routingContract: Script = {
  type: "PlutusV2",
  script: appliedScript,
};

export const routingContractAddress = lucid.utils.validatorToAddress(
  routingContract,
);
