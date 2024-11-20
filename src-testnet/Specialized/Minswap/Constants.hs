module Specialized.Minswap.Constants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PPubKeyCredential, PScriptCredential), PCurrencySymbol (PCurrencySymbol), PMaybeData (..), PPubKeyHash (PPubKeyHash), PStakingCredential (PStakingHash), PTokenName (PTokenName))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

pminswapLPSymbol :: ClosedTerm PCurrencySymbol
pminswapLPSymbol = pcon $ PCurrencySymbol (phexByteStr "e4214b7cce62ac6fbba385d164df48e157eae5863521b4b67ca71d86")

padaToMinLPTokenName :: ClosedTerm PTokenName
padaToMinLPTokenName = pcon $ PTokenName (phexByteStr "3bb0079303c57812462dec9de8fb867cef8fd3768de7f12c77f6f0dd80381d0d")

pminswapTokenSymbol :: ClosedTerm PCurrencySymbol
pminswapTokenSymbol = pcon $ PCurrencySymbol (phexByteStr "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72")

pminswapTokenName :: ClosedTerm PTokenName
pminswapTokenName = pcon $ PTokenName (phexByteStr "4d494e")

pminswapStakingHash :: ClosedTerm PStakingCredential
pminswapStakingHash =
  pcon $
    PStakingHash $
      pdcons
        # pdata
          ( pcon $
              PPubKeyCredential $
                pdcons
                  # pdata
                    (pcon $ PPubKeyHash $ phexByteStr "83ec96719dc0591034b78e472d6f477446261fec4bc517fa4d047f02")
                  #$ pdnil
          )
        #$ pdnil

pminswapAddress :: ClosedTerm PAddress
pminswapAddress =
  pcon $
    PAddress $
      pdcons @"credential"
        # pdata
          ( pcon $
              PScriptCredential $
                pdcons
                  # pdata
                    ( pcon $
                        PScriptHash
                          ( phexByteStr
                              "a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
                          )
                    )
                  #$ pdnil
          )
        #$ pdcons @"stakingCredential"
        # pdata (pcon $ PDJust $ pdcons # pdata pminswapStakingHash #$ pdnil)
        #$ pdnil

minswapBech32Address :: String
minswapBech32Address = "addr_test1zzn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uwurajt8r8wqtygrfduwgukk73m5gcnplmztc5tl5ngy0upq932hcy"

pbatcherFee :: ClosedTerm PInteger
pbatcherFee = 2_000_000

pdeposit :: ClosedTerm PInteger
pdeposit = 2_000_000
