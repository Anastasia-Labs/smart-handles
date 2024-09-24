module Specialized.MinswapV2.Constants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PScriptCredential), PCurrencySymbol (PCurrencySymbol), PMaybeData (PDNothing), PTokenName (PTokenName))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

pminswapV2LPSymbol :: ClosedTerm PCurrencySymbol
pminswapV2LPSymbol = pcon $ PCurrencySymbol (phexByteStr "d6aae2059baee188f74917493cf7637e679cd219bdfbbf4dcbeb1d0b")

padaToMinLPTokenName :: ClosedTerm PTokenName
padaToMinLPTokenName = pcon $ PTokenName (phexByteStr "6c3ea488e6ff940bb6fb1b18fd605b5931d9fefde6440117015ba484cf321200")

pminswapTokenSymbol :: ClosedTerm PCurrencySymbol
pminswapTokenSymbol = pcon $ PCurrencySymbol (phexByteStr "e16c2dc8ae937e8d3790c7fd7168d7b994621ba14ca11415f39fed72")

pminswapTokenName :: ClosedTerm PTokenName
pminswapTokenName = pcon $ PTokenName (phexByteStr "4d494e")

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
                              "da9525463841173ad1230b1d5a1b5d0a3116bbdeb4412327148a1b7a"
                          )
                    )
                  #$ pdnil
          )
        #$ pdcons @"stakingCredential"
        # pdata (pcon $ PDNothing pdnil)
        #$ pdnil

minswapBech32Address :: String
minswapBech32Address = "addr_test1wrdf2f2x8pq3wwk3yv936ksmt59rz94mm66yzge8zj9pk7s0kjph3"

pbatcherFee :: ClosedTerm PInteger
pbatcherFee = 2_000_000

pdeposit :: ClosedTerm PInteger
pdeposit = 2_000_000
