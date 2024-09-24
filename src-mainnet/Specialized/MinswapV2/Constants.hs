module Specialized.MinswapV2.Constants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PScriptCredential), PCurrencySymbol (PCurrencySymbol), PMaybeData (PDNothing), PTokenName (PTokenName))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

pminswapV2LPSymbol :: ClosedTerm PCurrencySymbol
pminswapV2LPSymbol = pcon $ PCurrencySymbol (phexByteStr "f5808c2c990d86da54bfc97d89cee6efa20cd8461616359478d96b4c")

padaToMinLPTokenName :: ClosedTerm PTokenName
padaToMinLPTokenName = pcon $ PTokenName (phexByteStr "82e2b1fd27a7712a1a9cf750dfbea1a5778611b20e06dd6a611df7a643f8cb75")

pminswapTokenSymbol :: ClosedTerm PCurrencySymbol
pminswapTokenSymbol = pcon $ PCurrencySymbol (phexByteStr "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6")

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
                              "c3e28c36c3447315ba5a56f33da6a6ddc1770a876a8d9f0cb3a97c4c"
                          )
                    )
                  #$ pdnil
          )
        #$ pdcons @"stakingCredential"
        # pdata (pcon $ PDNothing pdnil)
        #$ pdnil

minswapBech32Address :: String
minswapBech32Address = "addr1w8p79rpkcdz8x9d6tft0x0dx5mwuzac2sa4gm8cvkw5hcnqst2ctf"

pbatcherFee :: ClosedTerm PInteger
pbatcherFee = 2_000_000

pdeposit :: ClosedTerm PInteger
pdeposit = 2_000_000
