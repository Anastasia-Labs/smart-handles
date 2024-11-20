module Specialized.Minswap.Constants where

import Plutarch.Api.V1 (PAddress (PAddress), PCredential (PPubKeyCredential, PScriptCredential), PCurrencySymbol (PCurrencySymbol), PMaybeData (..), PPubKeyHash (PPubKeyHash), PStakingCredential (PStakingHash), PTokenName (PTokenName))
import Plutarch.Api.V2 (PScriptHash (PScriptHash))
import Plutarch.Prelude

pminswapLPSymbol :: ClosedTerm PCurrencySymbol
pminswapLPSymbol = pcon $ PCurrencySymbol (phexByteStr "e4214b7cce62ac6fbba385d164df48e157eae5863521b4b67ca71d86")

padaToMinLPTokenName :: ClosedTerm PTokenName
padaToMinLPTokenName = pcon $ PTokenName (phexByteStr "6aa2153e1ae896a95539c9d62f76cedcdabdcdf144e564b8955f609d660cf6a2")

pminswapTokenSymbol :: ClosedTerm PCurrencySymbol
pminswapTokenSymbol = pcon $ PCurrencySymbol (phexByteStr "29d222ce763455e3d7a09a665ce554f00ac89d2e99a1a83d267170c6")

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
                    (pcon $ PPubKeyHash $ phexByteStr "52563c5410bff6a0d43ccebb7c37e1f69f5eb260552521adff33b9c2")
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
minswapBech32Address = "addr1zxn9efv2f6w82hagxqtn62ju4m293tqvw0uhmdl64ch8uw6j2c79gy9l76sdg0xwhd7r0c0kna0tycz4y5s6mlenh8pq6s3z70"

pbatcherFee :: ClosedTerm PInteger
pbatcherFee = 2_000_000

pdeposit :: ClosedTerm PInteger
pdeposit = 2_000_000
