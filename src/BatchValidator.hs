module BatchValidator where

import PlutusTx qualified

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PAddress, PStakingCredential, PValidator)
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Conversions
import SingleValidator (PSmartHandleDatum)

data SmartRedeemer
  = SwapSmart
  | ReclaimSmart

PlutusTx.makeLift ''SmartRedeemer
PlutusTx.makeIsDataIndexed
  ''SmartRedeemer
  [ ('SwapSmart, 0)
  , ('ReclaimSmart, 1)
  ]

data PSmartRedeemer (s :: S)
  = PSwapSmart (Term s (PDataRecord '[]))
  | PReclaimSmart (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PSmartRedeemer where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PSmartRedeemer

instance PUnsafeLiftDecl PSmartRedeemer where type PLifted PSmartRedeemer = SmartRedeemer
deriving via (DerivePConstantViaData SmartRedeemer PSmartRedeemer) instance PConstantDecl SmartRedeemer

smartHandleRouteValidatorW :: Term s (PStakingCredential :--> PValidator)
smartHandleRouteValidatorW = phoistAcyclic $ plam $ \stakeScript datum redeemer ctx -> P.do
  let red = pconvert @PSmartRedeemer redeemer
      dat = pconvert @PSmartHandleDatum datum
  ctxF <- pletFields @'["txInfo"] ctx
  pmatch red $ \case
    PSwapSmart _ ->
      let stakeCerts = pfield @"wdrl" # ctxF.txInfo
       in pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
            PJust _ -> (popaque $ pconstant ())
            PNothing -> perror
    PReclaimSmart _ ->
      pmatch (pfield @"credential" # (pfield @"owner" # dat)) $ \case
        PPubKeyCredential ((pfield @"_0" #) -> pkh) ->
          ( pif
              (pelem @PBuiltinList # pkh # (pfield @"signatories" # ctxF.txInfo))
              (popaque $ pconstant ())
              perror
          )
        PScriptCredential _ -> perror -- TODO: is it refundable for a script?
