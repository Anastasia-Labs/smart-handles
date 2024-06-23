module BatchValidator where

import PlutusTx qualified

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PScriptContext, PStakingCredential, PValidator)
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import SingleValidator (PSmartHandleDatum (PAdvanced, PSimple))
import Utils (pconvertChecked, pconvertUnsafe, psignedByOwner)

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

pstakeScriptIsInvoked :: Term s (PScriptContext :--> PStakingCredential :--> POpaque)
pstakeScriptIsInvoked = plam $ \ctx stakeScript -> P.do
  ctxF <- pletFields @'["txInfo"] ctx
  let stakeCerts = pfield @"wdrl" # ctxF.txInfo
  pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
    PJust _ -> (popaque $ pconstant ())
    PNothing -> perror

smartHandleRouteValidatorW :: Term s (PStakingCredential :--> PValidator)
smartHandleRouteValidatorW = phoistAcyclic $ plam $ \stakeScript datum redeemer ctx -> P.do
  let red = pconvertUnsafe @PSmartRedeemer redeemer
      dat = pconvertChecked @PSmartHandleDatum datum
  pmatch red $ \case
    PSwapSmart _ ->
      pstakeScriptIsInvoked # ctx # stakeScript
    PReclaimSmart _ ->
      pmatch dat $ \case
        PSimple ((pfield @"owner" #) -> owner) ->
          popaque $ psignedByOwner # ctx # owner
        PAdvanced _ ->
          pstakeScriptIsInvoked # ctx # stakeScript
