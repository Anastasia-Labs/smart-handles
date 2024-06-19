module BatchValidator where

import PlutusTx qualified

import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V2 (PMaybeData (..), PStakingCredential, PValidator)
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

smartHandleRouteValidatorW :: Term s (PStakingCredential :--> PValidator)
smartHandleRouteValidatorW = phoistAcyclic $ plam $ \stakeScript datum redeemer ctx -> P.do
  let red = pconvertUnsafe @PSmartRedeemer redeemer
      dat = pconvertChecked @PSmartHandleDatum datum
  ctxF <- pletFields @'["txInfo"] ctx
  pmatch red $ \case
    PSwapSmart _ ->
      let stakeCerts = pfield @"wdrl" # ctxF.txInfo
       in pmatch (AssocMap.plookup # stakeScript # stakeCerts) $ \case
            PJust _ -> (popaque $ pconstant ())
            PNothing -> perror
    PReclaimSmart _ ->
      pmatch dat $ \case
        PSimple ((pfield @"owner" #) -> owner) ->
          popaque $ psignedByOwner # ctx # owner
        PAdvanced ((pfield @"mOwner" #) -> mOwner) -> P.do
          pmatch mOwner $ \case
            PDJust ((pfield @"_0" #) -> owner) ->
              popaque $ psignedByOwner # ctx # owner
            PDNothing _ -> perror
