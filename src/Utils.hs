{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils where

import PlutusLedgerApi.V2 (CurrencySymbol, TokenName)
import PlutusTx qualified

import Plutarch.Api.V1.Address (PCredential (..))
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V1.Value (padaSymbol, padaToken, pforgetPositive, psingleton)
import Plutarch.Api.V1.Value qualified as Value
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.DataRepr
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Maybe (pfromJust)
import Plutarch.Monadic qualified as P
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Unsafe (punsafeCoerce)
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

type PCustomValidator =
  ( PMaybeData PAddress -- possible owner
      :--> PInteger -- routing fee
      :--> PValue 'Sorted 'Positive -- value of the input utxo
      :--> PData -- extraInfo from the `Advanced` datum
      :--> PDatum -- routing address output datum (resolved hash, or inline)
      :--> PBool -- routing flag (`True` for routing, `False` for reclaiming)
      :--> PScriptContext -- script context
      :--> PBool
  )

data PAssetClass (s :: S) = PAssetClass (Term s (PDataRecord '["cs" ':= PCurrencySymbol, "tn" ':= PTokenName]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PAssetClass where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PAssetClass

data RequiredMint
  = Singleton CurrencySymbol TokenName Integer
  | None

PlutusTx.makeLift ''RequiredMint
PlutusTx.makeIsDataIndexed
  ''RequiredMint
  [ ('Singleton, 0)
  , ('None, 1)
  ]

data PRequiredMint (s :: S)
  = PSingleton
      ( Term
          s
          ( PDataRecord
              '[ "policy" ':= PCurrencySymbol
               , "name" ':= PTokenName
               , "quantity" ':= PInteger
               ]
          )
      )
  | PNone (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PRequiredMint where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PRequiredMint

instance PUnsafeLiftDecl PRequiredMint where type PLifted PRequiredMint = RequiredMint
deriving via (DerivePConstantViaData RequiredMint PRequiredMint) instance PConstantDecl RequiredMint

pexpectJust :: Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
pexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v
  PNothing -> escape

psymbolValueOfHelper ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( (PInteger :--> PBool)
        :--> PCurrencySymbol
        :--> ( PValue keys amounts
                :--> PInteger
             )
    )
psymbolValueOfHelper =
  phoistAcyclic $
    plam $ \cond sym value'' -> unTermCont $ do
      PValue value' <- pmatchC value''
      PMap value <- pmatchC value'
      m' <-
        pexpectJust
          0
          ( plookupAssoc
              # pfstBuiltin
              # psndBuiltin
              # pdata sym
              # value
          )
      PMap m <- pmatchC (pfromData m')
      pure $
        pfoldr
          # plam
            ( \x v ->
                plet (pfromData $ psndBuiltin # x) $ \q ->
                  pif
                    (cond # q)
                    (q + v)
                    v
            )
          # 0
          # m

pelemAt' :: (PIsListLike l a) => Term s (PInteger :--> l a :--> a)
pelemAt' = phoistAcyclic $
  pfix #$ plam $ \self n xs ->
    pif
      (n #== 0)
      (phead # xs)
      (self # (n - 1) #$ ptail # xs)

-- | @since 1.0.0
ppositiveSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
ppositiveSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (0 #<)

-- | @since 1.0.0
pnegativeSymbolValueOf ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PCurrencySymbol :--> (PValue keys amounts :--> PInteger))
pnegativeSymbolValueOf = phoistAcyclic $ psymbolValueOfHelper #$ plam (#< 0)

pand'List :: [Term s PBool] -> Term s PBool
pand'List ts' =
  case ts' of
    [] -> pconstant True
    ts -> foldl1 (\res x -> pand' # res # x) ts

pcond ::
  [(Term s PBool, Term s a)] ->
  Term s a ->
  Term s a
pcond [] def = def
pcond ((cond, res) : conds) def = pif cond res (pcond conds def)

(#>) :: (PPartialOrd t) => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

(#>=) :: (PPartialOrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a
infix 4 #>=

presolveDatum :: Term s (POutputDatum :--> PMap any PDatumHash PDatum :--> PDatum)
presolveDatum = phoistAcyclic $ plam $ \outputDatum datums ->
  outputDatum `pmatch` \case
    POutputDatum r -> (pfield @"outputDatum" # r)
    POutputDatumHash r -> pfromJust #$ plookup # (pfield @"datumHash" # r) # datums
    PNoOutputDatum _ -> ptraceError "No output datum"

presolveDatumData :: Term s (POutputDatum :--> PMap any PDatumHash PDatum :--> PData)
presolveDatumData = phoistAcyclic $ plam $ \outputDatum datums -> pto $ presolveDatum # outputDatum # datums

pfoldl2 ::
  (PListLike listA, PListLike listB, PElemConstraint listA a, PElemConstraint listB b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> listA a :--> listB b :--> acc)
pfoldl2 =
  phoistAcyclic $ plam $ \func ->
    pfix #$ plam $ \self acc la lb ->
      pelimList
        ( \a as ->
            pelimList
              (\b bs -> self # (func # acc # a # b) # as # bs)
              perror
              lb
        )
        (pif (pnull # lb) acc perror)
        la

pconvertChecked :: forall (b :: PType) (a :: PType) (s :: S). (PTryFrom a b) => Term s a -> Term s b
pconvertChecked x = ptryFrom x fst

pconvertUnsafe :: forall (b :: PType) (a :: PType) (s :: S). (PTryFrom a b) => Term s a -> Term s b
pconvertUnsafe = punsafeCoerce

psignedByOwner :: Term s (PScriptContext :--> PAddress :--> PBool)
psignedByOwner = plam $ \ctx owner ->
  pmatch (pfield @"credential" # owner) $ \case
    PPubKeyCredential ((pfield @"_0" #) -> pkh) ->
      pelem @PBuiltinList # pkh # (pfield @"signatories" # (pfield @"txInfo" # ctx))
    _ ->
      pcon PFalse

pvalueHasChangedByLovelaces :: Term s (PValue 'Sorted 'Positive :--> PValue 'Sorted 'Positive :--> PInteger :--> PBool)
pvalueHasChangedByLovelaces = plam $ \inVal outVal change ->
  pif
    (change #== 0)
    (outVal #== inVal)
    (pforgetPositive outVal #== (pforgetPositive inVal <> (psingleton # padaSymbol # padaToken # change)))

presolveMapToList ::
  forall
    (anyOrder :: KeyGuarantees)
    (a :: PType)
    (b :: PType)
    (s :: S).
  Term s (PMap anyOrder a b) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData a) (PAsData b)))
presolveMapToList m = pmatch m $ \(PMap l) -> l

{- | Converts a `PValue` to a `PBuiltinList`. Does not convert the inner `PMap`
of token names and quantities to a list.
-}
presolveValueToList ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees)
    (s :: S).
  Term s (PValue anyOrder anyAmount) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap anyOrder PTokenName PInteger))))
presolveValueToList v =
  pmatch v $ \(PValue v') ->
    pmatch (presolveMapToList v') $ \kvs -> pcon kvs

{- | Get the head of the list if the list contains exactly one element,
otherwise error.
-}
pheadSingleton ::
  (PListLike list, PElemConstraint list a) =>
  Term s (list a) ->
  Term s a
pheadSingleton =
  pelimList
    (pelimList (\_ _ -> ptraceError "List contains more than one element."))
    (ptraceError "List is empty.")

-- | Check if the mint field contains exactly the provided singleton.
pmintIsSameAsSingleton ::
  Term s PCurrencySymbol ->
  Term s PTokenName ->
  Term s PInteger ->
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PBool
pmintIsSameAsSingleton policy name qty mintVal =
  let
    mintAsset = pheadSingleton $ presolveValueToList mintVal
    mintCS = pfstBuiltin # mintAsset
   in
    plet (pheadSingleton $ pto $ pfromData (psndBuiltin # mintAsset)) $ \mintTnQtyPairs ->
      let
        mintTN = pfstBuiltin # mintTnQtyPairs
        mintQty = psndBuiltin # mintTnQtyPairs
       in
        pand'List
          [ policy #== pfromData mintCS
          , name #== pfromData mintTN
          , qty #== pfromData mintQty
          ]

papplyRequiredMintToInputValue ::
  Term s (PValue 'Sorted 'NoGuarantees) ->
  Term s PRequiredMint ->
  Term s (PValue 'Sorted 'Positive) ->
  Term s (PValue 'Sorted 'Positive)
papplyRequiredMintToInputValue mint requiredMint inputValue =
  pmatch requiredMint $ \case
    PSingleton rm -> P.do
      rmF <- pletFields @'["policy", "name", "quantity"] rm
      let requiredMintValue = Value.psingleton # rmF.policy # rmF.name # rmF.quantity
          inputAppendedWithMint = requiredMintValue <> pforgetPositive inputValue
      pif
        ( ptraceIfFalse
            "Tx mint doesn't match the reclaim mint"
            (pmintIsSameAsSingleton rmF.policy rmF.name rmF.quantity mint)
        )
        (Value.passertPositive # inputAppendedWithMint)
        perror
    PNone _ ->
      inputValue
