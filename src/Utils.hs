{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Utils where

import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Api.V2
import Plutarch.Bool
import Plutarch.Maybe (pfromJust)
import Plutarch.Prelude
import "liqwid-plutarch-extra" Plutarch.Extra.List (plookupAssoc)
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont

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
