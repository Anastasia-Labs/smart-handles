module Constants where

import Plutarch.Api.V1.Value (AmountGuarantees (..), KeyGuarantees (..), PValue, padaSymbol, padaToken, psingleton)
import Plutarch.Prelude (PInteger, Term, (#))

routerFeeAsNegativeLovelace :: Term s PInteger
routerFeeAsNegativeLovelace = -1_000_000

routerFeeAsNegativeValue :: Term s (PValue 'Sorted 'NonZero)
routerFeeAsNegativeValue = psingleton # padaSymbol # padaToken # routerFeeAsNegativeLovelace
