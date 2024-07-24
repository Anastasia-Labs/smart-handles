module Constants where

import Plutarch.Api.V1.Value (AmountGuarantees (..), KeyGuarantees (..), PValue, padaSymbol, padaToken, psingleton)
import Plutarch.Prelude (PInteger, Term, (#))

routerFeeForSimpleRoutes :: Term s PInteger
routerFeeForSimpleRoutes = 1_000_000

negativeRouterFeeForSimpleRoutes :: Term s PInteger
negativeRouterFeeForSimpleRoutes = -1_000_000

routerFeeAsNegativeValue :: Term s (PValue 'Sorted 'NonZero)
routerFeeAsNegativeValue =
  psingleton # padaSymbol # padaToken # negativeRouterFeeForSimpleRoutes
