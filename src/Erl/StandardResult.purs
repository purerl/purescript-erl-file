module Erl.StandardResult
( Reason(..)
, StandardResult
, ReasonResult
, standardResultToEither
) where

import Prelude

import Data.Either (Either(..))
import Erl.Atom (Atom)

newtype Reason = Reason Atom
derive newtype instance eqReason :: Eq Reason
derive newtype instance showReason :: Show Reason

foreign import data StandardResult :: Type -> Type -> Type
type ReasonResult a = StandardResult Reason a

foreign import standardResultToEither_ :: forall a b. (a -> Either a b) -> (b -> Either a b) -> StandardResult a b -> Either a b

standardResultToEither :: forall a. StandardResult Reason a -> Either Reason a
standardResultToEither = standardResultToEither_ Left Right