module Erl.FileLib
( ensureDir
, module StandardResult
)
where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Erl.StandardResult (Reason, ReasonResult, standardResultToEither)
import Erl.StandardResult (Reason(..)) as StandardResult

foreign import ensureDir_ :: String -> Effect (ReasonResult Unit)

ensureDir :: String -> Effect (Either Reason Unit)
ensureDir f = standardResultToEither <$> ensureDir_ f

foreign import fileSize :: String -> Effect Int

foreign import isDir :: String -> Effect Boolean

foreign import isFile :: String -> Effect Boolean

foreign import isRegular :: String -> Effect Boolean
