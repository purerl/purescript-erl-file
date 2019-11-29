module Erl.File
( readFile
, writeFile
, deleteFile
, module StandardResult
) where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.StandardResult (Reason, ReasonResult, standardResultToEither)
import Erl.StandardResult (Reason(..)) as StandardResult

foreign import writeFile_ :: String -> IOData -> Effect (ReasonResult Unit)

writeFile :: String -> IOData -> Effect (Either Reason Unit)
writeFile f x = standardResultToEither <$> writeFile_ f x


foreign import readFile_ :: String -> Effect (ReasonResult Binary)

readFile :: String -> Effect (Either Reason Binary)
readFile f = standardResultToEither <$> readFile_ f


foreign import deleteFile_ :: String -> Effect (ReasonResult Unit)

deleteFile :: String -> Effect (Either Reason Unit)
deleteFile f = standardResultToEither <$> deleteFile_ f
