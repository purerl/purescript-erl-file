module Erl.File
( readFile
, writeFile
, deleteDir
, deleteDirRecursive
, deleteFile
, listDir
, readFileInfo
, FileInfo
, FileType(..)
, FileAccess(..)
, module StandardResult
) where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.List (List)
import Erl.StandardResult (Reason(..)) as StandardResult
import Erl.StandardResult (Reason, ReasonResult, standardResultToEither)

foreign import writeFile_ :: String -> IOData -> Effect (ReasonResult Unit)

writeFile :: String -> IOData -> Effect (Either Reason Unit)
writeFile f x = standardResultToEither <$> writeFile_ f x

foreign import readFile_ :: String -> Effect (ReasonResult Binary)

readFile :: String -> Effect (Either Reason Binary)
readFile f = standardResultToEither <$> readFile_ f

foreign import deleteFile_ :: String -> Effect (ReasonResult Unit)

deleteFile :: String -> Effect (Either Reason Unit)
deleteFile f = standardResultToEither <$> deleteFile_ f

foreign import listDir_ :: String -> Effect (ReasonResult (List String))

listDir :: String -> Effect (Either Reason (List String))
listDir d = standardResultToEither <$> listDir_ d

data FileType
  = Device
  | Directory
  | Other
  | Regular
  | Symlink

data FileAccess
  = Read
  | Write
  | ReadWrite
  | None

type FileInfo
  = { size :: Int
    , type :: Maybe FileType
    , access :: Maybe FileAccess
    , atime :: Int
    , mtime :: Int
    , ctime :: Int
    , mode :: Int
    , links :: Int
    , major_device :: Int
    , minor_device :: Int
    , inode :: Int
    , uid :: Int
    , gid :: Int
    }

foreign import readFileInfo_ :: (Atom -> Maybe FileType) -> (Atom -> Maybe FileAccess) -> String -> Effect (ReasonResult FileInfo)

readFileInfo :: String -> Effect (Either Reason FileInfo)
readFileInfo f = standardResultToEither <$> readFileInfo_ atomToFileType atomToFileAccess f

atomToFileType :: Atom -> Maybe FileType
atomToFileType a
  | a == atom "device" = Just Device
  | a == atom "directory" = Just Directory
  | a == atom "other" = Just Other
  | a == atom "regular" = Just Regular
  | a == atom "symlink" = Just Symlink
  | otherwise = Nothing

atomToFileAccess :: Atom -> Maybe FileAccess
atomToFileAccess a
  | a == atom "read" = Just Read
  | a == atom "write" = Just Write
  | a == atom "read_write" = Just ReadWrite
  | otherwise = Nothing

foreign import deleteDir_ :: String -> Effect (ReasonResult Unit)

deleteDir :: String -> Effect (Either Reason Unit)
deleteDir f = standardResultToEither <$> deleteDir_ f

foreign import deleteDirRecursive_ :: String -> Effect (ReasonResult Unit)

deleteDirRecursive :: String -> Effect (Either Reason Unit)
deleteDirRecursive f = standardResultToEither <$> deleteDirRecursive_ f
