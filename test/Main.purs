module Test.Main where

import Debug.Trace
import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.Binary.UTF8 as UTF8String
import Erl.File (Reason(..), deleteFile, readFile, writeFile)
import Test.Assert (assert, assert', assertEqual)

testBinary :: Binary
testBinary = UTF8String.toBinary $ "Testing testing 123\nhello\n"

testData :: IOData
testData = IOData.fromBinary $ testBinary

main :: Effect Unit
main = do
  let tmpFile = "/tmp/erl-file-test-test-file.txt"

  log "writeFile"
  res <- writeFile tmpFile testData
  assertEqual { actual: res, expected:  Right unit }

  log "readFile"
  res1 <- readFile tmpFile
  assert $ res1 == Right testBinary

  res2 <- readFile "/tmp/junk/this-doesnt-exist"
  assert $ res2 == (Left $ Reason $ atom "enoent")

  log "deleteFile"
  res3 <- deleteFile tmpFile
  assert $ res3 == Right unit
    