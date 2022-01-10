module Test.Main where

import Prelude
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Erl.Atom (atom)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData)
import Erl.Data.Binary.IOData as IOData
import Erl.Data.Binary.UTF8 as UTF8String
import Erl.Data.List as List
import Erl.File (Reason(..), deleteDir, deleteDirRecursive, deleteFile, listDir, readFile, writeFile)
import Erl.FileLib (ensureDir)
import Test.Assert (assert, assertEqual)

testBinary :: Binary
testBinary = UTF8String.toBinary $ "Testing testing 123\nhello\n"

testData :: IOData
testData = IOData.fromBinary $ testBinary

main :: Effect Unit
main = do
  let tmpFile = "/tmp/erl-file-test-test-file.txt"
  log "writeFile"
  res <- writeFile tmpFile testData
  assertEqual { actual: res, expected: Right unit }
  log "readFile"
  res1 <- readFile tmpFile
  assert $ res1 == Right testBinary
  res2 <- readFile "/tmp/junk/this-doesnt-exist"
  assert $ res2 == (Left $ Reason $ atom "enoent")
  log "deleteFile"
  res3 <- deleteFile tmpFile
  assert $ res3 == Right unit
  log "create directories"
  res4 <- ensureDir "/tmp/erl-file-test/a/b/c/somefile"
  assert $ res4 == Right unit
  log "delete dir recursively"
  res5 <- deleteDirRecursive "/tmp/erl-file-test/a/b"
  assert $ res5 == Right unit
  log "check that we stopped in the right place"
  res6 <- listDir "/tmp/erl-file-test/a"
  assert $ res6 == Right List.nil
  log "delete a single dir"
  res7 <- deleteDir "/tmp/erl-file-test/a"
  assert $ res7 == Right unit
  log "check that it's not there"
  res8 <- listDir "/tmp/erl-file-test/a"
  assert $ res8 == (Left $ Reason $ atom "enoent")
  void <- deleteDirRecursive "/tmp/erl-file-test"
