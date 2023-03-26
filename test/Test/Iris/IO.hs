module Test.Iris.IO (ioSpec)
where

import Test.Hspec (Spec, describe, it, shouldBe)

-- Silently has side effect: writes file to tmp (or current dir) and then deletes it.

import System.IO (stderr, stdout)
import System.IO.Silently (hCapture_, hSilence)

import qualified Iris.IO as IO (err, errLn, out, outLn)

checkStdErr :: IO a -> IO String
checkStdErr = hCapture_ [stderr] . hSilence [stdout]

checkStdOut :: IO a -> IO String
checkStdOut = hCapture_ [stdout] . hSilence [stderr]

ioSpec :: Spec
ioSpec =
    describe "IO" $ do
        describe "out" $ do
            -- we need `flip` to get the expectation reported correctly in case of a failure
            it "writes to stdout, no LF " $ checkStdOut (IO.out "TEXT") >>= flip shouldBe "TEXT"
            it "does not write to stderr " $ checkStdErr (IO.out "TEXT") >>= flip shouldBe ""
        describe "outLn" $ do
            it "writes to stdout, LF " $ checkStdOut (IO.outLn "TEXT") >>= flip shouldBe "TEXT\n"
            it "does not write to stderr " $ checkStdErr (IO.outLn "TEXT") >>= flip shouldBe ""
        describe "err" $ do
            it "writes to sterr, no LF " $ checkStdErr (IO.err "TEXT") >>= flip shouldBe "TEXT"
            it "does not write to stdout " $ checkStdOut (IO.err "TEXT") >>= flip shouldBe ""
        describe "errLn" $ do
            it "writes to stderr, LF " $ checkStdErr (IO.errLn "TEXT") >>= flip shouldBe "TEXT\n"
            it "does not write to stdout " $ checkStdOut (IO.errLn "TEXT") >>= flip shouldBe ""
