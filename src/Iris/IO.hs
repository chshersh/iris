{- |
Module                  : Iris.IO
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

Functions for IO, such as writing Text to stdout and stderr.


Usage example:

@
import qualified Iris

main = do
    Iris.outLn "This goes to stdout"
    Iris.errLn "This goes to stderr"
@

Results in:

@
\$ ./app
This goes to stdout
This goes to stderr
@

@since x.x.x.x
-}
module Iris.IO (
    out,
    outLn,
    err,
    errLn,
) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.IO (stderr, stdout)

{- | Write the given Text to stdout.
No linefeed at the end.

@
ghci> do Iris.out "foo" >> Iris.out "bar"
foobarghci>

@
@since x.x.x.x
-}
out :: Text -> IO ()
out = Text.hPutStr stdout

{- | Write the given Text to stdout with linefeed at the end.

@
ghci> Iris.outLn "foo" >> Iris.outLn "bar"
foo
bar
ghci>

@
@since x.x.x.x
-}
outLn :: Text -> IO ()
outLn = Text.hPutStrLn stdout

{- | Write the given Text to stderr.
No linefeed at the end.

@
ghci> Iris.err "foo" >> Iris.err "bar"
foobarghci>

@
@since x.x.x.x
-}
err :: Text -> IO ()
err = Text.hPutStr stderr

{- | Write the given Text to stderr with linefeed at the end.

@
ghci> Iris.errLn "foo" >> Iris.errLn "bar"
foo
bar
ghci>

@
@since x.x.x.x
-}
errLn :: Text -> IO ()
errLn = Text.hPutStrLn stderr
