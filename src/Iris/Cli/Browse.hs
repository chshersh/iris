{- |
Module                  : Iris.Cli.Browse
Copyright               : (c) 2022 Dmitrii Kovanikov
SPDX-License-Identifier : MPL-2.0
Maintainer              : Dmitrii Kovanikov <kovanikov@gmail.com>
Stability               : Experimental
Portability             : Portable

CLI options parsing for @--browse@ and @--browse=<FILE_PATH>@.

@since 0.0.0.0
-}

module Iris.Cli.Browse
    ( browseP
    , browseFileP
    ) where

import qualified Options.Applicative as Opt

{- | A CLI option parser yields a boolean value if a file needs to be open in
a browser.

Use 'Iris.Browse.openInBrowser' to open the file of your choice in a
browser.

@since 0.0.0.0
-}
browseP
    :: String  -- ^ Flag description
    -> Opt.Parser Bool
browseP description = Opt.switch $ mconcat
    [ Opt.long "browse"
    , Opt.help description
    ]

{- | A CLI option parser for a 'FilePath' that needs to be open with a browser.

Use 'Iris.Browse.openInBrowser' to open the passed file in a browser.

@since 0.0.0.0
-}
browseFileP
    :: String  -- ^ Flag description
    -> Opt.Parser FilePath
browseFileP description = Opt.option Opt.str $ mconcat
    [ Opt.long "browse"
    , Opt.metavar "FILE_PATH"
    , Opt.help description
    ]
