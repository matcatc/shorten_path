-- Build rules for Shorten Path project
--
-- This build system will build the application twice, once for the main
-- binary, once for the unit-test binary. As such, we have two build
-- directories: _build and _build/test. This makes it easier to figure out what
-- files are what, and also avoid unnecessary rebuilding (due to ghc
-- overwriting files for the two binaries).
--
-- Please note that Shake doesn't handle specifying commands to create the
-- target _build and _build/test directories very well. While initially
-- developing this build script, I added targets for _build and _build/test,
-- but then later this broke the build. According to
-- https://hackage.haskell.org/package/shake-0.15.10/docs/Development-Shake.html#v:need
-- and http://shakebuild.com/faq#how-can-i-depend-on-directories, using need on
-- a directory will not work as desired. Furthermore, targets are supposed to
-- create directories automatically if they're needed.
--
-- @author: Matthew Todd
-- @date: 2019-01-29
--
-- Copyright 2019 Matthew Todd
-- 
-- This file is part of Shorten Path.
-- 
-- Shorten Path is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
-- 
-- Shorten Path is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- Shorten Path.  If not, see <http://www.gnu.org/licenses/>.

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


main :: IO ()
-- Use ChangeModtime so that a touch or any change to a file should cause it to
-- be rebuilt. This should be the default, according to the Shake
-- documentation, but updating some files wasn't being rebuilt until I manually
-- specified it.
main = shakeArgs shakeOptions{shakeFiles="_build", shakeChange=ChangeModtime} $ do
         -- Main executable
    want [ "_build/shorten_path" <.> exe
         -- TODO: bash auto-completion? (Not using CmdArgs currently)
         --, "_build/shorten_path.comp"
         -- Test executable and results
         -- unit tests
         , "_build/test/shorten_path_test" <.> exe
         , "_build/test/shorten_path_test.tix"
         , "_build/test/hpc_index.html"
         -- Docs
         , "_build/README.html"
         -- no haddock documentation currently since the module is so small
         ]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    -- Main binary
    "_build/shorten_path" <.> exe %> \out -> do
        -- hss = Haskell .hs files
        hss <- getDirectoryFiles "" ["//*.hs"]
        -- ins = input .hs files
        --   Ignore the Shake build file
        let ins = filter (/= "ShakeBuild.hs") hss
        need ins
        -- use *dir options to drop all off the created files in _build, so
        -- that they don't pollute the source tree and so we can easily clean
        -- them later
        cmd "ghc --make -hidir ./_build -odir ./_build -Wall -o " [out] ins

    -- Unittest binary
    -- TODO: refactor with the regular executable (DRY)
    "_build/test/shorten_path_test" <.> exe %> \out -> do
        -- hss = Haskell .hs files
        hss <- getDirectoryFiles "" ["//*.hs"]
        -- ins = input .hs files
        --   Ignore the Shake build file
        let ins = filter (/= "ShakeBuild.hs") hss
        need ins
        -- use *dir options to drop all off the created files in _build, so
        -- that they don't pollute the source tree and so we can easily clean
        -- them later
        cmd "ghc --make -hidir ./_build/test -odir ./_build/test -Wall -cpp -DTESTING -fhpc -hpcdir ./_build/test -o " [out] ins

    -- Run the test binary to generate the output
    --  We specify the .tix file here since that is a dependency for other
    --  rules. The .txt file is just so users can view the test output later.
    "_build/test/shorten_path_test.tix" %> \out -> do
        need ["_build/test/shorten_path_test"]
        -- Exit code indicates whether the tests passed. We assign it so that
        -- the build doesn't abruptly end if a test case fails. In case of a
        -- failure, the user will see it when the test output is printed below.
        (Exit code, Stdout result) <- cmd "_build/test/shorten_path_test --color"
        writeFile' "_build/test/test_results.txt" result
        putNormal result
        -- No way I know of yet to control where the .tix file is output, so just move it afterwards
        cmd "mv ./shorten_path_test.tix ./_build/test/"

    -- Generate code coverage results for test
    "_build/test/hpc_index.html" %> \out -> do
        need [ "_build/test/shorten_path_test"
             , "_build/test/shorten_path_test.tix"
             ]
        cmd "hpc markup ./_build/test/shorten_path_test --hpcdir ./_build/test --destdir=./_build/test/"

    -- Build the user/developer html files from asciidoc files
    "_build/*.html" %> \out -> do
        -- ascii = Asciidoc file
        let ascii = dropDirectory1 $ out -<.> "asciidoc"
        need [ascii]
        cmd "asciidoctor --out-file" [out] [ascii]

