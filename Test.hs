-- | The Main module.
--
-- @author: Matthew Todd
-- @date: 2019-01-29
--
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

{-# LANGUAGE OverloadedStrings #-}

-- Disable the function signature check so we don't get pestered by the missing
-- signature for "tests"
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test where

--  For test framework
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2 (testProperty)
--import Test.QuickCheck
import Test.HUnit

-- Stuff being tested
import ShortenPath

-- | Main function for running unit tests
testMain :: IO ()
testMain = defaultMain tests


-- | Tests that will be run
tests = [ testGroup "Basic shortenPath Test Cases" 
            [ testCase "empty path" test_shortenPath_emptyPath
            , testCase "whitespace path" test_shortenPath_whitespacePath
            , testCase "example path" test_shortenPath_examplePath
            , testCase "absolute path" test_shortenPath_absolutePath
            , testCase "relative path" test_shortenPath_relativePath
            , testCase "path to directory" test_shortenPath_pathToDirectory
            , testCase "path to directory with slash" test_shortenPath_pathToDirectoryWithSlash
            , testCase "relative path with no dot" test_shortenPath_relativePathNoDot
            , testCase "path with whitespace" test_shortenPath_pathWithWhitespace
            , testCase "path with no directories" test_shortenPath_pathNoDirs
            , testCase "path with extra slashes" test_shortenPath_extraSlashes
            , testCase "path with uppercase characters" test_shortenPath_uppercaseCharacters
            ]
        ]

-- | Test when no input is provided
test_shortenPath_emptyPath :: Assertion
test_shortenPath_emptyPath = shortenPath "" @?= ""

-- | Test when whitespace is included in the input
test_shortenPath_whitespacePath :: Assertion
test_shortenPath_whitespacePath = shortenPath " \t\n " @?= ""

-- | Test the example path used in a bunch of the documentation
test_shortenPath_examplePath :: Assertion
test_shortenPath_examplePath = shortenPath "./a/path/to/some/file.txt" @?= "./a/p/t/s/file.txt"

-- | Test an absolute path
test_shortenPath_absolutePath :: Assertion
test_shortenPath_absolutePath = shortenPath "/an/absolute/path/to/a/file.txt" @?= "/a/a/p/t/a/file.txt"

-- | Test a relative path
test_shortenPath_relativePath :: Assertion
test_shortenPath_relativePath = shortenPath "./../dir/../dir2/file.txt" @?= "./../d/../d/file.txt"

-- | Test a path to a directory (instead of a file)
-- Note: this could be a file, or it could be a directory. Doesn't matter from our perspective.
test_shortenPath_pathToDirectory :: Assertion
test_shortenPath_pathToDirectory = shortenPath "./path/to/some/directory" @?= "./p/t/s/directory"

-- | Test a path to a directory ending in a slash
-- TODO: What behavior do we want here? Keep the full or shorten "directory?" With or without slash?
test_shortenPath_pathToDirectoryWithSlash :: Assertion
test_shortenPath_pathToDirectoryWithSlash = shortenPath "./path/to/some/directory/" @?= "./p/t/s/d/"

-- | Test a relative path with no starting '.'
test_shortenPath_relativePathNoDot :: Assertion
test_shortenPath_relativePathNoDot = shortenPath "relative/path/to/file.txt" @?= "r/p/t/file.txt"

-- | Test a path that includes whitespace in it
-- TODO: should we strip whitespace from the beginning of a directory?
test_shortenPath_pathWithWhitespace :: Assertion
test_shortenPath_pathWithWhitespace = shortenPath " ./a/ path with/white space/to/file.txt " @?= "./a/p/w/t/file.txt"

-- | Test a path with no directories in it
test_shortenPath_pathNoDirs :: Assertion
test_shortenPath_pathNoDirs = shortenPath "file.txt" @?= "file.txt"

-- | Test a path with extra slashes in it
-- TODO: do we want to add logic to strip out extra slashes?
test_shortenPath_extraSlashes :: Assertion
test_shortenPath_extraSlashes = shortenPath "./a/path//to/some///file.txt" @?= "./a/p//t/s///file.txt"

-- | Test path with upper case characters (since many other examples are entirely lowercase)
test_shortenPath_uppercaseCharacters :: Assertion
test_shortenPath_uppercaseCharacters = shortenPath "./A/pATh/TO/SoMe/file.txt" @?= "./A/p/T/S/file.txt"

