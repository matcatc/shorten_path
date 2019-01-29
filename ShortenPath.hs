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

module ShortenPath where

-- Use Lazy Data.Text so we can interactive "REPL-like" behavior
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import System.Environment (getArgs)


-- | Shorten a single element in the path. Basically head with customized
-- behavior.
shortenElement :: T.Text -> T.Text
-- Error cases
shortenElement "" = ""
-- Special directories
shortenElement "." = "."
shortenElement ".." = ".."
-- Generic cases
shortenElement x = T.singleton $ T.head $ T.strip x


-- | Split a path up into its individual parts
splitPath :: T.Text -> [T.Text]
splitPath = T.splitOn "/"

-- |
-- Take a filepath (string) and shorten it. Like how vim does it when
-- displaying filepaths in tabs.
-- 
-- Example: "./a/path/to/some/file.txt" -> "./a/p/t/s/file.txt"
shortenPath :: T.Text -> T.Text
shortenPath path = T.intercalate "/" $ shortenedDirs ++ unshortenedItem
    where
        -- parts / directories of the path
        -- Example: [".", "a", "path", "to", "some", "file.txt"]
        parts :: [T.Text]
        parts = splitPath $ T.strip path

        -- shortened directories in path
        -- Example: [".", "a", "p", "t", "s"]
        shortenedDirs :: [T.Text]
        shortenedDirs = map shortenElement $ init parts

        -- Last item (usually file, but doesn't matter) which shouldn't be
        -- shortened.
        -- Example: ["file.txt"]
        unshortenedItem :: [T.Text]
        unshortenedItem = [last parts]


-- | Main function for the real behavior
shortenPathMain :: IO ()
shortenPathMain = do
        argv <- getArgs
        let numArgs = length argv in
            if numArgs == 0 then
                -- use lines and unlines to print each line back to the user
                -- after it is entered for REPL-like behavior. Requires that we
                -- use Data.Text.Lazy
                T.interact $ T.unlines . map shortenPath . T.lines
            else if numArgs == 1 then
                T.putStrLn $ shortenPath $ T.pack $ head argv
            else
                T.putStrLn "too many arguments provided"

