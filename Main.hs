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

{-# LANGUAGE CPP #-}

module Main where

#ifdef TESTING
import Test
#else
import ShortenPath
#endif


-- | Main function.
main :: IO ()
#ifdef TESTING
main = testMain
#else
main = shortenPathMain
#endif

