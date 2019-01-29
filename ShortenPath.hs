-- | The Main module.
--
-- @author: Matthew Todd
-- @date: 2019-01-29
--
--

{-# LANGUAGE CPP #-}

module Main where


-- | Main function for running unit tests
testMain :: IO ()
testMain = undefined

-- | Main function for the real behavior
shortenPathMain :: IO ()
shortenPathMain = undefined


-- | Main function.
main :: IO ()
#ifdef TESTING
main = testMain
#else
main = shortenPathMain
#endif

