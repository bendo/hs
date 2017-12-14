#!/usr/bin/env stack
-- stack runghc --resolver lts-9.1 --install-ghc

{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq

main = do
    putStrLn "Hello Script"
