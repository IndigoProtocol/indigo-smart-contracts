-- SPDX-License-Identifier: BUSL-1.1

module Indigo.Utils.Utils (filterMap) where

import PlutusTx.Prelude

{-# INLINEABLE filterMap #-}
filterMap :: (b -> Bool) -> (a -> b) -> [a] -> [b]
filterMap _ _ [] = []
filterMap p f (a : as) =
  let !b = f a in if p b then b : filterMap p f as else filterMap p f as
