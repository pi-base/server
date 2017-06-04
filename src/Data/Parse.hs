module Data.Parse
  ( theorems
  ) where

import Import

theorems :: MonadStore m => Committish -> m (Either Error [Theorem PropertyId])
