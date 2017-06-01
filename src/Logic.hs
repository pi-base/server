module Logic
  ( deduceTraits
  ) where

import Core
import Viewer

deduceTraits :: Viewer -> (Viewer, [(Trait Space Property, [Assumption])])
deduceTraits Viewer{..} = error "deduceTraits"
