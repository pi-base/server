{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Data.Structure
  ( LensFor(..)
  , HKD
  , getLenses
  , validate
  ) where

import Protolude

import Control.Lens.Combinators (Lens', iso)

-- From https://reasonablypolymorphic.com/blog/free-lenses/

type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data LensFor s a = LensFor
  { getLensFor :: Lens' s a
  }

class GLenses z i o where
  glenses :: Lens' (z Identity) (i p) -> o p

instance GLenses z (K1 _x a) (K1 _x (LensFor (z Identity) a)) where
  glenses l = K1 $ LensFor $ \f -> l $ fmap K1 . f . unK1
  {-# INLINE glenses #-}

instance (GLenses z i o) => GLenses z (M1 _a _b i) (M1 _a _b o) where
  glenses l = M1 $ glenses $ \f -> l $ fmap M1 . f . unM1
  {-# INLINE glenses #-}

instance (GLenses z i o, GLenses z i' o') => GLenses z (i :*: i') (o :*: o') where
  glenses l = glenses (\f -> l (\(a :*: b) -> fmap (:*: b) $ f a))
          :*: glenses (\f -> l (\(a :*: b) -> fmap (a :*:) $ f b))
  {-# INLINE glenses #-}

instance GLenses z U1 U1 where
  glenses _ = U1

getLenses :: forall z. ( Generic (z Identity)
                       , Generic (z (LensFor (z Identity)))
                       , GLenses z (Rep (z Identity)) (Rep (z (LensFor (z Identity)))))
                       => z (LensFor (z Identity))
getLenses = to $ glenses @z $ iso from to

class GValidate i o where
  gvalidate :: i p -> Maybe (o p)

instance GValidate (K1 a (Maybe k)) (K1 a k) where
  gvalidate (K1 k) = K1 <$> k
  {-# INLINE gvalidate #-}

instance (GValidate i o, GValidate i' o') => GValidate (i :*: i') (o :*: o') where
  gvalidate (l :*: r) = (:*:)
                    <$> gvalidate l
                    <*> gvalidate r
  {-# INLINE gvalidate #-}

instance (GValidate i o, GValidate i' o') => GValidate (i :+: i') (o :+: o') where
  gvalidate (L1 l) = L1 <$> gvalidate l
  gvalidate (R1 r) = R1 <$> gvalidate r
  {-# INLINE gvalidate #-}

instance GValidate i o => GValidate (M1 _a _b i) (M1 _a' _b' o) where
  gvalidate (M1 x) = M1 <$> gvalidate x
  {-# INLINE gvalidate #-}

instance GValidate U1 U1 where
  gvalidate U1 = Just U1
  {-# INLINE gvalidate #-}

validate :: ( Generic (f Maybe)
            , Generic (f Identity)
            , GValidate (Rep (f Maybe))
                        (Rep (f Identity))
            )
         => f Maybe
         -> Maybe (f Identity)
validate = fmap to . gvalidate . from