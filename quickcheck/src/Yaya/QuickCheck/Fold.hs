{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:arbitrarySteppable,shrinkSteppable #-}

module Yaya.QuickCheck.Fold
  ( arbitrarySteppable,
    shrinkSteppable,
  )
where

import safe qualified "QuickCheck" Test.QuickCheck as QC
import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Data.Foldable (Foldable)
import safe qualified "base" Data.Foldable as Foldable
import safe "base" Data.Function (flip)
import safe "base" Data.Functor (Functor, (<$>))
import safe "base" Data.Semigroup ((<>))
import safe "yaya" Yaya.Fold (Mu, Nu, Steppable, embed, project)
-- import safe "yaya" Yaya.Native.Fold (Cofix, Fix)
import safe "yaya" Yaya.Pattern
  ( AndMaybe (Indeed, Only),
    Log2 (Double, DoublePlus, One),
    XNor (Both, Neither),
  )

arbitrarySteppable ::
  (Steppable (->) t f, Functor f) => (QC.Gen t -> QC.Gen (f t)) -> QC.Gen t
arbitrarySteppable liftArbitraryF =
  embed <$> liftArbitraryF (arbitrarySteppable liftArbitraryF)

shrinkSteppable ::
  (Steppable (->) t f, Foldable f, Functor f) =>
  ((t -> [t]) -> f t -> [f t]) ->
  t ->
  [t]
shrinkSteppable liftShrinkF fix =
  let ft = project fix
   in Foldable.toList ft
        <> (embed <$> liftShrinkF (shrinkSteppable liftShrinkF) ft)

-- instance (Foldable f, Functor f, QC.Arbitrary1 f) => QC.Arbitrary (Cofix f) where
--   arbitrary = arbitrarySteppable QC.liftArbitrary
--   shrink = shrinkSteppable QC.liftShrink

-- instance (Foldable f, Functor f, QC.Arbitrary1 f) => QC.Arbitrary (Fix f) where
--   arbitrary = arbitrarySteppable QC.liftArbitrary
--   shrink = shrinkSteppable QC.liftShrink

instance (Foldable f, Functor f, QC.Arbitrary1 f) => QC.Arbitrary (Mu f) where
  arbitrary = arbitrarySteppable QC.liftArbitrary
  shrink = shrinkSteppable QC.liftShrink

instance (Foldable f, Functor f, QC.Arbitrary1 f) => QC.Arbitrary (Nu f) where
  arbitrary = arbitrarySteppable QC.liftArbitrary
  shrink = shrinkSteppable QC.liftShrink

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (XNor a b) where
  arbitrary = QC.liftArbitrary QC.arbitrary
  shrink = QC.liftShrink QC.shrink

instance (QC.Arbitrary a) => QC.Arbitrary1 (XNor a) where
  liftArbitrary = QC.liftArbitrary2 QC.arbitrary
  liftShrink = QC.liftShrink2 QC.shrink

instance QC.Arbitrary2 XNor where
  liftArbitrary2 a b = QC.frequency [(1, pure Neither), (3, Both <$> a <*> b)]
  liftShrink2 shrinkA shrinkB = \case
    Neither -> []
    Both a b -> Neither : (flip Both b <$> shrinkA a) <> (Both a <$> shrinkB b)

instance (QC.Arbitrary a, QC.Arbitrary b) => QC.Arbitrary (AndMaybe a b) where
  arbitrary = QC.liftArbitrary QC.arbitrary
  shrink = QC.liftShrink QC.shrink

instance (QC.Arbitrary a) => QC.Arbitrary1 (AndMaybe a) where
  liftArbitrary = QC.liftArbitrary2 QC.arbitrary
  liftShrink = QC.liftShrink2 QC.shrink

instance QC.Arbitrary2 AndMaybe where
  liftArbitrary2 a b = QC.frequency [(1, Only <$> a), (3, Indeed <$> a <*> b)]
  liftShrink2 shrinkA shrinkB = \case
    Only a -> Only <$> shrinkA a
    Indeed a b ->
      (Only <$> shrinkA a)
        <> (Only a : (flip Indeed b <$> shrinkA a) <> (Indeed a <$> shrinkB b))

instance (QC.Arbitrary a) => QC.Arbitrary (Log2 a) where
  arbitrary = QC.liftArbitrary QC.arbitrary
  shrink = QC.liftShrink QC.shrink

instance QC.Arbitrary1 Log2 where
  liftArbitrary a =
    QC.frequency [(1, pure One), (3, Double <$> a), (3, DoublePlus <$> a)]
  liftShrink shrinkA = \case
    One -> []
    Double a -> pure One <> (Double <$> shrinkA a)
    DoublePlus a ->
      (Double <$> shrinkA a)
        <> (Double a : (DoublePlus <$> shrinkA a))
