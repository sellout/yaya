{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

module Yaya.Strict
  ( IsStrict,
    IsNonStrict,
    Strict,
    PartialTypeError,
    unsatisfiable,
  )
where

import "base" Data.Bool (Bool (False, True))
import "base" Data.Either (Either)
import "base" Data.Functor.Const (Const)
import "base" Data.Functor.Identity (Identity)
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.Maybe (Maybe)
import "base" Data.Type.Bool (Not)
import "base" Data.Void (Void)
import "base" Numeric.Natural (Natural)
import "comonad" Control.Comonad.Trans.Env (EnvT)
import "free" Control.Comonad.Cofree (Cofree)
import "free" Control.Monad.Trans.Free (Free, FreeF, FreeT)
import qualified "lens" Control.Lens as Lens
import qualified "strict" Data.Strict as Strict
#if MIN_VERSION_base(4, 19, 0)
import "base" GHC.TypeError
  ( Assert,
    ErrorMessage (ShowType, Text, (:$$:), (:<>:)),
    Unsatisfiable,
    unsatisfiable
  )
import "base" GHC.TypeLits (Symbol)
#else
#if MIN_VERSION_base(4, 17, 0)
import "base" GHC.TypeError
  ( Assert,
    ErrorMessage (ShowType, Text, (:$$:), (:<>:)),
    TypeError,
  )
import "base" GHC.TypeLits (Symbol)
#else
import "base" Data.Kind (Constraint)
import "base" GHC.TypeLits
  ( ErrorMessage (ShowType, Text, (:$$:), (:<>:)),
    Symbol,
    TypeError,
  )
#endif
import "base" Prelude (error)
#endif

-- | This tags types (and type constructors) with whether or not they are
--   strict.
--
--   A type is only strict when all the following conditions hold:
-- - no referenced type is non-strict
-- - no non-phantom type parameter is applied to a non-strict type
-- - all unapplied type parameters are strict (or phantom)
--
--  __FIXME__: This needs a third case (so, @`Maybe` `Bool`@) to distinguish
--            “simple” types that can be used birecursively, because they have
--             no recursive structure.
type family Strict (t :: k) :: Bool

type instance Strict (,) = 'False

type instance Strict ((,) _a) = 'False

type instance Strict (_a, _b) = 'False

type instance Strict NonEmpty = 'False

type instance Strict (NonEmpty _a) = 'False

type instance Strict Natural = 'True

type instance Strict Void = 'True

type instance Strict Lens.Const = 'False

-- | This is effectively `Strict` because it’s second type parameter is phantom.
type instance Strict (Lens.Const _a) = 'True

type instance Strict Lens.Identity = 'True

type instance Strict [] = 'False

type instance Strict [_a] = 'False

type instance Strict Const = 'False

-- | This is effectively `Strict` because it’s second type parameter is phantom.
type instance Strict (Const _a) = 'True

type instance Strict Identity = 'True

type instance Strict Either = 'False

type instance Strict (Either _a) = 'False

type instance Strict (Either _a _b) = 'False

type instance Strict Maybe = 'False

type instance Strict (Maybe _a) = 'False

type instance Strict Cofree = 'False

type instance Strict (Cofree _f) = 'False

type instance Strict (Cofree _f _a) = 'False

type instance Strict (EnvT _a _f) = 'False

type instance Strict (Free _f) = 'False

type instance Strict (Free _f _a) = 'False

type instance Strict (FreeF _f _a) = 'False

type instance Strict FreeT = 'False

type instance Strict Strict.Either = 'True

type instance Strict (Strict.Either _a) = 'True

type instance Strict (Strict.Either _a _b) = 'True

type instance Strict Strict.Maybe = 'True

type instance Strict (Strict.Maybe _a) = 'True

type instance Strict Strict.Pair = 'True

type instance Strict (Strict.Pair _a) = 'True

type family ShowStrict (b :: Bool) :: Symbol where
  ShowStrict 'True = "strict"
  ShowStrict 'False = "non-strict"

#if !MIN_VERSION_base(4, 19, 0)
type Unsatisfiable msg = TypeError msg

unsatisfiable :: a
unsatisfiable = error "unsatisfiable"
#endif

#if !MIN_VERSION_base(4, 17, 0)
type family Assert (check :: Bool) (errMsg :: Constraint) :: Constraint where
  Assert 'True _ = ()
  Assert _ errMsg = errMsg
#endif

-- | This `TypeError` is used to communicate why partial instances are
--   unavailable, and what can be done instead.
{- ORMOLU_DISABLE -}
{- because it wants to remove the space after the tick, which triggers a GHC warning -}
type PartialTypeError t =
  Unsatisfiable
    ( 'Text "‘"
        ' :<>: 'ShowType t
        ' :<>: 'Text "’ is "
        ' :<>: 'Text (ShowStrict (Strict t))
        ' :<>: 'Text ", so the instance would be partial."
        ' :$$: 'Text
                "See ‘yaya-unsafe:Yaya.Unsafe.Unsafe’ for the intended approach."
    )
{- ORMOLU_ENABLE -}

type IsStrict t = Assert (Strict t) (PartialTypeError t)

-- |
--
--  __FIXME__: This needs a different error message.
type IsNonStrict t = Assert (Not (Strict t)) (PartialTypeError t)
