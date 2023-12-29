{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | This module re-exports a subset of `Yaya.Fold`, intended for when you want
--   to define recursion scheme instances for your existing recursive types.
--
--   This is /not/ the recommended way to use Yaya, but it solves some real
--   problems:
-- 1. you have existing directly-recursive types and you want to start taking
--    advantage of recursion schemes without having to rewrite your existing
--    code, or
-- 2. a directly-recursive type has been imposed on you by some other library
--    and you want to take advantage of recursion schemes.
--
--   The distinction between these two cases is whether you have control of the
--   @data@ declaration. In the first case, you probably do. In that case, you
--   should only generate the /safe/ instances, and ensure that all the
--   recursive type references are /strict/ (if you want a `Recursive`
--   instance). If you don't have control, then you /may/ need to generate all
--   instances.
--
--   Another difference when you have control is that it means you may migrate
--   away from direct recursion entirely, at which point this import should
--   disappear.
module Yaya.Retrofit
  ( module Yaya.Fold,
    PatternFunctorRules (..),
    defaultRules,
    extractPatternFunctor,
  )
where

-- NB: This module does not use the strict library, because its use of `Either`,
--    `Maybe`, etc. is tied to emplate-haskell and does not involve recursion
--     schemes.

import "base" Control.Applicative (Applicative (..))
import "base" Control.Category (Category (..))
import "base" Control.Exception (Exception (..), throw)
import "base" Control.Monad ((<=<))
import "base" Data.Bifunctor (Bifunctor (..))
import "base" Data.Bool (Bool (..), not, otherwise, (&&))
import "base" Data.Either (Either (..), either)
import "base" Data.Eq (Eq (..))
import "base" Data.Foldable (Foldable (..))
import "base" Data.Function (const, flip, ($))
import "base" Data.Functor (Functor (..), (<$>))
import "base" Data.Functor.Identity (Identity (..))
import "base" Data.List (all, null, zip, zip3)
import "base" Data.List.NonEmpty (NonEmpty)
import "base" Data.Maybe (Maybe (..), maybe)
import "base" Data.Semigroup (Semigroup (..))
import "base" Data.String (String)
import "base" Data.Traversable (Traversable (..))
import "base" Text.Read.Lex (isSymbolChar)
import "base" Text.Show (Show (..))
import "either" Data.Either.Validation (Validation (..), validationToEither)
import "template-haskell" Language.Haskell.TH as TH
import "template-haskell" Language.Haskell.TH.Syntax (mkNameG_tc)
import "th-abstraction" Language.Haskell.TH.Datatype as TH.Abs
import "this" Yaya.Fold
  ( Corecursive (..),
    Projectable (..),
    Recursive (..),
    Steppable (..),
    recursiveEq,
    recursiveShowsPrec,
  )
import "base" Prelude (error)

#if MIN_VERSION_template_haskell(2, 21, 0)
type TyVarBndr' = TyVarBndr BndrVis
#elif MIN_VERSION_template_haskell(2, 17, 0)
type TyVarBndr' = TyVarBndr ()
#else
type TyVarBndr' = TyVarBndr
#endif

conP' :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2, 18, 0)
conP' n = ConP n []
#else
conP' = ConP
#endif

-- | Extract a pattern functor and relevant instances from a simply recursive type.
--
-- /e.g./
--
-- @
-- data Expr a
--     = Lit a
--     | Add (Expr a) (Expr a)
--     | Expr a :* [Expr a]
--   deriving (Show)
--
-- `extractPatternFunctor` `defaultRules` ''Expr
-- @
--
-- will create
--
-- @
-- data ExprF a x
--     = LitF a
--     | AddF x x
--     | x :*$ [x]
--   deriving ('Functor', 'Foldable', 'Traversable')
--
-- instance `Projectable` (->) (Expr a) (ExprF a) where
--   `project` (Lit x)   = LitF x
--   `project` (Add x y) = AddF x y
--   `project` (x :* y)  = x :*$ y
--
-- instance `Steppable` (->) (Expr a) (ExprF a) where
--   `embed` (LitF x)   = Lit x
--   `embed` (AddF x y) = Add x y
--   `embed` (x :*$ y)  = x :* y
--
-- instance `Recursive` (->) (Expr a) (ExprF a) where
--   `cata` φ = φ . `fmap` (`cata` φ) . `project`
--
-- instance `Corecursive` (->) (Expr a) (ExprF a) where
--   `ana` ψ = `embed` . `fmap` (`ana` ψ) . ψ
-- @
--
-- /Notes:/
--
-- - `extractPatternFunctor` works properly only with ADTs.
--   Existentials and GADTs aren't supported,
--   as we don't try to do better than
--   <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#deriving-functor-instances GHC's DeriveFunctor>.
-- - we always generate both `Recursive` and `Corecursive` instances, but one of these is always unsafe.
--   In future, we should check the strictness of the recursive parameter and generate only the appropriate one (unless overridden by a rule).
extractPatternFunctor :: PatternFunctorRules -> Name -> Q [Dec]
extractPatternFunctor rules =
  either throw id . makePrimForDI rules <=< reifyDatatype

-- | Rules of renaming data names
data PatternFunctorRules = PatternFunctorRules
  { patternType :: Name -> Name,
    patternCon :: Name -> Name,
    patternField :: Name -> Name
  }

-- | Default 'PatternFunctorRules': append @F@ or @$@ to data type, constructors and field names.
defaultRules :: PatternFunctorRules
defaultRules =
  PatternFunctorRules
    { patternType = toFName,
      patternCon = toFName,
      patternField = toFName
    }

toFName :: Name -> Name
toFName = mkName . f . nameBase
  where
    f name
      | isInfixName name = name <> "$"
      | otherwise = name <> "F"

    isInfixName :: String -> Bool
    isInfixName = all isSymbolChar

data UnsupportedDatatype
  = UnsupportedInstTypes (NonEmpty Type)
  | UnsupportedVariant DatatypeVariant

instance Show UnsupportedDatatype where
  show = \case
    UnsupportedInstTypes tys ->
      "extractPatternFunctor: Couldn't process the following types " <> show tys
    UnsupportedVariant _variant ->
      "extractPatternFunctor: Data families are currently not supported."

instance Exception UnsupportedDatatype

makePrimForDI ::
  PatternFunctorRules -> DatatypeInfo -> Either UnsupportedDatatype (Q [Dec])
makePrimForDI
  rules
  ( DatatypeInfo
      { datatypeName = tyName,
        datatypeInstTypes = instTys,
        datatypeCons = cons,
        datatypeVariant = variant
      }
    ) =
    if isDataFamInstance
      then Left $ UnsupportedVariant variant
      else
        bimap
          UnsupportedInstTypes
          (flip (makePrimForDI' rules (variant == Newtype) tyName) cons)
          . validationToEither
          $ traverse (\ty -> maybe (Failure $ pure ty) Success $ toTyVarBndr ty) instTys
    where
      isDataFamInstance = case variant of
        DataInstance -> True
        NewtypeInstance -> True
        Datatype -> False
        Newtype -> False

      toTyVarBndr :: Type -> Maybe TyVarBndr'
      toTyVarBndr (VarT n) = pure $ plainTV n
      toTyVarBndr (SigT (VarT n) k) = pure $ kindedTV n k
      toTyVarBndr _ = Nothing

-- TH 2.12.O means GHC 8.2.1, otherwise, we work back to GHC 8.0.1
#if MIN_VERSION_template_haskell(2, 12, 0)
deriveds :: [DerivClause]
deriveds =
  pure $
    DerivClause
      Nothing
      [ ConT functorTypeName,
        ConT foldableTypeName,
        ConT traversableTypeName
      ]
#else
deriveds :: [TH.Type]
deriveds =
  [ ConT functorTypeName,
    ConT foldableTypeName,
    ConT traversableTypeName
  ]
#endif

makePrimForDI' ::
  PatternFunctorRules -> Bool -> Name -> [TyVarBndr'] -> [ConstructorInfo] -> Q [Dec]
makePrimForDI' rules isNewtype tyName vars cons = do
  -- variable parameters
  let vars' = fmap VarT (typeVars vars)
  -- Name of base functor
  let tyNameF = patternType rules tyName
  -- Recursive type
  let s = conAppsT tyName vars'
  -- Additional argument
  rName <- newName "r"
  let r = VarT rName

  -- Vars
  let varsF = vars <> [plainTV rName]

  -- #33
  cons' <- traverse (conTypeTraversal resolveTypeSynonyms) cons
  let consF =
        toCon
          . conNameMap (patternCon rules)
          . conFieldNameMap (patternField rules)
          . conTypeMap (substType s r)
          <$> cons'

  -- Data definition
  let dataDec = case consF of
        [conF]
          | isNewtype -> NewtypeD [] tyNameF varsF Nothing conF deriveds
        _ -> DataD [] tyNameF varsF Nothing consF deriveds

  recursiveDec <-
    [d|
      instance Projectable (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        project = $(LamCaseE <$> mkMorphism id (patternCon rules) cons')

      instance Steppable (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        embed = $(LamCaseE <$> mkMorphism (patternCon rules) id cons')

      instance Recursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        cata φ = φ . fmap (cata φ) . project

      instance Corecursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        ana ψ = embed . fmap (ana ψ) . ψ
      |]
  -- Combine
  pure ([dataDec] <> recursiveDec)

-- | makes clauses to rename constructors
mkMorphism ::
  (Name -> Name) ->
  (Name -> Name) ->
  [ConstructorInfo] ->
  Q [Match]
mkMorphism nFrom nTo =
  traverse
    ( \ci -> do
        let n = constructorName ci
        fs <- traverse (const $ newName "x") $ constructorFields ci
        pure $
          Match
            (conP' (nFrom n) (fmap VarP fs)) -- pattern
            (NormalB $ foldl AppE (ConE $ nTo n) (fmap VarE fs)) -- body
            [] -- where dec
    )

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

conNameTraversal :: Traversal' ConstructorInfo Name
conNameTraversal = lens constructorName (\s v -> s {constructorName = v})

conFieldNameTraversal :: Traversal' ConstructorInfo Name
conFieldNameTraversal =
  lens constructorVariant (\s v -> s {constructorVariant = v})
    . conVariantTraversal
  where
    conVariantTraversal :: Traversal' ConstructorVariant Name
    conVariantTraversal _ NormalConstructor = pure NormalConstructor
    conVariantTraversal _ InfixConstructor = pure InfixConstructor
    conVariantTraversal f (RecordConstructor fs) = RecordConstructor <$> traverse f fs

conTypeTraversal :: Traversal' ConstructorInfo Type
conTypeTraversal =
  lens constructorFields (\s v -> s {constructorFields = v})
    . traverse

conNameMap :: (Name -> Name) -> ConstructorInfo -> ConstructorInfo
conNameMap = over conNameTraversal

conFieldNameMap :: (Name -> Name) -> ConstructorInfo -> ConstructorInfo
conFieldNameMap = over conFieldNameTraversal

conTypeMap :: (Type -> Type) -> ConstructorInfo -> ConstructorInfo
conTypeMap = over conTypeTraversal

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

type Lens' s a = forall f. (Functor f) => (a -> f a) -> s -> f s

type Traversal' s a = forall f. (Applicative f) => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens sa sas afa s = sas s <$> afa (sa s)
{-# INLINE lens #-}

over :: Traversal' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

-------------------------------------------------------------------------------
-- Type mangling
-------------------------------------------------------------------------------

-- | Extract type variables
typeVars :: [TyVarBndr'] -> [Name]
typeVars = fmap tvName

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)

-- | Provides substitution for types
substType ::
  Type ->
  Type ->
  Type ->
  Type
substType a b = go
  where
    go x | x == a = b
    go (VarT n) = VarT n
    go (AppT l r) = AppT (go l) (go r)
    go (ForallT xs ctx t) = ForallT xs ctx (go t)
    -- This may fail with kind error
    go (SigT t k) = SigT (go t) k
    go (InfixT l n r) = InfixT (go l) n (go r)
    go (UInfixT l n r) = UInfixT (go l) n (go r)
    go (ParensT t) = ParensT (go t)
    -- Rest are unchanged
    go x = x

toCon :: ConstructorInfo -> Con
toCon
  ( ConstructorInfo
      { constructorName = name,
        constructorVars = vars,
        constructorContext = ctxt,
        constructorFields = ftys,
        constructorStrictness = fstricts,
        constructorVariant = variant
      }
    )
    | not (null vars && null ctxt) =
        error "makeBaseFunctor: GADTs are not currently supported."
    | otherwise =
        let bangs = fmap toBang fstricts
         in case variant of
              NormalConstructor -> NormalC name $ zip bangs ftys
              RecordConstructor fnames -> RecC name $ zip3 fnames bangs ftys
              InfixConstructor ->
                let [bang1, bang2] = bangs
                    [fty1, fty2] = ftys
                 in InfixC (bang1, fty1) name (bang2, fty2)
    where
      toBang (FieldStrictness upkd strct) =
        Bang
          (toSourceUnpackedness upkd)
          (toSourceStrictness strct)
        where
          toSourceUnpackedness :: Unpackedness -> SourceUnpackedness
          toSourceUnpackedness UnspecifiedUnpackedness = NoSourceUnpackedness
          toSourceUnpackedness NoUnpack = SourceNoUnpack
          toSourceUnpackedness Unpack = SourceUnpack

          toSourceStrictness :: Strictness -> SourceStrictness
          toSourceStrictness UnspecifiedStrictness = NoSourceStrictness
          toSourceStrictness Lazy = SourceLazy
          toSourceStrictness TH.Abs.Strict = SourceStrict

-------------------------------------------------------------------------------
-- Manually quoted names
-------------------------------------------------------------------------------
-- By manually generating these names we avoid needing to use the
-- TemplateHaskell language extension when compiling this library.
-- This allows the library to be used in stage1 cross-compilers.

functorTypeName :: Name
functorTypeName = mkNameG_tc "base" "GHC.Base" "Functor"

foldableTypeName :: Name
foldableTypeName = mkNameG_tc "base" "Data.Foldable" "Foldable"

traversableTypeName :: Name
traversableTypeName = mkNameG_tc "base" "Data.Traversable" "Traversable"
