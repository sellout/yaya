{-# LANGUAGE CPP #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 0, 0, 0)
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Trustworthy #-}
#endif

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

import safe "base" Control.Applicative (Applicative (..))
import safe "base" Control.Category (Category (..))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.Fail (MonadFail (fail))
import safe "base" Data.Bifunctor (Bifunctor (..))
import safe "base" Data.Bool (Bool (..), otherwise, (&&))
import safe "base" Data.Either (Either (..), either)
import safe "base" Data.Eq (Eq (..))
import safe "base" Data.Foldable (Foldable (..))
import safe "base" Data.Function (const, flip, ($))
import safe "base" Data.Functor (Functor (..), (<$>))
import safe "base" Data.Functor.Identity (Identity (..))
import safe "base" Data.List (all, zip, zip3)
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.Maybe (Maybe (..), maybe)
import safe "base" Data.Semigroup (Semigroup (..))
import safe "base" Data.String (String)
import safe "base" Data.Traversable (Traversable (..))
import safe "base" Text.Read.Lex (isSymbolChar)
import safe "base" Text.Show (Show (..))
import safe "either" Data.Either.Validation
  ( Validation (..),
    validationToEither,
  )
import safe "template-haskell" Language.Haskell.TH as TH
import safe "template-haskell" Language.Haskell.TH.Syntax (mkNameG_tc)
import safe qualified "th-abstraction" Language.Haskell.TH.Datatype as TH.Abs
import safe "this" Yaya.Fold
  ( Corecursive (..),
    Projectable (..),
    Recursive (..),
    Steppable (..),
    recursiveEq,
    recursiveShowsPrec,
  )

#if MIN_VERSION_template_haskell(2, 21, 0)
#elif MIN_VERSION_template_haskell(2, 17, 0)
type TyVarBndrVis = TyVarBndr ()
#else
type TyVarBndrUnit = TyVarBndr
type TyVarBndrVis = TyVarBndr
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
--   deriving stock (Show)
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
--   deriving stock ('Functor', 'Foldable', 'Traversable')
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
  either (fail . displayUnsupportedDatatype) id . makePrimForDI rules
    <=< TH.Abs.reifyDatatype

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
  | UnsupportedVariant TH.Abs.DatatypeVariant
  | UnsupportedGADT [TyVarBndrUnit] Cxt
  | NonBinaryInfixConstructor [(Bang, Type)]
  deriving stock (Show)

displayUnsupportedDatatype :: UnsupportedDatatype -> String
displayUnsupportedDatatype =
  ("extractPatternFunctor: " <>) . \case
    UnsupportedInstTypes tys ->
      "Couldn't process the following types " <> show tys
    UnsupportedVariant _variant -> "Data families are currently not supported."
    UnsupportedGADT _vars _context -> "GADTs are not currently supported."
    NonBinaryInfixConstructor bts ->
      "internal error: wrong number of BangTypes for InfixConstructor; expected 2, but got "
        <> show (length bts)

makePrimForDI ::
  PatternFunctorRules ->
  TH.Abs.DatatypeInfo ->
  Either UnsupportedDatatype (Q [Dec])
makePrimForDI
  rules
  ( TH.Abs.DatatypeInfo
      { TH.Abs.datatypeName = tyName,
        TH.Abs.datatypeInstTypes = instTys,
        TH.Abs.datatypeCons = cons,
        TH.Abs.datatypeVariant = variant
      }
    ) =
    maybe
      (Left $ UnsupportedVariant variant)
      ( \safeVariant ->
          bimap
            UnsupportedInstTypes
            (flip (makePrimForDI' rules safeVariant tyName) cons)
            . validationToEither
            $ traverse
              (\ty -> maybe (Failure $ pure ty) Success $ toTyVarBndr ty)
              instTys
      )
      $ excludeDataFamInstance variant
    where
      toTyVarBndr :: Type -> Maybe TyVarBndrVis
      toTyVarBndr (VarT n) = pure $ plainTV n
      toTyVarBndr (SigT (VarT n) k) = pure $ kindedTV n k
      toTyVarBndr _ = Nothing

deriveds :: [DerivClause]
deriveds =
  pure $
    DerivClause
      (pure StockStrategy)
      [ ConT functorTypeName,
        ConT foldableTypeName,
        ConT traversableTypeName
      ]

-- | A restricted version of `TH.Abs.DatatypeVariant` that excludes data family
--   declarations.
#if MIN_VERSION_th_abstraction(0, 5, 0)
data SafeDatatypeVariant  = Datatype  | Newtype  | TypeDataV
#else
data SafeDatatypeVariant  = Datatype  | Newtype
#endif

excludeDataFamInstance :: TH.Abs.DatatypeVariant -> Maybe SafeDatatypeVariant
#if MIN_VERSION_th_abstraction(0, 5, 0)
excludeDataFamInstance = \case
  TH.Abs.DataInstance -> Nothing
  TH.Abs.NewtypeInstance -> Nothing
  TH.Abs.Datatype -> Just Datatype
  TH.Abs.Newtype -> Just Newtype
  TH.Abs.TypeData -> Just TypeDataV
#else
excludeDataFamInstance = \case
  TH.Abs.DataInstance -> Nothing
  TH.Abs.NewtypeInstance -> Nothing
  TH.Abs.Datatype -> Just Datatype
  TH.Abs.Newtype -> Just Newtype
#endif

makeDataDefinition ::
  SafeDatatypeVariant -> Name -> [TyVarBndrVis] -> [Con] -> Dec
#if MIN_VERSION_template_haskell(2, 20, 0) && MIN_VERSION_th_abstraction(0, 5, 0)
makeDataDefinition safeVariant tyName vars cons =
  case (safeVariant, cons) of
       (Newtype, [con]) -> NewtypeD [] tyName vars Nothing con deriveds
       (TypeDataV, _) -> TypeDataD tyName vars Nothing cons
       (_, _) -> DataD [] tyName vars Nothing cons deriveds
#else
makeDataDefinition safeVariant tyName vars cons =
  case (safeVariant, cons) of
       (Newtype, [con]) -> NewtypeD [] tyName vars Nothing con deriveds
       (_, _) -> DataD [] tyName vars Nothing cons deriveds
#endif

makePrimForDI' ::
  PatternFunctorRules ->
  SafeDatatypeVariant ->
  Name ->
  [TyVarBndrVis] ->
  [TH.Abs.ConstructorInfo] ->
  Q [Dec]
makePrimForDI' rules safeVariant tyName vars cons = do
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

  -- ekmett/recursion-schemes#33
  cons' <- traverse (conTypeTraversal TH.Abs.resolveTypeSynonyms) cons
  consF <-
    either (fail . displayUnsupportedDatatype) pure $
      traverse
        ( toCon
            . conNameMap (patternCon rules)
            . conFieldNameMap (patternField rules)
            . conTypeMap (substType s r)
        )
        cons'

  (makeDataDefinition safeVariant tyNameF varsF consF :)
    <$> [d|
      instance Projectable (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        project = $(LamCaseE <$> mkMorphism id (patternCon rules) cons')

      instance Steppable (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        embed = $(LamCaseE <$> mkMorphism (patternCon rules) id cons')

      instance Recursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        cata φ = φ . fmap (cata φ) . project

      instance Corecursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        ana ψ = embed . fmap (ana ψ) . ψ
      |]

-- | makes clauses to rename constructors
mkMorphism ::
  (Name -> Name) ->
  (Name -> Name) ->
  [TH.Abs.ConstructorInfo] ->
  Q [Match]
mkMorphism nFrom nTo =
  traverse
    ( \ci -> do
        let n = TH.Abs.constructorName ci
        fs <- traverse (const $ newName "x") $ TH.Abs.constructorFields ci
        pure $
          Match
            (conP' (nFrom n) (fmap VarP fs)) -- pattern
            (NormalB $ foldl AppE (ConE $ nTo n) (fmap VarE fs)) -- body
            [] -- where dec
    )

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

conNameTraversal :: Traversal' TH.Abs.ConstructorInfo Name
conNameTraversal = lens TH.Abs.constructorName (\s v -> s {TH.Abs.constructorName = v})

conFieldNameTraversal :: Traversal' TH.Abs.ConstructorInfo Name
conFieldNameTraversal =
  lens TH.Abs.constructorVariant (\s v -> s {TH.Abs.constructorVariant = v})
    . conVariantTraversal
  where
    conVariantTraversal :: Traversal' TH.Abs.ConstructorVariant Name
    conVariantTraversal _ TH.Abs.NormalConstructor =
      pure TH.Abs.NormalConstructor
    conVariantTraversal _ TH.Abs.InfixConstructor = pure TH.Abs.InfixConstructor
    conVariantTraversal f (TH.Abs.RecordConstructor fs) =
      TH.Abs.RecordConstructor <$> traverse f fs

conTypeTraversal :: Traversal' TH.Abs.ConstructorInfo Type
conTypeTraversal =
  lens TH.Abs.constructorFields (\s v -> s {TH.Abs.constructorFields = v})
    . traverse

conNameMap :: (Name -> Name) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
conNameMap = over conNameTraversal

conFieldNameMap :: (Name -> Name) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
conFieldNameMap = over conFieldNameTraversal

conTypeMap :: (Type -> Type) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
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
typeVars :: [TyVarBndrVis] -> [Name]
typeVars = fmap TH.Abs.tvName

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

toCon :: TH.Abs.ConstructorInfo -> Either UnsupportedDatatype Con
toCon
  ( TH.Abs.ConstructorInfo
      { TH.Abs.constructorName = name,
        TH.Abs.constructorVars = vars,
        TH.Abs.constructorContext = ctxt,
        TH.Abs.constructorFields = ftys,
        TH.Abs.constructorStrictness = fstricts,
        TH.Abs.constructorVariant = variant
      }
    ) =
    if null vars && null ctxt
      then
        let bangs = fmap toBang fstricts
         in case variant of
              TH.Abs.NormalConstructor -> pure . NormalC name $ zip bangs ftys
              TH.Abs.RecordConstructor fnames ->
                pure . RecC name $ zip3 fnames bangs ftys
              TH.Abs.InfixConstructor -> case zip bangs ftys of
                [bt1, bt2] -> pure $ InfixC bt1 name bt2
                bts -> Left $ NonBinaryInfixConstructor bts
      else Left $ UnsupportedGADT vars ctxt
    where
      toBang (TH.Abs.FieldStrictness upkd strct) =
        Bang
          (toSourceUnpackedness upkd)
          (toSourceStrictness strct)
        where
          toSourceUnpackedness :: TH.Abs.Unpackedness -> SourceUnpackedness
          toSourceUnpackedness TH.Abs.UnspecifiedUnpackedness = NoSourceUnpackedness
          toSourceUnpackedness TH.Abs.NoUnpack = SourceNoUnpack
          toSourceUnpackedness TH.Abs.Unpack = SourceUnpack

          toSourceStrictness :: TH.Abs.Strictness -> SourceStrictness
          toSourceStrictness TH.Abs.UnspecifiedStrictness = NoSourceStrictness
          toSourceStrictness TH.Abs.Lazy = SourceLazy
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
