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
    PatternFunctorRules
      ( PatternFunctorRules,
        patternCon,
        patternField,
        patternType
      ),
    defaultRules,
    extractPatternFunctor,
  )
where

-- NB: This module does not use the strict library, because its use of `Either`,
--    `Maybe`, etc. is tied to template-haskell and does not involve recursion
--     schemes.
import safe "base" Control.Applicative (Applicative (pure))
import safe "base" Control.Category (Category (id, (.)))
import safe "base" Control.Monad ((<=<))
import safe "base" Control.Monad.Fail (MonadFail (fail))
import safe "base" Data.Bifunctor (Bifunctor (bimap))
import safe "base" Data.Bool (Bool, otherwise, (&&))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq (Eq ((==)))
import safe "base" Data.Foldable (Foldable (foldl, length, null))
import safe "base" Data.Function (const, flip, ($))
import safe "base" Data.Functor (Functor (fmap), (<$>))
import safe "base" Data.Functor.Identity (Identity (Identity, runIdentity))
import safe "base" Data.List (all, zip, zip3)
import safe "base" Data.List.NonEmpty (NonEmpty)
import safe "base" Data.Maybe (Maybe (Just, Nothing), maybe)
import safe "base" Data.Semigroup (Semigroup ((<>)))
import safe "base" Data.String (String)
import safe "base" Data.Traversable (Traversable (traverse))
import safe "base" Text.Read.Lex (isSymbolChar)
import safe "base" Text.Show (Show (show))
import safe "either" Data.Either.Validation
  ( Validation (Failure, Success),
    validationToEither,
  )
import safe qualified "template-haskell" Language.Haskell.TH as TH
import safe qualified "th-abstraction" Language.Haskell.TH.Datatype as TH.Abs
import safe "this" Yaya.Fold
  ( Corecursive (ana),
    Projectable (project),
    Recursive (cata),
    Steppable (embed),
    recursiveCompare,
    recursiveCompare',
    recursiveEq,
    recursiveEq',
    recursiveShowsPrec,
    recursiveShowsPrec',
    steppableReadPrec,
    steppableReadPrec',
  )

#if MIN_VERSION_template_haskell(2, 21, 0)
type TyVarBndrUnit = TH.TyVarBndrUnit
type TyVarBndrVis = TH.TyVarBndrVis
#elif MIN_VERSION_template_haskell(2, 17, 0)
type TyVarBndrUnit = TH.TyVarBndrUnit
type TyVarBndrVis = TH.TyVarBndr ()
#else
type TyVarBndrUnit = TH.TyVarBndr
type TyVarBndrVis = TH.TyVarBndr
#endif

conP' :: TH.Name -> [TH.Pat] -> TH.Pat
#if MIN_VERSION_template_haskell(2, 18, 0)
conP' n = TH.ConP n []
#else
conP' = TH.ConP
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
extractPatternFunctor :: PatternFunctorRules -> TH.Name -> TH.Q [TH.Dec]
extractPatternFunctor rules =
  either (fail . displayUnsupportedDatatype) id . makePrimForDI rules
    <=< TH.Abs.reifyDatatype

-- | Rules of renaming data names
data PatternFunctorRules = PatternFunctorRules
  { patternType :: TH.Name -> TH.Name,
    patternCon :: TH.Name -> TH.Name,
    patternField :: TH.Name -> TH.Name
  }

-- | Default 'PatternFunctorRules': append @F@ or @$@ to data type, constructors and field names.
defaultRules :: PatternFunctorRules
defaultRules =
  PatternFunctorRules
    { patternType = toFName,
      patternCon = toFName,
      patternField = toFName
    }

toFName :: TH.Name -> TH.Name
toFName = TH.mkName . f . TH.nameBase
  where
    f name
      | isInfixName name = name <> "$"
      | otherwise = name <> "F"

    isInfixName :: String -> Bool
    isInfixName = all isSymbolChar

data UnsupportedDatatype
  = UnsupportedInstTypes (NonEmpty TH.Type)
  | UnsupportedVariant TH.Abs.DatatypeVariant
  | UnsupportedGADT [TyVarBndrUnit] TH.Cxt
  | NonBinaryInfixConstructor [(TH.Bang, TH.Type)]
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
  Either UnsupportedDatatype (TH.Q [TH.Dec])
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
      toTyVarBndr :: TH.Type -> Maybe TyVarBndrVis
      toTyVarBndr (TH.VarT n) = pure $ TH.plainTV n
      toTyVarBndr (TH.SigT (TH.VarT n) k) = pure $ TH.kindedTV n k
      toTyVarBndr _ = Nothing

deriveds :: [TH.DerivClause]
deriveds =
  pure $
    TH.DerivClause
      (pure TH.StockStrategy)
      [TH.ConT ''Foldable, TH.ConT ''Functor, TH.ConT ''Traversable]

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
  SafeDatatypeVariant -> TH.Name -> [TyVarBndrVis] -> [TH.Con] -> TH.Dec
#if MIN_VERSION_template_haskell(2, 20, 0) && MIN_VERSION_th_abstraction(0, 5, 0)
makeDataDefinition safeVariant tyName vars cons =
  case (safeVariant, cons) of
       (Newtype, [con]) -> TH.NewtypeD [] tyName vars Nothing con deriveds
       (TypeDataV, _) -> TH.TypeDataD tyName vars Nothing cons
       (_, _) -> TH.DataD [] tyName vars Nothing cons deriveds
#else
makeDataDefinition safeVariant tyName vars cons =
  case (safeVariant, cons) of
       (Newtype, [con]) -> TH.NewtypeD [] tyName vars Nothing con deriveds
       (_, _) -> TH.DataD [] tyName vars Nothing cons deriveds
#endif

makePrimForDI' ::
  PatternFunctorRules ->
  SafeDatatypeVariant ->
  TH.Name ->
  [TyVarBndrVis] ->
  [TH.Abs.ConstructorInfo] ->
  TH.Q [TH.Dec]
makePrimForDI' rules safeVariant tyName vars cons = do
  -- variable parameters
  let vars' = fmap TH.VarT (typeVars vars)
  -- Name of base functor
  let tyNameF = patternType rules tyName
  -- Recursive type
  let s = conAppsT tyName vars'
  -- Additional argument
  rName <- TH.newName "r"
  let r = TH.VarT rName

  -- Vars
  let varsF = vars <> [TH.plainTV rName]

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
        project = $(TH.LamCaseE <$> mkMorphism id (patternCon rules) cons')

      instance Steppable (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        embed = $(TH.LamCaseE <$> mkMorphism (patternCon rules) id cons')

      instance Recursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        cata φ = φ . fmap (cata φ) . project

      instance Corecursive (->) $(pure s) $(pure $ conAppsT tyNameF vars') where
        ana ψ = embed . fmap (ana ψ) . ψ
      |]

-- | makes clauses to rename constructors
mkMorphism ::
  (TH.Name -> TH.Name) ->
  (TH.Name -> TH.Name) ->
  [TH.Abs.ConstructorInfo] ->
  TH.Q [TH.Match]
mkMorphism nFrom nTo =
  traverse
    ( \ci -> do
        let n = TH.Abs.constructorName ci
        fs <- traverse (const $ TH.newName "x") $ TH.Abs.constructorFields ci
        pure $
          TH.Match
            (conP' (nFrom n) (fmap TH.VarP fs)) -- pattern
            ( TH.NormalB . foldl TH.AppE (TH.ConE $ nTo n) $
                fmap TH.VarE fs -- body
            )
            [] -- where dec
    )

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

conNameTraversal :: Traversal' TH.Abs.ConstructorInfo TH.Name
conNameTraversal = lens TH.Abs.constructorName (\s v -> s {TH.Abs.constructorName = v})

conFieldNameTraversal :: Traversal' TH.Abs.ConstructorInfo TH.Name
conFieldNameTraversal =
  lens TH.Abs.constructorVariant (\s v -> s {TH.Abs.constructorVariant = v})
    . conVariantTraversal
  where
    conVariantTraversal :: Traversal' TH.Abs.ConstructorVariant TH.Name
    conVariantTraversal _ TH.Abs.NormalConstructor =
      pure TH.Abs.NormalConstructor
    conVariantTraversal _ TH.Abs.InfixConstructor = pure TH.Abs.InfixConstructor
    conVariantTraversal f (TH.Abs.RecordConstructor fs) =
      TH.Abs.RecordConstructor <$> traverse f fs

conTypeTraversal :: Traversal' TH.Abs.ConstructorInfo TH.Type
conTypeTraversal =
  lens TH.Abs.constructorFields (\s v -> s {TH.Abs.constructorFields = v})
    . traverse

conNameMap ::
  (TH.Name -> TH.Name) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
conNameMap = over conNameTraversal

conFieldNameMap ::
  (TH.Name -> TH.Name) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
conFieldNameMap = over conFieldNameTraversal

conTypeMap ::
  (TH.Type -> TH.Type) -> TH.Abs.ConstructorInfo -> TH.Abs.ConstructorInfo
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
typeVars :: [TyVarBndrVis] -> [TH.Name]
typeVars = fmap TH.Abs.tvName

-- | Apply arguments to a type constructor.
conAppsT :: TH.Name -> [TH.Type] -> TH.Type
conAppsT conName = foldl TH.AppT (TH.ConT conName)

-- | Provides substitution for types
substType ::
  TH.Type ->
  TH.Type ->
  TH.Type ->
  TH.Type
substType a b = go
  where
    go x | x == a = b
    go (TH.VarT n) = TH.VarT n
    go (TH.AppT l r) = TH.AppT (go l) (go r)
    go (TH.ForallT xs ctx t) = TH.ForallT xs ctx (go t)
    -- This may fail with kind error
    go (TH.SigT t k) = TH.SigT (go t) k
    go (TH.InfixT l n r) = TH.InfixT (go l) n (go r)
    go (TH.UInfixT l n r) = TH.UInfixT (go l) n (go r)
    go (TH.ParensT t) = TH.ParensT (go t)
    -- Rest are unchanged
    go x = x

toCon :: TH.Abs.ConstructorInfo -> Either UnsupportedDatatype TH.Con
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
              TH.Abs.NormalConstructor ->
                pure . TH.NormalC name $ zip bangs ftys
              TH.Abs.RecordConstructor fnames ->
                pure . TH.RecC name $ zip3 fnames bangs ftys
              TH.Abs.InfixConstructor -> case zip bangs ftys of
                [bt1, bt2] -> pure $ TH.InfixC bt1 name bt2
                bts -> Left $ NonBinaryInfixConstructor bts
      else Left $ UnsupportedGADT vars ctxt
    where
      toBang (TH.Abs.FieldStrictness upkd strct) =
        TH.Bang
          (toSourceUnpackedness upkd)
          (toSourceStrictness strct)
        where
          toSourceUnpackedness :: TH.Abs.Unpackedness -> TH.SourceUnpackedness
          toSourceUnpackedness = \case
            TH.Abs.UnspecifiedUnpackedness -> TH.NoSourceUnpackedness
            TH.Abs.NoUnpack -> TH.SourceNoUnpack
            TH.Abs.Unpack -> TH.SourceUnpack

          toSourceStrictness :: TH.Abs.Strictness -> TH.SourceStrictness
          toSourceStrictness = \case
            TH.Abs.UnspecifiedStrictness -> TH.NoSourceStrictness
            TH.Abs.Lazy -> TH.SourceLazy
            TH.Abs.Strict -> TH.SourceStrict
