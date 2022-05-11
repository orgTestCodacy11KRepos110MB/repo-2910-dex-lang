-- Copyright 2022 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# LANGUAGE PartialTypeSignatures #-}

module TraverseContexts (HasContexts, gatherContexts, addContextIds) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer
import LabeledItems
import Name
import Syntax
import Err

-- traverseUExprCtxs :: UExpr -

-- (a -> a) -> (a -> m a)

-- Naming: replace "Contexts" with "SourceInfo"

class HasContexts a where
  traverseContexts :: Applicative m => (SrcPosCtx -> m SrcPosCtx) -> a -> m a

tc :: HasContexts a => Applicative m => (SrcPosCtx -> m SrcPosCtx) -> a -> m a
tc = traverseContexts

instance HasContexts (UExpr' n) where
  traverseContexts f uexpr = case uexpr of
    UVar _ -> pure uexpr
    ULam e -> ULam <$> tc f e
    UPi e -> UPi <$> tc f e
    UApp callee arg -> UApp <$> tc f callee <*> tc f arg
    UTabLam e -> UTabLam <$> tc f e
    UTabPi e -> UTabPi <$> tc f e
    UTabApp callee arg -> UTabApp <$> tc f callee <*> tc f arg
    UDecl e -> UDecl <$> tc f e
    UFor direction e -> UFor direction <$> tc f e
    UCase e alts -> UCase <$> tc f e <*> tc f alts
    UHole -> pure uexpr
    UIndexType e -> UIndexType <$> tc f e
    UTypeAnn e ty -> UTypeAnn <$> tc f e <*> tc f ty
    UTabCon xs -> UTabCon <$> tc f xs
    UIndexRange low high -> UIndexRange <$> tc f low <*> tc f high
    UPrimExpr e -> UPrimExpr <$> tc f e
    ULabel _ -> pure uexpr
    URecord rows -> URecord <$> tc f rows
    UVariant li s e -> UVariant li s <$> tc f e
    UVariantLift li e -> UVariantLift li <$> tc f e
    ULabeledRow xs -> ULabeledRow <$> tc f xs
    URecordTy xs -> URecordTy <$> tc f xs
    UVariantTy items -> UVariantTy <$> tc f items
    UNatLit _ -> pure uexpr
    UIntLit _ -> pure uexpr
    UFloatLit _ -> pure uexpr

instance (HasContexts a, HasContexts b) => HasContexts (ExtLabeledItems a b) where
-- instance (HasContexts a) => (HasContexts b) => HasContexts (ExtLabeledItems a b) where
  traverseContexts f (Ext li rest) = Ext <$> tc f li <*> traverseContexts f rest

instance HasContexts (UFieldRowElem n) where
  traverseContexts f element = case element of
    UStaticField s e -> UStaticField s <$> tc f e
    UDynField s e -> UDynField s <$> tc f e
    UDynFields e -> UDynFields <$> tc f e

instance HasContexts (ULamExpr n) where
  traverseContexts f (ULamExpr arr pat body) =
    ULamExpr arr <$> tc f pat <*> tc f body

instance (Ord a, HasContexts a) => HasContexts (S.Set a) where
  traverseContexts f s = S.fromList <$> tc f (S.toList s)

instance HasContexts (e l) => HasContexts (EffectP e l) where
  traverseContexts _ eff = case eff of
    RWSEffect _ _ -> pure eff
    ExceptionEffect -> pure eff
    IOEffect -> pure eff

instance HasContexts (UEffectRow l) where
  traverseContexts f (EffectRow effs t) = EffectRow <$> tc f effs <*> tc f t

instance HasContexts (UPiExpr n) where
  traverseContexts f (UPiExpr arr pat eff ty) =
    UPiExpr arr <$> tc f pat <*> tc f eff <*> tc f ty

instance HasContexts (UTabLamExpr n) where
  traverseContexts f (UTabLamExpr pat body) =
    UTabLamExpr <$> tc f pat <*> tc f body

instance HasContexts (UTabPiExpr n) where
  traverseContexts f (UTabPiExpr pat body) =
    UTabPiExpr <$> tc f pat <*> tc f body

instance HasContexts (UDeclExpr n) where
  traverseContexts f (UDeclExpr decl body) =
    UDeclExpr <$> tc f decl <*> tc f body

instance HasContexts (UForExpr n) where
  traverseContexts f (UForExpr binder body) =
    UForExpr <$> tc f binder <*> tc f body

instance HasContexts (UAlt n) where
  traverseContexts f (UAlt pat body) =
    UAlt <$> tc f pat <*> tc f body

instance HasContexts e => HasContexts (PrimExpr e) where
  traverseContexts f expr = case expr of
    TCExpr con -> TCExpr <$> tc f con
    ConExpr con -> ConExpr <$> tc f con
    OpExpr e -> OpExpr <$> tc f e
    HofExpr e -> HofExpr <$> tc f e

instance HasContexts e => HasContexts (PrimTC e) where
  traverseContexts f con = case con of
    BaseType _ -> pure con
    ProdType es -> ProdType <$> tc f es
    SumType es -> SumType <$> tc f es
    Fin e -> Fin <$> tc f e
    IndexRange e li li' -> IndexRange <$> tc f e <*> tc f li <*> tc f li'
    RefType e e' -> RefType <$> tc f e <*> tc f e'
    TypeKind -> pure con
    EffectRowKind -> pure con
    LabeledRowKindTC -> pure con
    LabelType -> pure con
    IxTypeKind -> pure con

instance HasContexts e => HasContexts (PrimCon e) where
  traverseContexts f con = case con of
    Lit _ -> pure con
    ProdCon es -> ProdCon <$> tc f es
    SumCon e n e' -> SumCon <$> tc f e <*> pure n <*> tc f e'
    -- ClassDictHole ctx e -> ClassDictHole <$> f ctx <*> tc f e
    SumAsProd e e' xss -> SumAsProd <$> tc f e <*> tc f e' <*> tc f xss
    -- IntRangeVal e e' e'' -> IntRangeVal <$> tc f e <*> tc f e' <*> tc f e''
    FinVal e e' -> FinVal <$> tc f e <*> tc f e'
    IndexRangeVal e li li' e' -> IndexRangeVal <$> tc f e <*> tc f li <*> tc f li' <*> tc f e'
    -- IndexSliceVal e e' e'' -> IndexSliceVal <$> tc f e <*> tc f e' <*> tc f e''
    BaseTypeRef e -> BaseTypeRef <$> tc f e
    TabRef e -> TabRef <$> tc f e
    ConRef pc' -> ConRef <$> tc f pc'
    RecordRef xs -> RecordRef <$> tc f xs
    -- ParIndexCon e e' -> ParIndexCon <$> tc f e <*> tc f e'
    LabelCon _ -> pure con
    ExplicitDict e e' -> ExplicitDict <$> tc f e <*> tc f e'
    DictHole ctx e -> DictHole ctx <$> tc f e

instance HasContexts e => HasContexts (PrimOp e) where
  traverseContexts f expr = case expr of
    TabCon e es -> TabCon <$> tc f e <*> tc f es
    -- ScalarBinOp op e e' -> ScalarBinOp op <$> tc f e <*> tc f e'
    -- ScalarUnOp op e -> ScalarUnOp op <$> tc f e
    BinOp op e e' -> BinOp op <$> tc f e <*> tc f e'
    UnOp op e -> UnOp op <$> tc f e
    Select e e' e'' -> Select <$> tc f e <*> tc f e' <*> tc f e''
    PrimEffect e pe -> PrimEffect <$> tc f e <*> tc f pe
    IndexRef e e' -> IndexRef <$> tc f e <*> tc f e'
    ProjRef n e -> ProjRef n <$> tc f e
    Inject e -> Inject <$> tc f e
    -- SliceOffset e e' -> SliceOffset <$> tc f e <*> tc f e'
    -- SliceCurry e e' -> SliceCurry <$> tc f e <*> tc f e'
    IOAlloc ty e -> IOAlloc ty <$> tc f e
    IOFree e -> IOFree <$> tc f e
    PtrOffset e e' -> PtrOffset <$> tc f e <*> tc f e'
    PtrLoad e -> PtrLoad <$> tc f e
    PtrStore e e' -> PtrStore <$> tc f e <*> tc f e'
    AllocDest e -> AllocDest <$> tc f e
    Place e e' -> Place <$> tc f e <*> tc f e'
    Freeze e -> Freeze <$> tc f e
    -- VectorBinOp op e e' -> VectorBinOp op <$> tc f e <*> tc f e'
    -- VectorPack es -> VectorPack <$> tc f es
    -- VectorIndex e e' -> VectorIndex <$> tc f e <*> tc f e'
    -- UnsafeFromOrdinal e e' -> UnsafeFromOrdinal <$> tc f e <*> tc f e'
    -- ToOrdinal e -> ToOrdinal <$> tc f e
    -- IdxSetSize e -> IdxSetSize <$> tc f e
    ThrowError e -> ThrowError <$> tc f e
    ThrowException e -> ThrowException <$> tc f e
    CastOp e e' -> CastOp <$> tc f e <*> tc f e'
    RecordCons e e' -> RecordCons <$> tc f e <*> tc f e'
    RecordSplit e e' -> RecordSplit <$> tc f e <*> tc f e'
    RecordConsDynamic e e' e'' -> RecordConsDynamic <$> tc f e <*> tc f e' <*> tc f e''
    RecordSplitDynamic e e' -> RecordSplitDynamic <$> tc f e <*> tc f e'
    VariantLift li e -> VariantLift li <$> tc f e
    VariantSplit li e -> VariantSplit li <$> tc f e
    DataConTag e -> DataConTag <$> tc f e
    ToEnum e e' -> ToEnum <$> tc f e <*> tc f e'
    SumToVariant e -> SumToVariant <$> tc f e
    OutputStreamPtr -> pure expr
    ProjMethod e n -> ProjMethod <$> tc f e <*> pure n
    ExplicitApply e e' -> ExplicitApply <$> tc f e <*> tc f e'
    MonoLiteral e -> MonoLiteral <$> tc f e

instance HasContexts e => HasContexts (PrimEffect e) where
  traverseContexts f eff = case eff of
    MAsk -> pure eff
    MExtend monoid e -> MExtend monoid <$> tc f e
    MGet -> pure eff
    MPut e -> MPut <$> tc f e

instance HasContexts e => HasContexts (PrimHof e) where
  traverseContexts f expr = case expr of
    For ann ix e -> For ann <$> tc f ix <*> tc f e
    Seq direction ix carryDests body -> Seq direction <$> tc f ix <*> tc f carryDests <*> tc f body
    -- Tile n e e' -> Tile n <$> tc f e <*> tc f e'
    While e -> While <$> tc f e
    RunReader e e' -> RunReader <$> tc f e <*> tc f e'
    RunWriter monoid e -> RunWriter monoid <$> tc f e
    RunState e e' -> RunState <$> tc f e <*> tc f e'
    RunIO e -> RunIO <$> tc f e
    CatchException e -> CatchException <$> tc f e
    Linearize e -> Linearize <$> tc f e
    Transpose e -> Transpose <$> tc f e
    -- PTileReduce monoids e e' -> PTileReduce monoids <$> tc f e <*> tc f e'

instance (HasContexts a, HasContexts b) => HasContexts ((,) a b) where
  traverseContexts f (x, y) = (,) <$> tc f x <*> tc f y

instance HasContexts a => HasContexts [a] where
  traverseContexts f xs = traverse (tc f) xs

instance HasContexts a => HasContexts (NonEmpty a) where
  traverseContexts f xs = traverse (tc f) xs

instance (Ord k, HasContexts k, HasContexts a) => HasContexts (M.Map k a) where
  -- TODO(danielzheng): Is there a more efficient implementation that avoids
  -- conversion to and from `List`?
  traverseContexts f m = M.fromList <$> tc f (M.toList m)

instance HasContexts a => HasContexts (LabeledItems a) where
  traverseContexts f (LabeledItems xs) = LabeledItems <$> tc f xs

instance HasContexts a => HasContexts (Limit a) where
  traverseContexts f limit = case limit of
    InclusiveLim a -> InclusiveLim <$> tc f a
    ExclusiveLim a -> ExclusiveLim <$> tc f a
    Unlimited -> pure Unlimited

instance HasContexts a => HasContexts (Maybe a) where
  traverseContexts f x = case x of
    Nothing -> pure Nothing
    Just x' -> Just <$> tc f x'

instance HasContexts (UPat' n l) where
  -- traverseContexts f upat = case upat of { WithSrcB spc up -> _ }
  -- TODO(danielzheng): Check if this is correct.
  -- Do patterns require source context traversal? Intuition is yes.
  traverseContexts _ upat = pure upat
  -- traverseContexts _ upat = case upat of
  --   UPatBinder ub -> _
  --   UPatCon sno ne -> _
  --   UPatPair pb -> _
  --   UPatUnit ub -> _
  --   UPatRecord ufrp -> _
  --   UPatVariant li s wsb -> _
  --   UPatVariantLift li wsb -> _
  --   UPatTable ne -> _

instance HasContexts (UPatAnn n l) where
  traverseContexts f (UPatAnn wsb m_wse) = UPatAnn <$> tc f wsb <*> tc f m_wse

instance HasContexts (UPatAnnArrow n l) where
  traverseContexts f (UPatAnnArrow pat arr) =
    UPatAnnArrow <$> tc f pat <*> pure arr

instance HasContexts Char where
  traverseContexts _ c = pure c

instance HasContexts SourceNameWithPos where
  traverseContexts f (SourceNameWithPos pos name) = SourceNameWithPos <$> f pos <*> tc f name

instance HasContexts (SourceNameOr a n) where
  traverseContexts f name = case name of
    SourceName pos s -> SourceName <$> f pos <*> pure s
    InternalName pos s n -> InternalName <$> f pos <*> pure s <*> pure n

instance HasContexts (UDataDef n) where
  traverseContexts f (UDataDef params typeclasses datacons) =
    UDataDef <$> tc f params <*> tc f typeclasses <*> tc f datacons

instance HasContexts (UDataDefTrail l) where
  traverseContexts f (UDataDefTrail nest) = UDataDefTrail <$> tc f nest

instance HasContexts (UAnnBinder c n l) where
  traverseContexts f (UAnnBinder binder ty) = UAnnBinder <$> tc f binder <*> tc f ty

instance HasContexts (NameBinder c n l) where
  traverseContexts _ x = pure x

instance HasContexts (UBinder c n l) where
  traverseContexts f binder = case binder of
    UBindSource pos name -> UBindSource <$> f pos <*> tc f name
    UIgnore -> pure binder
    UBind pos name binder' -> UBind <$> f pos <*> tc f name <*> tc f binder'

instance (forall n' l'. HasContexts (b n' l')) => HasContexts (Nest b n l) where
  traverseContexts f nest = case nest of
    Empty -> pure Empty
    Nest bnh ne -> Nest <$> tc f bnh <*> tc f ne

instance HasContexts (UMethodType n) where
  traverseContexts f (UMethodType names types) = UMethodType names <$> tc f types

instance HasContexts (UMethodDef l) where
  traverseContexts f (UMethodDef name uexpr) = UMethodDef <$> tc f name <*> tc f uexpr

instance HasContexts (UnitB n l) where
  traverseContexts _ x = pure x

instance (HasContexts (b1 n l), HasContexts (b2 n l)) => HasContexts (EitherB b1 b2 n l) where
  traverseContexts f x = case x of
    LeftB l -> LeftB <$> tc f l
    RightB r -> RightB <$> tc f r

instance HasContexts (UDecl n l) where
  traverseContexts f udecl = case udecl of
    -- ULet letann upatann uexpr -> ULet <$> pure letann <*> rec upatann <*> rec uexpr
    ULet letann upatann uexpr -> ULet letann <$> tc f upatann <*> tc f uexpr
    UDataDefDecl udatadef ubinder nest -> UDataDefDecl <$> tc f udatadef <*> tc f ubinder <*> tc f nest
    UInterface params superclasses methodTys interfaceName methodNames ->
      UInterface <$> tc f params <*> tc f superclasses <*> tc f methodTys <*> tc f interfaceName <*> tc f methodNames
    UInstance className bs params methods name ->
      UInstance <$> tc f className <*> tc f bs <*> tc f params <*> tc f methods <*> tc f name

instance HasContexts (expr n) => HasContexts (WithSrcE expr n) where
  traverseContexts f (WithSrcE srcPosCtx expr) =
    WithSrcE <$> f srcPosCtx <*> tc f expr

instance HasContexts (binder n l) => HasContexts (WithSrcB binder n l) where
  traverseContexts f (WithSrcB srcPosCtx binder) =
    WithSrcB <$> f srcPosCtx <*> tc f binder

-- High-level approach:
-- - Store span info in SrcPosCtx
-- - Pass
--   - 1. Traverse UExpr to get [SpanInfo]
--   - 2. Traverse UExpr to get UExpr with SpanInfo filled in SrcPosCtx
--   - 3. Traverse UExpr to get UExpr with SpanInfo filled in SrcPosCtx – build into SourceRename

-- Span info computing approach:
-- - VarOccSpanInfo: need symbol table, SubstEnv

gatherSpanInfos :: HasContexts a => a -> [SpanInfo]
-- gatherSpanInfos x = execWriter (traverseContexts (\ctx -> tell [ctx] >> return ctx) x)
gatherSpanInfos x = execWriter (traverseContexts (\ctx -> tell [MiscSpanInfo] >> return ctx) x)

-- Given UExpr/UDecl, get all the SrcPosCtx
gatherContexts :: HasContexts a => a -> [SrcPosCtx]
gatherContexts x = execWriter (traverseContexts (\ctx -> tell [ctx] >> return ctx) x)

addContextIds :: HasContexts a => a -> a
addContextIds x = evalState (traverseContexts f x) 0
  where f (SrcPosCtx maybeSrcPos _ spanInfo) = do
          currentId <- get
          put (currentId + 1)
          return (SrcPosCtx maybeSrcPos (Just currentId) spanInfo)
