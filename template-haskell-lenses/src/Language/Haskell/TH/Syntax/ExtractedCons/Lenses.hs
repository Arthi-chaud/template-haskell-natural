{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Lenses where

import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons
import Language.Haskell.TH.Syntax.ExtractedCons.Internal

-- TODO For each extracted cons, for each field, generate lens using position

mkExtractedConsLens ''VarE "name" 0
mkExtractedConsLens ''ConE "name" 0
mkExtractedConsLens ''LitE "name" 0
mkExtractedConsLens ''AppE "callee" 0
mkExtractedConsLens ''AppE "argument" 1
mkExtractedConsLens ''AppTypeE "callee" 0
mkExtractedConsLens ''AppTypeE "argument" 1
mkExtractedConsLens ''InfixE "leftArgument" 0
mkExtractedConsLens ''InfixE "op" 1
mkExtractedConsLens ''InfixE "rightArgument" 2
mkExtractedConsLens ''UInfixE "leftArgument" 0
mkExtractedConsLens ''UInfixE "op" 1
mkExtractedConsLens ''UInfixE "rightArgument" 2
mkExtractedConsLens ''ParensE "body" 0
mkExtractedConsLens ''LamE "patterns" 0
mkExtractedConsLens ''LamE "body" 1
mkExtractedConsLens ''LamCaseE "matches" 0
mkExtractedConsLens ''LamCasesE "clauses" 0
mkExtractedConsLens ''TupE "fields" 0
mkExtractedConsLens ''UnboxedTupE "fields" 0
mkExtractedConsLens ''UnboxedSumE "body" 0
mkExtractedConsLens ''UnboxedSumE "alt" 1
mkExtractedConsLens ''UnboxedSumE "arity" 2
mkExtractedConsLens ''CondE "condition" 0
mkExtractedConsLens ''CondE "trueBranch" 1
mkExtractedConsLens ''CondE "falseBranch" 2
mkExtractedConsLens ''MultiIfE "guards" 0
mkExtractedConsLens ''LetE "decs" 0
mkExtractedConsLens ''LetE "body" 1
mkExtractedConsLens ''CaseE "body" 0
mkExtractedConsLens ''CaseE "matches" 1
mkExtractedConsLens ''DoE "modName" 0
mkExtractedConsLens ''DoE "matches" 1
mkExtractedConsLens ''MDoE "modName" 0
mkExtractedConsLens ''MDoE "matches" 1
mkExtractedConsLens ''CompE "statements" 0
mkExtractedConsLens ''ArithSeqE "range" 0
mkExtractedConsLens ''ListE "items" 0
mkExtractedConsLens ''SigE "body" 0
mkExtractedConsLens ''SigE "type_" 1
mkExtractedConsLens ''RecConE "name" 0
mkExtractedConsLens ''RecConE "fieldExps" 1
mkExtractedConsLens ''RecUpdE "expr" 0
mkExtractedConsLens ''RecUpdE "fieldExps" 1
mkExtractedConsLens ''StaticE "body" 0
mkExtractedConsLens ''UnboundVarE "name" 0
mkExtractedConsLens ''LabelE "str" 0
mkExtractedConsLens ''ImplicitParamVarE "str" 0
mkExtractedConsLens ''GetFieldE "expr" 0
mkExtractedConsLens ''GetFieldE "fieldName" 1
mkExtractedConsLens ''ProjectionE "projections" 0
mkExtractedConsLens ''TypedBracketE "expr" 0
mkExtractedConsLens ''TypedSpliceE "expr" 0

#if !MIN_VERSION_template_haskell(2,24,0)
-- mkExtractedConsLens ''TypeE "type_" 0
-- mkExtractedConsLens ''ForallE "tyVarBndr" 0
-- mkExtractedConsLens ''ForallE "expr" 1
-- mkExtractedConsLens ''ForallVisE "tyVarBndr" 0
-- mkExtractedConsLens ''ForallVisE "expr" 1
-- mkExtractedConsLens ''ConstrainedE "constraints" 0
-- mkExtractedConsLens ''ConstrainedE "expr" 1
#endif

-- TODO Match, Clause, Pat, Stmt, Con, Type, Dec
