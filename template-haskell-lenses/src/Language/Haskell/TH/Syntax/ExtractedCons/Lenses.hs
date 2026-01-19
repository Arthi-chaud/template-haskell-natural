{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Lenses where

import Language.Haskell.TH.Syntax (Clause, Match)
import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons hiding (Clause, Match)
import Language.Haskell.TH.Syntax.ExtractedCons.Internal

-- TODO For each extracted cons, for each field, generate lens using position

mkExtractedConsLenses ''VarE ["name"]
mkExtractedConsLenses ''ConE ["name"]
mkExtractedConsLenses ''LitE ["name"]
mkExtractedConsLenses ''AppE ["callee", "arg"]
mkExtractedConsLenses ''AppTypeE ["callee", "arg"]
mkExtractedConsLenses ''InfixE ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''UInfixE ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''ParensE ["body"]
mkExtractedConsLenses ''LamE ["pats", "body"]
mkExtractedConsLenses ''LamCaseE ["matches"]
mkExtractedConsLenses ''LamCasesE ["clauses"]
mkExtractedConsLenses ''TupE ["fields"]
mkExtractedConsLenses ''UnboxedTupE ["fields"]
mkExtractedConsLenses ''UnboxedSumE ["body", "alt", "arity"]
mkExtractedConsLenses ''CondE ["condition", "trueBranch", "falseBranch"]
mkExtractedConsLenses ''MultiIfE ["guards"]
mkExtractedConsLenses ''LetE ["decs", "body"]
mkExtractedConsLenses ''CaseE ["body", "matches"]
mkExtractedConsLenses ''DoE ["modName", "matches"]
mkExtractedConsLenses ''MDoE ["modName", "matches"]
mkExtractedConsLenses ''CompE ["stmts"]
mkExtractedConsLenses ''ArithSeqE ["range"]
mkExtractedConsLenses ''ListE ["items"]
mkExtractedConsLenses ''SigE ["body", "type_"]
mkExtractedConsLenses ''RecConE ["name", "fieldExps"]
mkExtractedConsLenses ''RecUpdE ["expr", "fieldExps"]
mkExtractedConsLenses ''StaticE ["body"]
mkExtractedConsLenses ''UnboundVarE ["name"]
mkExtractedConsLenses ''LabelE ["str"]
mkExtractedConsLenses ''ImplicitParamVarE ["str"]
mkExtractedConsLenses ''GetFieldE ["expr", "fieldName"]
mkExtractedConsLenses ''ProjectionE ["projection"]
mkExtractedConsLenses ''TypedBracketE ["exp"]
mkExtractedConsLenses ''TypedSpliceE ["exp"]

#if MIN_VERSION_template_haskell(2,22,0)
mkExtractedConsLenses ''TypeE ["type_"]
#endif
#if MIN_VERSION_template_haskell(2,23,0)
mkExtractedConsLenses ''ForallE ["tyVarBndr","expr"]
mkExtractedConsLenses ''ForallVisE ["tyVarBndr","expr"]
mkExtractedConsLenses ''ConstrainedE ["constraints", "expr"]
#endif

mkExtractedConsLenses ''Match ["pat", "body", "where_"]
mkExtractedConsLenses ''Clause ["pats", "body", "where_"]

mkExtractedConsLenses ''NormalB ["expr"]
mkExtractedConsLenses ''GuardedB ["guardedExpr"]
mkExtractedConsLenses ''NormalG ["expr"]
mkExtractedConsLenses ''PatG ["stmts"]

mkExtractedConsLenses ''LitP ["lit"]
mkExtractedConsLenses ''VarP ["name"]
mkExtractedConsLenses ''TupP ["pats"]
mkExtractedConsLenses ''UnboxedTupP ["pats"]
mkExtractedConsLenses ''UnboxedSumP ["pat", "alt", "arity"]
mkExtractedConsLenses ''ConP ["con", "types", "pats"]
mkExtractedConsLenses ''InfixP ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''UInfixP ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''ParensP ["body"]
mkExtractedConsLenses ''TildeP ["body"]
mkExtractedConsLenses ''BangP ["body"]
mkExtractedConsLenses ''AsP ["name", "body"]
mkExtractedConsLenses ''RecP ["name", "pats"]
mkExtractedConsLenses ''ListP ["pats"]
mkExtractedConsLenses ''SigP ["pat", "type_"]
mkExtractedConsLenses ''ViewP ["exp", "pat"]

#if MIN_VERSION_template_haskell(2,22,0)
mkExtractedConsLenses ''TypeP ["type_"]
mkExtractedConsLenses ''InvisP ["type_"]
mkExtractedConsLenses ''OrP ["pats"]
#endif

mkExtractedConsLenses ''FromR ["exp"]
mkExtractedConsLenses ''FromThenR ["exp0", "exp1"]
mkExtractedConsLenses ''FromToR ["exp0", "exp1"]
mkExtractedConsLenses ''FromThenToR ["exp0", "exp1", "expn"]

mkExtractedConsLenses ''CharL ["value"]
mkExtractedConsLenses ''StringL ["value"]
mkExtractedConsLenses ''IntegerL ["value"]
mkExtractedConsLenses ''RationalL ["value"]
mkExtractedConsLenses ''IntPrimL ["value"]
mkExtractedConsLenses ''WordPrimL ["value"]
mkExtractedConsLenses ''FloatPrimL ["value"]
mkExtractedConsLenses ''DoublePrimL ["value"]
mkExtractedConsLenses ''StringPrimL ["value"]
mkExtractedConsLenses ''BytesPrimL ["value"]
mkExtractedConsLenses ''CharPrimL ["value"]

mkExtractedConsLenses ''NumTyLit ["value"]
mkExtractedConsLenses ''StrTyLit ["value"]
mkExtractedConsLenses ''CharTyLit ["value"]

mkExtractedConsLenses ''BindS ["pat", "exp"]
mkExtractedConsLenses ''LetS ["decs"]
mkExtractedConsLenses ''NoBindS ["exp"]
mkExtractedConsLenses ''ParS ["stmts"]
mkExtractedConsLenses ''RecS ["stmts"]

mkExtractedConsLenses ''NormalC ["name", "bt"]
mkExtractedConsLenses ''RecC ["name", "varBangType"]
mkExtractedConsLenses ''InfixC ["leftBt", "name", "rightBt"]
mkExtractedConsLenses ''ForallC ["tyVarBndr", "cxt", "con"]
mkExtractedConsLenses ''GadtC ["conNames", "bts", "type_"]
mkExtractedConsLenses ''RecGadtC ["conNames", "vbts", "type_"]

-- TODO Type, Dec
