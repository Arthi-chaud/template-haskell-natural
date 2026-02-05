{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Lenses where

import Language.Haskell.TH.Syntax (Clause, Match)
import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons hiding (Clause)
import Language.Haskell.TH.Syntax.ExtractedCons.Internal
import Prelude hiding (exp)

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
mkExtractedConsLenses ''SigE ["body", "ty"]
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
mkExtractedConsLenses ''TypeE ["ty"]
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
mkExtractedConsLenses ''SigP ["pat", "ty"]
mkExtractedConsLenses ''ViewP ["exp", "pat"]

#if MIN_VERSION_template_haskell(2,22,0)
mkExtractedConsLenses ''TypeP ["ty"]
mkExtractedConsLenses ''InvisP ["ty"]
#endif

#if MIN_VERSION_template_haskell(2,23,0)
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
mkExtractedConsLenses ''GadtC ["conNames", "bts", "ty"]
mkExtractedConsLenses ''RecGadtC ["conNames", "vbts", "ty"]

mkExtractedConsLenses ''ForallT ["tyVarBndr", "cxt", "ty"]
mkExtractedConsLenses ''ForallVisT ["tyVarBndr", "ty"]
mkExtractedConsLenses ''AppT ["callee", "arg"]
mkExtractedConsLenses ''AppKindT ["callee", "kind"]
mkExtractedConsLenses ''SigT ["ty", "kind"]
mkExtractedConsLenses ''VarT ["name"]
mkExtractedConsLenses ''ConT ["name"]
mkExtractedConsLenses ''PromotedT ["name"]
mkExtractedConsLenses ''InfixT ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''UInfixT ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''PromotedInfixT ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''PromotedUInfixT ["leftArg", "op", "rightArg"]
mkExtractedConsLenses ''ParensT ["body"]
mkExtractedConsLenses ''TupleT ["count"]
mkExtractedConsLenses ''UnboxedTupleT ["count"]
mkExtractedConsLenses ''UnboxedSumT ["count"]
mkExtractedConsLenses ''PromotedTupleT ["count"]
mkExtractedConsLenses ''LitT ["lit"]
mkExtractedConsLenses ''ImplicitParamT ["name", "ty"]

mkExtractedConsLenses ''FunD ["name", "clauses"]
mkExtractedConsLenses ''ValD ["pat", "body", "where_"]
mkExtractedConsLenses ''DataD ["cxt", "name", "tyVarBndr", "kind", "con", "derive"]
mkExtractedConsLenses ''NewtypeD ["cxt", "name", "tyVarBndr", "kind", "con", "derive"]
mkExtractedConsLenses ''TypeDataD ["name", "tyVarBndr", "kind", "con"]
mkExtractedConsLenses ''TySynD ["name", "tyVarBndr", "ty"]
mkExtractedConsLenses ''ClassD ["cxt", "name", "tyVarBndr", "funDep", "decs"]
mkExtractedConsLenses ''InstanceD ["overlap", "cxt", "ty", "decs"]
mkExtractedConsLenses ''SigD ["name", "ty"]
mkExtractedConsLenses ''KiSigD ["name", "kind"]
mkExtractedConsLenses ''ForeignD ["foreign_"]
mkExtractedConsLenses ''DefaultD ["types"]
mkExtractedConsLenses ''PragmaD ["pragma"]
mkExtractedConsLenses ''DataFamilyD ["name", "tyVarBndr", "kind"]
mkExtractedConsLenses ''DataInstD ["cxt", "tyVarBndr", "ty", "kind", "con", "derive"]
mkExtractedConsLenses ''NewtypeInstD ["cxt", "tyVarBndr", "ty", "kind", "con", "derive"]
mkExtractedConsLenses ''TySynInstD ["tySynEqn"]
mkExtractedConsLenses ''OpenTypeFamilyD ["tyFamHead"]
mkExtractedConsLenses ''ClosedTypeFamilyD ["tyFamHead", "tySynEqn"]
mkExtractedConsLenses ''RoleAnnotD ["name", "role"]
mkExtractedConsLenses ''StandaloneDerivD ["strat", "cxt", "ty"]
mkExtractedConsLenses ''DefaultSigD ["name", "ty"]
mkExtractedConsLenses ''PatSynD ["name", "args", "dir", "pat"]
mkExtractedConsLenses ''PatSynSigD ["name", "ty"]
mkExtractedConsLenses ''ImplicitParamBindD ["name", "exp"]
#if MIN_VERSION_template_haskell(2,22,0)
mkExtractedConsLenses ''InfixD ["fixity", "namespace", "name"]
#else
mkExtractedConsLenses ''InfixD ["fixity", "name"]
#endif

mkExtractedConsLenses ''DeriveClause ["strat", "cxt"]

mkExtractedConsLenses ''ImportF ["callconv", "safety", "cName", "hName", "ty"]
mkExtractedConsLenses ''ExportF ["callconv", "cName", "hName", "ty"]

mkExtractedConsLenses ''InlineP ["name", "inline", "rule", "phases"]
mkExtractedConsLenses ''OpaqueP ["name"]

#if MIN_VERSION_template_haskell(2,24,0)
mkExtractedConsLenses ''SpecialiseEP ["tyVarbndr", "ruleBndr", "exp", "inline", "phases"]
mkExtractedConsLenses ''SpecialiseInstEP ["ty"]
#else
mkExtractedConsLenses ''SpecialiseP ["name", "ty", "inline", "phases"]
mkExtractedConsLenses ''SpecialiseInstP ["ty"]
#endif
mkExtractedConsLenses ''RuleP ["name", "tyVarBndr", "ruleBndr", "exp0", "exp1", "phases"]
mkExtractedConsLenses ''AnnP ["target", "exp"]
mkExtractedConsLenses ''LineP ["idx", "file"]
mkExtractedConsLenses ''CompleteP ["patNames", "tyName"]

#if MIN_VERSION_template_haskell(2,22,0)
mkExtractedConsLenses ''SCCP ["fName", "ccName"]
#endif
