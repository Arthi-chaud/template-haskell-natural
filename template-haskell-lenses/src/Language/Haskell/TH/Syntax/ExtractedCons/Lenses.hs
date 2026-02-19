{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Language.Haskell.TH.Syntax.ExtractedCons.Lenses where

import Control.Lens.TH.ADT
import Language.Haskell.TH.Syntax (Clause, Match)
import Language.Haskell.TH.Syntax.ExtractedCons.ExtractedCons hiding (Clause)
import Prelude hiding (exp)

makeADTLenses ''VarE ["name"]
makeADTLenses ''ConE ["name"]
makeADTLenses ''LitE ["name"]
makeADTLenses ''AppE ["callee", "arg"]
makeADTLenses ''AppTypeE ["callee", "arg"]
makeADTLenses ''InfixE ["leftArg", "op", "rightArg"]
makeADTLenses ''UInfixE ["leftArg", "op", "rightArg"]
makeADTLenses ''ParensE ["body"]
makeADTLenses ''LamE ["pats", "body"]
makeADTLenses ''LamCaseE ["matches"]
makeADTLenses ''LamCasesE ["clauses"]
makeADTLenses ''TupE ["fields"]
makeADTLenses ''UnboxedTupE ["fields"]
makeADTLenses ''UnboxedSumE ["body", "alt", "arity"]
makeADTLenses ''CondE ["condition", "trueBranch", "falseBranch"]
makeADTLenses ''MultiIfE ["guards"]
makeADTLenses ''LetE ["decs", "body"]
makeADTLenses ''CaseE ["body", "matches"]
makeADTLenses ''DoE ["modName", "matches"]
makeADTLenses ''MDoE ["modName", "matches"]
makeADTLenses ''CompE ["stmts"]
makeADTLenses ''ArithSeqE ["range"]
makeADTLenses ''ListE ["items"]
makeADTLenses ''SigE ["body", "ty"]
makeADTLenses ''RecConE ["name", "fieldExps"]
makeADTLenses ''RecUpdE ["expr", "fieldExps"]
makeADTLenses ''StaticE ["body"]
makeADTLenses ''UnboundVarE ["name"]
makeADTLenses ''LabelE ["str"]
makeADTLenses ''ImplicitParamVarE ["str"]
makeADTLenses ''GetFieldE ["expr", "fieldName"]
makeADTLenses ''ProjectionE ["projection"]
makeADTLenses ''TypedBracketE ["exp"]
makeADTLenses ''TypedSpliceE ["exp"]

#if MIN_VERSION_template_haskell(2,22,0)
makeADTLenses ''TypeE ["ty"]
#endif
#if MIN_VERSION_template_haskell(2,23,0)
makeADTLenses ''ForallE ["tyVarBndr","expr"]
makeADTLenses ''ForallVisE ["tyVarBndr","expr"]
makeADTLenses ''ConstrainedE ["constraints", "expr"]
#endif

makeADTLenses ''Match ["pat", "body", "where_"]
makeADTLenses ''Clause ["pats", "body", "where_"]

makeADTLenses ''NormalB ["expr"]
makeADTLenses ''GuardedB ["guardedExpr"]
makeADTLenses ''NormalG ["expr"]
makeADTLenses ''PatG ["stmts"]

makeADTLenses ''LitP ["lit"]
makeADTLenses ''VarP ["name"]
makeADTLenses ''TupP ["pats"]
makeADTLenses ''UnboxedTupP ["pats"]
makeADTLenses ''UnboxedSumP ["pat", "alt", "arity"]
makeADTLenses ''ConP ["con", "types", "pats"]
makeADTLenses ''InfixP ["leftArg", "op", "rightArg"]
makeADTLenses ''UInfixP ["leftArg", "op", "rightArg"]
makeADTLenses ''ParensP ["body"]
makeADTLenses ''TildeP ["body"]
makeADTLenses ''BangP ["body"]
makeADTLenses ''AsP ["name", "body"]
makeADTLenses ''RecP ["name", "pats"]
makeADTLenses ''ListP ["pats"]
makeADTLenses ''SigP ["pat", "ty"]
makeADTLenses ''ViewP ["exp", "pat"]

#if MIN_VERSION_template_haskell(2,22,0)
makeADTLenses ''TypeP ["ty"]
makeADTLenses ''InvisP ["ty"]
#endif

#if MIN_VERSION_template_haskell(2,23,0)
makeADTLenses ''OrP ["pats"]
#endif

makeADTLenses ''FromR ["exp"]
makeADTLenses ''FromThenR ["exp0", "exp1"]
makeADTLenses ''FromToR ["exp0", "exp1"]
makeADTLenses ''FromThenToR ["exp0", "exp1", "expn"]

makeADTLenses ''CharL ["value"]
makeADTLenses ''StringL ["value"]
makeADTLenses ''IntegerL ["value"]
makeADTLenses ''RationalL ["value"]
makeADTLenses ''IntPrimL ["value"]
makeADTLenses ''WordPrimL ["value"]
makeADTLenses ''FloatPrimL ["value"]
makeADTLenses ''DoublePrimL ["value"]
makeADTLenses ''StringPrimL ["value"]
makeADTLenses ''BytesPrimL ["value"]
makeADTLenses ''CharPrimL ["value"]

makeADTLenses ''NumTyLit ["value"]
makeADTLenses ''StrTyLit ["value"]
makeADTLenses ''CharTyLit ["value"]

makeADTLenses ''BindS ["pat", "exp"]
makeADTLenses ''LetS ["decs"]
makeADTLenses ''NoBindS ["exp"]
makeADTLenses ''ParS ["stmts"]
makeADTLenses ''RecS ["stmts"]

makeADTLenses ''NormalC ["name", "bts"]
makeADTLenses ''RecC ["name", "vbts"]
makeADTLenses ''InfixC ["leftBt", "name", "rightBt"]
makeADTLenses ''ForallC ["tyVarBndr", "cxt", "con"]
makeADTLenses ''GadtC ["conNames", "bts", "ty"]
makeADTLenses ''RecGadtC ["conNames", "vbts", "ty"]

makeADTLenses ''ForallT ["tyVarBndr", "cxt", "ty"]
makeADTLenses ''ForallVisT ["tyVarBndr", "ty"]
makeADTLenses ''AppT ["callee", "arg"]
makeADTLenses ''AppKindT ["callee", "kind"]
makeADTLenses ''SigT ["ty", "kind"]
makeADTLenses ''VarT ["name"]
makeADTLenses ''ConT ["name"]
makeADTLenses ''PromotedT ["name"]
makeADTLenses ''InfixT ["leftArg", "op", "rightArg"]
makeADTLenses ''UInfixT ["leftArg", "op", "rightArg"]
makeADTLenses ''PromotedInfixT ["leftArg", "op", "rightArg"]
makeADTLenses ''PromotedUInfixT ["leftArg", "op", "rightArg"]
makeADTLenses ''ParensT ["body"]
makeADTLenses ''TupleT ["count"]
makeADTLenses ''UnboxedTupleT ["count"]
makeADTLenses ''UnboxedSumT ["count"]
makeADTLenses ''PromotedTupleT ["count"]
makeADTLenses ''LitT ["lit"]
makeADTLenses ''ImplicitParamT ["name", "ty"]

makeADTLenses ''FunD ["name", "clauses"]
makeADTLenses ''ValD ["pat", "body", "where_"]
makeADTLenses ''DataD ["cxt", "name", "tyVarBndr", "kind", "con", "derive"]
makeADTLenses ''NewtypeD ["cxt", "name", "tyVarBndr", "kind", "con", "derive"]
makeADTLenses ''TypeDataD ["name", "tyVarBndr", "kind", "con"]
makeADTLenses ''TySynD ["name", "tyVarBndr", "ty"]
makeADTLenses ''ClassD ["cxt", "name", "tyVarBndr", "funDep", "decs"]
makeADTLenses ''InstanceD ["overlap", "cxt", "ty", "decs"]
makeADTLenses ''SigD ["name", "ty"]
makeADTLenses ''KiSigD ["name", "kind"]
makeADTLenses ''ForeignD ["foreign_"]
makeADTLenses ''DefaultD ["types"]
makeADTLenses ''PragmaD ["pragma"]
makeADTLenses ''DataFamilyD ["name", "tyVarBndr", "kind"]
makeADTLenses ''DataInstD ["cxt", "tyVarBndr", "ty", "kind", "con", "derive"]
makeADTLenses ''NewtypeInstD ["cxt", "tyVarBndr", "ty", "kind", "con", "derive"]
makeADTLenses ''TySynInstD ["tySynEqn"]
makeADTLenses ''OpenTypeFamilyD ["tyFamHead"]
makeADTLenses ''ClosedTypeFamilyD ["tyFamHead", "tySynEqn"]
makeADTLenses ''RoleAnnotD ["name", "role"]
makeADTLenses ''StandaloneDerivD ["strat", "cxt", "ty"]
makeADTLenses ''DefaultSigD ["name", "ty"]
makeADTLenses ''PatSynD ["name", "args", "dir", "pat"]
makeADTLenses ''PatSynSigD ["name", "ty"]
makeADTLenses ''ImplicitParamBindD ["name", "exp"]
#if MIN_VERSION_template_haskell(2,22,0)
makeADTLenses ''InfixD ["fixity", "namespace", "name"]
#else
makeADTLenses ''InfixD ["fixity", "name"]
#endif

makeADTLenses ''DeriveClause ["strat", "cxt"]

makeADTLenses ''ImportF ["callconv", "safety", "cName", "hName", "ty"]
makeADTLenses ''ExportF ["callconv", "cName", "hName", "ty"]

makeADTLenses ''InlineP ["name", "inline", "rule", "phases"]
makeADTLenses ''OpaqueP ["name"]

#if MIN_VERSION_template_haskell(2,24,0)
makeADTLenses ''SpecialiseEP ["tyVarbndr", "ruleBndr", "exp", "inline", "phases"]
makeADTLenses ''SpecialiseInstEP ["ty"]
#else
makeADTLenses ''SpecialiseP ["name", "ty", "inline", "phases"]
makeADTLenses ''SpecialiseInstP ["ty"]
#endif
makeADTLenses ''RuleP ["name", "tyVarBndr", "ruleBndr", "exp0", "exp1", "phases"]
makeADTLenses ''AnnP ["target", "exp"]
makeADTLenses ''LineP ["idx", "file"]
makeADTLenses ''CompleteP ["patNames", "tyName"]

#if MIN_VERSION_template_haskell(2,22,0)
makeADTLenses ''SCCP ["fName", "ccName"]
#endif
