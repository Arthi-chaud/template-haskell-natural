{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QualifiedDo #-}

module Linear.TH (derivePushable) where

import Control.Monad (forM, forM_)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Natural.Syntax.Builder
import Language.Haskell.TH.Natural.Syntax.Case as M
import Language.Haskell.TH.Natural.Syntax.Expr.Simple
import Language.Haskell.TH.Natural.Syntax.Func (bodyFromExp, newFunc)
import Language.Haskell.TH.Natural.Syntax.Instance
import qualified Language.Haskell.TH.Natural.Syntax.Instance as I
import Language.Haskell.TH.QBuilder
import Language.Haskell.TH.Quotable
import Linear.Box

derivePushable :: TH.Name -> TH.DecsQ
derivePushable tyName = do
    tyInfo <- reifyDatatype tyName
    gen $ newInstance ''Pushable $ I.do
        addInstanceArg $ TH.ConT $ datatypeName tyInfo
        addBody' $ newFunc (TH.nameBase 'push) $ bodyFromExp $ pushFunc tyInfo

pushFunc :: DatatypeInfo -> SimpleExprBuilder Empty Ready ()
pushFunc ty = I.do
    a <- arg
    innerList <- getField 'Box 0 a
    returns $ case_ innerList $ C.do
        forM_ (datatypeCons ty) $ \ConstructorInfo{..} -> matchCon constructorName $ M.do
            fieldsExpr <- forM ([0 ..] `zip` constructorFields) $ \(fieldIdx, fieldTy) -> do
                fieldPat <- field fieldIdx var
                return $ case fieldTy of
                    (TH.AppT (TH.ConT cname) _vname) ->
                        if cname == datatypeName ty
                            then [|push (Box $(q fieldPat))|]
                            else q fieldPat
                    TH.ConT _cname -> q fieldPat
                    TH.VarT _vname -> [|Box $(q fieldPat)|]
                    _ -> Prelude.fail ("unhandled case: " ++ show fieldTy)
            body $ apply (TH.conE constructorName) fieldsExpr
