{-# LANGUAGE TemplateHaskell #-}

module Main where

main :: IO ()
main = return ()

-- TODO: Use lenses
-- TODO: Use TH to derive invariant wrapper, e.g. InstanceD

-- Defining a typeclass
-- Need: TyVarBndr
-- Could: nice to have a quasi quoter for functional deps

mkToJSONClass :: ClassDefinition
mkToJSONClass = newClass 'ToJSON $ do
    a <- bindTypeVar
    addTyVar a
    addToContext [t|ToJSON a|]
    -- addFunDep ([a], [b]) (b does not exist, but it's an example)
    classBody $ do
        addSignature [|toJSON :: $(varT a) -> Maybe ByteString|]

-- Defining Instance

data Type a = MkT Name [Cons] | Var Name
data Cons = MkCons Name [(Name, Type)]

mkToJSONInstance :: Type a -> m InstanceDefinition
mkToJSONInstance ty = newInstance 'ToJSON [ty] $ do
    addToContext' $ forM_ (collectTypeArg ty) $ \arg -> [t|ToJSON $(varT arg)|]
    setOverlapping Overlappable
    instanceBody $ do
        addFunc $ inlined $ func 'toJSON $ mkToJSONFunction ty

inlined :: FuncDefinition -> m FuncDefinition
inlined = undefined -- Add Inline pragma in the second arg of the constructor

-- Hypothetical type definition

newtype ClassDefinition = MkInstanceDec ClassD -- TODO Should be Dec, but invariant
newtype InstanceDefinition = MkInstanceDec InstanceD -- TODO Same
newtype FuncDefinition f = MkFuncDec f {func :: Dec, pragmas :: [Pragma]}

-- TODO Instance of StateMonad

-- TODO How to allowgenerating function + signature??
-- TODO let + destruction

mkFunc :: Expression '[] t -> FuncDefinition t
-- NOTE: Expression is a monad liked packed reader, first argument confirms that it's ready + check that last statement is not a bind

mkGetSnd :: FuncDefinition ((a, b) -> b)
mkGetSnd = mkFunc $ F.do
    tupl <- arg
    snd' <- getField tupl (,) 1
    return_ snd'

mkToJSONFunction :: Type a -> FuncDefinition (a -> Maybe ByteString)
mkToJSONFunction ty = mkFunc $ F.do
    a <- arg
    strA <- let_ [|show $(varE a)|]
    -- strA <- letStrict_ [|show $(varE a)|]
    return_ [|Just $(varE strA)|]

mkMaybeFunction :: FunctionDefinition (Maybe Int)
mkMaybeFunction = mkFunc $ Do.do
    -- NOTE: Graded monad that checks if the last statement is not a bind
    n <- bind_ [|Nothing|]
    -- n <- bindStrict_ [|Nothing|]
    n0 <- bind_ [|Just (Left 1)|]
    n1 <- getField n0 'Left 0
    return_ [|return ($n + $n1)|]

-- TODO: Would be nice to be able deconstruct and get multiple values at once

mkCase :: FunctionDefinition (Maybe Int)
mkCase = mkFunc $ F.do
    res <- case_ [|Nothing|] $ M.do
        matchCon
            'Just
            ( \i -> case i of -- the index check does not make sense here as just only has one arg, but it's to showcase the possibilities
                0 -> constant [|1|]
                1 -> var
                _ -> patternMatch (,) (const var) (\[fst', snd'] -> return_ [|$fst' - $snd'|])
            )
            (\[] -> return_ [|1|])
        matchCon
            'Nothing
            Nothing -- Nothing to pm on, or could be 'Con {}'
            (\[] -> return_ [|0|])
        matchConst [p|Just 1|] $ return_ [|0|]
        matchVar (\i -> return_ [|0|])
        matchWild $ return_ [|0|]
