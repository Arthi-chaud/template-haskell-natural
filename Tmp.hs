{-# LANGUAGE TemplateHaskell #-}

module Main where

main :: IO ()
main = return ()

-- TODO: Split front end and Template Haskell types of tight coupling

-- TODO: Use lenses
-- TODO: Use TH to derive invariant wrapper, e.g. InstanceD

-- Defining a typeclass
-- Need: TyVarBndr
-- Could: nice to have a quasi quoter for functional deps

mkToJSONClass :: ClassDefinition
mkToJSONClass = do
    a <- bindTypeVar
    newClass 'ToJSON [a] $ do
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
-- NOTE: Expression is a monad liked packed reader, first argument confirms that it's ready
-- TODO: Make sure 'return_' is the last statement

mkGetSnd :: FuncDefinition ((a, b) -> b)
mkGetSnd = mkFunc $ F.do
    tupl <- arg
    snd' <- deconstruct tupl $ \b -> [|(_, $b)|]
    return_ snd'

mkToJSONFunction :: Type a -> FuncDefinition (a -> Maybe ByteString)
mkToJSONFunction ty = mkFunc $ do
    a <- arg
    strA <- let_ [|show $(varE a)|]
    -- strA <- letStrict_ [|show $(varE a)|]
    return_ [|Just $(varE strA)|]
