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

data Type = MkT Name [Cons] | Var Name
data Cons = MkCons Name [(Name, Type)]

mkToJSONInstance :: Type -> m InstanceDefinition
mkToJSONInstance ty = newInstance 'ToJSON [ty] $ do
    addToContext' $ forM_ (collectTypeArg ty) $ \arg -> [t|ToJSON $(varT arg)|]
    setOverlapping Overlappable
    instanceBody $ do
        addFunc $ inlined $ func 'toJSON todo

inlined :: FuncDefinition -> m FuncDefinition
inlined = undefined -- Add Inline pragma in the second arg of the constructor

todo :: FuncDefinition
todo = undefined -- TODO

-- Hypothetical type definition

newtype ClassDefinition = MkInstanceDec ClassD -- TODO Should be Dec, but invariant
newtype InstanceDefinition = MkInstanceDec InstanceD -- TODO Same
newtype FuncDefinition = MkFuncDec {func :: Dec, pragmas :: [Pragma]}

-- TODO Instance of StateMonad
