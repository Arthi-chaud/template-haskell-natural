# Build Template Haskell ASTs using monads 

Template Haskell allows building Haskell programs at compile time. Through an extensive AST type, it is possible to generate anything from typeclasses, to expressions. However, the sheer amount of ADTs defined by the library, along with the simplicity of the API can make it unpleasant to define simple things (e.g. instances or bind expressions to variables).

`th-builder` is a DSL that acts like a wrapper around Template Haskell's AST and helps build programs. It is designed so that the code that builds the AST follows the same flow as the generated program.


```haskell
-- The expression to generate:
_ = \a b -> a + b

-- Using vanilla Template Haskell
genAdd = LamE [VarP a, VarP b] (VarE a `AppE` VarE b)   
    where
        a = mkName "a"
        b = mkName "b"

-- Using th-builder
genAdd = genExpr $ newExpr $ B.do
    a <- arg
    b <- arg
    returns (a `AppE` b)
```

`th-builder` allows building the following declarations/expressions:

- Typeclasses
- Instances
- Functions (+ signatures)
- Lambda and Do- expressions
    - With a variant that uses typed Template Haskell
- Datatypes (+ newtypes) and most constructors
- Type Synonyms

## Internal

The DSL is backed by 2 internal libraries that make interacting with TH ASTs easier:

- `extract-cons`: Using TH, it extracts the constructors of a given data type into standalone data types.
- `th-lenses`: It defines lenses for most of the constructors in TH's API, _extracted_ by the previous library.

Building ASTs is mainly done through a graded state monad, whose state is an extracted constructor (`ClassD`, `InstanceD`) or a custom state (for lambdas and do- expressions). Thanks to the synergy between lenses and the state monad, it is easy to interact with the state imperatively. 

This means that while the library is not safe from breaking when TH's API evolves, it should be easy to maintain it over time.

## Documentation

The Haddock documentation is available on [GitHub Pages](https://arthi-chaud.github.io/th-builder/). It's still a work in progress.

## Examples

You can find examples and reimplementations of small libraries in the `examples/` directory.

```haskell
import Language.Haskell.TH.Syntax.Builder as B
import Language.Haskell.TH.Syntax.Expr.Simple
import Language.Haskell.TH.Syntax.Gen

-- | \(a, b, ..) -> (.., b, a)
reverseTuple :: TupleSize -> ExpQ
reverseTuple n = genExpr $ newExpr $ B.do
    tup <- arg
    tupleFields <- forM [0 .. n - 1] $ \i -> getTupleField n i tup
    returns (TupE (Just <$> reverse tupFields))
```
