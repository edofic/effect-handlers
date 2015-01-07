# effect-handlers

This is an extensible effects library for Haskell taking inspiration from the [Eff language](http://www.eff-lang.org/).

See these papers for the ideas and theory behind the library:

  - [O. Kammar et al: Handlers in Action!](http://homepages.inf.ed.ac.uk/slindley/papers/handlers.pdf)
  - [A. Bauer, M. Pretnar: Programming with Algebraic Effects and Handlers](http://arxiv.org/abs/1203.1539)
  - [O Kiselyov, A Sabry, C Swords: Extensible Effects](http://dl.acm.org/citation.cfm?id=2503791)

Implementation wise it's most close to [extensible-effects](http://hackage.haskell.org/package/extensible-effects) (also see the Extensible Effects paper) but it implements deep handlers instead of shallow and provides several "backends".

## What does this library provide?
There are 3 groups of modules that are source-compatible, you can import any one of them and your code will work. But you will have to stick to that implementation, since they are not type-compatible.

Different implementations exists for experimentation purposes. We do not have sufficient data to say for sure but it seems that `Control.Effects.Cont` is the fastest.

Each of the groups provides the effect monad `Eff` and modules

  - `Exception`
  - `IO`
  - `Reader`
  - `Search`
  - `State`
  - `Writer`

that provide corresponding effects.


## Example
Most of the types are inferred, you only need to provide enough to tell the compiler how to specialize some effect handlers (e.g. readerHandler).

```haskell
import Control.Effects.Cont.Eff
import Control.Effects.Cont.Reader
import Control.Effects.Cont.Exception

program = do
  v <- ask
  if v < 15 
  then throw $ show v
  else return (v+1)

run n = runPure 
                 . handle exceptionHandler 
                 . handle (readerHandler n)

res :: Integer -> Either String Integer
res n = run n program
```

## Documentation
Haddock docs are [available online](http://edofic.github.io/effect-handlers)
