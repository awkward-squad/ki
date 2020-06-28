[Next tutorial](02.md)

---

# Tutorial 1

In this tutorial, we'll perform the basic, boilerplate setup that the `ki` library requires.

First, we'll import the library qualified. You don't have to, but it's recommended.

```haskell
{-# LANGUAGE BlockArguments #-}
module Tutorial01 where
import qualified Ki.Implicit as Ki
```

Next, we'll run our `main` function inside a call to `global`.

```haskell
main :: IO ()
main =
  Ki.global do
    putStrLn "Hello, world!"
```

Why'd we do that? Well, let's look at the type of `global`.

```haskell ignore
global :: (Context => IO a) -> IO a
```

`global` provides the global **context** to an action. A **context** is used to propagage soft-cancellation signals,
which we'll learn about later. In this example, we didn't need it just to print a string to the console.

Threading a **context** around is boilerplate and error-prone, so `ki` handles it for you using an
[implicit parameter](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#implicit-parameters).

But you don't need to know that - just call `global` once in `main` (or anywhere you don't happen to have a **context**
already in scope), and forget about it.

---

[Next tutorial](02.md)