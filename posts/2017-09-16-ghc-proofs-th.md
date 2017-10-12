---
title: Writing a Template Haskell library for GHC.Proof
author: joomy
tags: haskell, tutorial
description: GHC.Proof is a great hack and I wanted to add another hack on top of it. It is simple enough that it could serve as a Template Haskell tutorial for others.
---

I learned about [Joachim Breitner](http://github.com/nomeata)'s
[ghc-proofs](http://github.com/nomeata/ghc-proofs) from his WPTE 2017 talk.
For those who are not familiar, it is a tool that looks at the compiled core
source of a program to see if two terms are represented by the same thing, and
if they are, that constitutes a proof that two terms are equal.
Even though it is very limited by the optimizations GHC does, and laziness and
polymorphism can sometimes prevent us from getting the desired result, I
thought it was a simple but great hack that should be utilized even more.

In [one of the examples](https://github.com/nomeata/ghc-proofs/blob/master/examples/Successors.hs),
a new data type is defined and then there are `Functor`, `Applicative`, and
`Monad` instances declared for this data type.  Then using the library there
are proof obligations for GHC, each functor, monad and applicative law written
by hand, such as below.

```haskell
functor_law1 :: Succs a -> Proof
functor_law1 v = fmap id v
             === v
```

Defining new data types and declaring type class instances is common in
Haskell. A user might also want to see if their definition obeys the relevant
type class laws in a way that is trivial to GHC. So let's automate that!
We want to generate new functions, and [Template
Haskell](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)
is a decent way to achieve this goal. (I'll abbreviate it to "TH".)
This post is a tutorial for people who want to see a real life example for TH.

## Basic types

Let's take a top-down approach.  The user of our library should be able to write

```haskell
$(makeLaws ''Maybe  [''Functor, ''Applicative, ''Monad])
```
or since the `$(...)` notation is unnecessary on top level, one can just write

```haskell
makeLaws ''Maybe  [''Functor, ''Applicative, ''Monad]
```

Note that the `''` notation is a way to get a value of type `Name`. This is how
names of variables, functions and types are represented in TH. However `''`
only works for types, for variables and functions you are supposed to use `'`,
like `'map`.

So we want to have a function that takes the name of the type, and the name of
the type class, and generates the functions. Here's what I came up with:

```haskell
makeLaws :: Name -> [Name] -> DecsQ
```

You might ask what `DecsQ` is.  Function declarations are represented by the
type `DecsQ` in TH. If we ask the REPL what that exactly is, we get the following
information:

```haskell
> :i DecsQ
type DecsQ = Q [Dec] 	-- Defined in ‘Language.Haskell.TH.Lib’
```

So it is simply a list of `Dec`s (declarations) in the `Q` (quotation) monad.
If we inquire further from the REPL:

```haskell
> :i Dec
data Dec
  = FunD Name [Clause]
  | ValD Pat Body [Dec]
  | DataD Cxt Name [TyVarBndr] (Maybe Kind) [Con] [DerivClause]
  ...
  | SigD Name Type
  ...
```

We get a long list of declaration constructors, of which only two are relevant
to us: `FunD` and `SigD`, i.e. function declarations and type signature
declarations. We will get back to this later.

## `Q` monad

Well, then what is the `Q` or quotation monad? For
now, we will say that anything that is quoted, or antiquoted will be in
the `Q` monad. In simple words, it is a way to get a syntax tree for some code
in the surface language syntax, and a way to inject a syntax tree in the code
you are writing. The syntax tree you get from some code or a syntax tree that
you inject both have to inhabit the `Q` monad. Let's see some examples:

```haskell
> :t [| map |]
[| map |] :: ExpQ

> runQ [| map |]
VarE GHC.Base.map

> :t [|| map ||]
[|| map ||] :: Q (TExp ((a -> b) -> [a] -> [b]))

> :t [t| Maybe Int |]
[t| Maybe Int |] :: TypeQ

> runQ [t| Maybe Int |]
AppT (ConT GHC.Base.Maybe) (ConT GHC.Types.Int)

> :t [d| f x = True |]
[d| f x = True |] :: DecsQ

> runQ [d| f x = True |]
[FunD f_0 [Clause [VarP x_1] (NormalB (ConE GHC.Types.True)) []]]
```

You can probably guess that `ExpQ = Q Exp` and `TypeQ = Q Type`, since you have
already seen `DecsQ = Q [Dec]`. They are simply expression and type syntax
trees in the `Q` monad. The second example with `TExp` is a typed quotation, in
which the type of the quoted expression appears in the type of the expression
syntax tree. We will not use this in our example, so I will not elaborate on
that.

## Back to our library

Now that we have a basic understanding of TH, we can go back to designing our library.
We said our entry point function would have the following type:

```haskell
makeLaws :: Name -> [Name] -> DecsQ
```

which would later be called like

```haskell
makeLaws ''Maybe  [''Functor, ''Applicative, ''Monad]
```

This call would create law proofs for the `Functor`, `Applicative` and `Monad`
instances. This seems like a simple iteration, so let's write a function that
takes one type and one type class to create laws for.

```haskell
singleTCLaws :: Name -> Name -> DecsQ
```

Obviously Haskell doesn't automatically know what laws there are for a given
type class, so we have to have a database from which we can lookup the laws.
Since we only have a handful of type classes here, let's just make an
association list.

```haskell
laws :: Name -> [(Name, DecsQ)]
```

We will define a function that takes the name of the type, like `''Maybe`,
and returns an association list where the keys are names of type classes,
such as `''Applicative` and the values are declaration lists.

Let's define one that only has the laws for the `Applicative` type class:

```haskell
laws :: Name -> [(Name, DecsQ)]
laws n = let t = returnQ (ConT n) in [
    (''Applicative,
     [d|
       law1 :: $t a -> Proof
       law1 v = pure id <*> v
            === v

       law2 :: $t (b -> c) -> $t (a -> b) -> $t a -> Proof
       law2 u v w = pure (.) <*> u <*> v <*> w
                === u <*> (v <*> w)

       law3 :: forall a b. (a -> b) -> a -> Proof
       law3 f x = pure f <*> (pure x :: $t a)
              === pure (f x)
     |])
  ]
```

Now this is a too much information to understand at once. Let's break it down.

Our function takes a name, and we want to use this name in a quotation, so we
have to convert this to a value of the type `TypeQ`. (i.e. `Q Type`)
We are using two things for that:

```haskell
> :t ConT
ConT :: Name -> Type

> :t returnQ
returnQ :: a -> Q a
```

The former is a constructor for `Type`, and the latter is a function to lift
the pure `Type` value to the `Q` monad.

Now that we have defined `t`, we can start our association list. For now we
only have one member of the list, which is a pair of `''Applicative`, which is
of the type `''Name`, and a declaration list quotation, of the type `DecsQ`.

The unusual thing in here is that we have some `$t` sprinkled around in our
code. This is called a splice*. (or antiquotation in this case, since it is
happening inside a quotation.) It is a way of injecting syntax tree
variables that we have in our metalanguage to the code that we are quoting. The
variable `t` that we defined now gets used in the type signature of the laws.

Notice that in `law3`, the type signature of the function does not contain `$t`.
Because of this, Haskell cannot infer that we only want to check things for a
specific applicative instance. Therefore we want to explicitly annotate some
part of the expression so that Haskell can infer that the entire function. If
we knew what exactly we were dealing with, such as `Maybe Int`, we could just
write `pure x :: Maybe Int`. However, we have to depend on the type variable
`a`. To do that, we need a language extension called
[ScopedTypeVariables](https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html).
To be able to use that, we need another language extension called
[ExplicitForAll](https://stackoverflow.com/questions/15800878/scoped-type-variables-require-explicit-foralls-why).
Now we can just say `pure x :: $t a` and the applicative instance for the rest
of the function will be inferred.

## Renaming functions

Now our `laws` function should be clear. But the problem now is that if we
generate laws for different types, their names will clash. We should have a way
to create names that contain the type and type class we are generating laws
for. Unfortunately we cannot do this in the quotation, we because there is [no
way to splice
names](https://stackoverflow.com/questions/16332124/template-haskell-names-of-declarations-as-strings).

Therefore we will have to generate names manually for our functions. Instead of generating entirely new names for laws, we can just take the approach of adding prefixes to all names. This will be a `map` over a call to `laws`.

```haskell
renamedLaws :: Name -> [(Name, DecsQ)]
renamedLaws n = map f (laws n)
  where
    ...
```

Now let's complete this function.
However, we first need a function to make an appropriate prefix.

```haskell
getName :: Name -> Name -> Name
getName t tc =
    mkName $ map toLower $ concat $ intersperse "_" [get t, get tc, ""]
  where get n | n == ''[] = "list"
              | otherwise = nameBase n
```

This function takes a type name and a type class name, and generates a prefix
of type `Name`. `getName ''Maybe ''Monad` will generate `maybe_monad_`. We
want a trailing underscore since this will be a prefix to something.
Notice that the `Name` for lists contains special characters that are not
allowed in variables, so we assign a different name for that as an exception.

Now let's write a function to concatenate two `Name`s.

```haskell
(+++) :: Name -> Name -> Name
(+++) a b = mkName (nameBase a ++ nameBase b)
```

Surely this is a bit hacky, since `Name` is meant to contain information like namespace and we are treating it like a string. For our purposes, this will work.

Let's go back to `renamedLaws`. We can complete the function as such:

```haskell
renamedLaws :: Name -> [(Name, DecsQ)]
renamedLaws n = map f (laws n)
  where
    prefix :: Name -> Dec -> Dec
    prefix newN (SigD oldN t) = SigD (newN +++ oldN) t
    prefix newN (FunD oldN t) = FunD (newN +++ oldN) t
    prefix _ d = d
    f :: (Name, DecsQ) -> (Name, DecsQ)
    f (tc, ds) = (tc, map (prefix (getName n tc)) <$> ds)
```

The function `f` acts on a pair of a type class name and declaration list. It leaves the name alone in the pair, but uses it to generate a new name for functions. Once we call `getName` for each combination of type and type class name, we can prefix it to the declarations in the `DecsQ`. However, notice that the declarations are in the `Q` monad, so we have to use `<$>` to apply the map.

The function `prefix` is more straightforward because we have already seen
`SigD` and `FunD` earlier. We check what declaration we are looking at; if we
have a type signature or function declaration, we prepend the prefix to the old
name.

Notice that the type of `renamedLaws` is a function from a type name to an
association list, just like `laws`.

## Finishing up

Now we can define `singleTCLaws` and `makeLaws`:

```haskell
singleTCLaws :: Name -> Name -> DecsQ
singleTCLaws n tc = fromMaybe
  (error $ "There are no laws defined for " ++ show tc)
  (lookup tc (renamedLaws n))

makeLaws :: Name -> [Name] -> DecsQ
makeLaws n tcs = concat <$> mapM (singleTCLaws n) tcs
```

The definition of `singleTCLaws` is merely a `lookup`from an association list.
If the lookup fails, we can just abort since this program will be run in compile time.

The definition of `makeLaws` is simply an iteration of a type class list. Once
we get different association lists from each call to `singleTCLaws`, we
`concat` them to get a single declaration list that a user of our library can
inject (i.e. splice) into their code.

In this post, we only defined laws for the `Applicative` instance, but you can
add more for other instances. [Here in the full
code](https://github.com/joom/ghc-proofs/blob/master/GHC/Proof/TH.hs), you can
see all the type classes for which we added laws.

# Example

Here is what a file that uses our library:

```haskell
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}
module ThTest where

import GHC.Proof.TH

makeLaws ''[]     [''Functor, ''Applicative, ''Monad]
makeLaws ''Maybe  [''Functor, ''Applicative, ''Monad]
makeLaws ''String [''Monoid]

main = putStrLn "Success."
```

And here's how we can see it work in compile time:

```
$ ghc -O ThTest.hs
[1 of 1] Compiling ThTest           ( ThTest.hs, ThTest.o )
GHC.Proof: Proving list_functor_law1 …
GHC.Proof: Proving list_functor_law2 …
GHC.Proof: Proving list_applicative_law1 …
GHC.Proof: Proving list_applicative_law2 …
GHC.Proof: Proving list_applicative_law3 …
GHC.Proof: Proving list_monad_law1 …
GHC.Proof: Proving list_monad_law2 …
GHC.Proof: Proving list_monad_law3 …
GHC.Proof: Proving list_monad_return_pure …
GHC.Proof: Proving list_monad_ap_star …
GHC.Proof: Proving maybe_functor_law1 …
GHC.Proof: Proving maybe_functor_law2 …
GHC.Proof: Proving maybe_applicative_law1 …
GHC.Proof: Proving maybe_applicative_law2 …
GHC.Proof: Proving maybe_applicative_law3 …
GHC.Proof: Proving maybe_monad_law1 …
GHC.Proof: Proving maybe_monad_law2 …
GHC.Proof: Proving maybe_monad_law3 …
GHC.Proof: Proving maybe_monad_return_pure …
GHC.Proof: Proving maybe_monad_ap_star …
GHC.Proof: Proving string_monoid_law2 …
GHC.Proof: Proving string_monoid_law1 …
GHC.Proof proved 22 equalities
```

So it works! It might not be a good idea to use in most cases since you cannot
tweak a specific law to get around laziness or other issues, but it makes a
good example of TH's power. Please let me know if there's any part of this that
is unclear or that can be improved!
