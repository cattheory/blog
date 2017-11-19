---
title: Scoped implicit conversions for Idris
author: joomy
tags: idris, proposal
description: The Idris community is not so fond of implicit conversions, I think they're a useful and powerful tool that are easy to misuse. Here's an idea to fix them.
---

The Idris community is not so fond of implicit conversions, I think they're a useful and powerful tool that are easy to misuse, especially because they are global. Here's an idea to fix them by making them scoped[^fn-1].

# What is wrong with implicit conversions?

[Implicit conversions](http://docs.idris-lang.org/en/latest/tutorial/miscellany.html#implicit-conversions)
are a controversial feature of Idris. Even in the Idris documentation, users
are discouraged to use them for simple types, and advised to use them for DSLs
or to simplify intricate proofs.

Here's how implicit conversions currently work:

```idris
implicit intString : Int -> String
intString = show

test : Int -> String
test x = "Number " ++ x
```

In the `test` function, the variable `x` is of type `Int`, but it is
automatically converted to a `String` by an application of `intString`.
Moreover, if `intString` is exported from the module which is later
imported to another module, `intString` will be implicitly applied in the outer
module too! 🎃 (well, Halloween is over but this is quite spooky.)

It is understandable that implicit conversions resemble weak typing, especially
when it is used on base types like `Int` and `String`. However, don't forget
that unlike weak typing, we know exactly what we're doing[^fn-2].
One can argue that implicit conversions bite us when we don't know what we are
doing, and they prevent the type system from doing its job of warning us in
those cases.

Here's an example of how it can bite us:
```idris
implicit boolString : Bool -> String
boolString = show

implicit stringBool : String -> Bool
stringBool "True" = True
stringBool _ = False

record Person where
  constructor MkPerson
  name : String
  hasCar : Bool

joe : Person
joe = MkPerson "Joe" True

fred : Person
fred = MkPerson True "Fred"
```

We wouldn't want `fred` to type check, because we made a mistake about the
argument order. But it does type check because we have carelessly defined two
implicit conversions that convert `Bool` and `String` to each other.
So now we ended up with an incorrect value for `fred`, which will evaluate to
`MkPerson "True" False`.

There might also be a use case for two implicit conversions that convert to
each other. The actual problem with this is that at a given point, we do not
have a way to know what implicit conversions are active. Especially if you
import libraries that export implicit conversions, this is a serious problem.

Another problem is when we have multiple applicable implicit conversions:

```idris
implicit natString : Nat -> String
natString Z = "Z"
natString (S n) = "S(" ++ natString n ++ ")"

implicit natString2 : Nat -> String
natString2 = show

test2 : Nat -> String
test2 x = "Nat " ++ x
```

This code doesn't type check, but if we remove either `natString` or
`natString2`, then it does. If we have no way of what implicit conversions we
have at a given point, we can easily make the mistake of having multiple
implicit conversions of the same type. We need a way to limit them.

# Making implicit conversions non-global

Here's a list of possible solutions and their pros and cons.

## Declaring the export level of implicit conversion

Currently if an implicit conversion function is exported, it will be an implicit
conversion wherever it is imported. One solution might be to add a new keyword
that declares the function to be an implicit conversion only inside that module.
Even though it strongly regulates implicits, this isn't really what we need
because if an implicit conversion is useful, it should be available in other
modules also as an implicit conversion.

We can currently (as of Idris 1.1.1) achieve this with a small hack:

```idris
public export
boolString : Bool -> String
boolString = show

private implicit
boolString' : Bool -> String
boolString' = boolString
```

This way, the actual function `boolString` is exported and not implicit, and we
will have `boolString'`, which is just an alias for `boolString`, only inside
this module, available as an implicit conversion.

Similarly, if we are in a different module, we can just define a `private
implicit` alias for the conversion you want. This works, but it is repetitive.

## Scoped implicit conversions

A better solution would be to have a new syntax block
(similar to [parametrized blocks](http://docs.idris-lang.org/en/latest/tutorial/modules.html#parameterised-blocks))
that limits the scope of the implicit conversions. This way, a function would
only become an implicit conversion if it is declared one for the block. For definitions outside the block, there would be no implicit conversion. Here's what it would look like: (This is not valid Idris yet, just a proposal)

```idris
intString : Int -> String
intString = show

natString : Nat -> String
natString Z = "Z"
natString (S n) = "S(" ++ natString n ++ ")"

implicits (intString)
  test : Int -> String
  test x = "Number " ++ x

implicits (intString, natString)
  test2 : Int -> Nat -> String
  test2 x y = "Hello " ++ x ++ ", " ++ y
```

While these aren't crucial use cases, but they illustrate what the idea is.
The only problem with this is, what should we do if there are too many implicit
conversions? Do we have to list all of them every time we want to use them? The
point of having implicit conversions is to save time, after all.

Here's an example that has many implicit conversions (in proposed syntax),
we will try to take advantage of the implicit conversion to write
S-expressions easily.

```idris
data SExp =
   SExpList (List SExp) | StringAtom String | BoolAtom Bool |
   IntegerAtom Integer | SymbolAtom String

okMessage : SExp
okMessage = SExpList [SymbolAtom "ok", BoolAtom True]

implicits (SExpList, BoolAtom, IntegerAtom, SymbolAtom)
  okMessage2 : SExp
  okMessage2 = ["ok", True]
```

We don't want to list all the implicit conversions every time, so what if we
had a way to collectively refer to them? Namespaces sound like a good way to do that. What if we could do something like this: (proposed syntax)

```idris
namespace SExpDef
  data SExp =
    SExpList (List SExp) | StringAtom String | BoolAtom Bool |
    IntegerAtom Integer | SymbolAtom String
  %implicits SExpList, BoolAtom, IntegerAtom, SymbolAtom

implicits SExpDef
  okMessage2 : SExp
  okMessage2 = ["ok", True]
```

This way we can declare the implicit conversions associated with a namespace,
without declaring them as global implicit conversions. Later we can
declare a block `implicits SExpDef` and use those as implicit conversions.

### Constructors as implicit conversions

Idris doesn't allow constructors to be declared implicit conversions yet.
I proposed this on Idris IRC recently, and got multiple replies that said
implicit conversions should be removed from Idris altogether.
Currently implicit conversions are limited to function definitions.
So we could do the little hack we mentioned above and define new functions whose
sole purpose is to function as implicit conversions.

```idris
implicit sExpList : List SExp -> SExp
sExpList = SExpList
implicit boolAtom : Bool -> SExp
boolAtom = BoolAtom
implicit integerAtom : Integer -> SExp
integerAtom = IntegerAtom
implicit symbolAtom : String -> SExp
symbolAtom = SymbolAtom
```

Once again, this is repetitive.  This new syntax that I'm proposing removes the
restriction that implicit conversions can be declared only in function
definitions.

# Conclusion

If you don't like implicit conversions, that is probably because they are
global, and they can override each other and cause type errors, and then we
don't have good enough type errors yet that deal with implicit conversions. So
let's invent a new way to handle implicit conversions by limiting which ones we
want to use in a definition. My suggestions is to introduce a new syntax block
that I called `implicits` that allows us to give a scope determine which
implicit conversions we want to apply, and where.


[^fn-1]: Thanks to Jaden Geller for [naming the idea](https://twitter.com/JadenGeller/status/930311056085409792). You might also want to check the previous/quoted tweets in the thread.
[^fn-2]: It's hard not to think about [Marco Rubio](https://www.youtube.com/watch?v=dqshYG4qvT4) when this phrase comes up.

