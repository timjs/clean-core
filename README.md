# Clean Core

A core library for Clean or a beefed up StdEnv ;-)

## Goals

The main goal of this package is to provide a modern and flexible base library, where possible, using Haskell conventions and naming.

One main difference between Clean Base and Haskell Base is the number hierarchy. Because the Haskell numeric classes combine operations on numbers and literal conversion, they are overly complicated. Clean doesn't have overloading of number literals, thus we can leave this feature out.

We introduce a hierarchy starting with `Seminum` which contains algebraic operations on *positive numbers* only. As the name suggests it is inspired on a *semiring* from abstract algebra. The `Num` class adds negation and subtraction (a mathematical *ring*). From `Seminum` (not `Num`) hereon we add classes `Integral` and `Fractional`, expanding the basic operations on numbers with integer division or real division. `Transcedental` contains power- and trigonometric operations (it is roughly equivalent with Haskell's `Floating` class). All these classes do *not* require `Eq`, `Ord` or `Enum`.

The `abs` and `signum` functions are part of the class `Signed`, which has constraints to `Ord` and `Num` and thus is an "ordered ring". `Signed` is complemented by the class `Unsigned` which represents "ordered semirings".

## Naming

Naming is aways difficult and subjective. Here, we'd like to provide some basic guidelines and conventions. They are based on naming guidelines from modern programming languages like Rust[^rust-guidelines], Swift[^swift-guidelines] and Elm[^elm-guidelines].


* Names of other types, variables, and fields should read as nouns.[^swift-guidelines]
* Functions using their arguments to calculating a new result should read as noun phrases, e.g. `distanceBetween x y`, `successorOf i`.[^swift-guidelines]
* Functions forming a new data structure, based on their last argument should read as imperative verb phrases, e.g., `show x`, `sort xs`, `append xs ys`.[^swift-guidelines]
* Interfaces (classes and generics) that describe a capability should be named using a verb, e.g. `Equate`, `Report`.[^swift-guidelines,rust-guidelines]
* Interfaces (classes and generics) that describe what something is should read as nouns, e.g. `Integral`, `Functor`.[^swift-guidelines]


### Use human readable names

Abbreviations are generally a silly idea for an API. Having an API that is clear is more important than saving three or four characters by dropping letters from a name.

Infix operators are not a substitute for human readable names. They are impossible to Google for. They encourage users to not use module prefixes, making it impossible to figure out what module they came from. This makes them even harder to find. More on this later.

### Module names should not reappear in function names

A function called `State.runState` is redundant and silly. More importantly, it encourages people to use `import State exposing (..)`` which does not scale well. In files with many so-called "unqualified" dependencies, it is essentially impossible to figure out where functions are coming from. This can make large code bases impossible to understand, especially if custom infix operators are used as well. Repeating the module name actively encourages this kind of unreadable code.

With a name like `State.run` the user is encouraged to disambiguate functions with namespacing, leading to a codebase that will be clearer to people reading the project for the first time. A great example from the standard library is Bitwise.and. This reads a lot better than &, which brings us to...

### Avoid infix operators

They should never take the place of a well-named human readable function. In a large code base that is maintained by many people, infix operators are typically a bad idea.

* They are difficult to search for online.
* They are difficult to search for in a codebase too because they are rarely prefixed with the module they were imported from.
* They usually offer no insight into what they actually do. To the uninitiated, things like `(<*>)` and `(!?)` are meaningless.
* Now lets assume you have a really great infix operator, an operator that actually represents its meaning in a very direct way, like `(<~)`. In this case, it is still recommended that you do not add the infix operator.

Okay, but lets say you want to do it anyway. One way to do it is to provide a recommended set of infix operators at the end of your library documentation. Experienced users can go see if they like them and define them if they really want. That way the API can be nice and human readable and encourage its users to write code that is nice and human readable.

Okay, but lets say you just don't care about recommendations and you have a great infix operator. Add them in a separate module. When someone sees an infix operator they are unfamiliar with, they can scan the imports for a Whatever.Infix module and limit the scope of their annoying search for your dumb operator.

## Rules

* Classes should represent mathematical properties, preferably accompanied by laws.
* A fully qualified name of any entity should not be redundant: `User.UserRole.Roles.getRoleForUser` should probably be something like `User.Role.get` [1].

## More tips

- http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html

[1]: https://www.reddit.com/r/haskell/comments/1f70wi/towards_a_better_haskell_package/
