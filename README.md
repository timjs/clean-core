# Clean Core

A core library for Clean or a beefed up StdEnv ;-)

## Goals

The main goal of this package is to provide a modern and flexible base library, where possible, using Haskell conventions and naming.

One main difference between Clean Base and Haskell Base is the number hierarchy. Because the Haskell numeric classes combine operations on numbers and literal conversion, they are overly complicated. Clean doesn't have overloading of number literals, thus we can leave this feature out.

We introduce a hierarchy starting with `Seminum` which contains algebraic operations on *positive numbers* only. As the name suggests it is inspired on a *semiring* from abstract algebra. The `Num` class adds negation and subtraction (a mathematical *ring*). From `Seminum` (not `Num`) hereon we add classes `Integral` and `Fractional`, expanding the basic operations on numbers with integer division or real division. `Transcedental` contains power- and trigonometric operations (it is roughly equivalent with Haskell's `Floating` class). All these classes do *not* require `Eq`, `Ord` or `Enum`.

The `abs` and `signum` functions are part of the class `Signed`, which has constraints to `Ord` and `Num` and thus is an "ordered ring". `Signed` is complemented by the class `Unsigned` which represents "ordered semirings".

## Rules

* Classes should represent mathematical properties, preferably accompanied by laws.
* A fully qualified name of any entity should not be redundant: `User.UserRole.Roles.getRoleForUser` should probably be something like `User.Role.get` [1].

## More tips

- http://fvisser.nl/post/2013/may/28/towards-a-better-haskell-package.html

[1]: https://www.reddit.com/r/haskell/comments/1f70wi/towards_a_better_haskell_package/
