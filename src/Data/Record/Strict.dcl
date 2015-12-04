definition module Data.Record.Strict

/// Strict records with strict components can be unboxed by the compiler.
/// The language does not (yet) support abstract record type synonyms.
/// Therefore we define some simple strict records in this module that can be
/// used as abstract type synonyms in other modules.

/// # Definition

:: Pair a b = !
    { x :: !a
    , y :: !b
    }

:: Triple a b c = !
    { x :: !a
    , y :: !b
    , z :: !c
    }

