---
date: 1900-01-01
---


pattern synonyms
=================

These can be used to remove overhead. The general idea is that you can add
synoyms to GHC's pattern matching system. For example, if you wind up with enums
that reference `CInt`s like:

    data Enum = A | B | C

    toEnum :: Enum -> CInt
    toEnum A = #{const CLIB_ENUM_REPRESENTING_0}
    toEnum B = #{const CLIB_ENUM_REPRESENTING_1}
    toEnum C = #{const CLIB_ENUM_REPRESENTING_2}

(Note that this example is just a [contextless version of O'Charles'][24-ps])

It works, but it comes with overhead: we still have to do runtime conversions
between the CInts and Enums. However, with pattern synonyms we can directly
alias the CInts:

    pattern A = #{const CLIB_ENUM_REPRESENTING_0} :: CInt
    pattern B = #{const CLIB_ENUM_REPRESENTING_1} :: CInt
    pattern C = #{const CLIB_ENUM_REPRESENTING_2} :: CInt

    -- and later:

    foo :: CInt -> IO ()
    foo A = do
      putStrLn "using enum A"
      -- ...

To make this more semantic and safe, we can use `newtype`. This makes the
pattern a bidirectional relationship:

    newtype Enum = MkEnum { unEnum :: CInt }

    pattern A = MkEnum #{const CLIB_ENUM_REPRESENTING_0}
    pattern B = MkEnum #{const CLIB_ENUM_REPRESENTING_1}
    pattern C = MkEnum #{const CLIB_ENUM_REPRESENTING_2}

Bidirectional because we can now use these patterns to construct data:

    bar :: IO ()
    bar = do
      baz (unEnum A)
      where
        baz :: CInt -> IO ()
        ...

[24-ps]: https://ocharles.org.uk/blog/posts/2014-12-03-pattern-synonyms.html

