# twine
A new Haskell String Representation, featuring O(1) Concatenation

This [XKCD Comic](https://xkcd.com/927/).  is relevant. 

It has already been observed by many people that Haskell has five (at
least!) different data structures that represent strings in various forms-
classic Strings (as a type alias for [Char]), ByteStrings in both strict and
lazy versions, and Texts in both strict and lazy versions.  It's natural to
ask in this situation whether Haskell really needs another string class.
What does the new data structure bring that the previous ones do not?

One of the unfortunate legacies of C is that is had trained generations of
programmers that the proper representation of a string is an array of
characters.  The conventional wisdom is that this the most efficient
representation of strings.  In C, this may even have been true- however, a
moments thought casts grave doubt on the validity of conventional wisdom in
a modern, garbage collected language.

The operation which arrays are most efficient at- random access- is almost
never used with strings.  By far the most common operations over strings are
concatenation and folding.  We make big strings by concatenating many small
strings together, and then we fold over them to perform various operations
(output and regular expression matching are two common operations which are
obviously built on top of fold).

When we consider the efficiencies of the common operations on the array
representation of strings, the problem becomes apparent: string
concatenation, arguably the single most important operation on strings, is
O(N).  This gives rise to the existance of Builder type designed for
efficient concatenation.

In C, where strings can be mutated at will and memory management is manual,
it may not be possible to avoid copying, and thus do any better than the
array of characters representation.  In imperative garbage collected
languages, the efficiency of the builder type is gained by side effects.

