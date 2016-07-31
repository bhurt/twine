# twine
A new Haskell String Representation, featuring O(1) Concatenation

This [XKCD Comic](https://xkcd.com/927/) is relevant. 

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
Here, the separation of the string class from the builder class is that the
string class is immutable, while the builder isn't.

Neither of these conditions hold for Haskell, however.  Builders, like the
Strings they build, are immutable.  And yet, both the Text and ByteString
types come with associated builders (thus arguably making it seven, not
five, different string classes).  It is the opinion of the author that the
existance of immutable builder classes in Haskell is a sign that it is the
wrong representation of strings.  This is due to the "C rot" having spread
even into Haskell- both Text and ByteString are, at heart, array
representations.  They are simply repeating without question the mistakes of
other languages.

There is another problem with treating strings as arrays of characters, one
more subtle but every bit as pernicious: it requires the implementation to
chose an encoding.  This is because the underlying array has to be regular-
you need to be able to concatenate any two arrays, and all the contents of
any given array need to be the same.  So this means there needs to be one,
and only one, encoding.

And which ever encoding is choosen, it is sure to be inefficient for some
common use cases.  7-bit ASCII and Latin-1 etc.  encodings simply give up
the ability to represent non-European languages.  UTF-8 approximately
doubles the size of non-English strings over their UTF-16 representation,
and UTF-16 doubles the size of English or European strings.  So Text uses
UTF-16, while ByteStrings tend to use either UTF-8 or Latin-1 encodings.
By choosing a data structure, you are implicitly hard coding a choice of
encoding, a choice that will be bad in many cases.

So, with all of this in mind, how do we implment a proper string
representation?

- For our fundamental underlying data structure, instead of arrays, we use
unbalanced trees.  By not balancing our trees, we gain O(1) concatenation,
and keep O(N) traversal (for N Leaf nodes there will be N-1 Branch nodes,
and each node is visited once).  Accessing a random element is O(N), but we
don't care about that.

- The tree holds blocks of characters.  Each block of characters can have a
different encoding.  This means we can concatenate two strings encoded in
different ways without having to recode one of them. Currently we just
support Latin-1 and UTF-16 encodings.

- The blocks must be appropriately sized- they need to be large enough to
amoritize the cost of the tree data structure that holds (the cost of
which is assumed to be approximately 40 bytes per block on a 64-bit system).
But they should not be so large as to make copying them an onerous expense.

