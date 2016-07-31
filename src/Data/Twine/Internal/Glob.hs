{-|
    Module      : Data.Twine.Internal.Glob
    Description : Unbalanced trees of bytearrays with fast first/last access.
    Copyright   : (c) Brian Hurt, 2016
    License     : BSD
    Maintainer  : bhurt42@gmail.com
    Stability   : experimental
    Portability : any

A glob is an unbalanced tree of bytearrays, with the first and last elements
stored separately for fast access.

-}
module Data.Twine.Internal.Glob(
    Glob,
    cons,
    snoc,
    empty,
    append,
    Data.Twine.Internal.Glob.foldl,
    Data.Twine.Internal.Glob.foldr,
    Data.Twine.Internal.Glob.null,
    appendCombine,
    first,
    Data.Twine.Internal.Glob.last,
    replaceFirst,
    replaceLast,
    uncons,
    unsnoc
) where

    import Data.Primitive.ByteArray
    import Data.Typeable
    import GHC.Generics

    data UTree =
        Leaf {-# UNPACK #-} !ByteArray
        | Branch !UTree !UTree
        deriving (Typeable, Generic)

    data Glob =
        Empty
        | One {-# UNPACK #-} !ByteArray
        | Two {-# UNPACK #-} !ByteArray {-# UNPACK #-} !ByteArray
        | Tree {-# UNPACK #-} !ByteArray !UTree {-# UNPACK #-} !ByteArray
        deriving (Typeable, Generic)

    rotateLeft :: UTree -> UTree -> UTree
    rotateLeft a (Branch b c) = Branch (Branch a b) c
    rotateLeft a xs           = Branch a xs

    rotateRight :: UTree -> UTree -> UTree
    rotateRight (Branch a b) c = Branch a (Branch b c)
    rotateRight xs           c = Branch xs c

    cons :: ByteArray -> Glob -> Glob
    cons a Empty         = One a
    cons a (One b)       = Two a b
    cons a (Two b c)     = Tree a (Leaf b) c
    cons a (Tree b t c)  = Tree a (rotateLeft (Leaf b) t) c

    snoc :: Glob -> ByteArray -> Glob
    snoc Empty         a = One a
    snoc (One a)       b = Two a b
    snoc (Two a b)     c = Tree a (Leaf b) c
    snoc (Tree a t b)  c = Tree a (rotateRight t (Leaf b)) c

    empty :: Glob
    empty = Empty

    append :: Glob -> Glob -> Glob
    append Empty        ys              = ys
    append xs           Empty           = xs
    append (One a)      ys              = cons a ys
    append xs           (One a)         = snoc xs a
    append (Two a b)    ys              = cons a (cons b ys)
    append xs           (Two a b)       = snoc (snoc xs a) b
    append (Tree a s b) (Tree c t d)    = Tree a u d
        where
            u = Branch (rotateRight s (Leaf b))
                        (rotateLeft (Leaf c) t)

    foldl :: (b -> ByteArray -> b) -> b -> Glob -> b
    foldl _ i Empty = i
    foldl f i (One a) = f i a
    foldl f i (Two a b) = f (f i a) b
    foldl f i (Tree a t b) = f (go (f i a) t) b
        where
            go curr (Branch l r) = go (go curr l) r
            go curr (Leaf x)     = f curr x

    foldr :: (ByteArray -> b -> b) -> b -> Glob -> b
    foldr _ i Empty = i
    foldr f i (One a) = f a i
    foldr f i (Two a b) = f a (f b i)
    foldr f i (Tree a t b) = f a (go t (f b i))
        where
            go (Branch l r) curr = go l (go r curr)
            go (Leaf x)     curr = f x curr

    null :: Glob -> Bool
    null Empty = True
    null _     = False

    appendCombine :: (ByteArray -> ByteArray -> Maybe ByteArray)
                        -> Glob -> Glob -> Glob
    appendCombine _ Empty   ys    = ys
    appendCombine _ xs      Empty = xs
    appendCombine f (One a) (One b) =
        case f a b of
            Nothing -> Two a b
            Just c -> One c
    appendCombine f (One a) (Two b c) =
        case f a b of
            Nothing -> Tree a (Leaf b) c
            Just d -> Two d c
    appendCombine f (One a) (Tree b t c) =
        case f a b of
            Nothing -> Tree a (rotateLeft (Leaf b) t) c
            Just d -> Tree d t c
    appendCombine f (Two a b) (One c) =
        case f b c of
            Nothing -> Tree a (Leaf b) c
            Just d -> Two a d
    appendCombine f (Two a b) (Two c d) =
        case f b c of
            Nothing -> Tree a (Branch (Leaf b) (Leaf c)) d
            Just e -> Tree a (Leaf e) d
    appendCombine f (Two a b) (Tree c t d) =
        case f b c of
            Nothing -> Tree a (Branch (Branch (Leaf b) (Leaf c)) t) d
            Just e -> Tree a (rotateLeft (Leaf e) t) d
    appendCombine f (Tree a t b) (One c) =
        case f b c of
            Nothing -> Tree a (rotateRight t (Leaf b)) c
            Just e -> Tree a t e
    appendCombine f (Tree a t b) (Two c d) =
        case f b c of
            Nothing -> Tree a (Branch t (Branch (Leaf b) (Leaf c))) d
            Just e -> Tree a (rotateRight t (Leaf e)) d
    appendCombine f (Tree a s b) (Tree c t d) =
        case f b c of
            Nothing -> Tree a (Branch (rotateRight s (Leaf b))
                                    (rotateLeft (Leaf c) t)) d
            Just e -> Tree a (Branch (rotateRight s (Leaf e)) t) d

    first :: Glob -> Maybe ByteArray
    first Empty = Nothing
    first (One a) = Just a
    first (Two a _) = Just a
    first (Tree a _ _) = Just a

    last :: Glob -> Maybe ByteArray
    last Empty = Nothing
    last (One a) = Just a
    last (Two _ a) = Just a
    last (Tree _ _ a) = Just a

    replaceFirst :: ByteArray -> Glob -> Glob
    replaceFirst a Empty = One a
    replaceFirst a (One _) = One a
    replaceFirst a (Two _ b) = Two a b
    replaceFirst a (Tree _ t b) = Tree a t b

    replaceLast :: Glob -> ByteArray -> Glob
    replaceLast Empty a = One a
    replaceLast (One _) a = One a
    replaceLast (Two a _) b = Two a b
    replaceLast (Tree a t _) b = Tree a t b

    uncons :: Glob -> Maybe (ByteArray, Glob)
    uncons Empty = Nothing
    uncons (One a) = Just (a, Empty)
    uncons (Two a b) = Just (a, One b)
    uncons (Tree a t b) = Just (a, go t b)
        where
            go (Leaf c) d = Two c d
            go (Branch l r) d = go2 l r d
            go2 (Leaf c) r d = Tree c r d
            go2 (Branch x y) r d = go2 x (Branch y r) d

    unsnoc :: Glob -> Maybe (Glob, ByteArray)
    unsnoc Empty = Nothing
    unsnoc (One a) = Just (Empty, a)
    unsnoc (Two a b) = Just (One a, b)
    unsnoc (Tree a t b) = Just (go a t, b)
        where
            go c (Leaf d) = Two c d
            go c (Branch l r) = go2 c l r
            go2 c l (Leaf d) = Tree c l d
            go2 c l (Branch x y) = go2 c (Branch l x) y

