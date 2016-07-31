{-# LANGUAGE TypeFamilies #-}

{-|
    Module      : Data.Twine
    Description : A new, improved string type, featuring O(1) concatenation
    Copyright   : (c) Brian Hurt, 2016
    License     : BSD
    Maintainer  : bhurt42@gmail.com
    Stability   : experimental
    Portability : any

This XKCD Comic is relevant: <https://xkcd.com/927/>.

Please see the full defense of the existance of this library on the
project's home page, here: <https://github.com/bhurt/twine>.

-}
module Data.Twine(
    Twine
) where

    import Data.Twine.Internal.Glob
    import Data.Twine.Stringish
    import Data.Typeable
    import GHC.Generics
    import qualified GHC.Read
    import qualified Text.ParserCombinators.ReadPrec as PC
    import GHC.Exts
    import Data.String

    {-# WARNING todo "todo left in code!" #-}
    todo :: a
    todo = undefined

    newtype Twine = Twine { getGlob :: Glob }
        deriving (Typeable, Generic)

    instance Eq Twine where
        -- (==) :: Twine -> Twine -> Bool
        (==) = todo

    instance Ord Twine where
        -- compare :: Twine -> Twine -> Ordering
        compare = todo

    instance Show Twine where
        -- showsPrec :: Int -> Twine -> ShowS
        showsPrec prec twine = todo
        -- show :: Twine -> String
        show twine = todo
        -- showList :: [Twine] -> ShowS
        showList twines = todo

    instance Read Twine where
        -- readsPrec :: Int -> ReadS Twine
        readsPrec prec = todo
        -- readList :: ReadS [Twine]
        readList = todo
        -- readPrec :: PC.ReadPrec Twine
        readPrec = todo
        -- readListPrec :: PC.ReadPrec [Twine]
        readListPrec = todo

    instance IsList Twine where
        type Item Twine = Char
        -- fromList :: [ Char ] -> Twine
        fromList s = todo
        -- fromListN :: Int -> [ Char ] -> Twine
        fromListN n s = todo
        -- toList :: Twine -> [ Char ]
        toList twine = todo

    instance IsString Twine where
        -- fromString :: String -> Twine
        fromString s = todo

    instance Stringish Twine where
        type C Twine  = Char
        type Len Twine = Int

        -- pack :: [ C t ] -> t
        pack = todo
        -- unpack :: t -> [ C t ]
        unpack = todo
        -- singleton :: C t -> t
        singleton = todo
        -- empty :: t
        empty = todo
        -- cons :: C t -> t -> t
        cons = todo
        -- snoc :: t -> C t -> t
        snoc = todo
        -- append :: t -> t -> t
        append = todo
        -- uncons :: t -> Maybe (C t, t)
        uncons = todo
        -- head :: t -> C t
        head = todo
        -- last :: t -> C t
        last = todo
        -- tail :: t -> t
        tail = todo
        -- init :: t -> t
        init = todo
        -- null :: t -> Bool
        null = todo
        -- length :: t -> Len t
        length = todo
        -- map :: (C t -> C t) -> t -> t
        map = todo
        -- intercalate :: t -> [ t ] -> t
        intercalate = todo
        -- intersperse :: C t -> t -> t
        intersperse = todo
        -- transpose :: [ t ] -> [ t ]
        transpose = todo
        -- reverse :: t -> t
        reverse = todo
        -- foldl :: (a -> C t -> a) -> a -> t -> a
        foldl = todo
        -- foldl' :: (a -> C t -> a) -> a -> t -> a
        foldl' = todo
        -- foldl1 :: (C t -> C t -> C t) -> t -> C t
        foldl1 = todo
        -- foldl1' :: (C t -> C t -> C t) -> t -> C t
        foldl1' = todo
        -- foldr :: (C t -> a -> a) -> a -> t -> a
        foldr = todo
        -- foldr1 :: (C t -> C t -> C t) -> t -> C t
        foldr1 = todo
        -- concat :: [ t ] -> t
        concat = todo
        -- concatMap :: (C t -> t) -> t -> t
        concatMap = todo
        -- any :: (C t -> Bool) -> t -> Bool
        any = todo
        -- all :: (C t -> Bool) -> t -> Bool
        all = todo
        -- maximum :: t -> C t
        maximum = todo
        -- minimum :: t -> C t
        minimum = todo
        -- scanl :: (C t -> C t -> C t) -> C t -> t -> t
        scanl = todo
        -- mapAccumL :: (a -> C t -> (a, C t)) -> a -> t -> (a, t)
        mapAccumL = todo
        -- mapAccumR :: (a -> C t -> (a, C t)) -> a -> t -> (a, t)
        mapAccumR = todo
        -- unfoldr :: (a -> Maybe (C t, a)) -> a -> t
        unfoldr = todo
        -- take :: Len t -> t -> t
        take = todo
        -- drop :: Len t -> t -> t
        drop = todo
        -- takeWhile :: (C t -> Bool) -> t -> t
        takeWhile = todo
        -- dropWhile :: (C t ->  Bool) -> t -> t
        dropWhile = todo
        -- splitAt :: Len t -> t -> (t, t)
        splitAt = todo
        -- break :: (C t -> Bool) -> t -> (t, t)
        break = todo
        -- span :: (C t -> Bool) -> t -> (t, t)
        span = todo
        -- group :: t -> [ t ]
        group = todo
        -- groupBy :: (C t -> C t -> Bool) -> t -> [ t ]
        groupBy = todo
        -- inits :: t -> [ t ]
        inits = todo
        -- tails :: t -> [ t ]
        tails = todo
        -- lines :: t -> [ t ]
        lines = todo
        -- words :: t -> [ t ]
        words = todo
        -- unlines :: [ t ] -> t
        unlines = todo
        -- unwords :: [ t ] -> t
        unwords = todo
        -- isPrefixOf :: t -> t -> Bool
        isPrefixOf = todo
        -- isSuffixOf :: t -> t -> Bool
        isSuffixOf = todo
        -- filter :: (C t -> Bool) -> t -> t
        filter = todo
        -- find :: (C t -> Bool) -> t -> Maybe (C t)
        find = todo
        -- index :: t -> Len t -> C t
        index = todo
        -- zip :: t -> t -> [ (C t, C t) ]
        zip = todo

    instance Textish Twine where
        -- replace :: t -> t -> t -> t
        replace = todo
        -- toCaseFold :: t -> t
        toCaseFold = todo
        -- toLower :: t -> t
        toLower = todo
        -- toUpper :: t -> t
        toUpper = todo
        -- toTitle :: t -> t
        toTitle = todo
        -- justifyLeft :: Len t -> C t -> t -> t
        justifyLeft = todo
        -- justifyRight :: Len t -> C t -> t -> t
        justifyRight = todo
        -- center :: Len t -> C t -> t -> t
        center = todo
        -- takeEnd :: Len t -> t -> t
        takeEnd = todo
        -- dropEnd :: Len t -> t -> t
        dropEnd = todo
        -- takeWhileEnd :: (C t -> Bool) -> t -> t
        takeWhileEnd = todo
        -- dropWhileEnd :: (C t -> Bool) -> t -> t
        dropWhileEnd = todo
        -- dropAround :: (C t -> Bool) -> t -> t
        dropAround = todo
        -- strip :: t -> t
        strip = todo
        -- stripStart :: t -> t
        stripStart = todo
        -- stripEnd :: t -> t
        stripEnd = todo
        -- breakOn :: t -> t -> (t, t)
        breakOn = todo
        -- breakOnEnd :: t -> t -> (t, t)
        breakOnEnd = todo
        -- splitOn :: t -> t -> [ t ]
        splitOn = todo
        -- chunksOf :: Len t -> t -> [ t ]
        chunksOf = todo
        -- stripPrefix :: t -> t -> Maybe t
        stripPrefix = todo
        -- stripSuffix :: t -> t -> Maybe t
        stripSuffix = todo
        -- commonPrefixes :: t -> t -> Maybe (t, t, t)
        commonPrefixes = todo
        -- breakOnAll :: t -> t -> [ (t, t) ]
        breakOnAll = todo
        -- partition :: (C t -> Bool) -> t -> (t, t)
        partition = todo
        -- scanl1 :: (C t -> C t -> C t) -> t -> t
        scanl1 = todo
        -- scanr :: (C t -> C t -> C t) -> C t -> t -> t
        scanr = todo
        -- scanr1 :: (C t -> C t -> C t) -> t -> t
        scanr1 = todo
        -- unfoldrN :: Len t -> (a -> Maybe (C t, a)) -> a -> t
        unfoldrN = todo
        -- count :: t -> t -> Len t
        count = todo
        -- split :: (C t -> Bool) -> t -> [ t ]
        split = todo
        -- replicate :: Len t -> t -> t
        replicate = todo
        -- zipWith :: (C t -> C t -> C t) -> t -> t -> t
        zipWith = todo
        -- compareLength :: t -> Len t -> Ordering
        compareLength = todo


