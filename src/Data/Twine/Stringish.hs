{-# LANGUAGE TypeFamilies #-}

module Data.Twine.Stringish where

    import qualified Data.String
    import qualified Data.Text
    import qualified Data.Text.Lazy
    import qualified Data.ByteString.Char8
    import qualified Data.ByteString.Lazy.Char8
    import qualified Data.Int

    class Data.String.IsString t => Stringish t where
        type C t :: *
        type Len t :: *

        pack :: [ C t ] -> t
        unpack :: t -> [ C t ]
        singleton :: C t -> t
        empty :: t
        cons :: C t -> t -> t
        snoc :: t -> C t -> t
        append :: t -> t -> t
        uncons :: t -> Maybe (C t, t)
        head :: t -> C t
        last :: t -> C t
        tail :: t -> t
        init :: t -> t
        null :: t -> Bool
        length :: t -> Len t
        map :: (C t -> C t) -> t -> t
        intercalate :: t -> [ t ] -> t
        intersperse :: C t -> t -> t
        transpose :: [ t ] -> [ t ]
        reverse :: t -> t
        foldl :: (a -> C t -> a) -> a -> t -> a
        foldl' :: (a -> C t -> a) -> a -> t -> a
        foldl1 :: (C t -> C t -> C t) -> t -> C t
        foldl1' :: (C t -> C t -> C t) -> t -> C t
        foldr :: (C t -> a -> a) -> a -> t -> a
        foldr1 :: (C t -> C t -> C t) -> t -> C t
        concat :: [ t ] -> t
        concatMap :: (C t -> t) -> t -> t
        any :: (C t -> Bool) -> t -> Bool
        all :: (C t -> Bool) -> t -> Bool
        maximum :: t -> C t
        minimum :: t -> C t
        scanl :: (C t -> C t -> C t) -> C t -> t -> t
        mapAccumL :: (a -> C t -> (a, C t)) -> a -> t -> (a, t)
        mapAccumR :: (a -> C t -> (a, C t)) -> a -> t -> (a, t)
        unfoldr :: (a -> Maybe (C t, a)) -> a -> t
        take :: Len t -> t -> t
        drop :: Len t -> t -> t
        takeWhile :: (C t -> Bool) -> t -> t
        dropWhile :: (C t ->  Bool) -> t -> t
        splitAt :: Len t -> t -> (t, t)
        break :: (C t -> Bool) -> t -> (t, t)
        span :: (C t -> Bool) -> t -> (t, t)
        group :: t -> [ t ]
        groupBy :: (C t -> C t -> Bool) -> t -> [ t ]
        inits :: t -> [ t ]
        tails :: t -> [ t ]
        lines :: t -> [ t ]
        words :: t -> [ t ]
        unlines :: [ t ] -> t
        unwords :: [ t ] -> t
        isPrefixOf :: t -> t -> Bool
        isSuffixOf :: t -> t -> Bool
        filter :: (C t -> Bool) -> t -> t
        find :: (C t -> Bool) -> t -> Maybe (C t)
        index :: t -> Len t -> C t
        zip :: t -> t -> [ (C t, C t) ]

    class Stringish t => Textish t where
        replace :: t -> t -> t -> t
        toCaseFold :: t -> t
        toLower :: t -> t
        toUpper :: t -> t
        toTitle :: t -> t
        justifyLeft :: Len t -> C t -> t -> t
        justifyRight :: Len t -> C t -> t -> t
        center :: Len t -> C t -> t -> t
        takeEnd :: Len t -> t -> t
        dropEnd :: Len t -> t -> t
        takeWhileEnd :: (C t -> Bool) -> t -> t
        dropWhileEnd :: (C t -> Bool) -> t -> t
        dropAround :: (C t -> Bool) -> t -> t
        strip :: t -> t
        stripStart :: t -> t
        stripEnd :: t -> t
        breakOn :: t -> t -> (t, t)
        breakOnEnd :: t -> t -> (t, t)
        splitOn :: t -> t -> [ t ]
        chunksOf :: Len t -> t -> [ t ]
        stripPrefix :: t -> t -> Maybe t
        stripSuffix :: t -> t -> Maybe t
        commonPrefixes :: t -> t -> Maybe (t, t, t)
        breakOnAll :: t -> t -> [ (t, t) ]
        partition :: (C t -> Bool) -> t -> (t, t)
        scanl1 :: (C t -> C t -> C t) -> t -> t
        scanr :: (C t -> C t -> C t) -> C t -> t -> t
        scanr1 :: (C t -> C t -> C t) -> t -> t
        unfoldrN :: Len t -> (a -> Maybe (C t, a)) -> a -> t
        count :: t -> t -> Len t
        split :: (C t -> Bool) -> t -> [ t ]
        replicate :: Len t -> t -> t
        zipWith :: (C t -> C t -> C t) -> t -> t -> t
        compareLength :: t -> Len t -> Ordering

    instance Stringish Data.Text.Text where
        type C Data.Text.Text = Char
        type Len Data.Text.Text = Int

        pack = Data.Text.pack
        unpack = Data.Text.unpack
        singleton = Data.Text.singleton
        empty = Data.Text.empty
        cons = Data.Text.cons
        snoc = Data.Text.snoc
        append = Data.Text.append
        uncons = Data.Text.uncons
        head = Data.Text.head
        last = Data.Text.last
        tail = Data.Text.tail
        init = Data.Text.init
        null = Data.Text.null
        length = Data.Text.length
        map = Data.Text.map
        intercalate = Data.Text.intercalate
        intersperse = Data.Text.intersperse
        transpose = Data.Text.transpose
        reverse = Data.Text.reverse
        foldl = Data.Text.foldl
        foldl' = Data.Text.foldl'
        foldl1 = Data.Text.foldl1
        foldl1' = Data.Text.foldl1'
        foldr = Data.Text.foldr
        foldr1 = Data.Text.foldr1
        concat = Data.Text.concat
        concatMap = Data.Text.concatMap
        any = Data.Text.any
        all = Data.Text.all
        maximum = Data.Text.maximum
        minimum = Data.Text.minimum
        scanl = Data.Text.scanl
        mapAccumL = Data.Text.mapAccumL
        mapAccumR = Data.Text.mapAccumR
        unfoldr = Data.Text.unfoldr
        take = Data.Text.take
        drop = Data.Text.drop
        takeWhile = Data.Text.takeWhile
        dropWhile = Data.Text.dropWhile
        splitAt = Data.Text.splitAt
        break = Data.Text.break
        span = Data.Text.span
        group = Data.Text.group
        groupBy = Data.Text.groupBy
        inits = Data.Text.inits
        tails = Data.Text.tails
        lines = Data.Text.lines
        words = Data.Text.words
        unlines = Data.Text.unlines
        unwords = Data.Text.unwords
        isPrefixOf = Data.Text.isPrefixOf
        isSuffixOf = Data.Text.isSuffixOf
        filter = Data.Text.filter
        find = Data.Text.find
        index = Data.Text.index
        zip = Data.Text.zip

    instance Textish Data.Text.Text where
        replace = Data.Text.replace
        toCaseFold = Data.Text.toCaseFold
        toLower = Data.Text.toLower
        toUpper = Data.Text.toUpper
        toTitle = Data.Text.toTitle
        justifyLeft = Data.Text.justifyLeft
        justifyRight = Data.Text.justifyRight
        center = Data.Text.center
        takeEnd = Data.Text.takeEnd
        dropEnd = Data.Text.dropEnd
        takeWhileEnd = Data.Text.takeWhileEnd
        dropWhileEnd = Data.Text.dropWhileEnd
        dropAround = Data.Text.dropAround
        strip = Data.Text.strip
        stripStart = Data.Text.stripStart
        stripEnd = Data.Text.stripEnd
        breakOn = Data.Text.breakOn
        breakOnEnd = Data.Text.breakOnEnd
        splitOn = Data.Text.splitOn
        chunksOf = Data.Text.chunksOf
        stripPrefix = Data.Text.stripPrefix
        stripSuffix = Data.Text.stripSuffix
        commonPrefixes = Data.Text.commonPrefixes
        breakOnAll = Data.Text.breakOnAll
        partition = Data.Text.partition
        scanl1 = Data.Text.scanl1
        scanr = Data.Text.scanr
        scanr1 = Data.Text.scanr1
        unfoldrN = Data.Text.unfoldrN
        count = Data.Text.count
        split = Data.Text.split
        replicate = Data.Text.replicate
        zipWith = Data.Text.zipWith
        compareLength = Data.Text.compareLength

    instance Stringish Data.Text.Lazy.Text where
        type C Data.Text.Lazy.Text = Char
        type Len Data.Text.Lazy.Text = Data.Int.Int64

        pack = Data.Text.Lazy.pack
        unpack = Data.Text.Lazy.unpack
        singleton = Data.Text.Lazy.singleton
        empty = Data.Text.Lazy.empty
        cons = Data.Text.Lazy.cons
        snoc = Data.Text.Lazy.snoc
        append = Data.Text.Lazy.append
        uncons = Data.Text.Lazy.uncons
        head = Data.Text.Lazy.head
        last = Data.Text.Lazy.last
        tail = Data.Text.Lazy.tail
        init = Data.Text.Lazy.init
        null = Data.Text.Lazy.null
        length = Data.Text.Lazy.length
        map = Data.Text.Lazy.map
        intercalate = Data.Text.Lazy.intercalate
        intersperse = Data.Text.Lazy.intersperse
        transpose = Data.Text.Lazy.transpose
        reverse = Data.Text.Lazy.reverse
        foldl = Data.Text.Lazy.foldl
        foldl' = Data.Text.Lazy.foldl'
        foldl1 = Data.Text.Lazy.foldl1
        foldl1' = Data.Text.Lazy.foldl1'
        foldr = Data.Text.Lazy.foldr
        foldr1 = Data.Text.Lazy.foldr1
        concat = Data.Text.Lazy.concat
        concatMap = Data.Text.Lazy.concatMap
        any = Data.Text.Lazy.any
        all = Data.Text.Lazy.all
        maximum = Data.Text.Lazy.maximum
        minimum = Data.Text.Lazy.minimum
        scanl = Data.Text.Lazy.scanl
        mapAccumL = Data.Text.Lazy.mapAccumL
        mapAccumR = Data.Text.Lazy.mapAccumR
        unfoldr = Data.Text.Lazy.unfoldr
        take = Data.Text.Lazy.take
        drop = Data.Text.Lazy.drop
        takeWhile = Data.Text.Lazy.takeWhile
        dropWhile = Data.Text.Lazy.dropWhile
        splitAt = Data.Text.Lazy.splitAt
        break = Data.Text.Lazy.break
        span = Data.Text.Lazy.span
        group = Data.Text.Lazy.group
        groupBy = Data.Text.Lazy.groupBy
        inits = Data.Text.Lazy.inits
        tails = Data.Text.Lazy.tails
        lines = Data.Text.Lazy.lines
        words = Data.Text.Lazy.words
        unlines = Data.Text.Lazy.unlines
        unwords = Data.Text.Lazy.unwords
        isPrefixOf = Data.Text.Lazy.isPrefixOf
        isSuffixOf = Data.Text.Lazy.isSuffixOf
        filter = Data.Text.Lazy.filter
        find = Data.Text.Lazy.find
        index = Data.Text.Lazy.index
        zip = Data.Text.Lazy.zip

    instance Textish Data.Text.Lazy.Text where
        replace = Data.Text.Lazy.replace
        toCaseFold = Data.Text.Lazy.toCaseFold
        toLower = Data.Text.Lazy.toLower
        toUpper = Data.Text.Lazy.toUpper
        toTitle = Data.Text.Lazy.toTitle
        justifyLeft = Data.Text.Lazy.justifyLeft
        justifyRight = Data.Text.Lazy.justifyRight
        center = Data.Text.Lazy.center
        takeEnd = Data.Text.Lazy.takeEnd
        dropEnd = Data.Text.Lazy.dropEnd
        takeWhileEnd = Data.Text.Lazy.takeWhileEnd
        dropWhileEnd = Data.Text.Lazy.dropWhileEnd
        dropAround = Data.Text.Lazy.dropAround
        strip = Data.Text.Lazy.strip
        stripStart = Data.Text.Lazy.stripStart
        stripEnd = Data.Text.Lazy.stripEnd
        breakOn = Data.Text.Lazy.breakOn
        breakOnEnd = Data.Text.Lazy.breakOnEnd
        splitOn = Data.Text.Lazy.splitOn
        chunksOf = Data.Text.Lazy.chunksOf
        stripPrefix = Data.Text.Lazy.stripPrefix
        stripSuffix = Data.Text.Lazy.stripSuffix
        commonPrefixes = Data.Text.Lazy.commonPrefixes
        breakOnAll = Data.Text.Lazy.breakOnAll
        partition = Data.Text.Lazy.partition
        scanl1 = Data.Text.Lazy.scanl1
        scanr = Data.Text.Lazy.scanr
        scanr1 = Data.Text.Lazy.scanr1
        unfoldrN = Data.Text.Lazy.unfoldrN
        count = Data.Text.Lazy.count
        split = Data.Text.Lazy.split
        replicate = Data.Text.Lazy.replicate
        zipWith = Data.Text.Lazy.zipWith
        compareLength = Data.Text.Lazy.compareLength

    instance Stringish Data.ByteString.Char8.ByteString where
        type C Data.ByteString.Char8.ByteString = Char
        type Len Data.ByteString.Char8.ByteString = Int

        pack = Data.ByteString.Char8.pack
        unpack = Data.ByteString.Char8.unpack
        singleton = Data.ByteString.Char8.singleton
        empty = Data.ByteString.Char8.empty
        cons = Data.ByteString.Char8.cons
        snoc = Data.ByteString.Char8.snoc
        append = Data.ByteString.Char8.append
        uncons = Data.ByteString.Char8.uncons
        head = Data.ByteString.Char8.head
        last = Data.ByteString.Char8.last
        tail = Data.ByteString.Char8.tail
        init = Data.ByteString.Char8.init
        null = Data.ByteString.Char8.null
        length = Data.ByteString.Char8.length
        map = Data.ByteString.Char8.map
        intercalate = Data.ByteString.Char8.intercalate
        intersperse = Data.ByteString.Char8.intersperse
        transpose = Data.ByteString.Char8.transpose
        reverse = Data.ByteString.Char8.reverse
        foldl = Data.ByteString.Char8.foldl
        foldl' = Data.ByteString.Char8.foldl'
        foldl1 = Data.ByteString.Char8.foldl1
        foldl1' = Data.ByteString.Char8.foldl1'
        foldr = Data.ByteString.Char8.foldr
        foldr1 = Data.ByteString.Char8.foldr1
        concat = Data.ByteString.Char8.concat
        concatMap = Data.ByteString.Char8.concatMap
        any = Data.ByteString.Char8.any
        all = Data.ByteString.Char8.all
        maximum = Data.ByteString.Char8.maximum
        minimum = Data.ByteString.Char8.minimum
        scanl = Data.ByteString.Char8.scanl
        mapAccumL = Data.ByteString.Char8.mapAccumL
        mapAccumR = Data.ByteString.Char8.mapAccumR
        unfoldr = Data.ByteString.Char8.unfoldr
        take = Data.ByteString.Char8.take
        drop = Data.ByteString.Char8.drop
        takeWhile = Data.ByteString.Char8.takeWhile
        dropWhile = Data.ByteString.Char8.dropWhile
        splitAt = Data.ByteString.Char8.splitAt
        break = Data.ByteString.Char8.break
        span = Data.ByteString.Char8.span
        group = Data.ByteString.Char8.group
        groupBy = Data.ByteString.Char8.groupBy
        inits = Data.ByteString.Char8.inits
        tails = Data.ByteString.Char8.tails
        lines = Data.ByteString.Char8.lines
        words = Data.ByteString.Char8.words
        unlines = Data.ByteString.Char8.unlines
        unwords = Data.ByteString.Char8.unwords
        isPrefixOf = Data.ByteString.Char8.isPrefixOf
        isSuffixOf = Data.ByteString.Char8.isSuffixOf
        filter = Data.ByteString.Char8.filter
        find = Data.ByteString.Char8.find
        index = Data.ByteString.Char8.index
        zip = Data.ByteString.Char8.zip

    instance Stringish Data.ByteString.Lazy.Char8.ByteString where
        type C Data.ByteString.Lazy.Char8.ByteString = Char
        type Len Data.ByteString.Lazy.Char8.ByteString = Data.Int.Int64

        pack = Data.ByteString.Lazy.Char8.pack
        unpack = Data.ByteString.Lazy.Char8.unpack
        singleton = Data.ByteString.Lazy.Char8.singleton
        empty = Data.ByteString.Lazy.Char8.empty
        cons = Data.ByteString.Lazy.Char8.cons
        snoc = Data.ByteString.Lazy.Char8.snoc
        append = Data.ByteString.Lazy.Char8.append
        uncons = Data.ByteString.Lazy.Char8.uncons
        head = Data.ByteString.Lazy.Char8.head
        last = Data.ByteString.Lazy.Char8.last
        tail = Data.ByteString.Lazy.Char8.tail
        init = Data.ByteString.Lazy.Char8.init
        null = Data.ByteString.Lazy.Char8.null
        length = Data.ByteString.Lazy.Char8.length
        map = Data.ByteString.Lazy.Char8.map
        intercalate = Data.ByteString.Lazy.Char8.intercalate
        intersperse = Data.ByteString.Lazy.Char8.intersperse
        transpose = Data.ByteString.Lazy.Char8.transpose
        reverse = Data.ByteString.Lazy.Char8.reverse
        foldl = Data.ByteString.Lazy.Char8.foldl
        foldl' = Data.ByteString.Lazy.Char8.foldl'
        foldl1 = Data.ByteString.Lazy.Char8.foldl1
        foldl1' = Data.ByteString.Lazy.Char8.foldl1'
        foldr = Data.ByteString.Lazy.Char8.foldr
        foldr1 = Data.ByteString.Lazy.Char8.foldr1
        concat = Data.ByteString.Lazy.Char8.concat
        concatMap = Data.ByteString.Lazy.Char8.concatMap
        any = Data.ByteString.Lazy.Char8.any
        all = Data.ByteString.Lazy.Char8.all
        maximum = Data.ByteString.Lazy.Char8.maximum
        minimum = Data.ByteString.Lazy.Char8.minimum
        scanl = Data.ByteString.Lazy.Char8.scanl
        mapAccumL = Data.ByteString.Lazy.Char8.mapAccumL
        mapAccumR = Data.ByteString.Lazy.Char8.mapAccumR
        unfoldr = Data.ByteString.Lazy.Char8.unfoldr
        take = Data.ByteString.Lazy.Char8.take
        drop = Data.ByteString.Lazy.Char8.drop
        takeWhile = Data.ByteString.Lazy.Char8.takeWhile
        dropWhile = Data.ByteString.Lazy.Char8.dropWhile
        splitAt = Data.ByteString.Lazy.Char8.splitAt
        break = Data.ByteString.Lazy.Char8.break
        span = Data.ByteString.Lazy.Char8.span
        group = Data.ByteString.Lazy.Char8.group
        groupBy = Data.ByteString.Lazy.Char8.groupBy
        inits = Data.ByteString.Lazy.Char8.inits
        tails = Data.ByteString.Lazy.Char8.tails
        lines = Data.ByteString.Lazy.Char8.lines
        words = Data.ByteString.Lazy.Char8.words
        unlines = Data.ByteString.Lazy.Char8.unlines
        unwords = Data.ByteString.Lazy.Char8.unwords
        isPrefixOf = Data.ByteString.Lazy.Char8.isPrefixOf
        isSuffixOf = Data.ByteString.Lazy.Char8.isSuffixOf
        filter = Data.ByteString.Lazy.Char8.filter
        find = Data.ByteString.Lazy.Char8.find
        index = Data.ByteString.Lazy.Char8.index
        zip = Data.ByteString.Lazy.Char8.zip
