data List a = List | a :> List a

infixr 5 :>

instance Semigroup (List a) where
    (<>)  list (List) = list
    (<>) (List) list  = list

    (<>) ( x :> xs ) ys = x :> xs <> ys

instance Monoid (List a) where
    mempty  = List
    mappend = (<>)
    mconcat = foldr (<>) List -- Should use foldr'

instance Functor List where
    fmap _ (  List   ) = List
    fmap f ( x :> xs ) = f x :> fmap f xs

    (<$) _ (  List   ) = List
    (<$) a ( _ :> xs ) = a :> (a <$ xs)
 -- (<$)               = fmap . const

instance Applicative List where
    pure = (:> List)

    (<*>) (  List   ) __ = List
    (<*>) ( f :> fs ) xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
    return = pure

    (>>=) (  List   ) _ = List
    (>>=) ( x :> xs ) f = f x <> (xs >>= f)

instance Foldable List where
    foldr _ accumulator (  List   ) = accumulator
    foldr f accumulator ( x :> xs ) = f x (foldr f accumulator xs)

    foldl _ accumulator (  List   ) = accumulator
    foldl f accumulator ( x :> xs ) = f (foldl f accumulator xs) x

 -- import Data.Foldable
 --
 -- foldr' _ accumulator (  List   ) = accumulator
 -- foldr' f accumulator ( x :> xs ) =
 --     let
 --         r = foldr f accumulator xs
 --     in
 --         r `seq` f x r

 -- foldl' _ accumulator (  List   ) = accumulator
 -- foldl' f accumulator ( x :> xs ) =
 --     let
 --         l = foldr f accumulator xs
 --     in
 --         l `seq` f l x

    length (  List   ) = 0
    length ( x :> xs ) = 1 + length xs

    null    = (== 0) . length
    sum     = foldr (+) 0 -- Should use foldr'
    product = foldr (*) 1 -- Should use foldr'

    elem _ (  List   ) = False
    elem y ( x :> xs )
                     | x == y    = True
                     | otherwise = elem y xs

instance Traversable List where
    traverse  lift (  List   ) = pure List
    traverse  lift ( x :> xs ) = (:>) <$> lift x <*> traverse  lift xs
    sequenceA      ( x :> xs ) = (:>) <$>      x <*> sequenceA      xs
 -- sequenceA                  = traverse id

instance Eq a => Eq (List a) where
    (==) (List) (List) = True
    (==)  list  (List) = False
    (==) (List)  list  = False

    (==) ( x :> xs ) ( y :> ys )
                               | x == y = xs == ys
                               | otherwise = False

instance Show a => Show (List a) where
    show (  List   ) = "{}"
    show ( x :> xs ) =
        let
            showr :: List a -> String
            showr (  List   ) = "}"
            showr ( x :> xs ) = "; " <> show x <> showr xs
        in
            "{" <> show x <> showr xs

toList :: List a -> [a]
toList = foldr (:) []

fromList :: [a] -> List a
fromList = foldr (:>) List
