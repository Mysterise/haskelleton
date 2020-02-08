import qualified Data.List

data Tree a = Nil
            | Leaf a
            | Node (Tree a) a (Tree a)  
            deriving Show

instance Functor Tree where
    fmap f (Nil)        = Nil
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Applicative Tree where
    pure                = Leaf
    Nil <*> _           = Nil
    (Leaf f) <*> t      = f <$> t
    (Node l f r) <*> t  = Node (l <*> t) (fmap f t) (r <*> t) 