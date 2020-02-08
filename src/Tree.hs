data Tree a = Nil
            | Leaf a
            | Node a (Tree a) (Tree a)  
            deriving Show

instance Functor Tree where
    fmap f Nil          = Nil
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node a l r) = Node (f a) (f <$> l) (f <$> r)

instance Applicative Tree where
    pure                            = Leaf
    Nil         <*> _               = Nil
    Leaf f      <*> t               = f <$> t
    Node f l r  <*> Node a l' r'    = Node (f a) (l <*> l') (r <*> r') 