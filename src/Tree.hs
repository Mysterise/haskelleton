data BTree a = Nil
            | Leaf a
            | Node a (BTree a) (BTree a)  
            deriving Show

instance Functor BTree where
    fmap f Nil          = Nil
    fmap f (Leaf a)     = Leaf (f a)
    fmap f (Node a l r) = Node (f a) (f <$> l) (f <$> r)

instance Applicative BTree where
    pure                            = Leaf
    Nil         <*> _               = Nil
    Leaf f      <*> t               = f <$> t
    Node f l r  <*> Node a l' r'    = Node (f a) (l <*> l') (r <*> r') 