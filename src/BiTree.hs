data BiTree a = Nil
              | Leaf a
              | BiTree a (BiTree a) (BiTree a)  
              deriving Show

instance Functor BiTree where
    fmap f Nil              = Nil
    fmap f (Leaf a)         = Leaf (f a)
    fmap f (BiTree a l r)   = BiTree (f a) (f <$> l) (f <$> r)

instance Applicative BiTree where
    pure                                = return
    Nil             <*> _               = Nil
    Leaf f          <*> t               = f <$> t
    BiTree f l r    <*> BiTree a l' r'  = BiTree (f a) (l <*> l') (r <*> r')

instance Monad BiTree where
    return                  = Leaf
    Nil             >>= f   = Nil
    Leaf a          >>= f   = f a
    BiTree a l r    >>= f   = distribute (l >>= f) (r >>= f) (f a)

distribute :: BiTree a -> BiTree a -> BiTree a -> BiTree a
distribute l r Nil                  = Nil
distribute l r (Leaf a)             = BiTree a l r
distribute l r (BiTree a Nil cr)    = BiTree a l                      (distribute Nil r cr)
distribute l r (BiTree a cl  Nil)   = BiTree a (distribute l Nil cl)  r
distribute l r (BiTree a cl  cr)    = BiTree a (distribute l Nil cl)  (distribute Nil r cr)