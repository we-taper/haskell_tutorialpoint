class Functor2 f where  
    fmap2 :: (a -> b) -> f a -> f b  
instance Functor2 [] where  
    fmap2 = map
instance Functor2 Maybe where  
    fmap2 g (Just x) = Just (g x)  
    fmap2 g Nothing = Nothing