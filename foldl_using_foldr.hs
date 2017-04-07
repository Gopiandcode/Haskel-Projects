

foldl' f b xs = foldr step id xs b
              where step x g a = g(f x a)
