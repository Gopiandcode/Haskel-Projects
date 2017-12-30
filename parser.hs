type Error = String
newtype Parser b a = P { parse :: (b, String) -> (Either Error a, (b,String))
