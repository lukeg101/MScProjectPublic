> {-# LANGUAGE 
>	GADTs #-}


> data Expr 
>   = Val Int 
> 	| Add Expr Expr

> eval :: Expr -> Int
> eval (Val n) = n
> eval (Add e1 e2) = eval e1 + eval e2

> data Code where
>   HALT :: Code
>   PUSH :: Int -> Code -> Code
>   ADD  :: Code -> Code

> comp :: Expr -> Code
> comp x = comp' x HALT

> comp' :: Expr -> Code -> Code  
> comp' (Val n) t = PUSH n t
> comp' (Add e1 e2) t = comp' e1 (comp' e2 (ADD t))

> type Stack = [Int]

> exec  :: Code -> Stack -> Stack
> exec HALT c = c
> exec (PUSH n t) c = exec t (n:c)
> exec (ADD t) (n:m:c) = exec t ((m+n):c)  

> exec' :: Code -> Stack -> Maybe Stack
> exec' HALT c              = Just c
> exec' (PUSH n t) c        = exec' t (n:c)
> exec' (ADD t) (x:y:xs)    = exec' t (x+y:xs)
> exec' (ADD t) _           = Nothing

> double :: Int -> Int 
> double x = x + x
> 
> quadruple :: Int -> Int
> quadruple x = double (double x)

> exec'' :: Code -> Stack -> Maybe Stack
> exec'' HALT c       = return c
> exec'' (PUSH n t) c = exec'' t (n:c)
> exec'' (ADD t) c    = do 
>   case c of
>     (x:y:xs) -> exec'' t (x+y:xs)
>     _ -> Nothing
 
