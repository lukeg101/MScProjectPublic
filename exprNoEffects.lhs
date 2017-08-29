
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-} 
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE InstanceSigs #-} --enables type signatures in patterns 

> import CompClasses --contains Correct Compiler classes 

--toy source language without effects

> data Expr = 
>    Val Int 
>    | Add Expr Expr deriving Show

--toy target language without effects (GADT allows compositional code)

> data Code where
>   HALT :: Code
>   PUSH :: ExprValue -> Code -> Code
>   ADD  :: Code -> Code

--toy language Values

> data ExprValue = Num Int

--evaluation semantics for toy language without effects

> eval' :: Expr -> Int 
> eval' (Val n) = n
> eval' (Add e1 e2) = eval' e1 + eval' e2


> instance Configuration Expr [ExprValue] where
> 	eval :: Expr -> [ExprValue] -> [ExprValue]
>	eval (Val n) c = (Num n):c
> 	eval (Add e1 e2) c = 
> 	  case eval e1 c of 
> 	  	c' -> case eval e2 c' of 
> 		  ((Num m):(Num n):c'') -> ((Num (m+n)) : c'')

> instance CorrectCompiler Expr Code [ExprValue] where
>   exec  :: Code -> [ExprValue] -> [ExprValue]
>   exec HALT c = c
>   exec (PUSH n t) c = exec t (n:c)
>   exec (ADD t) ((Num m):(Num n):c) = exec t ((Num (m+n)):c)  
>   comp :: Expr -> Code
>   comp x = comp' x HALT
>   comp' :: Expr -> Code -> Code  
>   comp' (Val n) t = PUSH (Num n) t
>   comp' (Add e1 e2) t = comp' e1 (comp' e2 (ADD t))
>   --laws
>   --exec t (eval s c) = exec (comp' s t) c 
>   --exec (comp s) c = eval s c






