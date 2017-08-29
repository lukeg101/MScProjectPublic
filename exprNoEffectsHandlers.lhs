> {-# LANGUAGE DeriveFunctor #-} 
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> 

> import CompClasses2

--toy source language without effects

> data Expr = 
>     Val Int 
>     | Add Expr Expr deriving (Show)

--toy target language without effects (GADT allows compositional code)

> data Code where
>     HALT :: Code
>     PUSH :: ExprValue -> Code -> Code
>     ADD  :: Code -> Code

--toy language Values

> data ExprValue = Num Int deriving (Show)

--type synonym for value domain stack

> type Stack = [ExprValue]

--example state functor

> data StackFunctor s a 
>    =  Pop (s -> a)
>    |  Push s a
>        deriving Functor

--example closed Stack Handler

> handleStackClosed :: Stack -> 
> 	Free (StackFunctor ExprValue) a -> 
> 	(Stack, a)
> handleStackClosed s (Var x) = (s,x)
> handleStackClosed (x:xs) (Cons (Pop k)) = handleStackClosed xs (k x)
> handleStackClosed xs (Cons (Push x k)) = handleStackClosed (x:xs) k

--example program using abstract ops with closed state handler

> evalfree :: Expr -> 
> 	Free (StackFunctor ExprValue) ()
> evalfree (Val n) = Cons (Push (Num n) (Var ()))
> evalfree (Add e1 e2) = do 
>   evalfree e1 
>   evalfree e2
>   (Num n) <- Cons (Pop Var)
>   (Num m) <- Cons (Pop Var)
>   Cons (Push (Num (n+m)) (Var ())) 

{-
programming with trees

Main> handleStackClosed [Num 11] (evalfree (Add (Val 2) (Val 1)))

([Num 3,Num 11],())

Main> handleStackClosed [] (evalfree (Add (Val 2) (Val 1)))

([Num 3],())
-}

--sneaky syntactic trick to use do notation

> pop :: Free (StackFunctor ExprValue) ExprValue
> pop = Cons (Pop Var)
> push :: ExprValue -> Free (StackFunctor ExprValue) ()
> push v = Cons (Push v (Var ())) 

> evalfree' :: Expr -> 
> 	Free (StackFunctor ExprValue) ()
> evalfree' (Val n) = push (Num n)
> evalfree' (Add e1 e2) = do 
>   evalfree' e1 
>   evalfree' e2
>   (Num n) <- pop
>   (Num m) <- pop
>   push (Num (n+m)) 

{-Now to extend this with data types a la carte for free monads-}

--open handler fixed on Stack type as state 
--we don't need more general handlers since we know source and target

> handleStackOpen :: Functor g => 
>       Stack -> 
>       Free (StackFunctor ExprValue :+: g) a -> 
>       Free g (Stack, a)
> handleStackOpen s (Var a) = return (s, a) 
> handleStackOpen (x:xs) (Cons (Inl (Pop k))) = handleStackOpen xs (k x) 
> handleStackOpen xs (Cons (Inl (Push x k))) = handleStackOpen (x:xs) k 

--base type to return values in tree

> data Void k deriving Functor

--top level closed handler 

> handleVoid :: Free Void a -> a
> handleVoid = fold undefined id 

--sneaky syntactic trick to use do notation 
-- with open handlers

> pop' :: (StackFunctor ExprValue :<: g)=>
> 	Free g ExprValue
> pop' = inject (Pop Var) 
> push' :: (StackFunctor ExprValue :<: g)=>
> 	ExprValue -> Free g ()
> push' v = inject (Push v (Var ()))  

{-massaging this into compiler example-}

> evalfree2 :: (StackFunctor ExprValue :<: g) => 
>   Expr -> 
>   Free g () 
> evalfree2 (Val n) = push' (Num n) 
> evalfree2 (Add e1 e2) = do 
>   evalfree2 e1
>   evalfree2 e2
>   (Num n) <- pop'
>   (Num m) <- pop'
>   push' (Num (m+n)) 

{-

Main> handleStackClosed [] (evalfree2 (Val 1))

([Num 1],())

Main> handleStackClosed [Num 22] (evalfree2 (Val 1))

([Num 1,Num 22],())

Main> (handleVoid . handleStackOpen [Num 22]) (evalfree2 (Val 1))

([Num 1,Num 22],())

-}

> eval' :: (StackFunctor ExprValue :<: g) => 
>   Expr -> 
>   Free g () -> 
>   Free g ()
> eval' (Val n) c = do  
>    c
>    push' (Num n) 
> eval' (Add e1 e2) c = do
>    let c' = eval' e1 c
>    eval' e2 c'
>    (Num n) <- pop'
>    (Num m) <- pop'
>    push' (Num (m+n))

{-

Main> (handleVoid . handleStackOpen [Num 2]) 
	(eval' (Add (Val 1) (Val 2)) (return ()))

([Num 3,Num 2],())

much more satisfying than evalfree, nobody should program in trees!
-}

> exec' ::(StackFunctor ExprValue :<: g) => 
> 	Code -> 
> 	Free g () -> 
> 	Free g ()
> exec' HALT c = c
> exec' (PUSH v t) c = exec' t (push' v)
> exec' (ADD t) c = exec' t (do 
> 	c
> 	(Num n) <- pop'
>	(Num m) <- pop'
> 	push' (Num (m+n))) 

{-

Main> (handleVoid . handleStackOpen [Num 5]) 
	(exec' (PUSH (Num 1) HALT) (return ()))

([Num 1,Num 5],())

Main> (handleVoid . handleStackOpen [Num 5, Num 6]) 
	(exec' (ADD HALT) (return ()))

([Num 11],())

Main> (handleVoid . handleStackOpen [Num 5, Num 6, Num 1]) 
	(exec' (ADD (ADD HALT)) (return ()))

([Num 12],())

-}

--top level compiler function

> comp1 :: Expr -> Code
> comp1 x = comp1' x HALT 

--main compiler function that is calculated with induction

> comp1' :: Expr -> Code -> Code
> comp1' (Val n) t = PUSH (Num n) t
> comp1' (Add e1 e2) t = comp1' e1 (comp1' e2 (ADD t))

{-Instantiating the class with this compiler-}

-- Type synonym capturing all effects present in the configuration

> type Sigma = StackFunctor ExprValue :+: Void

-- Configuration for stack based machine with handlers

> instance (StackFunctor ExprValue :<: Sigma) => 
>   Configuration Expr (Free Sigma) () (Stack, ()) where
>   eval = eval'    
>   handle = handleVoid . handleStackOpen []  

> instance CorrectCompiler Expr Code (Free Sigma) () (Stack, ()) where
>   exec = exec'
>   comp = comp1
>   comp' = comp1'




