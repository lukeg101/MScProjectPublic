 
> {-# LANGUAGE DeriveFunctor #-} 
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE PatternSynonyms #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE KindSignatures#-}

> import CompClasses3

{-

--Compiler calculation method
1. Define source language, semantic domain and target language 
(target can be  alternatively be derived in calculation process)
2. define evaluation semantics eval0 (pure-no handlers)
3. Calculate handlers using Handler Calculation method, 
one for each effect, gradually reducing eval0 until it's
defined almost purely in terms of abstract ops
that is, eval' is an abstract computation
4. proceed to calculate compiler and VM definitions with eval'

--Handler Calculation Method
1. Given some evaluation semantics for the language+config, pick an effect 
exhibited in the language you want to handle. 
A good one to start with the the stack (state) 
2. Define abstract operations that the effect will involve, 
e.g for state it is Get, Put
3. using pattern trick, define do notation for abstract ops
4. define handlers to capture abstract ops implementation, 
optionally turn into folds
5. refine evaluation semantics to include these abstract ops 
on first pass this turns the function from a pure function a abstract
syntax tree for the langauge - using pattern trick should introduce Do 
notation rather than free monad tree syntax which is not nice by itself
- on successive passes it should turn eval into almost purely abstract 
syntax tree
6. repeat 1-5 until all effects have handlers
-}

--1.define the source language, domain and target
--toy source language with exceptions

> data Expr = 
>   Val Int 
>   | Add Expr Expr 
>   | ThrowE
>   | CatchE Expr Expr
>   deriving Show

--toy target language without effects (GADT allows compositional code)

> data Code where
>   HALT :: Code
>   PUSH :: ExprValue -> Code -> Code
>   ADD  :: Code -> Code
>   FAIL :: Code
>   deriving Show

--toy language Values

> data ExprValue = Num Int deriving Show

--type synonym for value domain stack

> type Stack = [ExprValue]

--concrete evaluation semantics for language with exceptions

> eval0 :: Expr -> Stack -> Maybe Stack
> eval0 (Val n) s = Just ((Num n):s)
> eval0 (Add e1 e2) s = case eval0 e1 s of
>   Nothing -> Just s
>   Just s' -> case eval0 e2 s' of 
>       Nothing -> Just s
>       Just ((Num m):(Num n):xs)-> Just ((Num (m+n)):xs)
> eval0 ThrowE s = Nothing
> eval0 (CatchE e1 e2) s = case eval0 e1 s of 
>   Just s' -> Just s'
>   Nothing -> eval0 e2 s

{-

*Main> eval0 (CatchE (Add (Val 2) ThrowE) (Val 44)) [Num 1]

Just [Num 1]
-}

--2.first define abstract ops and effect you want to handle

> data StackFunctor s a 
>    =  Pop (s -> a)
>    |  Push s a
>        deriving Functor
 
> type HStack s = Lift (StackFunctor s) 

--3. using pattern trick, define do notation for abstract ops

> pop :: (HStack ExprValue :<: g)=>
>   Free g ExprValue
> pop = inject (Lift (Pop Var))
> push :: (HStack ExprValue :<: g)=>
>   ExprValue -> Free g ()
> push v = inject (Lift (Push v (Var ()))) 

--4. define handlers to capture abstract ops implementation, 

> handleStackOpen :: Syntax g => 
>       Stack -> 
>       Free (HStack ExprValue :+: g) a -> 
>       Free g (Stack, a)
> handleStackOpen s (Var a) = return (s,a)
> handleStackOpen (x:xs) (Cons (Inl (Lift (Pop k)))) = handleStackOpen xs (k x)  
> handleStackOpen xs (Cons (Inl (Lift (Push x k)))) = handleStackOpen (x:xs) k 
> handleStackOpen s (Other op) = 
>   Cons (weave (s,()) (uncurry handleStackOpen) op) 

--2. Define abstract operations that the effect will involve, 

> data ExcFunctor m a where
>   Throw :: ExcFunctor m a
>   Catch :: (m x) -> (m x) -> (x -> m a) -> ExcFunctor m a 

> instance Functor m => Functor (ExcFunctor m) where
>   fmap f Throw = Throw
>   fmap f (Catch p h k) = Catch p h (fmap f . k)

> type HExc = ExcFunctor 

--3. using pattern trick, define do notation for abstract ops

> throw ::(HExc :<: g) => Free g a
> throw = inject Throw
> catch ::(HExc :<: g) => 
>       Free g a -> 
>       Free g a -> 
>       Free g a
> catch p h = inject (Catch p h return) 

--4. define handlers to capture abstract ops implementation

> instance HFunctor HExc where
>    hmap t (Throw) = Throw
>    hmap t (Catch p h k) = Catch (t p) (t h) (t . k)

> instance Syntax HExc where
>     emap f Throw = Throw
>     emap f (Catch p h k) = Catch p h (f . k)
>     weave f hdl Throw = Throw
>     weave f hdl (Catch p h k) = 
>        Catch (hdl (fmap (const p) f))
>              (hdl (fmap (const h) f))
>              (hdl . fmap k)

--Higher order open handler for exceptions

> handleExc :: Syntax g => 
>   Free (HExc :+: g) a -> 
>   Free g (Maybe a)
> handleExc (Var x) = return (Just x) 
> handleExc (Cons (Inl Throw)) = return Nothing 
> handleExc (Cons (Inl (Catch p h k))) = do
>    r <- handleExc p 
>    case r of
>       Just x -> handleExc (k x)
>       Nothing -> do 
>           r <- handleExc h 
>           case r of
>               Just x -> handleExc (k x)
>               Nothing  -> return Nothing 
> handleExc (Other op) = Cons (weave (Just ()) hdl op) where
>   hdl = maybe (return Nothing) handleExc 

--base type to return values in tree

> data Void k deriving Functor
> type HVoid = Lift Void

--top level closed handler 

> handleVoid :: Free HVoid a -> a
> handleVoid (Var a) = a 

--5. refine evaluation semantics to include these abstract ops 
--the semantics now form a syntax tree to be interpreted by handlers 

--free monadic eval without syntactic trick

> evalfree :: (HStack ExprValue :<: g, HExc :<: g) => 
>     Expr -> 
>     Free g () ->
>     Free g ()
> evalfree (Val n) c = do
>   c
>   Cons (inj (Lift (Push (Num n) (Var ()))))
> evalfree (Add e1 e2) c = do
>   let c' = evalfree e1 c
>   evalfree e2 c'
>   (Num n) <- Cons (inj (Lift (Pop Var)))
>   (Num m) <- Cons (inj (Lift (Pop Var)))
>   Cons (inj (Lift (Push (Num (m+n)) (Var ()))))
> evalfree ThrowE c = do 
>   c
>   Cons (inj Throw)
> evalfree (CatchE x h) c = 
>   Cons (inj (Catch (evalfree x c) (evalfree h c) return))

--eval with syntactic sugar

> eval' :: (HStack ExprValue :<: g, HExc :<: g) => 
>     Expr -> 
>     Free g () ->
>     Free g ()
> eval' (Val n) c = do
>   c
>   push (Num n) 
> eval' (Add e1 e2) c = catch (do
>   let c' = eval' e1 c
>   eval' e2 c'
>   (Num n) <- pop
>   (Num m) <- pop       
>   push (Num (m+n))) throw
> eval' ThrowE c = do {c; throw}
> eval' (CatchE x h) e = catch (eval' x e) (eval' h e) 

{-
--local 

Main> (handleVoid . handleStackOpen [] . handleExc) (eval' (Add (Val 1) (Val 2)) (return ()))

([Num 3],Just ())

Main> (handleVoid . handleStackOpen [] . handleExc) (eval' (CatchE (Add (Val 1) (Val 2)) (Val 2)) (return ()))

([Num 3],Just ())

Main> (handleVoid . handleStackOpen [] . handleExc) (eval' (CatchE (Add (ThrowE) (Val 1)) (Val 2)) (return ()))

([Num 2],Just ())


--global

Main> (handleVoid . handleExc . handleStackOpen []) (eval' (CatchE (Add (ThrowE) (Val 1)) (Val 2)) (return ()))

Just ([Num 2],())

Main> (handleVoid . handleExc . handleStackOpen [Num 15]) 
  (eval' (CatchE (Add (ThrowE) (Val 1)) ThrowE) (return ()))

Nothing


swapping position of state and exc handler gives global state
semantics rather than local
-this shows how easily we could get global
blessing and curse in disguise
-}

--Virtual machine code 

> exec' :: (HStack ExprValue :<: g, HExc :<: g) =>
>    Code ->
>    Free g () ->
>    Free g ()
> exec' HALT c = c
> exec' (PUSH v t) c = exec' t (do {c; push v})
> exec' FAIL c = throw 
> exec' (ADD t) c = exec' t (do
>   c
>   (Num n) <- pop
>   (Num m) <- pop       
>   push (Num (m+n))) 

{-

*Main> (handleVoid . handleStackOpen [Num 1] . handleExc) (exec' (PUSH (Num 2) FAIL) (return ()))

([Num 1],Nothing)
-}

--top level compiler function

> comp1 :: Expr -> Code
> comp1 x = comp1' x HALT 

--main compiler function

> comp1' :: Expr -> Code -> Code
> comp1' (Val n) t = PUSH (Num n) t
> comp1' ThrowE t = FAIL
> comp1' (Add e1 e2) t = comp1' e2 (comp1' e1 (ADD t))
> comp1' (CatchE ThrowE h) t = comp1' h t
> comp1' (CatchE x h) t = comp1' x t 

{-
pattern matching on catch equivalent to following:

comp1' (CatchE x h) t = case x of

    ThrowE -> case h of 

        ThrowE -> comp1' ThrowE t

        (Val n)-> comp1' (Val n) t

    (Val n)-> comp1' (Val n) t

equivalent to:

comp1' (CatchE x h) t = case x of 
    ThrowE -> comp1' h t
    $\_$ -> comp1' x t
    
-}
{-

Main> (handleVoid . handleStackOpen [Num 1, Num 2] . handleExc) 
  (exec' (comp1' (Val 14) HALT) (return ()))

([Num 14,Num 1,Num 2],Just ())

Main> (handleVoid . handleStackOpen [Num 1, Num 2] . handleExc) 
  (exec' HALT (eval' (Val 14) (return ())))

([Num 14,Num 1,Num 2],Just ())
-}

> type Sigma = HExc :+: HStack ExprValue :+: HVoid 

-- Configuration for stack based machine with handlers and effects

> instance (HStack ExprValue :<: Sigma, HExc :<: Sigma) => 
>   Configuration 
>   Expr (Free Sigma) () (Stack, Maybe ()) where
>   eval = eval'    
>   handle = handleVoid . handleStackOpen [] . handleExc 

> instance (HStack ExprValue :<: Sigma, HExc :<: Sigma) =>
>   CorrectCompiler
>   Expr Code (Free Sigma) () (Stack, Maybe ()) where
>   exec = exec'
>   comp = comp1
>   comp' = comp1'




