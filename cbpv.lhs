> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE MultiParamTypeClasses #-}

> import CompClasses3

-- syntax

> type Name = String
> type EMsg   = String

-- toy CBPV language with exceptions

> data Expr 
>  -- value types
>  =  Val Int             -- integer constant
>  |  Vars Name           -- variable 
>  |  Thunk Expr          -- thunk of a computation 
>  |  ThrowE EMsg         -- exception throwing
>  -- computation types
>  |  Add Expr Expr       -- sum of two ints 
>  |  Force Expr          -- forcing a thunk to run 
>  |  Return Expr         -- value returning computation 
>  |  Do Name Expr Expr   -- sequencing |do n <- e1 in e2| 
>  |  Let Name Expr Expr  -- let-binding |let n = e1 in e2| 
>  |  Fun Name Expr 	     -- function |\ x -> e| 
>  |  Apply Expr Expr     -- application |f v| 
>  |  CatchE Expr Expr    -- exception catching
>  deriving Show 

-- semantic domain of CBPV with exceptions

> data ExprValue 
>  = VNum Int             -- Integer literal
>  | VThunk Env Expr      -- thunk value 
>  | VThunk' Env Code     -- thunk value with compiled computation
>  | VReturn ExprValue    -- return value
>  | VFun Env Name Expr   -- function closure returned
>  | VFun' Env Name Code  -- abstraction with compiled body + closure
>  deriving Show

-- aux types and terms

> type Env   = [(Name, ExprValue)] 
> emptyEnv   = []
> type Stack = [ExprValue]
> emptyStack = []
> type Conf  = (Stack, Env)
> emptyConf  = (emptyStack,emptyEnv) 

-- concrete semantics of cbpv with exceptions

> eval0 :: Expr -> Env -> Either EMsg ExprValue
> -- value types 
> eval0 (Val n) env        = Right (VNum n)
> eval0 (Vars n) env       = case lookup n env of
>   Nothing -> Left ("Unknown Variable" ++ show n)
>   Just v  -> Right v
> eval0 (Thunk e1) env     = Right (VThunk env e1)
> eval0 (ThrowE msg) env   = Left msg
> -- computation types
> eval0 (Add e1 e2) env    = case eval0 e1 env of
>   Right (VNum m) -> case eval0 e2 env of 
>     Right (VNum n) -> Right (VNum (m+n))
>     Left e -> Left ("Integers Expected in Addition " ++ e)
>   Left e -> Left ("Integers Expected in Addition " ++ e)
> eval0 (Force e1) env     = case eval0 e1 env of
> 	Right (VThunk env' e2) -> eval0 e2 env'
> 	Left e -> Left ("Thunk Expected in Force " ++ e) 
> eval0 (Return e1) env    = let Right v1 = eval0 e1 env in
>   Right (VReturn v1)
> eval0 (Do n e1 e2) env   = case eval0 e1 env of
> 	Right (VReturn v) -> eval0 e2 ((n,v):env)
> 	Left e -> Left ("return expected " ++ e)
> eval0 (Let n e1 e2) env  = let Right v = eval0 e1 env in
> 	eval0 e2 ((n, v):env)
> eval0 (Fun n e1) env     = Right (VFun env n e1)
> eval0 (Apply e1 e2) env  = case eval0 e1 env of
> 	Right (VFun env' n e3) -> let Right v = eval0 e2 env in
> 	  eval0 e3 ((n, v):env')
> 	Left e -> Left ("function expected" ++ e)
> eval0 (CatchE e1 e2) env = case eval0 e1 env of
>   Left e -> case eval0 e2 env of 
>     Left e' -> Left ("catch and exception failed" ++ e' ++ e)
>     r -> r
>   r -> r

> test1 = (Fun "n" (Return (Add (Vars "n") (Val 3))))
> test2 = (Return (Add (Val 1) (Val 2)))
> test3 = (Force (Thunk (Return (Val 1))))
> test4 = (Let "x" (Add (Val 3) (Val 5)) (Return (Add (Vars "x") (Vars "x"))))
> test5 = (Do "x" (Return (Add (Val 3) (Val 5))) (Return (Add (Vars "x") (Vars "x"))))
> test6 = (Add (Val 2) (ThrowE "we have an error"))
> test7 = CatchE (Add (Val 2) (ThrowE "we have an error")) (Val 3)

{-

Main> eval0 test1 [("v", (VNum 3))]

Right (VFun [("v",VNum 3)] "n" (Return (Add (Var "n") (Val 3))))

Main> eval0 test2 emptyEnv

Right (VReturn (VNum 3))

Main> eval0 test3 emptyEnv

Right (VReturn (VNum 1))

Main> eval0 test4 emptyEnv

Right (VReturn (VNum 16))

Main> eval0 test5 emptyEnv

Right (VReturn (VNum 16))

Main> eval0 test6 emptyEnv

Left "Integers Expected in Addition we have an error"

Main> eval0 test7 emptyEnv

Right (VNum 3)


-}

{-massage into Expr -> Conf -> Conf form-}

> eval1 :: Expr -> Conf -> Either EMsg Conf
> -- value types 
> eval1 (Val n) (s,env)        = Right (VNum n:s, env)
> eval1 (Vars n) (s,env)       = case lookup n env of
>   Nothing -> Left ("Unknown Variable" ++ show n)
>   Just v  -> Right (v:s, env)
> eval1 (Thunk e1) (s,env)     = Right ((VThunk env e1):s, env)
> eval1 (ThrowE msg) c         = Left msg
> -- computation types
> eval1 (Add e1 e2) c          = case eval1 e1 c of
>   Right c' -> case eval1 e2 c' of 
>     Right ((VNum m):(VNum n):s, env) -> Right ((VNum (m+n)):s, env)
>     Left e -> Left ("Integers Expected in Addition " ++ e)
>   Left e -> Left ("Integers Expected in Addition " ++ e)
> eval1 (Force e1) c           = case eval1 e1 c of
> 	Right ((VThunk env'' e2):s', env') -> eval1 e2 (s', env'')
> 	Left e -> Left ("Thunk Expected in Force " ++ e)
> eval1 (Return e1) c@(s,env)  = case eval1 e1 c of
>   Right (v:s, env) -> Right ((VReturn v):s, env)
>   l -> l
> eval1 (Do n e1 e2) c@(s,env) = case eval1 e1 c of
> 	Right ((VReturn v):s, env) -> eval1 e2 (s, (n,v):env)
> 	Left e -> Left ("return expected " ++ e)
> eval1 (Let n e1 e2) c@(s,env)= let Right ((v:vs), env') = eval1 e1 c in 
> 	eval1 e2 (s,(n, v):env) 
> eval1 (Fun n e1) c@(s,env)   = Right ((VFun env n e1):s, env)
> eval1 (Apply e1 e2) c        = case eval1 e1 c of
> 	Right ((VFun env' n e3):s, env) -> let Right (v:vs, env'') = eval1 e2 c in
> 	  eval1 e3 (s, (n, v):env')
> 	Left e -> Left ("function expected" ++ e)
> eval1 (CatchE e1 e2) c       = case eval1 e1 c of
>   Left e -> case eval1 e2 c of 
>     Left e' -> Left ("Catch and Exception Failed" ++ e' ++ e)
>     r -> r
>   r -> r

{-

Main> eval1 test1 (emptyStack, [("v", (VNum 3))])

Right ([VFun [("v",VNum 3)] "n" (Return (Add (Vars "n") (Val 3)))],[("v",VNum 3)])

Main> eval1 test2 emptyConf

Right ([VReturn (VNum 3)],[])

Main> eval1 test3 emptyConf

Right ([VReturn (VNum 1)],[])

Main> eval1 test4 emptyConf

Right ([VReturn (VNum 16)],[("x",VNum 8)])

Main> eval1 test5 emptyConf

Right ([VReturn (VNum 16)],[("x",VNum 8)])

Main> eval1 test6 emptyConf

Left "Integers Expected in Addition we have an error"

Main> eval1 test7 emptyConf

Right ([VNum 3],[])

-}

{-define algebraic handlers for CBPV with exceptions-}

{-stack functor to store resultant stack values-}
{-same as stack functor from Exc example-}

> data StackFunctor s a 
>    =  PopS (s -> a)
>    |  PushS s a
>        deriving Functor
 
> type HStack s = Lift (StackFunctor s) 

-- popS = pop Stack, pushS = push Stack

> popS :: (HStack ExprValue :<: g)=>
>   Free g ExprValue
> popS = inject (Lift (PopS Var))
> pushS :: (HStack ExprValue :<: g)=>
>   ExprValue -> Free g ()
> pushS v = inject (Lift (PushS v (Var ()))) 

-- open handler for stack functor

> handleStackOpen :: Syntax g => 
>       [v] -> 
>       Free (HStack v :+: g) a -> 
>       Free g ([v], a)
> handleStackOpen s (Var a) = return (s,a)
> handleStackOpen [] (Cons (Inl (Lift (PopS k)))) = error "Cannot Pop Empty Stack" 
> handleStackOpen (x:xs) (Cons (Inl (Lift (PopS k)))) = handleStackOpen xs (k x)  
> handleStackOpen xs (Cons (Inl (Lift (PushS x k)))) = handleStackOpen (x:xs) k 
> handleStackOpen s (Other op) = 
>   Cons (weave (s,()) (uncurry handleStackOpen) op) 

{-Exception functor to store resultant stack values-}
{-upgraded so that throw now supports error logging-}
{-CBPV is really good for error tracing-}

> data ExcFunctor e m a where
>   Throw :: e -> ExcFunctor e m a
>   Catch :: (m x) -> (e -> m x) -> (x -> m a) -> ExcFunctor e m a 

> instance Functor m => Functor (ExcFunctor e m) where
>   fmap f (Throw e) = (Throw e)
>   fmap f (Catch p h k) = Catch p h (fmap f . k)

> type HExc = ExcFunctor 

-- standard tricks to capture tree notation

> throw ::(HExc EMsg :<: g) => 
> 	EMsg -> Free g a
> throw e = inject (Throw e)
> catch ::(HExc EMsg :<: g) => 
>       Free g a -> 
>       (EMsg -> Free g a) -> 
>       Free g a
> catch p h = inject (Catch p h return) 

> instance HFunctor (HExc e) where
>    hmap t (Throw e) = Throw e
>    hmap t (Catch p h k) = Catch (t p) (t . h) (t . k)

> instance Syntax (HExc e) where
>     emap f (Throw e) = Throw e
>     emap f (Catch p h k) = Catch p h (f . k)
>     weave f hdl (Throw e) = Throw e
>     weave f hdl (Catch p h k) = 
>        Catch (hdl (fmap (const p) f))
>              (\e -> hdl (fmap (const (h e)) f))
>              (hdl . fmap k) 

--Higher order open handler for exceptions
-- now uses EITHER type, reflective of concrete semantics
-- and Exc e m a type

> handleExc :: (Syntax g, Monoid e) => 
>   Free (HExc e :+: g) a -> 
>   Free g (Either e a)
> handleExc (Var x)                = return (Right x)
> handleExc (Cons (Inl (Throw e))) = return (Left e)
> handleExc (Cons (Inl (Catch p h k))) = do
>   r <- handleExc p 
>   case r of
>     (Right x) -> handleExc (k x)
>     (Left e) -> do 
>       r <- handleExc (h e) 
>       case r of
>         (Right x) -> handleExc (k x)
>         (Left e')  -> return (Left (mappend e' e))
> handleExc (Other op) = Cons (weave (Right ()) hdl op) where
>   hdl = either (return . Left) handleExc 

-- HFunctor for pure computations

> data Void k deriving Functor
> type HVoid = Lift Void

> handleVoid :: Free HVoid a -> a
> handleVoid (Var a) = a 

-- State Functor for Environment
-- forms the state computational effect with GET and PUT

> data StateFunctor s a 
>    =  Get (s -> a)
>    |  Put s a
>     deriving Functor

> type HState s = Lift (StateFunctor s) 

-- standard trick to capture tree notation of state functor

> get :: (HState e :<: g)=>
>   Free g e
> get = inject (Lift (Get Var))
> put :: (HState e :<: g)=>
>   e -> Free g ()
> put e = inject (Lift (Put e (Var ()))) 

-- additional abuse of notation to simulate a stack using state

> pushE :: (HState [v] :<: g)=>
>   v -> Free g ()
> pushE x = do {env<-get; put (x:env)} 

-- handle with catch if used

-- open handler for state effect

> handleStateOpen :: Syntax g => 
>       e -> 
>       Free (HState e :+: g) a -> 
>       Free g (e, a)
> handleStateOpen env (Var a) = return (env,a)
> handleStateOpen env (Cons (Inl (Lift (Get k))))      = handleStateOpen env (k env)  
> handleStateOpen env (Cons (Inl (Lift (Put env' k)))) = handleStateOpen env' k 
> handleStateOpen env (Other op) = 
>   Cons (weave (env,()) (uncurry handleStateOpen) op)

-- abstract evaluation semantics for CBPV with Exceptions, variables,
-- and recursive functions 
-- (could add bools, sum, products but it wouldn't add to the proof really)

> eval' :: (HStack ExprValue :<: g, HExc EMsg :<: g, HState Env :<: g) => 
>     Expr -> 
>     Free g () ->
>     Free g ()
> -- value types
> eval' (Val n) c = do 
>   c
>   pushS (VNum n)
> eval' (Vars n) c = do
>   c
>   env <- get
>   case lookup n env of 
>     Nothing -> throw ("Unknown Variable " ++ n)
>     Just v  -> pushS v
> eval' (Thunk e1) c = do
>   c
>   env <- get
>   pushS (VThunk env e1)
> eval' (ThrowE e) c = throw e
> -- computation types
> eval' (Add e1 e2) c = do
>   eval' e2 (eval' e1 c)
>   v1 <- popS
>   v2 <- popS
>   case (v1, v2) of 
>     ((VNum n), (VNum m)) -> pushS (VNum (m+n))
>     _ -> throw ("VNums Expected in Addition " ++ show v1  ++ " " ++ show v2 ++ " received")
> eval' (Force e1) c = do
>   eval' e1 c
>   v <- popS
>   case v of 
>     (VThunk env e) -> do
>       let c' = put env
>       eval' e c'
>     _ -> throw ("VThunk expected in Force, " ++ show v ++ " received") 
> eval' (Return e1) c = do
>   eval' e1 c
>   v <- popS
>   pushS (VReturn v)
> eval' (Do n e1 e2) c = do
>   eval' e1 c
>   v1 <- popS
>   case v1 of
>     (VReturn v2) -> do
>       let c' = pushE (n,v2)
>       eval' e2 c'
>     _ -> throw ("VReturn expected in Do, " ++ show v1 ++ " received")
> eval' (Let n e1 e2) c = do
>   eval' e1 c
>   v <- popS
>   let c' = pushE (n,v)
>   eval' e2 c'
> eval' (Fun n e1) c = do
>   c
>   env <- get
>   pushS (VFun env n e1)
> eval' (Apply e1 e2) c = do
>   eval' e2 (eval' e1 c)
>   v <- popS
>   f <- popS
>   case f of
>     (VFun env' n f) -> do
>       let c'' = put ((n,v):env')
>       eval' f c''
>     _ -> throw ("VFun function expected " ++ show f ++ " received")
> eval' (CatchE e1 e2) c = catch (eval' e1 c) (\e -> catch (eval' e2 c) 
>    (\e' -> throw ("Catch and Exception Failed " ++ e' ++ e)))

> handler stack env = (handleVoid . handleStackOpen stack . handleStateOpen env . handleExc)
> defaultHandler = handler (emptyStack::Stack) (emptyEnv::Env)

> initConf :: Syntax g => Free g ()
> initConf = return ()

{-
Main> defaultHandler (eval' test1 initConf)

([VFun [] "n" (Return (Add (Vars "n") (Val 3)))],([],Right ()))

Main> defaultHandler (eval' test2 initConf)

([VReturn (VNum 3)],([],Right ()))

Main> defaultHandler (eval' test3 initConf)

([VReturn (VNum 1)],([],Right ()))

Main> defaultHandler (eval' test4 initConf)

([VReturn (VNum 16)],([("x",VNum 8)],Right ()))

Main> defaultHandler (eval' test5 initConf)

([VReturn (VNum 16)],([("x",VNum 8)],Right ()))

-}

-- conversion function

> conv :: ExprValue -> ExprValue
> conv (VThunk env e1)  = VThunk' (conve env) (comp1 e1)
> conv (VReturn v)      = VReturn (conv v)
> conv (VFun env n e1)  = VFun' (conve env) n (comp1 e1) 
> conv x = x

> conve :: Env -> Env
> conve = fmap (\(n,v) -> (n, conv v)) 

--
correctness specification is now 

exec (comp s t) c = exec t (eval s c[conv v/v]), if s evals to v in config c

{-target machine code, derived-}

> data Code where
>   HALT :: Code
>   PUSH :: ExprValue -> Code -> Code
>   LOOKUP :: Name -> Code -> Code
>   FAIL :: EMsg -> Code
>   THK  :: Code -> Code -> Code
>   ADD  :: Code -> Code
>   FRC  :: Code -> Code
>   RET  :: Code -> Code 
>   APP  :: Code -> Code
>   FUN  :: Name -> Code -> Code -> Code 
>   DO   :: Name -> Code -> Code -> Code
>   LET  :: Name -> Code -> Code -> Code
>   deriving Show


{-virtual machine code, derived-}

> exec' :: (HStack ExprValue :<: g, HExc EMsg :<: g, HState Env :<: g) =>
>    Code ->
>    Free g () ->
>    Free g ()
> exec' HALT c         = c
> exec' (PUSH v t) c   = exec' t (do {c; pushS v})
> exec' (LOOKUP n t) c = exec' t (do
>   c
>   env <- get
>   case lookup n env of 
>     Nothing -> throw ("Unknown Variable " ++ n)
>     Just v  -> pushS v)
> exec' (FAIL e) c     = throw e
> exec' (ADD t) c      = exec' t (do
>   c
>   v1 <- popS
>   v2 <- popS
>   case (v1, v2) of 
>     ((VNum n), (VNum m)) -> pushS (VNum (m+n))
>     _ -> throw ("VNums Expected in Addition " ++ show v1  ++ " " ++ show v2 ++ " received"))
> exec' (FRC t) c      = exec' t (do                  
>   c
>   v <- popS
>   case v of 
>     (VThunk' env t1) -> do
>       let c' = put env
>       exec' t1 c'
>     _ -> throw ("VThunk expected in Force, " ++ show v ++ " received"))
> exec' (RET t) c      = exec' t (do
>   c
>   v <- popS
>   pushS (VReturn (conv v)))
> exec' (APP t) c      = exec' t (do    -- ! JEREMY WHAT DO YOU THINK
>   c
>   v <- popS
>   f <- popS
>   case f of
>     (VFun' env' n f) -> do
>       let c'' = put ((n, v):env')
>       exec' f c''
>     _ -> throw ("VFun function expected " ++ show f ++ " received"))
> exec' (FUN n t1 t2) c = exec' t2 (do    -- ! JEREMY WHAT DO YOU THINK
>   c
>   env <- get
>   pushS (VFun' (conve env) n t1))
> exec' (THK t1 t2) c = exec' t2 (do
>   c
>   env <- get
>   pushS (VThunk' (conve env) t1))
> exec' (DO n t1 t2) c = exec' t2 (do
>   c
>   v1 <- popS
>   case v1 of
>     (VReturn v2) -> do
>       let c' = pushE (n,conv v2)
>       exec' t1 c'
>     _ -> throw ("VReturn expected in Do, " ++ show v1 ++ " received"))
> exec' (LET n t1 t2) c = exec' t2 (do
>   c
>   v <- popS
>   let c' = pushE (n,v)
>   exec' t1 c')

{-top level compiler function-}

> comp1 :: Expr -> Code
> comp1 x = comp1' x HALT 

{-compiler function, derived-}

> comp1' :: Expr -> Code -> Code
> --value types
> comp1' (Val n) t      = PUSH (VNum n) t
> comp1' (Vars n) t     = LOOKUP n t
> comp1' (Thunk e1) t   = THK (comp1 e1) t                       
> comp1' (ThrowE msg) t = FAIL msg
> --computation types
> comp1' (Add e1 e2) t  = comp1' e1 (comp1' e2 (ADD t))
> comp1' (Force e1) t   = comp1' e1 (FRC t)       
> comp1' (Return e1) t  = comp1' e1 (RET t)
> comp1' (Do n e1 e2) t = comp1' e1 (DO n (comp1 e2) t)                        
> comp1' (Let n e1 e2) t= comp1' e1 (LET n (comp1 e2) t)                                                 
> comp1' (Fun n e1) t   = FUN n (comp1 e1) t                     
> comp1' (Apply e1 e2) t= comp1' e1 (comp1' e2 (APP t))      
> comp1' (CatchE (ThrowE e) h) t = comp1' h t           
> comp1' (CatchE x _) t = comp1' x t                    

{-

Main> defaultHandler (exec' (comp1 test1) initConf)

([VFun' [] "n" (LOOKUP "n" (PUSH (VNum 3) (ADD (RET HALT))))],([],Right ()))

Main> defaultHandler (exec' (comp1 test2) initConf)

([VReturn (VNum 3)],([],Right ()))

Main> defaultHandler (exec' (comp1 test3) initConf)

([VReturn (VNum 1)],([],Right ()))

Main> defaultHandler (exec' (comp1 test4) initConf)

([VReturn (VNum 16)],([("x",VNum 8)],Right ()))

Main> defaultHandler (exec' (comp1 test5) initConf)

([VReturn (VNum 16)],([("x",VNum 8)],Right ()))

-}

> type Sigma = HExc EMsg :+: HState Env :+: HStack ExprValue :+: HVoid 

--configuration instance

> instance (HStack ExprValue :<: Sigma, 
>     HExc EMsg :<: Sigma, 
>     HState Env :<: Sigma) =>
>   Configuration 
>   Expr (Free Sigma) () (Stack, (Env, Either EMsg ())) where
>   eval = eval'    
>   handle = defaultHandler 

-- correct compiler instance

> instance (HStack ExprValue :<: Sigma, 
>     HExc EMsg :<: Sigma, 
>     HState Env :<: Sigma) =>
>   CorrectCompiler
>   Expr Code (Free Sigma) () (Stack, (Env, Either EMsg ())) where
>   exec = exec'
>   comp = comp1
>   comp' = comp1'

-- pretty printer

> pp :: (Show a, Show b) => (a, (b, c)) -> IO ()
> pp (a, (b, c)) = do
>    putStrLn ("Stack: " ++ show a)
>    putStrLn ("Env: " ++ show b)

{-

Main> defaultHandler (exec' HALT (eval' test3 initConf))

([VReturn (VNum 1)],([],Right ()))

Main> defaultHandler (exec' (comp1 test3) initConf)

([VReturn (VNum 1)],([],Right ()))

-}




-- unsatisfactory as expressions are not compiled proper, and not tail recursive

-- lack of time DNF

-- modified semantic domain with exprs replaced with code

> data ExprValue' 
>  = VNum' Int            -- Integer literal
>  | VThunk'' Env' Code'     -- thunk value 
>  | VReturn' ExprValue'  -- return value
>  | VFun'' Env' Name Code' -- function closure returned
>  deriving Show

> type Env'   = [(Name, ExprValue')] 
> emptyEnv'   = []
> type Stack' = [ExprValue']
> emptyStack' = []
> type Conf'  = (Stack', Env')

> conv' :: ExprValue -> ExprValue'
> conv' (VNum n)         = VNum' n
> conv' (VThunk env e1)  = VThunk'' (conve' env) (comp2 e1)
> conv' (VReturn v)      = VReturn' (conv' v)
> conv' (VFun env n e1)  = VFun'' (conve' env) n (comp2 e1) 

> conve' :: Env -> Env'
> conve' = fmap (\(n,v) -> (n, conv' v)) 

> convs' :: Stack -> Stack'
> convs' = fmap conv'

> execHandler :: Free (HExc EMsg :+: (HState Env' :+: (HStack ExprValue' :+: HVoid))) a
>   -> ([ExprValue'], (Env', Either EMsg a))
> execHandler = handler (emptyStack'::Stack') (emptyEnv'::Env')

Main> pp (execHandler (exec2' (comp2 (Let "n" (Val 2) (Vars "n"))) initConf))

Stack: [VNum' 2]
Env: [("n",VNum' 2)]

Main> pp (defaultHandler (eval' (Let "n" (Val 2) (Vars "n")) initConf))

Stack: [VNum 2]
Env: [("n",VNum 2)]


> popS' :: (HStack ExprValue' :<: g)=>
>   Free g ExprValue'
> popS' = inject (Lift (PopS Var))
> pushS' :: (HStack ExprValue' :<: g)=>
>   ExprValue' -> Free g ()
> pushS' v = inject (Lift (PushS v (Var ()))) 

> data Code' where
>   HALT' :: Code'
>   PUSH' :: ExprValue' -> Code' -> Code'
>   LOOKUP' :: Name -> Code' -> Code'
>   FAIL' :: EMsg -> Code'
>   THK'  :: Code' -> Code' -> Code'
>   ADD'  :: Code' -> Code'
>   FRC'  :: Code' -> Code'
>   RET'  :: Code' -> Code' 
>   APP'  :: Code' -> Code'
>   FUN'  :: Name -> Code' -> Code' -> Code'
>   DO'   :: Name -> Code' -> Code' -> Code'
>   LET'  :: Name -> Code' -> Code' -> Code'
>   deriving Show

> exec2' :: (HStack ExprValue' :<: g, HExc EMsg :<: g, HState Env' :<: g) =>
>    Code' ->
>    Free g () ->
>    Free g ()
> exec2' HALT' c         = c
> exec2' (PUSH' v t) c   = exec2' t (do {c; pushS' v})
> exec2' (LOOKUP' n t) c = exec2' t (do
>   c
>   env <- get
>   case lookup n env of 
>     Nothing -> throw ("Unknown Variable " ++ n)
>     Just v  -> pushS' v)
> exec2' (FAIL' e) c     = throw e
> exec2' (ADD' t) c      = exec2' t (do
>   c
>   v1 <- popS'
>   v2 <- popS'
>   case (v1, v2) of 
>     ((VNum' n), (VNum' m)) -> pushS' (VNum' (m+n))
>     _ -> throw ("VNums Expected in Addition " ++ show v1  ++ " " ++ show v2 ++ " received"))
> exec2' (FRC' t) c      = exec2' t (do                  
>   c
>   v <- popS'
>   case v of 
>     (VThunk'' env t2) -> do
>       let c' = put env
>       exec2' t2 c'
>     _ -> throw ("VThunk expected in Force, " ++ show v ++ " received"))
> exec2' (RET' t) c      = exec2' t (do
>   c
>   v <- popS'
>   pushS' (VReturn' v)) 
> exec2' (APP' t) c      = exec2' t (do    
>   c
>   v <- popS'
>   f <- popS'
>   case f of
>     (VFun'' env' n f) -> do
>       let c'' = put ((n,v):env')
>       exec2' f c''
>     _ -> throw ("VFun function expected " ++ show f ++ " received")) 
> exec2' (FUN' n t1 t) c = exec2' t (do    
>   c
>   env <- get
>   pushS' (VFun'' env n t1)) 
> exec2' (THK' t1 t) c = exec2' t (do
>   c
>   env <- get
>   pushS' (VThunk'' env t1)) 
> exec2' (DO' n t1 t2) c = exec2' t2 (do
>   c
>   v1 <- popS'
>   case v1 of
>     (VReturn' v2) -> do
>       let c' = pushE (n,v2)
>       exec2' t1 c'
>     _ -> throw ("VReturn expected in Do, " ++ show v1 ++ " received")) 
> exec2' (LET' n t1 t2) c = exec2' t2 (do
>   c
>   v <- popS'
>   let c' = pushE (n,v)
>   exec2' t1 c') 

> comp2 :: Expr -> Code'
> comp2 x = comp2' x HALT' 

> comp2' :: Expr -> Code' -> Code'
> --value types
> comp2' (Val n) t      = PUSH' (VNum' n) t
> comp2' (Vars n) t     = LOOKUP' n t
> comp2' (Thunk e1) t   = THK' (comp2 e1) t                        
> comp2' (ThrowE msg) t = FAIL' msg
> --computation types
> comp2' (Add e1 e2) t  = comp2' e1 (comp2' e2 (ADD' t))
> comp2' (Force e1) t   = comp2' e1 (FRC' t)       
> comp2' (Return e1) t  = comp2' e1 (RET' t)
> comp2' (Do n e1 e2) t = comp2' e1 (DO' n (comp2' e2 HALT') t)                        
> comp2' (Let n e1 e2) t= comp2' e1 (LET' n (comp2' e2 HALT') t)                                                 
> comp2' (Fun n e1) t   = FUN' n (comp2 e1) t                     
> comp2' (Apply e1 e2) t= comp2' e1 (comp2' e2 (APP' t))      
> comp2' (CatchE (ThrowE e) h) t = comp2' h t           
> comp2' (CatchE x h) t = comp2' x t                    













