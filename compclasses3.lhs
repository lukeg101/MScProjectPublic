> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE  GADTs #-}
> {-# LANGUAGE  TypeOperators #-}
> {-# LANGUAGE  MultiParamTypeClasses #-}
> {-# LANGUAGE  FlexibleInstances #-}
> {-# LANGUAGE  FlexibleContexts #-}
> {-# LANGUAGE  AllowAmbiguousTypes #-}
> {-# LANGUAGE  PatternSynonyms #-}
> {-# LANGUAGE  ViewPatterns #-}
> {-# LANGUAGE  RankNTypes #-}
> {-# LANGUAGE  KindSignatures #-}
> {-# LANGUAGE  FunctionalDependencies#-}

> module CompClasses3 where

> import Control.Applicative (Applicative(..))
> import Control.Monad       (liftM, ap)

> type f ->. g = forall x. f x -> g x

> class HFunctor h where
>     hmap ::(Functor f ,Functor g) => (f ->. g) -> (h f ->. h g)

> class HFunctor f => Syntax f where
>     emap ::(m a -> m b)-> (f m a -> f m b)
>     weave ::(Monad m,Monad n,Functor s) => 
>       s () -> Handler s m n -> (f m a -> f n (s a))

> type Handler s m n = forall x. s (m x) -> n (s x)

> data Free f a = Var a | Cons (f (Free f) a)

> instance Syntax f => Functor (Free f) where
>     fmap = liftM
 
> instance Syntax f => Applicative (Free f) where
>     pure  = Var
>     (<*>) = ap

> instance Syntax f => Monad (Free f) where
>   return = Var
>   Var v >>= f = f v
>   Cons op >>= f = Cons (emap (>>= f) op)

{-
--fold over functor types


fold :: Functor f => (f b -> b) -> (a -> b) -> (Free f a -> b)

fold alg gen (Var x) = gen x

fold alg gen (Cons op) = alg (fmap (fold alg gen) op)
-}

> infixr 6 :+:
> data (f :+: g) (m:: * -> *) a = 
>    Inl (f m a) 
>    | Inr (g m a)
>    deriving Functor

> instance (HFunctor f, HFunctor g) => HFunctor (f :+: g) where
>   hmap t (Inl op) = Inl (hmap t op)
>   hmap t (Inr op) = Inr (hmap t op)

> instance (Syntax f,Syntax g) => Syntax (f :+: g) where
>   emap f (Inl op) = Inl (emap f op)
>   emap f (Inr op) = Inr (emap f op)
>   weave s hdl (Inl op) = Inl (weave s hdl op)
>   weave s hdl (Inr op) = Inr (weave s hdl op)

> newtype (Lift f) (m:: * -> *) a = Lift (f (m a))
> instance Functor f => HFunctor (Lift f) where
>   hmap t (Lift op) = Lift (fmap t op)

> instance Functor f => Syntax (Lift f) where
>   emap f (Lift op) = Lift (fmap f op)
>   weave s hdl (Lift op) =
>       Lift (fmap (\p -> hdl (fmap (const p) s)) op)

-- Swiestra's smart constructor from data a la carte
modified with hinze syntax

> class (Syntax f, Syntax g) => f :<: g where
>   inj :: f m a -> g m a 
>   prj :: g m a -> Maybe (f m a)

-- The only necessary instances of :<: 

> instance Syntax f => f :<: f where
>   inj = id
>   prj = Just
> instance {-# OVERLAPPING #-} (Syntax f, Syntax g) => f :<: (f :+: g) where
>   inj = Inl
>   prj (Inl fa) = Just fa
>   prj _ = Nothing
> instance {-# OVERLAPPABLE #-} (Syntax h,f :<: g) => f :<: (h :+: g) where
>   inj = Inr . inj
>   prj (Inr ga) = prj ga
>   prj _ = Nothing

--injection operator - Swiestra Data types a la carte
--operation g is injected into/supported by f

> inject :: (g :<: f) => g (Free f) a -> Free f a
> inject = Cons . inj

> project :: (f :<: g) => Free g a -> Maybe (f (Free g) a)
> project (Cons s) = prj s
> project _ = Nothing

> pattern Other s = Cons (Inr s)

{-classes to encode correct compiler method-}

> class Configuration s e d a | e d -> a where
>     eval  :: s -> e d -> e d
>     handle :: e d -> a

> class Configuration s e d a => CorrectCompiler s t e d a | e a -> s where
>     exec  :: t -> e d -> e d
>     comp  :: s -> t 
>     comp' :: s -> t -> t 
>     --laws
>     --exec t (eval s c) = exec (comp' s t) c 
>     --exec (comp s) c = eval s c




