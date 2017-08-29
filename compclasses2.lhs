
> {-# LANGUAGE	DeriveFunctor #-} 
> {-# LANGUAGE	GADTs #-}
> {-# LANGUAGE	TypeOperators #-}
> {-# LANGUAGE	MultiParamTypeClasses #-}
> {-# LANGUAGE	FlexibleInstances #-}
> {-# LANGUAGE	FlexibleContexts #-}
> {-# LANGUAGE	AllowAmbiguousTypes #-}
> {-# LANGUAGE	PatternSynonyms #-}
> {-# LANGUAGE	ViewPatterns #-}
> {-# LANGUAGE  FunctionalDependencies #-}
> 

> module CompClasses2 where

> import Control.Applicative (Applicative(..))
> import Control.Monad       (liftM, ap)

{- classes to encode correct compiler method -}

--Free type consists of a leaf (Var) or Node in the tree

> data Free f a = Var a | Cons (f (Free f a))

--Monad instance comes for free with fold

> instance Functor f => Functor (Free f) where
>    fmap = liftM
 
> instance Functor f => Applicative (Free f) where
>    pure  = Var
>    (<*>) = ap

> instance Functor f => Monad (Free f) where
> 	return = pure
> 	m >>= f = fold Cons f m

--fold over functor types

> fold :: Functor f => (f b -> b) -> (a -> b) -> (Free f a -> b)
> fold alg gen (Var x) = gen x
> fold alg gen (Cons op) = alg (fmap (fold alg gen) op)

-- coproduct functor type

> data (:+:) f g a where
>	Inl :: f a -> (f :+: g) a
>	Inr :: g a -> (f :+: g) a
>	deriving Functor

-- Swiestra's smart constructor from data a la carte

> class (Functor f, Functor g) => f :<: g where
>	inj :: f a -> g a 
> 	prj :: g a -> Maybe (f a)

-- The only necessary instances of :<: 

> instance Functor f => f :<: f where
>	inj = id
>	prj = Just
> instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
>	inj = Inl
> 	prj (Inl fa) = Just fa
> 	prj _ = Nothing
> instance {-# OVERLAPPABLE #-} 
>   (Functor f , Functor g, Functor h,f :<: g) => f :<: (h :+: g) where
>	inj = Inr . inj
> 	prj (Inr ga) = prj ga
> 	prj _ = Nothing

--injection operator - Swiestra Data types a la carte
--operation g is injected into/supported by f

> inject :: (g :<: f) => g (Free f a) -> Free f a
> inject = Cons . inj

> project :: (f :<: g) => Free g a -> Maybe (f (Free g a))
> project (Cons s) = prj s
> project _ = Nothing

{-simplify the handlers into folds, in line with fusion for free:-}
--functor combinator type 

> (\/) :: (f b -> b) -> (g b -> b) -> ((f :+: g) b -> b)
> (\/) algf alg (Inl s) = algf s
> (\/) algf algg(Inr s) = algg s

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

> --eval :: source -> conf -> conf
> --exec :: target -> conf -> conf
> --comp :: source -> target
> --comp':: source -> target -> target

