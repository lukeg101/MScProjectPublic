
> {-# LANGUAGE MultiParamTypeClasses #-} 
> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE FunctionalDependencies#-}

> module CompClasses where

{-classes to encode correct compiler method
-}

> class Configuration s c where
>     eval :: s -> c -> c

> class Configuration s c => CorrectCompiler s t c | t -> c, c -> s where
>     exec  :: t -> c -> c
>     comp  :: s -> t 
>     comp' :: s -> t -> t 
>     --laws
>     --exec t (eval s c) = exec (comp' s t) c 
>     --exec (comp s) c = eval s c

> --eval :: source -> conf -> conf
> --exec :: target -> conf -> conf
> --comp :: source -> target
> --comp':: source -> target -> target

