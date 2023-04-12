{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
-- {-# LANGUAGE ScopedTypeVariables   #-}

module Named
  (Named, type (~~), name, Defn, defn, Defining) where

import The
import Data.Coerce

newtype Named name a = Named a
instance The (Named name a) a
type a ~~ name = Named name a
-- read a ~~ n as: values of type a with name n


-- Morally, the type of `name` is
--      a -> (exists name. (a ~~ name))
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

data Defn = Defn
type Defining f = (Coercible f Defn, Coercible Defn f)
-- the above requires lang extension ConstraintKinds and FlexibleContexts

-- Allow library authors to introduce their own names.
defn :: Defining f => a -> (a ~~ f)
defn = coerce
