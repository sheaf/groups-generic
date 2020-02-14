{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , TypeApplications
  , UndecidableInstances
#-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Module: Data.Group.Generics

Orphan instances allowing generic deriving of 'Group' instances:

> > data MyRecord
> >   = MyRecord
> >   { field1 :: Sum Double
> >   , field2 :: Product Double
> >   , field3 :: ( Sum Int, Sum Int )
> >   }
> >   deriving Generic
> >   deriving ( Semigroup, Monoid, Group )
> >     via Generically MyRecord

Also includes some instances for newtypes from @base@ such as 'Identity' and 'Const'.
-}


module Data.Group.Generics
  ( )
  where

-- base
import Data.Functor.Const
  ( Const(..) )
import Data.Functor.Identity
  ( Identity(..) )
import Data.Ord
  ( Down(..) )
import Data.Proxy
  ( Proxy(..) )
import GHC.Generics
  ( Generic(..)
  , U1(..), Rec1(..), M1(..), K1(..), Par1(..)
  , (:*:)(..), (:.:)(..)
  )

-- generic-data
import Generic.Data
  ( Generically(..) )

-- groups
import Data.Group
  ( Group(..), Abelian )

-----------------------------------------------------------------------
-- Instances for 'Group'.

instance Group (U1 p) where
  invert _ = U1
  pow  _ _ = U1
instance Group (f p) => Group (Rec1 f p) where
  invert (Rec1 g) = Rec1 (invert g)
  pow (Rec1 g) n = Rec1 (pow g n)
instance Group (f p) => Group (M1 i c f p) where
  invert (M1 g) = M1 (invert g)
  pow (M1 g) n = M1 (pow g n)
instance Group g => Group (K1 i g p) where
  invert (K1 g) = K1 (invert g)
  pow (K1 g) n = K1 (pow g n)
instance Group g => Group (Par1 g) where
  invert (Par1 g) = Par1 (invert g)
  pow (Par1 g) n = Par1 (pow g n)
instance (Group (f1 p), Group (f2 p) ) => Group ((:*:) f1 f2 p) where
  invert ( g1 :*: g2 ) = ( invert g1 :*: invert g2 )
  pow ( g1 :*: g2 ) n = ( pow g1 n :*: pow g2 n )
instance Group (f (g p)) => Group ((:.:) f g p) where
  invert (Comp1 g) = Comp1 (invert g)
  pow (Comp1 g) n = Comp1 (pow g n)

ginvert :: forall g. ( Generic g, Group ( Rep g () ) ) => g -> g
ginvert = to . invert @( Rep g () ) . from

gpow :: forall n g. ( Integral n, Generic g, Group ( Rep g () ) ) => g -> n -> g
gpow x n = to ( pow @( Rep g () ) ( from x ) n )

instance
  ( Generic g
  , Group ( Rep g () )
  )
  => Group ( Generically g ) where
  invert  = ginvert
  pow x n = gpow x n

-- Other useful instances.
instance Group a => Group (Down a) where
  invert (Down a) = Down (invert a)
  pow (Down a) n = Down (pow a n)
instance Group a => Group (Identity a) where
  invert (Identity a) = Identity (invert a)
  pow (Identity a) n = Identity (pow a n)
instance Group a => Group (Const a b) where
  invert (Const a) = Const (invert a)
  pow (Const a) n = Const (pow a n)
instance Group (Proxy s) where
  invert _ = Proxy
  pow  _ _ = Proxy


-----------------------------------------------------------------------
-- Instances for 'Abelian'.

instance Abelian (U1 p)
instance Abelian (f p) => Abelian (Rec1 f p)
instance Abelian (f p) => Abelian (M1 i c f p)
instance Abelian g => Abelian (K1 i g p)
instance Abelian g => Abelian (Par1 g)
instance (Abelian (f1 p), Abelian (f2 p)) => Abelian ((:*:) f1 f2 p)
instance Abelian (f (g p)) => Abelian ((:.:) f g p)

instance
  ( Generic g
  , Abelian ( Rep g () )
  )
  => Abelian ( Generically g )

-- Other useful instances.
instance Abelian a => Abelian (Down a)
instance Abelian a => Abelian (Identity a) where
instance Abelian a => Abelian (Const a b) where
instance Abelian (Proxy s) where
