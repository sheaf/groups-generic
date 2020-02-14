
# groups-generic <a href="https://hackage.haskell.org/package/groups-generic" alt="Hackage"><img src="https://img.shields.io/hackage/v/groups-generic.svg" /></a>

Extends the [groups](https://hackage.haskell.org/package/groups) library with functionality for deriving `Group` instances using generics:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Example where

-- base
import Data.Semigroup
  ( Sum(..), Product(..) )
import GHC.Generics
  ( Generic )

-- generic-data
import Generic.Data
  ( Generically(..) )

-- groups
import Data.Group
  ( Group(..) )

-- groups-generic
import Data.Group.Generics
  ( ) -- imports generic instances

-----------------------------------------------------

data Point2D a = Point2D !a !a
  deriving stock Generic
newtype Vector2D a = Vector2D { tip :: Point2D a }
  deriving ( Semigroup, Monoid, Group )
    via Generically ( Point2D ( Sum a ) )

data MyRecord
  = MyRecord
  { field1 :: Sum Double
  , field2 :: Product Double
  , field3 :: ( Sum Int, Sum Int )
  }
  deriving stock Generic
  deriving ( Semigroup, Monoid, Group )
    via Generically MyRecord
```
