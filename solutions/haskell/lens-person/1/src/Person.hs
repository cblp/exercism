{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Person (
    Address (..),
    Born (..),
    Name (..),
    Person (..),
    bornStreet,
    renameStreets,
    setBirthMonth,
    setCurrentStreet,
) where

import Control.Lens (Iso', Lens', iso, lens, makeLenses, over, set, view, _2)
import Control.Lens.Unsound (adjoin)
import Data.Time.Calendar (Day, DayOfMonth, MonthOfYear, Year, fromGregorian, toGregorian)

data Address = Address
    { _street :: String
    , _houseNumber :: Int
    , _place :: String
    , _country :: String
    }
makeLenses ''Address

data Name = Name
    { _foreNames :: String
    , _surName :: String
    }

data Born = Born
    { _bornAt :: Address
    , _bornOn :: Day
    }
makeLenses ''Born

data Person = Person
    { _name :: Name
    , _born :: Born
    , _address :: Address
    }
makeLenses ''Person

bornStreet :: Born -> String
bornStreet = view $ bornAt . street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = set $ address . street

setBirthMonth :: MonthOfYear -> Person -> Person
setBirthMonth = set $ born . bornOn . month

renameStreets :: (String -> String) -> Person -> Person
renameStreets = over $ (address `adjoin` (born . bornAt)) . street

month :: Lens' Day MonthOfYear
month = gregorian . _2

gregorian :: Iso' Day (Year, MonthOfYear, DayOfMonth)
gregorian = iso toGregorian $ \(y, m, d) -> fromGregorian y m d
