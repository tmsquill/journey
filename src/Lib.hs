module Lib
  ( someFunc
  , allNationalParks
  , filterByLocation
  , filterByEstablishedBefore
  , filterByEstablishedAfter
  ) where

import Data.Time

data NationalPark =
  NationalPark
    { name :: String
    , location :: [String]
    , established :: Day
    }

instance Show NationalPark where
  show (NationalPark name location established) =
    show name ++ " - " ++ show location ++ " - " ++ show established

type NationalParks = [NationalPark]

allNationalParks :: NationalParks
allNationalParks =
  [ NationalPark "Acadia" ["Maine"] (fromGregorian 1919 02 26)
  , NationalPark "American Samoa" ["American Samoa"] (fromGregorian 1988 10 31)
  , NationalPark "Arches" ["Utah"] (fromGregorian 1971 11 12)
  , NationalPark "Badlands" ["South Dakota"] (fromGregorian 1978 11 10)
  , NationalPark "Big Bend" ["Texas"] (fromGregorian 1944 06 12)
  , NationalPark "Biscayne" ["Florida"] (fromGregorian 1980 06 28)
  , NationalPark
      "Black Canyon of the Gunnison"
      ["Colorado"]
      (fromGregorian 1999 10 21)
  , NationalPark "Bryce Canyon" ["Utah"] (fromGregorian 1928 02 05)
  , NationalPark "Canyonlands" ["Utah"] (fromGregorian 1964 09 12)
  , NationalPark "Capitol Reef" ["Utah"] (fromGregorian 1971 12 18)
  , NationalPark "Carlsbad Caverns" ["New Mexico"] (fromGregorian 1930 05 14)
  , NationalPark "Channel Islands" ["California"] (fromGregorian 1980 03 05)
  , NationalPark "Congaree" ["South Carolina"] (fromGregorian 2003 11 10)
  , NationalPark "Crater Lake" ["Oregon"] (fromGregorian 1902 05 22)
  , NationalPark "Cuyahoga Valley" ["Ohio"] (fromGregorian 2000 10 11)
  , NationalPark "Death Valley" ["California"] (fromGregorian 1994 10 31)
  , NationalPark "Denali" ["Alaska"] (fromGregorian 1917 02 26)
  , NationalPark "Dry Tortugas" ["Florida"] (fromGregorian 1992 10 26)
  , NationalPark "Everglades" ["Florida"] (fromGregorian 1934 05 30)
  , NationalPark "Gates of the Arctic" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Gateway Arch" ["Missouri"] (fromGregorian 2018 02 22)
  , NationalPark "Glacier" ["Montana"] (fromGregorian 1910 05 11)
  , NationalPark "Glacier Bay" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Grand Canyon" ["Arizona"] (fromGregorian 1919 02 26)
  , NationalPark "Grand Teton" ["Wyoming"] (fromGregorian 1929 02 26)
  , NationalPark "Great Basin" ["Nevada"] (fromGregorian 1986 10 27)
  , NationalPark "Great Sand Dunes" ["Colorado"] (fromGregorian 2004 09 13)
  , NationalPark
      "Great Smoky Mountains"
      ["Tennessee", "North Carolina"]
      (fromGregorian 1934 06 15)
  , NationalPark "Guadalupe Mountains" ["Texas"] (fromGregorian 1966 10 15)
  , NationalPark "Haleakalā" ["Hawaii"] (fromGregorian 1916 08 01)
  , NationalPark "Hawaiʻi Volcanoes" ["Hawaii"] (fromGregorian 1916 08 01)
  , NationalPark "Hot Springs" ["Arkansas"] (fromGregorian 1921 03 04)
  , NationalPark "Indiana Dunes" ["Indiana"] (fromGregorian 2019 02 15)
  , NationalPark "Isle Royale" ["Michigan"] (fromGregorian 1940 04 03)
  , NationalPark "Joshua Tree" ["California"] (fromGregorian 1994 10 31)
  , NationalPark "Katmai" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Kenai Fjords" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Kings Canyon" ["California"] (fromGregorian 1940 03 04)
  , NationalPark "Kobuk Valley" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Lake Clark" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Lassen Volcanic" ["California"] (fromGregorian 1916 08 09)
  , NationalPark "Mammoth Cave" ["Kentucky"] (fromGregorian 1941 07 01)
  , NationalPark "Mesa Verde" ["Colorado"] (fromGregorian 1906 06 29)
  , NationalPark "Mount Rainier" ["Washington"] (fromGregorian 1899 03 02)
  , NationalPark "North Cascades" ["Washington"] (fromGregorian 1968 10 02)
  , NationalPark "Olympic" ["Washington"] (fromGregorian 1938 06 29)
  , NationalPark "Petrified Forest" ["Arizona"] (fromGregorian 1962 12 09)
  , NationalPark "Pinnacles" ["California"] (fromGregorian 2013 01 10)
  , NationalPark "Redwood" ["California"] (fromGregorian 1968 10 02)
  , NationalPark "Rocky Mountain" ["Colorado"] (fromGregorian 1915 01 26)
  , NationalPark "Saguaro" ["Arizona"] (fromGregorian 1994 10 14)
  , NationalPark "Sequoia" ["California"] (fromGregorian 1890 09 25)
  , NationalPark "Shenandoah" ["Virginia"] (fromGregorian 1935 12 26)
  , NationalPark
      "Theodore Roosevelt"
      ["North Dakota"]
      (fromGregorian 1978 11 10)
  , NationalPark
      "Virgin Islands"
      ["United States Virgin Islands"]
      (fromGregorian 1956 08 02)
  , NationalPark "Voyageurs" ["Minnesota"] (fromGregorian 1971 01 08)
  , NationalPark "Wind Cave" ["South Dakota"] (fromGregorian 1903 01 09)
  , NationalPark "Wrangell-St. Elias" ["Alaska"] (fromGregorian 1980 12 02)
  , NationalPark "Yellowstone" ["Wyoming"] (fromGregorian 1872 03 01)
  , NationalPark "Yosemite" ["California"] (fromGregorian 1890 10 01)
  , NationalPark "Zion" ["Utah"] (fromGregorian 1919 11 19)
  ]

filterByLocation :: String -> NationalParks -> NationalParks
filterByLocation l = filter (\p -> l `elem` location p)

filterByEstablishedBefore :: Day -> NationalParks -> NationalParks
filterByEstablishedBefore d = filter (\p -> established p < d)

filterByEstablishedAfter :: Day -> NationalParks -> NationalParks
filterByEstablishedAfter d = filter (\p -> established p > d)

someFunc :: IO ()
someFunc = print allNationalParks
