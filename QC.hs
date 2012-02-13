import Test.QuickCheck (Arbitrary(..), Gen, choose, (==>), Property)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework
import Data.Bits
import Data.Monoid

import Types

instance Arbitrary Int12 where
  arbitrary = fmap fromIntegral (choose (-2^11,2^11-1) :: Gen Int)

pXOR :: Int12 -> Int12 -> Bool
pXOR a b = (fromIntegral (a `xor` b)) ==
           (fromIntegral a :: Int) `xor` (fromIntegral b)

pAND :: Int12 -> Int12 -> Bool
pAND a b = (fromIntegral (a .&. b)) ==
           (fromIntegral a :: Int) .&. (fromIntegral b)

pOR :: Int12 -> Int12 -> Bool
pOR a b = (fromIntegral (a .|. b)) ==
          (fromIntegral a :: Int) .|. (fromIntegral b)

isBounded :: (Int12 -> Int12 -> Int12) -> Int12 -> Int12 -> Bool
isBounded op a b = let x = a `op` b in x >= minBound && x <= maxBound

bPlus  = isBounded (+)
bMinus = isBounded (-)
bMult  = isBounded (*)

bDiv :: Int12 -> Int12 -> Property
bDiv a b = b /= 0 ==> isBounded div a b

bAbs i = let x = abs i :: Int12 in x >= minBound && x <= maxBound
bFromInteger i = let x = fromInteger i :: Int12
                 in x >= minBound && x <= maxBound

testInt12 :: Test
testInt12 = testGroup "Int12"
  [ testGroup "Bit Instance"
     [ testProperty ".|." pOR
     , testProperty ".&." pAND
     , testProperty "xor" pXOR
     ]
  , testGroup "In Bounds"
     [ testProperty "b +" bPlus
     , testProperty "b -" bMinus
     , testProperty "b *" bMult
     , testProperty "b /" bDiv
     , testProperty "b abs" bAbs
     , testProperty "b fi" bFromInteger
     ]
  ]

tests = [testInt12]

main = defaultMainWithOpts tests myCfg

myCfg  = mempty { ropt_test_options = Just myOpts }

myOpts = mempty { topt_maximum_generated_tests = Just 3000 }
