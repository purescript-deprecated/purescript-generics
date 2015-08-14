module Test.UnitTests
  (unitTests) where

import Prelude
import Test.Unit
import Data.Either
import Data.Generic

data Color = Red | Green | Blue
derive instance genericColor :: Generic Color

data Colorish = Redish Int | Greenish Int | Blueish Int
derive instance genericColorish :: Generic Colorish

data RGB = RGB { red   :: Int
               , green :: Int
               , blue  :: Int
               }
derive instance genericRGB :: Generic RGB

grayscale :: Int -> RGB
grayscale brightness = RGB { red: brightness
                           , green: brightness
                           , blue: brightness
                           }

data MyUnit = MyUnit
derive instance genericMyUnit :: Generic MyUnit

unitTests = runTest do
  test "Red | Green | Blue" do
    assert "Bottom is Red" $ eitherGBottom `gEq` Right Red
    assert "Top is Blue" $ eitherGTop `gEq` Right Blue

  test "Redish Int | Greenish Int | Blueish Int" do
    assert "Bottom is Redish bottom" $ eitherGBottom `gEq` Right (Redish bottom)
    assert "Top is Blueish top" $ eitherGTop `gEq` Right (Blueish top)

  test "RGB { red :: Int, green :: Int, blue :: Int }" do
    assert "Bottom is black" $ eitherGBottom `gEq` Right (grayscale bottom)
    assert "Top is white" $ eitherGTop `gEq` Right (grayscale top)

  test "MyUnit" do
    assert "bottom is MyUnit" $ eitherGBottom `gEq` Right MyUnit
    assert "top is MyUnit" $ eitherGBottom `gEq` Right MyUnit

  test "String is not Bounded" do
    assert "by bottom" $ isLeft (eitherGBottom :: _ String)
    assert "by top" $ isLeft (eitherGTop :: _ String)

  test "Array is not Bounded" do
    assert "by bottom" $ isLeft (eitherGBottom :: _ (Array Int))
    assert "by top" $ isLeft (eitherGTop :: _ (Array Int))

  test "Number is not Bounded" do
    assert "by bottom" $ isLeft (eitherGBottom :: _ Number)
    assert "by top" $ isLeft (eitherGTop :: _ Number)
