{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Test.Hspec
import Data.Either (isRight)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Lib (parseBS, RawLine)

main :: IO ()
main = hspec do
  describe "Basic behavior" do

    it "parses a single line" do
      let input = "1\n00:00:03,001 --> 00:00:05,073\nMy mother died today. Or maybe yesterday,\n\n"
      parseBS @RawLine input `shouldSatisfy` isRight

    it "parses more than one line" do
      let
        line1 = "1\n00:00:03,001 --> 00:00:05,073\nMy mother died today.\n\n"
        line2 = "2\n00:00:06,004 --> 00:00:10,145\nOr maybe yesterday,\n\n"
      parseBS @RawLine (BS.concat [line1, line2]) `shouldSatisfy` isRight
