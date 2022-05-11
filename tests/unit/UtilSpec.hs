-- Copyright 2022 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# OPTIONS_GHC -Wno-orphans #-}

module UtilSpec (spec) where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as At
import Text.Blaze.Html.Renderer.String
import Test.Hspec
import RenderHtml
import Util qualified as U
import Debug.Trace

--   [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
-- a: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- b:    ~~~~~~~~~~~~~
-- c:    ~~~~
-- d:             ~~~~
-- e:                      ~~~~~~~

xs :: String
-- xs = ['a' .. 'z']
xs = "0123456789!"

spanInfos :: [U.SpanInfo]
spanInfos =
  [ (0, 10, 0)
  , (1, 5, 1)
  , (1, 2, 2)
  -- , (3, 3, 3)
  , (4, 5, 3)
  -- , (6, 6, 0)
  , (7, 10, 4)
  ]

posTree :: U.SpanTree ()
posTree = U.Span (0, 10, 0) [U.Span (0, 5, 1) [U.Leaf (0, 2, 2) (), U.Leaf (4, 5, 3) ()], U.Leaf (7, 10, 4) ()]

positionSpanTree :: U.SpanTree ()
positionSpanTree =
  U.Span (0, 10, 0) [
    U.Trivia (0, 1) (),
    U.Span (1, 5, 1) [
      U.Leaf (1, 2, 2) (),
      U.Leaf (4, 5, 3) ()
    ],
    U.Leaf (7, 10, 4) ()
  ]

positionFilledSpanTree :: U.SpanTree ()
positionFilledSpanTree =
  U.Span (0, 10, 0) [
    U.Trivia (0, 1) (),
    U.Span (1, 5, 1) [
      U.Leaf (1, 2, 2) (),
      U.Trivia (2, 4) (),
      U.Leaf (4, 5, 3) ()
    ],
    U.Trivia (5, 7) (),
    U.Leaf (7, 10, 4) ()
  ]

contentSpanTree :: U.SpanTree String
contentSpanTree =
  U.Span (0, 10, 0) [
    U.Trivia (0, 1) "0",
    U.Span (1, 5, 1) [
      U.Leaf (1, 2, 2) "1",
      U.Trivia (2, 4) "23",
      U.Leaf (4, 5, 3) "4"
    ],
    U.Trivia (5, 7) "56",
    U.Leaf (7, 10, 4) "789!"
  ]

spec :: Spec
spec = do
  describe "SpanTreeTest" do
    it "works" do
      -- U.fillTreeAndAddTrivialLeaves xs positionSpanTree `shouldBe` contentSpanTree
      U.fillTreeAndAddTrivialLeaves xs positionSpanTree `shouldBe` positionFilledSpanTree

    -- it "works 2" do
    --   -- U.runSpanTree (return 1) spanInfos `shouldBe` (1 :: Int)
    --   U.evalSpanTree (U.makeSpanTreeRec $ head spanInfos) (tail spanInfos) `shouldBe` Just positionSpanTree

    it "works 3" do
      -- U.runSpanTree (return 1) spanInfos `shouldBe` (1 :: Int)
      -- U.makeSpanTree xs spanInfos `shouldBe` Just contentSpanTree
      U.makeSpanTree xs spanInfos `shouldBe` Just positionFilledSpanTree

    it "works 4" do
      let html = treeToHtml' xs positionFilledSpanTree in
        trace ("Html: " ++ renderHtml html)
        -- renderHtml html `shouldBe` "<span id=\"0\"><span>0</span><span id=\"1\"><span id=\"2\">1</span><span>3</span><span id=\"3\">4</span></span><span>6</span><span id=\"4\">789</span></span>"
        -- renderHtml html `shouldBe` "<span id=\"0\"><span>0</span><span id=\"1\"><span id=\"2\">1</span><span>3</span><span id=\"3\">4</span><span>56</span></span><span id=\"4\">789</span></span>"
        renderHtml html `shouldBe` "<span id=\"0\"><span>0</span><span id=\"1\"><span id=\"2\" class=\"code-span\">1</span><span>23</span><span id=\"3\" class=\"code-span\">4</span></span><span>56</span><span id=\"4\" class=\"code-span\">789</span></span>"
      -- treeToHtml' xs positionFilledSpanTree `shouldBe` H.span (H.span ) ! At.id "0"
