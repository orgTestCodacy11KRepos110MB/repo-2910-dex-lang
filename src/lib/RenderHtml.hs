-- Copyright 2019 Google LLC
--
-- Use of this source code is governed by a BSD-style
-- license that can be found in the LICENSE file or at
-- https://developers.google.com/open-source/licenses/bsd

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RenderHtml (pprintHtml, progHtml, ToMarkup, treeToHtml') where

import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes as At
import Text.Blaze.Html.Renderer.String
import Data.List    qualified as L
import Data.Text    qualified as T
import Data.Text.IO qualified as T
import CMark (commonmarkToHtml)
import System.IO.Unsafe

import Control.Monad
import Debug.Trace
import Text.Megaparsec hiding (chunk)
import Text.Megaparsec.Char as C

import Paths_dex  (getDataFileName)
import Syntax
import PPrint
import Parser
import Serialize ()
import TraverseContexts
import Err
import Util
import GHC.Exts (fromString)

cssSource :: T.Text
cssSource = unsafePerformIO $
  T.readFile =<< getDataFileName "static/style.css"
{-# NOINLINE cssSource #-}

javascriptSource :: T.Text
javascriptSource = unsafePerformIO $
  T.readFile =<< getDataFileName "static/index.js"
{-# NOINLINE javascriptSource #-}

pprintHtml :: ToMarkup a => a -> String
pprintHtml x = renderHtml $ toMarkup x

progHtml :: (ToMarkup a, ToMarkup b) => [(a, b)] -> String
progHtml blocks = renderHtml $ wrapBody $ map toHtmlBlock blocks
  where toHtmlBlock (block,result) = toMarkup block <> toMarkup result

wrapBody :: [Html] -> Html
wrapBody blocks = docTypeHtml $ do
  H.head $ do
    H.meta ! charset "UTF-8"
    -- Base CSS stylesheet.
    H.style ! type_ "text/css" $ toHtml cssSource
    -- KaTeX CSS and JavaScript.
    H.link ! rel "stylesheet" ! href "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.css"
    H.script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/katex.min.js" $ mempty
    H.script ! defer "" ! src "https://cdn.jsdelivr.net/npm/katex@0.12.0/dist/contrib/auto-render.min.js"
             ! onload jsSource $ mempty
  H.body $ H.div inner ! At.id "main-output"
  where
    inner = foldMap (cdiv "cell") blocks
    jsSource = textValue $ javascriptSource <> "render(RENDER_MODE.STATIC);"

instance ToMarkup Result where
  toMarkup (Result outs err _) = foldMap toMarkup outs <> err'
    where err' = case err of
                   Failure e  -> cdiv "err-block" $ toHtml $ pprint e
                   Success () -> mempty

instance ToMarkup Output where
  toMarkup out = case out of
    HtmlOut s -> preEscapedString s
    _ -> cdiv "result-block" $ toHtml $ pprint out

instance ToMarkup SourceBlock where
  toMarkup block = case sbContents block of
    ProseBlock s -> cdiv "prose-block" $ mdToHtml s
    -- NOTE: Add more cases here.
    -- Options:
    -- 1. Faithfully print UDecl, requires storing whitespace and trivia.
    -- 2. Pass UDecl and string. We will do this.
    EvalUDecl udecl -> renderUDecl udecl block
    Command _ uexpr -> renderUExpr uexpr block
    _ -> cdiv "code-block" $ highlightSyntax (sbText block)

mdToHtml :: T.Text -> Html
mdToHtml s = preEscapedText $ commonmarkToHtml [] s

cdiv :: String -> Html -> Html
cdiv c inner = H.div inner ! class_ (stringValue c)

-- === syntax highlighting ===

spanDelimitedCode :: SourceBlock -> [SrcPosCtx] -> Html
spanDelimitedCode block ctxs =
  let (Just tree) = srcCtxsToTree block ctxs in
  spanDelimitedCode' block tree

spanDelimitedCode' :: SourceBlock -> SpanTree () -> Html
spanDelimitedCode' block tree =
  let source' = sbText block in
  treeToHtml' source' tree

{-
treeToHtml :: SourceBlock -> SpanTree () -> Html
treeToHtml block = treeToHtml' (sbText block)
-}

treeToHtml' :: T.Text -> SpanTree () -> Html
treeToHtml' source' tree =
  -- case tree of
  let tree' = fillTreeAndAddTrivialLeaves (T.unpack source') tree in
  trace ("Tree: " ++ show tree')
  treeToHtml'' source' tree'

treeToHtml'' :: T.Text -> SpanTree a -> Html
treeToHtml'' source' tree = case tree of
  Span (_, _, spanId) children ->
    let body' = foldMap (treeToHtml'' source') children in
    createSpan (Just spanId) body' ! spanClass
  Leaf (l, r, spanId) _ ->
    let spanText = sliceText l r source' in
    -- Note: only leaves need the code-span class.
    createSpan (Just spanId) (highlightSyntax spanText) ! spanLeaf
    -- createSpan (Just spanId) (toHtml spanText)
  Trivia (l, r) _ ->
    let spanText = sliceText l r source' in
    createSpan Nothing (highlightSyntax spanText)
    -- createSpan Nothing (toHtml spanText)
  where createSpan :: Maybe SpanId -> Html -> Html
        createSpan spanId body' = case spanId of
          Nothing -> H.span body'
          Just id' -> H.span body' ! spanIdAttribute id'
        spanIdAttribute :: SpanId -> Attribute
        spanIdAttribute spanId =
          At.id (fromString (show spanId))
        spanLeaf :: Attribute
        spanLeaf = At.class_ "code-span-leaf"
        spanClass :: Attribute
        spanClass = At.class_ "code-span"

srcCtxsToSpanInfos :: SourceBlock -> [SrcPosCtx] -> [SpanPayload]
srcCtxsToSpanInfos block ctxs =
  let blockOffset = sbOffset block in
  let ctxs' = L.sort ctxs in
  -- map (convert blockOffset) ctxs
  (0, maxBound, -1) : mapMaybe (convert' blockOffset) ctxs'
  where convert' :: Int -> SrcPosCtx -> Maybe SpanPayload
        convert' offset (SrcPosCtx (Just (l, r)) (Just spanId) _) = Just (l - offset, r - offset, spanId)
        convert' _ _ = Nothing

srcCtxsToTree :: SourceBlock -> [SrcPosCtx] -> Maybe (SpanTree ())
srcCtxsToTree block ctxs =
  let infos = srcCtxsToSpanInfos block ctxs in
  -- TODO: Use `makeSpanTree`? Need to update result type
  makeEmptySpanTree $ trace ("[SpanInfo]:\n" ++ stringifyList infos) infos
  -- makeSpanTree $ trace ("[SpanInfo]:\n" ++ stringifyList infos) infos

stringifyList :: Show a => [a] -> String
stringifyList xs = L.intercalate "\n" (map show xs)

renderUDecl :: UDecl n n' -> SourceBlock -> Html
renderUDecl udecl block =
  let source' = sbText block in
  let udecl' = addContextIds udecl in
  let ctxs = gatherContexts udecl' in
  -- TODO(danielzheng): Ensure that no two SrcPosCtx have overlapping spans.
  -- TODO(danielzheng): How to map spans to html source range?
  -- toHtml $ cdiv "code-block" $ highlightSyntax source
  -- toHtml $ cdiv "code-block" $ trace ("[SrcPosCtx]: " ++ show ctxs) highlightSyntax source
  -- toHtml $ cdiv "code-block" $ trace ("[SrcPosCtx]:\n" ++ show (sbOffset block) ++ "\n" ++ source' ++ "\n" ++ stringifyList ctxs ++ "\n" ++ stringifyList (contextsToSpanEndpoints ctxs)) highlightSyntax source'
  toHtml $ cdiv "code-block" $ trace (
    "[SrcPosCtx]:\n" ++ show (sbOffset block) ++ "\n" ++ T.unpack source' ++ "\n" ++
    stringifyList ctxs
    ++ "\n" -- ++ renderHtml (spanDelimitedCode block ctxs)
  )
  -- highlightSyntax source'
  spanDelimitedCode block ctxs

renderUExpr :: UExpr n -> SourceBlock -> Html
renderUExpr uexpr block =
  let source' = sbText block in
  let uexpr' = addContextIds uexpr in
  let ctxs = gatherContexts uexpr' in
  -- toHtml $ cdiv "code-block" $ highlightSyntax source
  -- toHtml $ cdiv "code-block" $ trace ("[SrcPosCtx]: " ++ show ctxs) highlightSyntax source
  -- toHtml $ cdiv "code-block" $ trace ("[SrcPosCtx]:\n" ++ L.intercalate "\n" (map show ctxs)) highlightSyntax source
  toHtml $ cdiv "code-block" $ trace (
    "[SrcPosCtx]:\n" ++ show (sbOffset block) ++ "\n" ++ T.unpack source' ++ "\n" ++
    stringifyList ctxs
    ++ "\n" -- ++ renderHtml (spanDelimitedCode block ctxs)
  )
  -- highlightSyntax source'
  spanDelimitedCode block ctxs
  -- 1. We have `source`
  -- 2. We have source ranges from UDecl
  --    - Expressions within UDecl (e.g. instance methods) should have annotated
  --      SrcPos at this point.
  --    - May want a function that traverses UDecl to collect all annotations
  -- 3. Produce HTML

-- -- TODO: This needs to be beefed up to accept annotation data.
-- -- NOTE: This is called per block.
-- highlightSyntax' :: (String, UExpr) -> Html
-- highlightSyntax' s = foldMap (uncurry syntaxSpan) classified
--   where classified = ignoreExcept $ parseit s (many (withSource classify) <* eof)

highlightSyntax :: T.Text -> Html
highlightSyntax s = foldMap (uncurry syntaxSpan) classified
  where classified = ignoreExcept $ parseit s (many (withSource classify) <* eof)

syntaxSpan :: T.Text -> StrClass -> Html
syntaxSpan s NormalStr = toHtml s
syntaxSpan s c = H.span (toHtml s) ! class_ (stringValue className)
  where
    className = case c of
      CommentStr  -> "comment"
      KeywordStr  -> "keyword"
      CommandStr  -> "command"
      SymbolStr   -> "symbol"
      TypeNameStr -> "type-name"
      IsoSugarStr -> "iso-sugar"
      WhitespaceStr -> "whitespace"
      NormalStr -> error "Should have been matched already"

data StrClass = NormalStr
              | CommentStr | KeywordStr | CommandStr | SymbolStr | TypeNameStr
              | IsoSugarStr | WhitespaceStr

classify :: Parser StrClass
classify =
       (try (char ':' >> lowerWord) >> return CommandStr)
   <|> (symbol "-- " >> manyTill anySingle (void eol <|> eof) >> return CommentStr)
   <|> (do s <- lowerWord
           return $ if s `elem` keyWordStrs then KeywordStr else NormalStr)
   <|> (upperWord >> return TypeNameStr)
   <|> try (char '#' >> (char '?' <|> char '&' <|> char '|' <|> pure ' ')
        >> lowerWord >> return IsoSugarStr)
   <|> (some symChar >> return SymbolStr)
   <|> (some space1 >> return WhitespaceStr)
   <|> (anySingle >> return NormalStr)

lowerWord :: Parser String
lowerWord = (:) <$> lowerChar <*> many alphaNumChar

upperWord :: Parser String
upperWord = (:) <$> upperChar <*> many alphaNumChar
