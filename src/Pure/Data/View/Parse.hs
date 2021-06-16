{-# language CPP, ViewPatterns, ScopedTypeVariables, OverloadedStrings #-}
module Pure.Data.View.Parse 
  ( parseDocumentFragment
  , parse
  ) where

import Pure.Data.Lifted
import Pure.Data.Txt as Txt
import Pure.Data.View
import Pure.Data.View.Patterns

import Data.Traversable
import qualified Data.List as List

#ifdef __GHCJS__
import GHCJS.Marshal (FromJSVal(..))
#endif

parseDocumentFragment :: Node -> IO [View]
parseDocumentFragment (coerce -> n)
  | Just (11 :: Int) <- n .# "nodeType" = do
    mcs :: Maybe [JSV] <- n ..# "childNodes"
    case mcs of
      Just cs -> traverse (parse . coerce) cs
      _       -> pure []

  | otherwise = 
    pure []

-- | Parse a live DOM node into a View. Parses classes, styles, data attributes,
-- tags and text content only. Anything other than element and text nodes are 
-- parsed to Null views.
parse :: Node -> IO View
parse node@(coerce -> n) = 
  case n .# "nodeType" of
    Just (3 :: Int) | Just cnt <- n .# "textContent" -> 
      pure (fromTxt cnt)
    
    Just 1 -> do
      mcs :: Maybe [JSV] <- n ..# "childNodes"
      case mcs of
        Just cs 
          | Just (t :: Txt)     <- n .# "tagName"
          , mss :: Maybe [Txt]  <- fmap (Txt.splitOn ";") (n .# "styles")
          , mcs :: Maybe [Txt]  <- fmap Txt.words (n .# "className")
          -> do
            das :: [(Txt,Txt)]  <- entries node
            let 
              styles
                | Just ss <- mss = 
                  let 
                    style (Txt.splitOn ":" -> [k,v]) = Style k v
                    style _ = id
                  in
                    flip (List.foldr style) ss 

                | otherwise = id

              classes 
                | Just cls <- mcs = flip (List.foldr Class) cls
                | otherwise       = id

              dataattrs = flip (List.foldr (uncurry Attribute)) das
              
            cs' <- traverse (parse . coerce) cs

            let 
              wrap 
                | n .# "namespaceURI" == Just ("http://www.w3.org/2000/svg" :: Txt) = SimpleSVG
                | otherwise = SimpleHTML

            pure $ 
              wrap (Txt.toLower t) <| styles . classes . dataattrs |> 
                cs'

        _ -> pure Null

    _ -> pure Null

#ifdef __GHCJS__
foreign import javascript unsafe
  "var d = $1.dataset; var o = {}; o['keys'] = Object.keys(d); o['values'] = Object.values(d); $r = o;"
    data_entries_js :: Node -> IO JSV
#endif

entries :: Node -> IO [(Txt,Txt)]
entries node = do
#ifdef __GHCJS__
  o <- data_entries_js node
  case (,) <$> o .# "keys" <*> o .# "values" of
    Just (ks,vs) -> do
      Just keys   <- fromJSValListOf ks
      Just values <- fromJSValListOf vs
      pure (List.zip keys values)
    _ -> 
      pure []
#else
  pure []
#endif
