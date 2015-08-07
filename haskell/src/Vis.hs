{-# LANGUAGE OverloadedStrings #-}
module Vis where

import Prelude ()
import MyPrelude

import qualified Data.ByteString.Lazy as BL

import Data.String
import Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String
import Diagrams.Prelude as D
import Diagrams.Backend.SVG as D
import qualified Graphics.Rendering.Chart.Renderable as C
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Graphics.Rendering.Chart.Backend.Diagrams as C

import System.Directory
import Lucid.Svg

newtype Vis a = Vis { unVis :: ReaderT Handle IO a }
              deriving (Functor, Applicative, Monad, MonadIO)

visLine ∷ String → Vis ()
visLine s = Vis $ ReaderT $ \h → do
  hPutStrLn h $ renderHtml
    $ h4 (fromString s)
  hPutStrLn h $ renderHtml br

visDiagram ∷ D.Diagram D.SVG → Vis ()
visDiagram d = Vis $ ReaderT $ \h → do
  BL.hPutStr h $ renderBS $ D.renderDia SVG (SVGOptions (mkWidth 400) Nothing "") d
  hPutStrLn h $ renderHtml br

visChart :: (C.ToRenderable a, C.Default a) => C.EC a b -> Vis ()
visChart c = Vis $ ReaderT $ \h -> do
  BL.hPutStr h . fst =<< C.renderableToSVGString (C.toRenderable (C.execEC c)) 400 400
  hPutStrLn h $ renderHtml br

runVis :: FilePath -> Vis a -> IO a
runVis p (Vis v) = do
  f <- getDirectoryContents p
       <&> filter (all isDigit)
       <&> fmap read <&> maximumOf folded <&> maybe (0 :: Integer) (+ 1) <&> show
  createDirectory (p ++ "/" ++ f)
  let file = p ++ "/" ++ f ++ "/index.html"
  hPutStrLn stderr $ "Vis file : " ++ file
  withFile file WriteMode $ \h -> do
    hPutStrLn h "<meta charset=\"utf-8\">"
    hPutStrLn h "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    hPutStrLn h "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css\">"
    hPutStrLn h "<div class=\"container\">"
    runReaderT v h
