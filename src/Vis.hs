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
              deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

visLine ∷ String → Vis ()
visLine s = Vis $ ReaderT $ \h → do
  hPutStrLn h $ renderHtml
    $ h4 (fromString s)
  hPutStrLn h $ renderHtml br
  hFlush h

visDiagram ∷ D.Diagram D.SVG → Vis ()
visDiagram d = Vis $ ReaderT $ \h → do
  BL.hPutStr h $ renderBS $ D.renderDia SVG (SVGOptions (D.mkSizeSpec $ Just . (* 60) <$> D.size d) Nothing "") d
  hPutStrLn h $ renderHtml br
  hFlush h

visChart :: (C.ToRenderable a, C.Default a) => C.EC a b -> Vis ()
visChart c = Vis $ ReaderT $ \h -> do
  BL.hPutStr h . fst =<< C.renderableToSVGString (C.toRenderable (C.execEC c)) 400 400
  hPutStrLn h $ renderHtml br
  hFlush h

visProgress :: String -> Vis ()
visProgress s = liftIO $ do
  hPutChar stderr '\r'
  hPutStr stderr s

runVis :: FilePath -> Vis a -> IO a
runVis p (Vis v) = do
  f <- getDirectoryContents p
       <&> filter (all isDigit)
       <&> fmap read <&> maximumOf folded <&> maybe (0 :: Integer) (+ 1)
  createDirectory (p ++ "/" ++ show f)
  let file = p ++ "/" ++ show f ++ "/index.html"
  hPutStrLn stderr $ "Vis file : " ++ file
  withFile file WriteMode $ \h -> do
    hPutStrLn h "<meta charset=\"utf-8\">"
    hPutStrLn h "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    hPutStrLn h "<link rel=\"stylesheet\" href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css\">"
    hPutStrLn h "<div class=\"container\">"
    hPutStrLn h $ renderHtml $ h1 $ fromString ("Visualisation #" ++ show f)
    when (f /= 0) (hPutStrLn h $ "<a href=\"../" ++ show (f-1) ++ "/index.html" ++ "\">Previous</a>")
    hPutStrLn h $ "<a href=\"../" ++ show (f+1) ++ "/index.html" ++ "\">Next</a>"
    hPutStrLn h $ renderHtml br
    hFlush h
    runReaderT v h
