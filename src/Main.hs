module Main where

import Prelude ()
import MyPrelude

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Codec.Picture.Types as IM
import qualified Codec.Picture.Png as IM

import System.Environment

import Options.Applicative

import qualified DNA as DNA
import qualified RNA as RNA

import Control.DeepSeq

data Options = Options
               { _dnaInput   :: Either String String
               , _dnaPrefix  :: Either String String
               , _outputFile :: String
               }
makeLenses ''Options

options :: Parser Options
options = Options
          <$> (Left <$> strOption
               ( long "dna"
                 <> short 'd'
                 <> metavar "DNA"
                 <> help "DNA input file" )
               <|> (Right <$> strOption
                    ( long "dnastring"
                    <> metavar "DNASTRING"
                    <> help "DNA input string" )))
          <*> (Left <$> strOption
               ( long "prefix"
                 <> short 'p'
                 <> metavar "DNAPREFIX"
                 <> help "DNA prefix file" )
               <|> (Right <$> strOption
                    ( long "prefixstring"
                    <> metavar "DNAPREFIXSTRING"
                    <> help "DNA prefix string" )))
          <*> strOption
          ( long "output"
            <> short 'o'
            <> help "Output file" )

writeImg :: String -> Vector (Vector RNA.Pixel) -> IO ()
writeImg f img = deepseq img $ do
  BL.writeFile f $ IM.encodePng $
    IM.generateImage (\i j -> let RNA.Pixel r g b a = img V.! i V.! j
                              in IM.PixelRGBA8 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
                     ) 600 600

main :: IO ()
main = do
  options <- execParser $ info (helper <*> options)
             (fullDesc
              <> progDesc "ICFP 2007 !"
              <> header "Test Header" )
  dna     <- either readFile pure (options ^. dnaInput)
  prefix  <- either readFile pure (options ^. dnaPrefix)
  rna     <- DNA.runParser (DNA.readBase <$> (prefix ++ dna)) DNA.execute
  (v, vs) <- rna `deepseq` RNA.runR rna
  forM_ (zip [0..] vs) $ \(i, img) -> do
    writeImg (options ^. outputFile ++ show i ++ ".png") img
  writeImg (options ^. outputFile ++ ".png") v
