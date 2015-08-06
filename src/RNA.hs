module RNA where

import Prelude ()
import MyPrelude

import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import DNA

data Pixel = Pixel !Int !Int !Int !Int
           deriving (Eq, Generic)
instance NFData Pixel where
  rnf = genericRnf
type Pos = (Int, Int)
type Bitmap = Vector (IOVector Pixel)
data Color = RGB !Int !Int !Int | Trans !Int
data Bucket = Bucket !Int !Int !Int !Int !Int !Int
data Dir = N | S | W | E

data RNAState = RNAState
                  { _bucket :: Bucket
                  , _pos :: Pos
                  , _mark :: Pos
                  , _dir :: Dir
                  , _bitmaps :: [Bitmap]
                  }
emptyState :: RNAState
emptyState = RNAState
             (Bucket 0 0 0 0 0 0)
             (0, 0)
             (0, 0)
             E
             []
makeLenses ''RNAState

type R a = StateT RNAState (WriterT [Vector (Vector Pixel)] IO) a

build :: [Base] -> R ()
build [P,I,P,I,I,I,C] = bucket %= addColor (RGB 0 0 0)
build [P,I,P,I,I,I,P] = bucket %= addColor (RGB 255 0 0)
build [P,I,P,I,I,C,C] = bucket %= addColor (RGB 0 255 0)
build [P,I,P,I,I,C,F] = bucket %= addColor (RGB 255 255 0)
build [P,I,P,I,I,C,P] = bucket %= addColor (RGB 0 0 255)
build [P,I,P,I,I,F,C] = bucket %= addColor (RGB 255 0 255)
build [P,I,P,I,I,F,F] = bucket %= addColor (RGB 0 255 255)
build [P,I,P,I,I,P,C] = bucket %= addColor (RGB 255 255 255)
build [P,I,P,I,I,P,F] = bucket %= addColor (Trans 0)
build [P,I,P,I,I,P,P] = bucket %= addColor (Trans 255)
build [P,I,I,P,I,C,P] = bucket .= Bucket 0 0 0 0 0 0
build [P,I,I,I,I,I,P] = do d <- use dir; pos %= move d
build [P,C,C,C,C,C,P] = dir %= turn1
build [P,F,F,F,F,F,P] = dir %= turn2
build [P,C,C,I,F,F,P] = mark <~ use pos
build [P,F,F,I,C,C,P] = do p <- use pos; m <- use mark; line p m
build [P,I,I,P,I,I,P] = tryfill
build [P,C,C,P,F,F,P] = addBitmap
build [P,F,F,P,C,C,P] = compose
build [P,F,F,I,C,C,F] = clip
build a = pure ()

addBitmap :: R ()
addBitmap = do
  v <- lift $ V.generateM 600 (const $ VM.replicate 600 (Pixel 0 0 0 0))
  bs <- use bitmaps
  when (length bs < 10) (bitmaps .= v : bs)

compose, clip :: R ()
compose = use bitmaps >>= \case
  (x:y:_) -> do
    bitmaps %= tail
    forM_ [0..599] $ \i -> seq i $
      forM_ [0..599] $ \j -> seq j $ do
        Pixel r0 g0 b0 a0 <- VM.read (x V.! i) j
        Pixel r1 g1 b1 a1 <- VM.read (y V.! i) j
        let f a b = a + (b*(255-a0))`div`255
        let r2 = f r0 r1
            g2 = f g0 g1
            b2 = f b0 b1
            a2 = f a0 a1
        VM.write (y V.! i) j (Pixel r2 g2 b2 r2)
  _ -> pure ()
clip = use bitmaps >>= \case
  (x:y:_) -> do
    bitmaps %= tail
    forM_ [0..599] $ \i -> seq i $
      forM_ [0..599] $ \j -> seq j $ do
        Pixel r0 g0 b0 a0 <- VM.read (x V.! i) j
        Pixel r1 g1 b1 a1 <- VM.read (y V.! i) j
        let f a b = (b * a0) `div` 255
        VM.write (y V.! i) j (Pixel (f r0 r1) (f g0 g1) (f b0 b1) (f a0 a1))
  _ -> pure ()

line :: Pos -> Pos -> R ()
line (x0, y0) (x1, y1) = do
  p <- currentPixel
  let deltax = x1-x0
      deltay = y1-y0
      d = max (abs deltax) (abs deltay)
      c = if deltax*deltay <= 0 then 1 else 0
      x = x0*d+(d-c)`div`2
      y = y0*d+(d-c)`div`2
  forM_ [0..d-1] $ \i -> setPixel ((x+i*deltax)`div`d, (y+i*deltay)`div` d) p
  setPixel (x1, y1) p

tryfill :: R ()
tryfill = do
  new <- currentPixel
  old <- getPixel =<< use pos
  when (new /= old) (fill new old =<< use pos)

fill :: Pixel -> Pixel -> Pos -> R ()
fill new old (x, y) = do
  a <- getPixel (x, y)
  when (a == old) $ do
    setPixel (x, y) new
    when (x > 0) (fill new old (x-1, y))
    when (x < 599) (fill new old (x+1, y))
    when (y > 0) (fill new old (x, y-1))
    when (y < 599) (fill new old (x, y+1))

addColor :: Color -> Bucket -> Bucket
addColor (RGB x y z) (Bucket n1 n2 r g b a) = Bucket (n1+1) n2 (r+x) (g+y) (b+z) a
addColor (Trans x)   (Bucket n1 n2 r g b a) = Bucket n1 (n2+1) r g b (a+x)

currentPixel :: R Pixel
currentPixel = do
  Bucket n1 n2 r g b a <- use bucket
  let !rc = if n1 == 0 then 0 else r `div` n1
      !gc = if n1 == 0 then 0 else g `div` n1
      !bc = if n1 == 0 then 0 else b `div` n1
      !ac = if n2 == 0 then 255 else a `div` n2
  pure $ (Pixel ((rc*ac)`div`255) ((gc*ac)`div`255) ((bc*ac)`div`255) ac)

getPixel :: Pos -> R Pixel
getPixel (x, y) = do
  b:_ <- use bitmaps
  VM.read (b V.! x) y

setPixel :: Pos -> Pixel -> R ()
setPixel (x, y) p = do
  b:_ <- use bitmaps
  VM.write (b V.! x) y p

move ∷ Dir → Pos → Pos
move N (x, y) = (x, (y-1)`mod`600)
move E (x, y) = ((x+1)`mod`600, y)
move S (x, y) = (x, (y+1)`mod`600)
move W (x, y) = ((x-1)`mod`600, y)

turn1, turn2 ∷ Dir → Dir
turn1 N = W
turn1 W = S
turn1 S = E
turn1 E = N

turn2 N = E
turn2 E = S
turn2 S = W
turn2 W = N

runR :: [[Base]] -> IO (Vector (Vector Pixel), [Vector (Vector Pixel)])
runR b = runWriterT $ evalStateT ?? emptyState $ do
  addBitmap
  forM_ (zip [0..] b) $ \(i, b) -> do
    when (i`mod`10 == 0) $
      liftIO $ hPutStr stderr $ "\r" ++ show i
    build b
  b:_ <- use bitmaps
  lift $ forM b V.freeze
