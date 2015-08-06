
module KMP
  ( Table
  , build
  , match
  ) where

import Data.Array
  ( Array
  , listArray
  , bounds
  , (!)
  )

import Prelude ()
import MyPrelude hiding ((!))

data Table a = Table
  { alphabetTable :: Array Int a
  , jumpTable :: Array Int Int
  }

build :: Eq a => [a] -> Table a
build pat =
  let
    len = length pat

    resTable = Table
      { alphabetTable = listArray (0,len-1) pat
      , jumpTable = listArray (-1,len-1) $ (-2) : genJump (-1) 0
      }

    genJump _ 0 =
      let
        o = if 1 == len || alphabetTable resTable ! 0 /= alphabetTable resTable ! 1
          then -1
          else -2

        later = genJump (-1) 1
      in
        o : later

    genJump lastMPJump i =
      let
        ch = alphabetTable resTable ! i

        findJ j
          | j == -2 = -2
          | alphabetTable resTable ! (j+1) == ch = j
          | j == -1 = -2
          | otherwise = findJ (jumpTable resTable ! j)

        j = findJ lastMPJump

        o = if i+1 == len || alphabetTable resTable ! (i+1) /= alphabetTable resTable ! (j+2)
          then j+1
          else jumpTable resTable ! (j+1)

        later = genJump (j+1) (i+1)
      in o : later

  in
    resTable

match :: Eq a => Table a -> Seq a -> Maybe (Seq a, Seq a)
match table str =
  let
    len = 1 + snd ( bounds (alphabetTable table) )

    go i j acc str =
      let
        later = case str of
          (s:<ss) ->
            let
              (i', j', acc', str')
                | j < 0 || j < len && s == alphabetTable table ! j = (i + 1, j + 1, acc :> s, ss)
                | otherwise = (i, 1 + (jumpTable table ! (j - 1)), acc, str)
            in
              go i' j' acc' str'
          _ -> Nothing
      in
        if j == len
          then Just (acc, str)
          else later
  in
    go 0 0 Empty str
