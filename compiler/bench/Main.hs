{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoStrictData #-}

module Main where

import Control.DeepSeq (NFData)
import Data.HashSet qualified as HS
import Data.IdMap qualified as IdMap
import Data.List qualified as List
import Data.NonDet.Set qualified as NDSet
import Data.Str
import Data.String (fromString)
import Data.Text qualified as T
import Imports
import System.Random
import Test.Tasty.Bench

data Env = Env
  { strs :: [Str],
    strs' :: [(Str, ())],
    texts :: [T.Text],
    str1 :: Str,
    str2 :: Str,
    text1 :: Text,
    text2 :: Text
  }
  deriving (Generic)

instance NFData Env

main :: IO ()
main =
  defaultMain
    [ env setupEnv \(~Env {..}) ->
        bgroup
          "bench"
          [ bgroup
              "interner"
              [ bgroup
                  "single"
                  [ bench "text" $ whnf (compare text1) text2,
                    bench "str" $ whnf (compareContents str1) str2
                  ],
                bgroup
                  "multiple"
                  [ bench "text" $ whnf (compareBench compare) texts,
                    bench "str" $ whnf (compareBench compareContents) strs,
                    bench "text sort" $ whnf (length . List.sort) texts,
                    bench "str sort" $ whnf (length . List.sort) strs
                  ],
                bgroup
                  "fromList"
                  [ bench "str hashmap" $ nf (HS.fromList) strs,
                    bench "text hashmap" $ nf (HS.fromList) texts,
                    bench "str map" $ nf (NDSet.fromList) strs,
                    bench "text map" $ nf (NDSet.fromList) texts,
                    bench "str intmap" $ nf (IdMap.fromList) strs'
                  ]
              ]
          ]
    ]
  where
    setupEnv =
      ( pure
          Env
            { strs = fmap (fromString @Str) strings,
              strs' = fmap ((,()) . fromString @Str) strings,
              texts = T.pack <$> strings,
              str1 = fromString s1,
              str2 = fromString s2,
              text1 = T.pack s1,
              text2 = T.pack s2
            }
      )
    strings = rnd 10 100000

    s1 = "adfsasdfasdfadsfasdffadsfasdfasdf"
    s2 = reverse s1

    compareBench :: (a -> a -> Ordering) -> [a] -> Ordering
    compareBench f (x : xs) = foldMap (f x) xs <> compareBench f xs
    compareBench f _ = mempty
    {-# INLINE compareBench #-}

-- | Generate a number of fixed length strings where the content of
-- the strings are letters in random order.
rnd ::
  -- | Length of each string
  Int ->
  -- | Number of strings
  Int ->
  [String]
rnd strlen num = take num $ split $ randomRs ('a', 'z') $ mkStdGen 1234
  where
    split cs = case splitAt strlen cs of (str, cs') -> str : split cs'
