{-# OPTIONS_GHC -Wno-orphans #-}

module Imports.Dot where

import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as VB
import Data.Void (Void)
import GHC.Records
import Prettyprinter qualified as Pr
import System.OsPath
import System.OsPath qualified as P

instance HasField "tb" String TB.Builder where
  getField = TB.fromText . T.pack
  {-# INLINE getField #-}

instance HasField "tb" Text TB.Builder where
  getField = TB.fromText
  {-# INLINE getField #-}

instance HasField "pr" Text (Pr.Doc Void) where
  getField = Pr.pretty
  {-# INLINE getField #-}

instance HasField "pr" String (Pr.Doc Void) where
  getField = Pr.pretty
  {-# INLINE getField #-}

instance HasField "t" String Text where
  getField = T.pack
  {-# INLINE getField #-}

instance HasField "tl" String TL.Text where
  getField = TL.pack
  {-# INLINE getField #-}

instance HasField "b" String ByteString where
  getField = T.encodeUtf8 . T.pack
  {-# INLINE getField #-}

instance HasField "b" Text ByteString where
  getField = T.encodeUtf8
  {-# INLINE getField #-}

instance HasField "b" TL.Text ByteString where
  getField = T.encodeUtf8 . TL.toStrict
  {-# INLINE getField #-}

instance HasField "bl" String BL.ByteString where
  getField = BL.fromStrict . T.encodeUtf8 . T.pack
  {-# INLINE getField #-}

instance HasField "bl" Text BL.ByteString where
  getField = BL.fromStrict . T.encodeUtf8
  {-# INLINE getField #-}

instance HasField "bl" ByteString BL.ByteString where
  getField = BL.fromStrict
  {-# INLINE getField #-}

instance HasField "b" BL.ByteString ByteString where
  getField = BL.toStrict
  {-# INLINE getField #-}

instance HasField "tl" Text TL.Text where
  getField = TL.fromStrict
  {-# INLINE getField #-}

instance HasField "t" TL.Text Text where
  getField = TL.toStrict
  {-# INLINE getField #-}

instance HasField "s" Text String where
  getField = T.unpack
  {-# INLINE getField #-}

instance HasField "s" TL.Text String where
  getField = TL.unpack
  {-# INLINE getField #-}

encodeUtf8Partial :: String -> OsPath
encodeUtf8Partial s =
  case P.encodeUtf s of
    Left e -> Exception.throw e
    Right x -> x
{-# INLINE encodeUtf8Partial #-}

instance HasField "p" String OsPath where
  getField = encodeUtf8Partial
  {-# INLINE getField #-}

instance HasField "p" Text OsPath where
  getField = encodeUtf8Partial . T.unpack
  {-# INLINE getField #-}

instance HasField "vec" [a] (VB.Vector a) where
  getField = VB.fromList
  {-# INLINE getField #-}

instance (Ord k) => HasField "map" [(k, v)] (Map k v) where
  getField = M.fromList
  {-# INLINE getField #-}

instance (Hashable k) => HasField "hm" [(k, v)] (HashMap k v) where
  getField = HM.fromList
  {-# INLINE getField #-}
