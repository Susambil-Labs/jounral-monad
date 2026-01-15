{-# LANGUAGE OverloadedStrings #-}

module Journal where

import Control.Monad (replicateM)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (Bits (..), (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.List (List)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word

data Header = MkHeader
    { version :: !Word32
    , entryCount :: !Word64
    , hToffset :: !Word64
    , dataEndOffset :: !Word64
    }
    deriving (Show, Eq)

data Field = MkField
    { key :: !Text
    , val :: !BS.ByteString
    }
    deriving (Show, Eq)

data Entry = MkEntry
    { eTimestamp :: !Word64
    , fields :: !(List Field)
    }
    deriving (Show, Eq)

data Jurnal = MkJournal
    { header :: Header
    , entry :: !(List Entry)
    }
    deriving (Show, Eq)

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8]

octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

readEntry :: Get Entry
readEntry = do
    ln <- getWord32le
    ts <- getWord64le
    fieldCnt <- getWord16le
    let fnc = fromIntegral fieldCnt
    x <- replicateM fnc readField
    return $ MkEntry ts x

readField :: Get Field
readField = do
    -- Read key
    keyLen <- getWord16le
    keyBytes <- getByteString (fromIntegral keyLen)
    let key = decodeUtf8 keyBytes
    -- Read value
    valueLen <- getWord32le
    value <- getByteString (fromIntegral valueLen)
    return $ MkField key value

readHeader :: Get Header
readHeader = MkHeader <$> getWord32le <*> getWord64le <*> getWord64le <*> getWord64le

readBin = do
    fLen <- getWord32be
    let r = BS.pack $ octets fLen -- FIXME: Check magic string
    h <- readHeader
    skip 32
    cn <- replicateM (fromIntegral h.entryCount) readEntry

    return $ MkJournal h cn

run :: IO ()
run = do
    cn <- BL.readFile "data/demo.journal"
    let dc = runGet readBin cn
    print dc
