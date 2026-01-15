{-# LANGUAGE OverloadedStrings #-}

module Journal where

import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (Bits (..), (.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [x .&. 0xFF, (x .&. 0xFF00) `shiftR` 8]

octets :: Word32 -> [Word8]
octets w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

getFormat :: Get (BS.ByteString, Word32, Word64, Word64, Word64)
getFormat = do
    fLen <- getWord32be
    let r = BS.pack $ octets fLen

    vr <- getWord32le
    eCount <- getWord64le
    hTabOffset <- getWord64le
    dataEndOffset <- getWord64le
    return (r, vr, eCount, hTabOffset, dataEndOffset)

run :: IO ()
run = do
    cn <- BL.readFile "demo.journal"
    let dc = runGet getFormat cn
    print dc
