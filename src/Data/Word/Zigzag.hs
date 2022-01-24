{-# LANGUAGE TypeApplications #-}

module Data.Word.Zigzag
  ( toZigzag
  , fromZigzag
  , toZigzag32
  , fromZigzag32
  , toZigzag64
  , fromZigzag64
  ) where

import Data.Bits (unsafeShiftL, unsafeShiftR, complement, (.&.), xor)
import Data.Word(Word32,Word64)
import Data.Int(Int32,Int64)

toZigzag :: Int -> Word
toZigzag = fromIntegral . toZigzag64 . fromIntegral

fromZigzag :: Word -> Int
fromZigzag = fromIntegral . fromZigzag64 . fromIntegral

toZigzag32 :: Int32 -> Word32
toZigzag32 = fromIntegral . toZigzag64 . fromIntegral

fromZigzag32 :: Word32 -> Int32
fromZigzag32 = fromIntegral . fromZigzag64 . fromIntegral

-- In C, this is: @(n << 1) ^ (n >> 63)@
toZigzag64 :: Int64 -> Word64
toZigzag64 i = fromIntegral @Int64 @Word64 $
  (i `unsafeShiftL` 1) `xor` (i `unsafeShiftR` 63)

-- In C, this is: @(n >> 1) ^ (~(n & 1) + 1)@
fromZigzag64 :: Word64 -> Int64
fromZigzag64 n = fromIntegral $
  (n `unsafeShiftR` 1) `xor` (complement (n .&. 1) + 1)
