{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Smallpt.Mutable
import Smallpt.Storable
import Smallpt.Unboxed

main = do
  Smallpt.Mutable.smallpt 40 40 16
--  Smallpt.Storable.smallpt 40 40 16
--  Smallpt.Unboxed.smallpt 40 40 16

