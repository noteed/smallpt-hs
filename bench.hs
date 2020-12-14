{-# LANGUAGE ForeignFunctionInterface #-}
module Main where
import Foreign.C.Types
import Criterion.Main
import Smallpt.Mutable
import Smallpt.Storable
import Smallpt.Unboxed

foreign import ccall unsafe "smallpt"
  c_smallpt :: CInt -> CInt -> CInt -> IO ()

main = defaultMain
--  [ bench "c++" $ c_smallpt 20 20 8
  [ bench "mutable" $ nfIO (Smallpt.Mutable.smallpt 20 20 8) ]
--  , bench "storable" $ Smallpt.Storable.smallpt 20 20 8
--  , bench "unboxed" $ Smallpt.Unboxed.smallpt 20 20 8
--  ]

