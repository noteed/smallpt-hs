{-# LANGUAGE ForeignFunctionInterface #-}
module Smallpt.Unboxed where
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Data.List (find, minimumBy, foldl')
import Data.IORef
import Text.Printf
import Foreign
import Foreign.C.Types
import System.IO (stderr, withFile, IOMode(..))
-- position, also color (r,g,b)
type Vec = (Double,Double,Double)
zerov = (0,0,0)
addv (a,b,c) (x,y,z) = (a+x, b+y, c+z)
subv (a,b,c) (x,y,z) = (a-x, b-y, c-z)
mulvs (a,b,c) x = (a*x, b*x, c*x)
mulv (a,b,c) (x,y,z) = (a*x, b*y, c*z)
len (a,b,c) = sqrt $ a*a+b*b+c*c
norm v = v `mulvs` (1/len v)
dot (a,b,c) (x,y,z) = a*x+b*y+c*z
cross (a,b,c) (x,y,z) = (b*z-c*y, c*x-a*z, a*y-b*x)
maxv (a,b,c) = maximum [a,b,c]

data Ray = Ray Vec Vec -- origin, direction

data Refl = DIFF | SPEC | REFR -- material types, used in radiance

-- radius, position, emission, color, reflection
data Sphere = Sphere Double Vec Vec Vec Refl
intersect (Ray o d) (Sphere r p e c refl) =
  if det<0 then Nothing else f (b-sdet) (b+sdet)
  where op = p `subv` o
        eps = 1e-4
        b = op `dot` d
        det = b*b - (op `dot` op) + r*r
        sdet = sqrt det
        f a b = if a>eps then Just a else if b>eps then Just b else Nothing

spheres = let s = Sphere ; z = zerov ; (*) = mulvs in
  [ s 1e5 (1e5+1, 40.8, 81.6)    z (0.75, 0.25, 0.25) DIFF --Left
  , s 1e5 (-1e5+99, 40.8, 81.6)  z (0.25, 0.25, 0.75) DIFF --Rght
  , s 1e5 (50, 40.8, 1e5)        z (0.75, 0.75, 0.75) DIFF --Back
  , s 1e5 (50, 40.8, -1e5+170)   z z                  DIFF --Frnt
  , s 1e5 (50, 1e5, 81.6)        z (0.75, 0.75, 0.75) DIFF --Botm
  , s 1e5 (50, -1e5+81.6, 81.6)  z (0.75, 0.75, 0.75) DIFF --Top
  , s 16.5(27, 16.5, 47)         z ((1, 1, 1)* 0.999) SPEC --Mirr
  , s 16.5(73, 16.5, 78)         z ((1, 1, 1)* 0.999) REFR --Glas
  , s 600 (50, 681.6-0.27, 81.6) (12, 12, 12)       z DIFF]--Lite

clamp x = if x<0 then 0 else if x>1 then 1 else x

toInt :: Double -> Int
toInt x = floor $ clamp x ** (1/2.2) * 255 + 0.5

intersects ray = (k, s)
  where (k,s) = foldl' f (Nothing,undefined) spheres
        f (k,sp) s = case (k,intersect ray s) of
                  (Nothing,Just x) -> (Just x,s)
                  (Just y,Just x) | x < y -> (Just x,s)
                  _ -> (k,sp)

radiance :: Ray -> Int -> Ptr CUShort -> IO Vec
radiance ray@(Ray o d) depth xi = case intersects ray of
  (Nothing,_) -> return zerov
  (Just t,Sphere r p e c refl) -> do
    let x = o `addv` (d `mulvs` t)
        n = norm $ x `subv` p
        nl = if n `dot` d < 0 then n else n `mulvs` (-1)
        pr = maxv c
        depth' = depth + 1
        continue f = case refl of
          DIFF -> do
            r1 <- ((2*pi)*) `fmap` erand48 xi
            r2 <- erand48 xi
            let r2s = sqrt r2
                w@(wx,_,_) = nl
                u = norm $ (if abs wx > 0.1 then (0,1,0) else (1,0,0)) `cross` w
                v = w `cross` u
                d' = norm $ (u`mulvs`(cos r1*r2s)) `addv` (v`mulvs`(sin r1*r2s)) `addv` (w`mulvs`sqrt (1-r2))
            rad <- radiance (Ray x d') depth' xi
            return $ e `addv` (f `mulv` rad)

          SPEC -> do
            let d' = d `subv` (n `mulvs` (2 * (n`dot`d)))
            rad <- radiance (Ray x d') depth' xi
            return $ e `addv` (f `mulv` rad)

          REFR -> do
            let reflRay = Ray x (d `subv` (n `mulvs` (2* n`dot`d))) -- Ideal dielectric REFRACTION
                into = n`dot`nl > 0                -- Ray from outside going in?
                nc = 1
                nt = 1.5
                nnt = if into then nc/nt else nt/nc
                ddn= d`dot`nl
                cos2t = 1-nnt*nnt*(1-ddn*ddn)
            if cos2t<0    -- Total internal reflection
              then do
                rad <- radiance reflRay depth' xi
                return $ e `addv` (f `mulv` rad)
              else do
                let tdir = norm $ (d`mulvs`nnt `subv` (n`mulvs`((if into then 1 else -1)*(ddn*nnt+sqrt cos2t))))
                    a=nt-nc
                    b=nt+nc
                    r0=a*a/(b*b)
                    c = 1-(if into then -ddn else tdir`dot`n)
                    re=r0+(1-r0)*c*c*c*c*c
                    tr=1-re
                    pp=0.25+0.5*re
                    rp=re/pp
                    tp=tr/(1-pp)
                rad <-
                  if depth>2
                    then do er <- erand48 xi
                            if er<pp -- Russian roulette
                              then (`mulvs` rp) `fmap` radiance reflRay depth' xi
                              else (`mulvs` tp) `fmap` radiance (Ray x tdir) depth' xi
                    else do rad0 <- (`mulvs` re) `fmap` radiance reflRay depth' xi
                            rad1 <- (`mulvs` tr) `fmap` radiance(Ray x tdir) depth' xi
                            return $ rad0 `addv` rad1
                return $ e `addv` (f `mulv` rad)

    if depth'>5
      then do
        er <- erand48 xi
        if er < pr then continue $ c `mulvs` (1/pr)
                  else return e
      else continue c

smallpt :: Int -> Int -> Int -> IO ()
smallpt w h nsamps = do
  let samps = nsamps `div` 4
      org = (50, 52, 295.6)
      dir = norm $ (0, -0.042612, -1)
      cx = (fromIntegral w * 0.5135 / fromIntegral h, 0, 0)
      cy = norm (cx `cross` dir) `mulvs` 0.5135
  c <- VM.newWith (w * h) zerov
  allocaArray 3 $ \xi ->
    flip mapM_ [0..h-1] $ \y -> do
      --hPrintf stderr "\rRendering (%d spp) %5.2f%%" (samps*4::Int) (100.0*fromIntegral y/(fromIntegral h-1)::Double)
      writeXi xi y
      flip mapM_ [0..w-1] $ \x -> do
        let i = (h-y-1) * w + x
        flip mapM_ [0..1] $ \sy -> do
          flip mapM_ [0..1] $ \sx -> do
            r <- newIORef zerov
            flip mapM_ [0..samps-1] $ \s -> do
              r1 <- (2*) `fmap` erand48 xi
              let dx = if r1<1 then sqrt r1-1 else 1-sqrt(2-r1)
              r2 <- (2*) `fmap` erand48 xi
              let dy = if r2<1 then sqrt r2-1 else 1-sqrt(2-r2)
                  d = (cx `mulvs` (((sx + 0.5 + dx)/2 + fromIntegral x)/fromIntegral w - 0.5)) `addv`
                      (cy `mulvs` (((sy + 0.5 + dy)/2 + fromIntegral y)/fromIntegral h - 0.5)) `addv` dir
              rad <- radiance (Ray (org`addv`(d`mulvs`140)) (norm d)) 0 xi
              -- Camera rays are pushed forward ^^^^^ to start in interior
              modifyIORef r (`addv` (rad `mulvs` (1 / fromIntegral samps)))
            ci <- VM.unsafeRead c i
            (rr,rg,rb) <- readIORef r
            VM.unsafeWrite c i $ ci `addv` ((clamp rr, clamp rg, clamp rb) `mulvs` 0.25)

  withFile "image-unboxed.ppm" WriteMode $ \hdl -> do
    hPrintf hdl "P3\n%d %d\n%d\n" w h (255::Int)
    flip mapM_ [0..w*h-1] $ \i -> do
      (r,g,b) <- VM.unsafeRead c i
      hPrintf hdl "%d %d %d " (toInt r) (toInt g) (toInt b)

writeXi :: Ptr CUShort -> Int -> IO ()
writeXi xi y = do
  let y' = fromIntegral y
  pokeElemOff xi 0 0
  pokeElemOff xi 1 0
  pokeElemOff xi 2 (y' * y' * y')

foreign import ccall unsafe "erand48"
  c_erand48 :: Ptr CUShort -> IO CDouble

erand48 :: Ptr CUShort -> IO Double
erand48 xi = realToFrac `fmap` c_erand48 xi 

