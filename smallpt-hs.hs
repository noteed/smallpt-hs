import qualified Data.Vector.Mutable as VM -- smallpt-hs, a haskell port
import Data.IORef; import Foreign; import Foreign.C.Types -- of smallpt,
import Data.List hiding(intersect); import Text.Printf; import System.IO
data Vec = Vec   {-#UNPACK#-}!CDouble -- the 99 lines C++ Path Tracer by
{-position also-}{-#UNPACK#-}!CDouble -- Kevin Beason. Vo Minh Thu, 2010
{-color (r,g,b)-}{-#UNPACK#-}!CDouble deriving (Show,Eq) -- for the port
instance Num Vec where (+) (Vec a b c) (Vec x y z)=Vec (a+x) (b+y) (c+z)
                       (-) (Vec a b c) (Vec x y z)=Vec (a-x) (b-y) (c-z)
                       (*) (Vec a b c) (Vec x y z)=Vec (a*x) (b*y) (c*z)
(*.) (Vec a b c) x=Vec (a*x) (b*x) (c*x); vx=Vec 1 0 0; vy=Vec 0 1 0
black = Vec 0 0 0; norm v@(Vec a b c) = v *. (1/sqrt (a*a+b*b+c*c))
dot (Vec a b c) (Vec x y z) = a*x+b*y+c*z                      -- cross:
(%.) (Vec a b c) (Vec x y z) = Vec (b*z-c*y) (c*x-a*z) (a*y-b*x)
data Ray = Ray Vec Vec                              -- origin, direction
data Refl = DIFF | SPEC | REFR     -- material types, used in 'radiance'
--radius, position, emission, color, reflection type (DIFFuse, SPECular,
data Sphere = Sphere CDouble Vec Vec Vec Refl          -- or REFRactive)
intersect (Ray o d) (Sphere r p e c refl) =
  fi (det<0) Nothing $ f (b-sd) (b+sd) where
  op=p-o; eps=1e-4; b=op`dot`d; det=b*b-(op`dot`op)+r*r; sd=sqrt det
  f a b = fi (a>eps) (Just a) $ fi (b>eps) (Just b) $ Nothing
                   -- Scene: radius, position, emission, color, material
spheres = let s = Sphere; z = black; v = Vec in
  [ s 1e5 (v (1e5+1) 40.8 81.6)    z (v 0.75 0.25 0.25) DIFF   --   Left
  , s 1e5 (v (-1e5+99) 40.8 81.6)  z (v 0.25 0.25 0.75) DIFF   --  Right
  , s 1e5 (v 50 40.8 1e5)          z (v 0.75 0.75 0.75) DIFF   --   Back
  , s 1e5 (v 50 40.8 (-1e5+170))   z z                  DIFF   --  Front
  , s 1e5 (v 50 1e5 81.6)          z (v 0.75 0.75 0.75) DIFF   -- Bottom
  , s 1e5 (v 50 (-1e5+81.6) 81.6)  z (v 0.75 0.75 0.75) DIFF   --    Top
  , s 16.5(v 27 16.5 47)           z ((v 1 1 1)*.0.999) SPEC   -- Mirror
  , s 16.5(v 73 16.5 78)           z ((v 1 1 1)*.0.999) REFR   --  Glass
  , s 600 (v 50 (681.6-0.27) 81.6) (v 12 12 12)       z DIFF ] --  Light
clamp x = fi (x<0) 0 $ fi (x>1) 1 x
toInt x = floor $ clamp (x::CDouble) ** (1/2.2) * 255 + 0.5 :: Int
intersects ray = foldl' f (Nothing,undefined) spheres where
  f (k,sp) s = case (k,intersect ray s) of
    (Nothing,Just x)->(Just x,s); (Just y,Just x) | x<y->(Just x,s);
     _ -> (k,sp)
radiance ray@(Ray o d) depth xi = case intersects ray of
  (Nothing,_) -> return black; (Just t,Sphere r p e c refl) -> do
  fi (depth'<=5) (continue c)           -- or perform a Russian roulette 
    (erand48 xi >>= \er -> fi(er<pr)(continue $ c *. (1/pr))(return e))
  where
    x=o+(d *. t); n=norm$x-p; maxv (Vec a b c)=maximum [a,b,c]
    nl=fi(n`dot`d<0)n$n*.(-1); depth'=depth+1; pr=maxv c;   -- max refl.
    continue f = case refl of {
  DIFF -> do                                 -- ideal DIFFuse reflection
  r1 <- ((2*pi)*) `fmap` erand48 xi; r2 <- erand48 xi; let r2s=sqrt r2
  let u=norm$fi(abs wx>0.1) vy vx%.w; v=w%.u; w@(Vec wx _ _)=nl
      d' = norm$(u*.(cos r1*r2s))+(v*.(sin r1*r2s))+(w*.sqrt(1-r2))
  ((+) e . (*) f) `fmap` radiance (Ray x d') depth' xi
; SPEC -> let d' = d - (n *. (2 * (n`dot`d))) in -- ideal SPECular refl.
  ((+) e . (*) f) `fmap` radiance (Ray x d') depth' xi
; REFR -> do  -- Ideal dielectric REFRACTION; Ray from outside going in?
  let reflRay = Ray x (d-(n*.(2*n`dot`d))); into=n`dot`nl>0
      nc=1; nt=1.5; nnt= fi into (nc/nt) (nt/nc); ddn=d`dot`nl
      cos2t=1-nnt*nnt*(1-ddn*ddn) -- if <0 : Total internal reflection
  fi (cos2t<0) (((+) e . (*) f) `fmap` radiance reflRay depth' xi)
    (let tdir=norm$(d*.nnt-(n*.(fi into 1 (-1)*(ddn*nnt+sqrt cos2t))))
         a=nt-nc; b=nt+nc; r0=a*a/(b*b); c=1-fi into (-ddn) (tdir`dot`n)
         re=r0+(1-r0)*c*c*c*c*c;tr=1-re;q=0.25+re/2;rp=re/q;tp=tr/(1-q)
     in ((+) e . (*) f) `fmap` fi (depth>2) (do{er<-erand48 xi; fi(er<q)
          ((*. rp) `fmap` radiance reflRay depth' xi)    -- ^^^^ Russian
          ((*. tp) `fmap` radiance (Ray x tdir) depth' xi)}) -- roulette
          (do rad0 <- (*. re) `fmap` radiance reflRay depth' xi
              rad1 <- (*. tr) `fmap` radiance (Ray x tdir) depth' xi
              return $ rad0 + rad1))
}
smallpt w h ns = do  -- number of samples, camera position and direction
  let samps=ns`div`4;pos=Vec 50 52 295.6;dir=norm$Vec 0 (-0.042612) (-1)
      cx=Vec (flt w * 0.5135 / flt h) 0 0; cy=norm (cx %. dir) *. 0.5135
  c <- VM.replicate (w * h) black; allocaArray 3 $ \xi ->
  -- no parallelism yet
	flip mapM_ [0..h-1] $ \y -> do               -- loop over image rows
      let fmt = "\rRendering (%d spp) %5.2f%%"; flt' = fromIntegral
      hPrintf stderr fmt (samps*4::Int) (100*flt' y/(flt' h-1)::Double)
      writeXi xi y; flip mapM_ [0..w-1] $ \x -> do
        let i = (h-y-1) * w + x in flip mapM_ [0..1] $ \sy -> do
          flip mapM_ [0..1] $ \sx -> do
            r <- newIORef black; flip mapM_ [0..samps-1] $ \s -> do {
  r1<-(2*)`fmap`erand48 xi; let dx=fi (r1<1) (sqrt r1-1) (1-sqrt(2-r1))
; r2<-(2*)`fmap`erand48 xi; let dy=fi (r2<1) (sqrt r2-1) (1-sqrt(2-r2))
; let d = (cx *. (((sx + 0.5 + dx)/2 + flt x)/flt w - 0.5)) +
          (cy *. (((sy + 0.5 + dy)/2 + flt y)/flt h - 0.5)) + dir
; rad <- radiance (Ray (pos+(d*.140)) (norm d)) 0 xi
            -- Camera rays are ^^^^^ pushed forward to start in interior
; modifyIORef r (+ (rad *. (1 / flt samps)))}
            ci <- VM.unsafeRead c i; Vec rr rg rb <- readIORef r
            VM.unsafeWrite c i $ ci + (Vec (clamp rr) (clamp rg)
                                                     (clamp rb) *. 0.25)
  withFile "image.ppm" WriteMode $ \f -> do
    hPrintf f "P3\n%d %d\n%d\n" w h (255::Int)
    flip mapM_ [0..w*h-1] $ \i -> do { Vec r g b <- VM.unsafeRead c i;
      hPrintf f "%d %d %d " (toInt r) (toInt g) (toInt b) }
foreign import ccall unsafe "erand48" erand48::Ptr CUShort->IO CDouble
writeXi xi y = let y' = fromIntegral y in pokeElemOff xi 0 0
  >> pokeElemOff xi 1 0 >> pokeElemOff xi 2 (y' * y' * y')
        -- compile with ghc --make -O2 -fffi smallpt-hs.hs -o smallpt-hs
fi a b c=if a then b else c; flt=fromIntegral; main=smallpt 200 200 256
