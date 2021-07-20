import Graphics.Image
import Data.Bool.HT

carpetColor::Bool->[(Int,Int)]->Bool
carpetColor x [] = x
carpetColor x ((a,b):z)
  | not x = carpetColor True z
  | odd (a + b) = carpetColor True z
  | True = carpetColor False z

denr::Int->(Int,Int)->[(Int,Int)]
denr 0 _ = []
denr n (a,x) = (denr (n-1) (b,y))++[(c,z)] where
  (b,c)=divMod a 3
  (y,z)=divMod x 3

carpetPixel::(Int,Int)->Pixel Y Double
carpetPixel x = PixelY $ (carpetColor True $ denr 6 x) ?: (1.0,0.0)

theCarpet::Image RPU Y Double
theCarpet = makeImageR RPU (729,729) carpetPixel

main::IO()
main=writeImage "217i_haferman_carpet.png" theCarpet
