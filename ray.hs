import Data.List

data Colour = Black | White deriving (Show)
data Image = Image { width :: Int, height :: Int, pixels :: [[Colour]] } deriving (Show)
type Vec3 = (Float, Float, Float)
data Object = Sphere Vec3 Float deriving (Show)

-- PBM file support

colourToPBM :: Colour -> String
colourToPBM Black = "1"
colourToPBM White = "0"

imageToPBM :: Image -> String
imageToPBM (Image w h pixels) =
  "P1\n" ++ show w ++ " " ++ show h ++ "\n" ++ pixelData
  where pixelData = intercalate "\n" $ map pixelDataForRow pixels
        pixelDataForRow row = intercalate " " $ map colourToPBM row

-- Vector stuff

vplus :: Vec3 -> Vec3 -> Vec3
vplus (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

vsub :: Vec3 -> Vec3 -> Vec3
vsub (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

vscale :: Float -> Vec3 -> Vec3
vscale k (x, y, z) = (k * x, k * y, k * z)

vlen :: Vec3 -> Float
vlen (x, y, z) = sqrt $ (x * x) + (y * y) + (z * z)

vnormalize :: Vec3 -> Vec3
vnormalize v = vscale (1 / vlen v) v

vdot :: Vec3 -> Vec3 -> Float
vdot (ax, ay, az) (bx, by, bz) = ax * bx + ay * by + az * bz

-- Geometry intersection

intersects :: Vec3 -> Object -> Bool
intersects ray (Sphere pos r) =
  let a = vdot ray ray
      b = -2 * vdot pos ray
      c = vdot pos pos - r * r
  in (b * b - 4 * a * c) >= 0

-- Scene description

geometry :: [Object]
geometry = [Sphere ( 0,  0, 15) 3,
            Sphere (-3, -3, 15) 1,
            Sphere (-5, -5, 15) 1,
            Sphere ( 7,  7, 15) 1,
            Sphere (-5,  3, 15) 1]

-- Output parameters

imageWidth = 512
imageHeight = 512
aspectRatio = fromIntegral imageWidth / fromIntegral imageHeight

-- view angles are relative to the centre of the screen
hViewingAngle = 30 * pi / 180
vViewingAngle = atan $ (tan hViewingAngle) / aspectRatio

-- Main program

rayForPixel :: Int -> Int -> Vec3
rayForPixel ix iy = vnormalize (vx, vy, 1.0)
  where project x width angle = tan angle * (x - width) / width
        vx = project (fromIntegral ix) (fromIntegral imageWidth/2) hViewingAngle
        vy = project (fromIntegral iy) (fromIntegral imageHeight/2) vViewingAngle

renderPixel :: Int -> Int -> Colour
renderPixel x y =
  let ray = rayForPixel x y
      hit = any (intersects ray) geometry
  in if hit then White else Black

renderedImage :: Image
renderedImage = Image width height pixels
  where (width, height) = (imageWidth, imageHeight)
        pixels = [[renderPixel x y | x <- [0..width-1]] | y <- [0..height-1]]

main :: IO ()
main = putStrLn $ imageToPBM renderedImage
