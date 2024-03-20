module Ex01 where

import Codec.Picture (writePng)
import ShapeGraphics

-- For this assignment, you will use the `ShapeGraphics`
-- library. You can (and should) read the source code
-- of the library, provided to you in `ShapeGraphics.hs`,
-- to find out which data types and constructors you should
-- use.
-- An `example` picture, showing the usage of `ShapeGraphics`
-- is provided at the end of the file.



-- PART 1 --
-- picture of a house

housePic :: Picture
housePic = [house, door]
  where
    houseCoords :: [Point]
    houseCoords = merge houseCOx houseCOy
    house :: PictureObject
    house = Path houseCoords green Solid

    doorCoords :: [Point]
    doorCoords = convertPoint doorCOs
    door :: PictureObject
    door = Path doorCoords red Solid

-- These are the lists of X and Y coordinates of the path
-- that makes up the house outline.
-- E.g. the first point of this path is `Point 300 750`.
houseCOx :: [Float]
houseCOx = [300.0,300.0,270.0,500.0,730.0,700.0,700.0]
houseCOy :: [Float]
houseCOy = [750.0,450.0,450.0,200.0,450.0,450.0,750.0]

-- Define a merge function which converts the two lists
-- of coordinates above into a list of points usable
-- by the `house` function above. 

merge :: [Float] -> [Float] -> [Point]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = Point x y : merge xs ys

convertPoint :: [(Float, Float)] -> [Point]
convertPoint a = map (\(x,y) -> Point x y) a

-- The door coordinates are given in a different format.
-- Convert them to the correct format and define `door`
-- as a `Path` object.
doorCOs :: [(Float, Float)]
doorCOs = [(550, 750), (550, 550), (650, 550), (650, 750)]

-- These are the additional coordinates you should use to
-- draw the chimney.
chimneyCOs :: [(Float, Float)]
chimneyCOs = [(605, 325), (605, 250), (650, 250), (650, 363)]

-- The window should be a rectangle polygon with the given coordinates,
-- filled with the `cyan` colour defined below.
windowCOs :: [(Float, Float)]
windowCOs = [(350, 650), (350, 550), (450, 550), (450, 650)]

cyan :: Colour
cyan = Colour 96 192 255 255

windowCoords :: [Point]
windowCoords = convertPoint windowCOs
window :: PictureObject
window = Polygon windowCoords cyan Solid SolidFill

chimHouseCOx :: [Float]
chimHouseCOx = [300.0,300.0,270.0,500.0,605.0,605.0,650.0,650.0,730.0,700.0,700.0]
chimHouseCOy :: [Float]
chimHouseCOy = [750.0,450.0,450.0,200.0,325.0,250.0,250.0,363.0,450.0,450.0,750.0]

chimneyHouse :: Picture
chimneyHouse = [window, chimHouse, door]
  where
    chimHouseCoords :: [Point]
    chimHouseCoords = merge chimHouseCOx chimHouseCOy
    chimHouse :: PictureObject
    chimHouse = Path chimHouseCoords green Solid

    doorCoords :: [Point]
    doorCoords = convertPoint doorCOs
    door :: PictureObject
    door = Path doorCoords red Solid

-- PART 2 --

movePoint :: Point -> Vector -> Point
movePoint (Point x y) (Vector xv yv)
  = Point (x + xv) (y + yv)

moveListPoints :: [Point] -> Vector -> [Point]
moveListPoints pointsList vector =
  map (\x -> movePoint x vector) pointsList

movePictureObject :: Vector -> PictureObject -> PictureObject

movePictureObject vec (Path pointsList colour lineStyle) = 
  Path (moveListPoints pointsList vec) colour lineStyle

movePictureObject vec (Circle points radius colour lineStyle fillStyle) = 
  Circle (movePoint points vec) radius colour lineStyle fillStyle

movePictureObject vec (Ellipse center width height rotation colour lineStyle fillStyle) = 
  Ellipse (movePoint center vec) width height rotation colour lineStyle fillStyle
  
movePictureObject vec (Polygon pointsList colour lineStyle fillStyle) = 
  Polygon (moveListPoints pointsList vec) colour lineStyle fillStyle


-- PART 3 --

-- generate the picture consisting of circles:
-- [Circle (Point 400 400) (400/n) col Solid SolidFill,
--  Circle (Point 400 400) 2 * (400/n) col Solid SolidFill,
--  ....
--  Circle (Point 400 400) 400 col Solid SolidFill]

simpleCirclePic :: Colour -> Float -> Picture
simpleCirclePic col n =
  map (\x -> Circle (Point 400 400) (x * (400/n)) col Solid SolidFill) (enumFromThenTo 1 2 n)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program.
-- e.g., call `writeToFile housePic` or even `writeToFile [window]`,
-- the output image will be in `ex01.png`.

writeToFile pic
  = writePng "ex01.png" (drawPicture 3 pic)



-- EXAMPLE --

-- The following is an example picture, showing the usage of the
-- ShapeGraphics library. Use `writeToFile example` to view it.
example :: Picture
example = [redEllipse, blueEllipse] where
  redEllipse :: PictureObject
  redEllipse =
    Ellipse (Point 400 400) --center
            100             --width
            200             --height
            (pi/4)          --rotation (radians)
            red             --color
            Solid           --line (stroke) style
            SolidFill       --fill style
  blueEllipse :: PictureObject
  blueEllipse =
    Ellipse (Point 400 400) --center
            150             --width
            250             --height
            0.0             --rotation (radians)
            blue            --color
            Solid           --line (stroke) style
            NoFill          --fill style
