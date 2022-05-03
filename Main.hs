
module Main where

import System.Environment
import Graphics.Svg
import Data.Maybe
import Data.List
import System.FilePath
import Linear.V2

type XY = V2 Double

main :: IO ()
main = do
    args <- getArgs
    mapM_ convertFile args

convertFile :: FilePath -> IO ()
convertFile file = do
    Just doc <- loadSvgFile file
    let f (V2 x1 y1, V2 x2 y2) = intercalate "," $ map show [x1,y1,x2,y2]
    writeFile (file <.> "csv") $ unlines $
        map f $ mapMaybe convertElement $ _elements doc

convertElement :: Tree -> Maybe (XY, XY)
convertElement None = Nothing
convertElement (PathTree (Path _ xs)) = case xs of
    [MoveTo OriginRelative [v1,v2]] -> Just (v1, v1 + v2)
    [MoveTo OriginAbsolute [v1,v2]] -> Just (v1, v2)
    [MoveTo OriginRelative [v1], CurveTo OriginRelative [(_,_,v2)]] -> Just (v1, v1 + v2)
    [MoveTo OriginAbsolute [v1], CurveTo OriginAbsolute [(_,_,v2)]] -> Just (v1, v2)
    [MoveTo OriginAbsolute [v1@(V2 x _)], VerticalTo OriginAbsolute [v2]] -> Just (v1, V2 x v2)
    [MoveTo OriginRelative [v1,v2], VerticalTo OriginRelative [v3]] -> Just (v1, v1 + v2 + V2 0 v3)
    [MoveTo OriginRelative [v1], HorizontalTo OriginRelative [v2]] -> Just (v1, v1 + V2 v2 0)
    _ -> error $ show xs
convertElement EllipseTree{} = Nothing
convertElement x = error $ "Can't convertElement: " ++ take 100 (show x)
