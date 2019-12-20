{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Game.Logic where

import           Data.Aeson
import           GHC.Generics
import           Debug.Trace


-- сущности

instance {-# OVERLAPS  #-} ToJSON Point
instance {-# OVERLAPS  #-} ToJSON Edge
instance {-# OVERLAPS  #-} ToJSON Box
instance {-# OVERLAPS  #-} ToJSON Board

type Point = (Int, Int)
type Edge = (Point, Point)
type Box = ([Edge], Int)
type Board = [[Box]]


newPoint :: (Int, Int) -> Point
newPoint (x, y) = (x, y)

newEdge :: (Point, Point) -> Edge
newEdge (p1, p2) = (p1, p2)

-- логика игры

changePlayer :: Int -> Int
changePlayer 1 = 2
changePlayer 2 = 1

--changeStrategy :: (Board -> [Edge] -> IO Edge, Board -> [Edge] -> IO Edge) -> (Board -> [Edge] -> IO Edge, Board -> [Edge] -> IO Edge)
--changeStrategy (s1,s2) = (s2,s1)

normalizeEdge :: [String] -> String -> Edge
normalizeEdge [c1,c2] d
  | d == "v"  = ((x,y),(x+1,y))
  | d == "h"  = ((x,y),(x,y+1))
  | otherwise = ((x,y),(x,y))
    where x = read c1 :: Int
          y = read c2 :: Int




-- обновление доски

updateBoard :: Board -> Edge -> Int -> (Board, Bool)
updateBoard b e p
  | isHorizontal e = moveHorizontal b e p
  | otherwise      = moveVertical b e p

moveHorizontal :: Board -> Edge -> Int -> (Board, Bool)
moveHorizontal b e@((p1,p2),(q1,q2)) p
  | p1 == 0        = (y1 ++ tail b, y2)
  | p1 == length b = (init b ++ x1, x2)
  | otherwise      = (take (p1-1) b ++ x1 ++ y1 ++ drop (p1+1) b, x2 || y2)
    where x  = fst $ (b !! (p1-1)) !! p2
          x1 = [take p2 (b !! (p1-1)) ++ [(e:x, if x2 then p else 0)] ++ drop (p2+1) (b !! (p1-1))]
          x2 = length x == 3
          y  = fst $ (b !! p1) !! p2
          y1 = [take p2 (b !! p1) ++ [(e:y, if y2 then p else 0)] ++ drop (p2+1) (b !! p1)]
          y2 = length y == 3

moveVertical :: Board -> Edge -> Int -> (Board, Bool)
moveVertical b e@((p1,p2),(q1,q2)) p
  | p2 == 0               = (take (max 0 p1) b ++ [a1] ++ drop (min (length (head b)) (p1+1)) b, y1)
  | p2 == length (head b) = (take (max 0 p1) b ++ [a2] ++ drop (min (length (head b)) (p1+1)) b, x1)
  | otherwise             = (take (max 0 p1) b ++ [a3] ++ drop (min (length (head b)) (p1+1)) b, x1 || y1)
    where x  = fst $ (b !! p1) !! (p2-1)
          x1 = length x == 3
          y  = fst $ (b !! p1) !! p2
          y1 = length y == 3
          z  = b !! p1
          a1 = (e:y, if y1 then p else 0) : tail z
          a2 = init z ++ [(e:x, if x1 then p else 0)]
          a3 = take (p2-1) z ++ [(e:x, if x1 then p else 0)] ++ [(e:y, if y1 then p else 0)] ++ drop (p2+1) z


-- проверки доски


isAvailable :: Edge -> [Edge] -> Bool
isAvailable = elem

isHorizontal :: Edge -> Bool
isHorizontal ((_,p1),(_,p2)) = abs (p1-p2) == 1

isValid :: Edge -> Bool
isValid ((p1,p2),(q1,q2)) = x || y
  where x = abs (p1-q1) == 1 && p2 == q2
        y = abs (p2-q2) == 1 && p1 == q1

scores :: Board -> (Int,Int)
scores b = (length p1, length p2)
  where p1 = foldl (\x y -> x ++ filter (\z -> snd z == 1) y) [] b
        p2 = foldl (\x y -> x ++ filter (\z -> snd z == 2) y) [] b


-- начало игры


buildBoard :: Int -> Int -> Board
buildBoard 0 _ = []
buildBoard v h = buildRow h : buildBoard (v-1) h 

buildRow :: Int -> [Box]
buildRow 0 = []
buildRow h = ([],0) : buildRow (h-1)

buildAvailables :: Int -> Int -> [Edge]
buildAvailables v h = horizontal ++ vertical
  where horizontal = [((x,y),(x,y+1)) | x <- [0..v], y <- [0..(h-1)]]
        vertical = [((x,y),(x+1,y)) | x <- [0..(v-1)], y <- [0..h]]

deleteAvailable :: Edge -> [Edge] -> [Edge]
deleteAvailable e = filter (/= e)


-- вывод доски


boardToString :: Board -> String
boardToString b = x ++ "\n" ++ fst (foldl printBoxes ("",(0,0)) b)
  where x = fst $ foldl printHorizontal ("*",(0,0)) $ head b

printBoxes :: (String,Point) -> [Box] -> (String,Point)
printBoxes (s,(p1,p2)) lb = (s++x++"\n"++y++"\n",(p1+1,p2))
  where z = if ((p1,p2),(p1+1,p2)) `elem` fst (head lb) then "-" else " "
        x = fst $ foldl printVertical (z,(p1,p2)) lb
        y = fst $ foldl printHorizontal ("*",(p1+1,p2)) lb

printHorizontal :: (String,Point) -> Box -> (String,Point)
printHorizontal (s,(p1,p2)) (le,_) = if ((p1,p2),(p1,p2+1)) `elem` le then (s++" - *",(p1,p2+1)) else (s++"   *",(p1,p2+1))

printVertical :: (String,Point) -> Box -> (String,Point)
printVertical (s,(p1,p2)) (le,n) = if ((p1,p2+1),(p1+1,p2+1)) `elem` le then (s++" "++show n++" -",(p1,p2+1)) else (s++" "++show n++"  ",(p1,p2+1))

printBoard :: Board -> IO ()
printBoard board = do
                  putStrLn ""
                  putStrLn $ boardToString board
                  putStrLn ""


-- игра




--main :: IO ()
--main = do
--          let n = 2
--          let board = buildBoard n n
--          let availables = buildAvailables n n
--          rn 1 (mve, mve) board availables


--rn :: Int -> (Board -> [Edge] -> IO Edge, Board -> [Edge] -> IO Edge) -> Board -> [Edge] -> IO ()
--rn _ _ board [] = gameFinished board
--rn playerNum mv board edges = do
--  putStrLn $ "Turn to Player " ++ show playerNum
--  edge <- fst mv board edges
----                    putStrLn $ "Player " ++ show playerNum ++ " moves to " ++ show edge
--  nb <- updateBoard board edge playerNum
--  if snd nb
--    then rn playerNum mv (fst nb) (deleteAvailable edge edges)
--    else rn (changePlayer playerNum) (changeStrategy mv) (fst nb) (deleteAvailable edge edges)
--
--gameFinished :: Board -> IO ()
--gameFinished b = do let (p1,p2) = scores b
--                    putStrLn ""
--                    putStrLn "GAME FINISHED!"
--                    putStrLn ""
--                    printBoard b
--                    putStrLn "SCORES:"
--                    putStr "Player 1: "
--                    print p1
--                    putStr "Player 2: "
--                    print p2
--                    putStrLn ""
--
---- стратегия
--
--mve :: Board -> [Edge] -> IO Edge
--mve board le = do
--          printBoard board
--          input <- getLine
--          let p = take 2 $ words input
--          let d = last $ words input
--          let e = normalizeEdge p d
--          if not $ isValid e
--            then do
--              putStrLn "The selected coordinates can't make a line, please enter a correct ones"
--              mve board le
--            else if not $ isAvailable e le then do
--              putStrLn "The selected line is not available, please enter another"
--              mve board le
--          else return e
