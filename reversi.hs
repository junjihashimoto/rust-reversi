{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

data Disk=
    White
  | Black deriving (Read,Eq,Ord)
                   
instance Show Disk where  
  show White = "o"
  show Black = "x"
  
showDisk (Just disk) = show disk
showDisk Nothing     = " "

showDisk' (x,y) board = showDisk $ M.lookup (x,y) board

type Pos=(Int,Int)
type Board=M.Map Pos Disk

showBoard board =
  let xline y= foldr (\x v-> (showDisk' (x,y) board)++v) "" [0..7]
      yline  = foldr (\y v-> "|"++(xline y)++"|"++ show y ++"\n"++v) "" [0..7]
  in " 01234567\n" ++ " --------\n" ++ yline ++ " --------\n"

type Value=Int

value::Board->Disk->Value
value board disk=length $ filter (\(p,d)->d==disk) $ M.toList board

all' cond []  =False
all' cond list=all cond list


between::Pos->Pos->[Pos]
between posa@(xa,ya) posb@(xb,yb) 
  | xa==xb || ya==yb  ||  abs(xa-xb) == abs(ya-yb) = 
      map (\i -> plus i posa (direct posa posb)) [1..(dist posa posb -1)]
  | otherwise = []
  where
    dist   posa@(xa,ya) posb@(xb,yb) = max (abs (xa-xb)) (abs (ya-yb))
    direct::Pos->Pos->Pos
    direct posa@(xa,ya) posb@(xb,yb) = 
      let d = dist posa posb 
      in ((xb-xa) `div` d,(yb-ya) `div` d)
    plus::Int -> Pos -> Pos -> Pos
    plus i posa@(xa,ya) posd@(xd,yd) = (xa+xd*i,ya+yd*i)
    
allsame board disk posa posb=
  all' ( \pos -> M.lookup pos board == (Just disk)) (between posa posb) 

next::Board->Disk->[Pos]
next board disk = 
  let r = ndisk disk
      n = map fst $ filter (\(k,v)->v==r)    (M.toList board)
      n'= map fst $ filter (\(k,v)->v==disk) (M.toList board)
      s = filter (\(x,y)->0<=x&&x<8&&0<=y&&y<8) $ msum $ flip map n $ \(x,y)-> do
        x'' <-[-1..1]
        y'' <-[-1..1]
        return (x+x'',y+y'')
      empty=filter (\pos-> M.lookup pos board == Nothing) s
  in S.toList $ S.fromList $ filter (\e -> any (\n''-> allsame board r e n'') n' ) empty

ndisk disk=case disk of
  White -> Black
  Black -> White

put::Board->Disk->Pos->Board
put board disk pos =
  let r  = ndisk disk
      n' = map fst $ filter (\(k,v)->v==disk) (M.toList board)
      n'''= msum $ map (\n''-> if allsame board r pos n'' then between pos n'' else [] ) n'
  in foldr (\p->M.alter (\_ -> Just disk) p) board (pos:n''')
      
nboard::Board->Disk->[(Value,(Pos,Board))]
nboard board disk = 
  let n = next board disk
      b = map (put board disk) n
      p = map (flip value disk) b 
  in zip p $ zip n b

maxmin board disk=
  let nb =nboard board disk 
      (v,(p,b))=L.maximumBy (\(a,_) (b,_) -> compare a b) nb
  in case nb of
    [] -> Nothing
    _  -> Just p

main' disk board=do
  let nd=ndisk disk
  let p' =next board disk
  let p''=next board nd
  putStr $ showBoard board
  if p'==[] && p''==[] 
    then do
    if value board Black > value board White
      then print "black win"
      else if value board Black < value board White
        then print "white win"
        else print "draw"
    else do
    b <- case  p' of
      [] -> return board
      _  -> do
        v <- getLine
        let p=read v :: Pos
        if (filter (==p) p')==[] 
          then main' disk board
          else return ()
--        let (Just p)=maxmin board disk
        let b=put board disk p
        return b
    putStr $ showBoard b
    let p'=maxmin b nd 
    let b'=case p' of 
          Just v -> put b nd v
          _      -> b
    main' disk b'

initboard=M.insert (3,4) Black $
          M.insert (4,3) Black $
          M.insert (4,4) White $
          M.insert (3,3) White M.empty

main=do
  main' Black initboard
  
