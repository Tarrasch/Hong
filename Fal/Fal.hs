module Fal.Fal where

import Graphics.SOE.Gtk hiding (Region, Event)
import qualified Graphics.SOE.Gtk as G (Region, Event)
import Fal.Animation (picToGraphic)
import Fal.Shape
import Fal.Picture
import Fal.Memo1

import Fal.Draw (xWin,yWin,intToFloat)
-- import Word (word32ToInt)
import Control.Concurrent.Chan

infixr 1 =>>, ->>
infixr 1 `untilB`, `switch`, `stepAccum`, `step`
infixl 0 .|.
infixr 4 <*, >*
infixr 3 &&*
infixr 2 ||*

type Time = Float


type UserAction = G.Event








newtype Behavior1 a
  = Behavior1 ([(UserAction,Time)] -> Time -> a)

inList :: [Int] -> Int -> Bool

inList xs y = elem y xs

result1 :: [Bool]
result1 = map (inList xs) ys

xs = [2,4,6,8,10] :: [Int]
ys = [3,6,9]      :: [Int]

result2 :: [Bool]
result2 = manyInList xs ys

manyInList :: [Int] -> [Int] -> [Bool]
manyInList [] _ = []
manyInList _ [] = []
manyInList (x:xs) (y:ys) =
  if x<y then          manyInList xs (y:ys)
         else (x==y) : manyInList (x:xs) ys



newtype Behavior2 a
  = Behavior2 ([(UserAction,Time)] -> [Time] -> [a])


newtype Behavior3 a
  = Behavior3 ([UserAction] -> [Time] -> [a])

newtype Behavior4 a
  = Behavior4 ([Maybe UserAction] -> [Time] -> [a])

newtype Behavior a
  = Behavior (([Maybe UserAction],[Time]) -> [a])

runBehavior (Behavior x) = x

newtype Event a
  = Event (([Maybe UserAction],[Time]) -> [Maybe a])

time :: Behavior Time
time = Behavior (\(_,ts) -> ts)

constB :: a -> Behavior a
constB x = Behavior (\_ -> repeat x)


($*) :: Behavior (a->b) -> Behavior a -> Behavior b
Behavior ff $* Behavior fb
  = Behavior (\uts -> zipWith ($) (ff uts) (fb uts))

lift0 :: a -> Behavior a
lift0 = constB

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f b1
  = lift0 f $* b1

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 f b1 b2
  = lift1 f b1 $* b2

lift3 :: (a -> b -> c -> d) ->
         (Behavior a -> Behavior b -> Behavior c -> Behavior d)
lift3 f b1 b2 b3
  = lift2 f b1 b2 $* b3


lift4 :: (a -> b -> c -> d -> e) ->
         (Behavior a -> Behavior b -> Behavior c -> Behavior d -> Behavior e)
lift4 f b1 b2 b3 b4
  = lift3 f b1 b2 b3 $* b4

lift5 f b1 b2 b3 b4 b5
  = lift4 f b1 b2 b3 b4 $* b5

lift6 f b1 b2 b3 b4 b5 b6
  = lift5 f b1 b2 b3 b4 b5 $* b6

pairB :: Behavior a -> Behavior b -> Behavior (a,b)
pairB = lift2 (,)


unPair :: Behavior (a, b) -> (Behavior a, Behavior b)
unPair beh = (fstB beh, sndB beh)

pair3 :: a -> b -> c -> (a, b, c)
pair3 a b c = (a, b, c)

pairB3 :: Behavior a -> Behavior b -> Behavior c -> Behavior (a,b,c)
pairB3 = lift3 pair3


fstB :: Behavior (a,b) -> Behavior a
fstB  = lift1 fst
sndB :: Behavior (a,b) -> Behavior b
sndB  = lift1 snd


fst3 (a, b, c) = a
snd3 (a, b, c) = b
trd3 (a, b, c) = c

fstB3 = lift1 fst3
sndB3 = lift1 snd3
trdB3 = lift1 trd3

unPairB3 :: Behavior (a, b, c) -> (Behavior a, Behavior b, Behavior c)
unPairB3 beh = (fstB3 beh, sndB3 beh, trdB3 beh)


paint :: Behavior Color -> Behavior Region -> Behavior Picture
paint = lift2 Region

red, blue, yellow, green, white, black :: Behavior Color
red    = lift0 Red
blue   = lift0 Blue
yellow = lift0 Yellow
green  = lift0 Green
white  = lift0 White
black  = lift0 Black

shape :: Behavior Shape -> Behavior Region
shape   = lift1 Shape

ell, rec :: Behavior Float -> Behavior Float -> Behavior Region
ell x y = shape (lift2 Ellipse   x y)
rec x y = shape (lift2 Rectangle x y)

translate :: (Behavior Float, Behavior Float)
             -> Behavior Region -> Behavior Region
translate (Behavior fx, Behavior fy) (Behavior fp)
      = Behavior (\uts -> zipWith3 aux (fx uts) (fy uts) (fp uts))
        where aux x y p = Translate (x,y) p

(>*),(<*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
(>*) = lift2 (>)
(<*) = lift2 (<)

(&&*),(||*) :: Behavior Bool -> Behavior Bool -> Behavior Bool
(&&*) = lift2 (&&)
(||*) = lift2 (||)

over :: Behavior Picture -> Behavior Picture -> Behavior Picture
over = lift2 Over

instance Fractional a => Fractional (Behavior a) where
  (/) = lift2 (/)
  fromRational = lift0 . fromRational

instance Num a => Num (Behavior a) where
  (+) = lift2 (+)
  (*) = lift2 (*)
  negate = lift1 negate
  abs = lift1 abs
  signum = lift1 signum
  fromInteger = lift0 . fromInteger

instance Show (Behavior a)  where
  showsPrec n a s = "<< Behavior >>"

instance Eq (Behavior a) where
  a1 == a2 = error "Can't compare behaviors."

instance Floating a => Floating (Behavior a) where
  pi    = lift0 pi
  sqrt  = lift1 sqrt
  exp   = lift1 exp
  log   = lift1 log
  sin   = lift1 sin
  cos   = lift1 cos
  tan   = lift1 tan
  asin  = lift1 asin
  acos  = lift1 acos
  atan  = lift1 atan
  sinh  = lift1 sinh
  cosh  = lift1 cosh
  tanh  = lift1 tanh
  asinh = lift1 asinh
  acosh = lift1 acosh
  atanh = lift1 atanh


memoB :: Behavior a -> Behavior a
memoB (Behavior fb) = Behavior (memo1 fb)


untilB, switch :: Behavior a -> Event (Behavior a) -> Behavior a

Behavior fb `untilB` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) =
            b : case e of
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> fb' (us,ts)

Behavior fb `switch` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) ~(b:bs) =
            b : case e of
                  Nothing             -> loop us ts es bs
                  Just (Behavior fb') -> loop us ts es (fb' (us,ts))

untilB' :: Behavior a -> Event (a -> Behavior a) -> Behavior a
Behavior fb `untilB'` Event fe =
  memoB $ Behavior (\uts@(us,ts) -> loop us ts (fe uts) (fb uts))
    where loop (_:us) (_:ts) ~(e:es) (b:bs) =
            b : case e of
                  Nothing      -> loop us ts es bs
                  Just (f_fb') -> runBehavior (f_fb' b) (us,ts)

lbp :: Event ()
lbp = Event (\(uas,_) -> map getlbp uas)
      where getlbp (Just (Button _ True True)) = Just ()
            getlbp _                           = Nothing




(=>>) :: Event a -> (a->b) -> Event b


Event fe =>> f = Event (map (fmap f) . fe)

e ->> v = e =>> \_ -> v

while :: Behavior Bool -> Event ()

while (Behavior fb)
  = Event (\uts -> map aux (fb uts))
    where aux True  = Just ()
          aux False = Nothing

unique :: (Show a, Eq a) => Event a -> Event a
unique (Event fe) =
      Event (\uts -> aux (fe uts))
      where aux xs = zipWith remdup (Nothing:xs) xs
            remdup x y | x==y      = Nothing
                       | otherwise = y

when :: Behavior Bool -> Event ()
when = unique . while

integral :: Behavior Float -> Behavior Float
integral (Behavior fb)
  = Behavior (\uts@(us,t:ts) -> 0 : loop t 0 ts (fb uts))
      where loop t0 acc (t1:ts) (a:as)
                 = let acc' = acc + (t1-t0)*a
                   in acc' : loop t1 acc' ts as

color1 :: Behavior Color
color1 = red `untilB` lbp ->> blue

uas = cycle [Nothing, Just (Button (0,0) True True), Nothing]
ts  = [1,2 ..] :: [Time]

stream1 = let Behavior fb = color1
          in take 3 (fb (uas,ts))






test beh = reactimate "FAL Test" (lift1 picToGraphic beh)

cball1   = paint color1  circ
cball1r  = paint color1r circ
cball1h  = paint color1h circ
cball2   = paint color2  circ
cball2r  = paint color2r circ
cball2h  = paint color2h circ
cball3   = paint color3  circ
cball4   = paint color4  circ
cball5   = paint color5  circ
circ     = translate (cos time, sin time) (ell 0.2 0.2)

ball1 :: Behavior Picture
ball1 = paint color1 circ

color1r = red  `untilB` lbp ->>
          blue `untilB` lbp ->>
          color1r

color2r = red `untilB` colorEvent where
          colorEvent = (lbp ->> blue   `untilB` colorEvent) .|.
                       (key ->> yellow `untilB` colorEvent)

color2h = red `switch` ((lbp ->> blue) .|. (key ->> yellow))

color5 = red `untilB` when (time >* 5) ->> blue

sim1 = drawIt "Bouncing Ball"
              (b `Over` Region White (Shape (Rectangle 6 5)))

drawIt :: String -> Picture -> IO ()
drawIt s p
  = runGraphics (
    do w <- openWindow s (xWin,yWin)
       drawPic w p
       spaceClose w
    )

b :: Picture
b = let Behavior f = ball2Sim
    in foldr Over EmptyPic
         (take 100 (f (repeat Nothing, [0.0, 0.1 ..])))

ball2Sim = paint red (translate (x,y) (ell 0.08 0.08))
       where g =  -4
             x =  -3 + integral 0.7
             y = 1.5 + integral v
             v = integral g `switch` (hit `snapshot_` v =>> \v'->
                 lift0 (-v') + integral g)
             hit = when (y <* -1.5)

ball2 = paint red (translate (x,y) (ell 0.2 0.2))
       where g =  -4
             x =  -3 + integral 0.5
             y = 1.5 + integral v
             v = integral g `switch` (hit `snapshot_` v =>> \v'->
                 lift0 (-v') + integral g)
             hit = when (y <* -1.5)

color1h = red `switch` (lbp `withElem_` cycle [blue,red])

withElem  :: Event a -> [b] -> Event (a,b)
withElem (Event fe) bs = Event (\uts -> loop (fe uts) bs)
  where loop (Just a  : evs) (b:bs) = Just (a,b) : loop evs bs
        loop (Nothing : evs)    bs  = Nothing    : loop evs bs

withElem_ :: Event a -> [b] -> Event b
withElem_ e bs = e `withElem` bs =>> snd

color2 = red `untilB` (lbp ->> blue .|. key ->> yellow)

(.|.) :: Event a -> Event a -> Event a
Event fe1 .|. Event fe2
  = Event (\uts -> zipWith aux (fe1 uts) (fe2 uts))
      where aux Nothing  Nothing  = Nothing
            aux (Just x) _        = Just x
            aux _        (Just y) = Just y

key :: Event Char
key = Event (\(uas,_) -> map getkey uas)
      where getkey (Just (Key ch True)) = Just ch
            getkey _                    = Nothing

color3 = white `switch` (key =>> \c ->
           case c of 'R' -> red
                     'B' -> blue
                     'Y' -> yellow
                     _   -> white  )

color4 = white `switch` (key `snapshot` color4 =>> \(c,old) ->
           case c of 'R' -> red
                     'B' -> blue
                     'Y' -> yellow
                     _   -> lift0 old)

snapshot :: Event a -> Behavior b -> Event (a,b)
Event fe `snapshot` Behavior fb
  = Event (\uts -> zipWith' aux (fe uts) (fb uts))
      where aux (Just x) y = Just (x, y)
            aux Nothing  _ = Nothing

zipWith' f ~(x:xs) ~(y:ys) = f x y : zipWith' f xs ys

snapshot_ :: Event a -> Behavior b -> Event b
snapshot_ e b = e `snapshot` b =>> snd


step :: a -> Event a -> Behavior a

a `step` e = constB a `switch` e =>> constB

stepAccum :: a -> Event (a->a) -> Behavior a

a `stepAccum` e = b
   where b = a `step` (e `snapshot` b =>> uncurry ($))

counter = 0 `stepAccum` lbp ->> (+1)

stream2 = let Behavior fb = counter
          in take 20 (fb (uas,ts))

mm :: Event Coordinate
mm = Event (\(uas,_) -> map getmm uas)
     where getmm (Just (MouseMove pt)) = Just (gPtToPt pt)
           getmm _                     = Nothing


gPtToPt :: (Int, Int) -> Coordinate
gPtToPt (x,y) = ( pixelToInch (x - 300)
                , pixelToInch (250 - y) )

pixelToInch  :: Int -> Float
pixelToInch n = intToFloat n / 100

mouse :: (Behavior Float, Behavior Float)

mouse = (fstB m, sndB m)
          where m = (0,0) `step` mm


ball3 = paint color4 circ3
circ3 = translate mouse (ell 0.2 0.2)



reactimate :: String -> Behavior Graphic -> IO ()
reactimate title franProg
  = runGraphics $
    do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin))
              drawBufferedGraphic Nothing
       (us,ts,addEvents) <- windowUser w
       addEvents
       let drawPic (Just g) =
             do setGraphic w g
                quit <- addEvents
                if quit
                  then return True
                  else return False
           drawPic Nothing  = return False
       let Event fe = sample `snapshot_` franProg
       run drawPic (fe (us,ts))
       closeWindow w
  where
    run f (x:xs) = do
      quit <- f x
      if quit
        then return ()
        else run f xs
    run f [] = return ()

sample :: Event ()
sample = Event (\(us,_) -> map aux us)
  where aux Nothing  = Just ()
        aux (Just _) = Nothing

windowUser :: Window -> IO ([Maybe UserAction], [Time], IO Bool)
windowUser w
  = do (evs, addEv) <- makeStream
       t0 <- timeGetTime
       let addEvents =
             let loop rt = do
                   mev <- maybeGetWindowEvent w
                   case mev of
                     Nothing -> return False
                     Just e  -> case e of
                        Key ' ' True -> return True
                        Closed -> return True
                        _ -> addEv (rt, Just e) >> loop rt
             in do t <- timeGetTime
                   let rt = w32ToTime (t-t0)
                   quit <- loop rt
                   addEv (rt, Nothing)
                   return quit
       return (map snd evs, map fst evs, addEvents)

w32ToTime t = intToFloat (fromInteger (toInteger t)) / 1000

makeStream :: IO ([a], a -> IO ())
makeStream = do
  ch <- newChan
  contents <- getChanContents ch
  return (contents, writeChan ch)

