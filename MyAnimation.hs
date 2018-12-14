--- Sarath Vaman          ID: acsh122

module MyAnimation where

import Animation

picture :: Animation
picture = background `plus` chainOfRect

background :: Animation
background = withPaint (always black)
                (rect (always 800) (always 600))

chainOfRect :: Animation
chainOfRect = translate (always (400, 300))
          (combine [orbit i x | i <- [10..30], x <- [175, 150..50]])

orbit :: Int -> Int -> Animation
orbit i j =
  rotate (spinner w)
    (translate (cycleSmooth 3 (coordinates x))
     (withGenPaint (cycleSmooth (x/100) ((hslFade (fromIntegral j)) : rectColours)) (cycleSmooth 17 [0.1, 1])
      (rect (cycleSmooth 5 [x/2, 1.5]) (cycleSmooth 20 [x/2, 2/x]))))
 where

    x = fromIntegral j
    w = -10 + 0.5*fromIntegral i

coordinates :: Length -> [(Length, Length)]
coordinates x = concat [[(x/b, x/b),(-x/b, -x/b),(x/b, -x/b),(-x/b, x/b)] | b<-[2,4]]

rectColours :: [Colour]
rectColours = [yellow, lime, purple, cyan, grey, olive, red, magenta]

hslFade :: Length -> Colour
hslFade x = hsl (x * 270) 1 0.5

test :: IO ()
test = writeFile "test.svg" (svg 800 600 picture)
