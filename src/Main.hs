module Main where

import Graphics.Gloss
import Musica

tamanhoBotao :: Float
tamanhoBotao = 20;

window :: Display
window = InWindow "Guitar Haskell" (400,800) (10,10)


drawing :: Picture
drawing = botoes


botoes :: Picture
botoes = pictures
  [
    mkBotao green 0,
    mkBotao red 1,
    mkBotao yellow 2,
    mkBotao blue 3,
    mkBotao orange 4
  ]

mkBotao :: Color -> Int -> Picture
mkBotao col x = pictures
   [
     translate ((((tamanhoBotao*2) + 10 ) * (fromIntegral x))) (tamanhoBotao+10) $ color col $ circleSolid tamanhoBotao
   ]

main :: IO ()
main = display window black drawing


