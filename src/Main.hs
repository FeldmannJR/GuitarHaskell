module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display

data GuitarState = State{
  notas :: [NotaBotao],
  pont :: Int,
  pressionado :: [Bool]
}
data NotaBotao = Botao{
  nota :: (Float,Int),
  posY :: Float
}

tamanhoBotao :: Float
tamanhoBotao = 20;

tamanhoNota :: Float
tamanhoNota = 10;

tamanhoY :: Float
tamanhoY = 600;

tamanhoX :: Float
tamanhoX = 400;

window :: Display
window = InWindow "Guitar Haskell" (round (tamanhoX),round (tamanhoY)) (10,10)

convertNota :: NotaBotao -> Picture
convertNota (Botao (tempo,tipo) posY) = translate (convertTipo tipo) (posY) $ color  (light (convertTipoColor tipo)) $ circleSolid tamanhoNota

convertNotas :: Picture -> [NotaBotao] -> Picture
convertNotas p [] = p
convertNotas p (x:xs) = convertNotas (pictures [(convertNota x), p]) xs

convertTipo :: Int -> Float
convertTipo x = (-(tamanhoX)/2)  + (16 + tamanhoBotao + (((tamanhoBotao*2)+ 16) * fromIntegral x))

convertState :: GuitarState -> Picture
convertState (State (notas) (pont) (pressionado)) = convertNotas (botoes pressionado) notas

convertBoolToColor :: Bool -> Color
convertBoolToColor b
    | b = white
    | otherwise = black

convertTipoColor :: Int -> Color
convertTipoColor x
  | x == 0 = green
  | x == 1 = red
  | x == 2 = yellow
  | x == 3 = blue
  | otherwise = orange


inicial :: GuitarState
inicial = State
  { notas = [Botao (0.5,1) 250,
    Botao (0.5,2) 200,
    Botao (0.5,4) 220,
    Botao (0.5,3) 100,
    Botao (0.5,0) 10
    ]
  , pont = 0
  , pressionado = [True,True,False,True,True]
}

botoes :: [Bool] -> Picture
botoes b = pictures
  [
    mkLinha (((convertTipo 0) - tamanhoBotao - 8)),
    mkBotao (b!!0) 0,
    mkBotao (b!!1) 1,
    mkBotao (b!!2) 2,
    mkBotao (b!!3) 3,
    mkBotao (b!!4) 4
  ]

mkLinha :: Float -> Picture
mkLinha x = translate x 0 $ color white $ rectangleSolid 2 tamanhoY

mkBotao :: Bool -> Int -> Picture
mkBotao pres x = pictures
   [
     translate (posX) (posBotaoY) $ color (convertTipoColor x) $ circleSolid tamanhoBotao,
     translate (posX) (posBotaoY) $ color (convertBoolToColor pres) $ circleSolid (tamanhoBotao*0.5),
     mkLinha (posX + tamanhoBotao + 8)
   ]
   where
     posX = convertTipo $  x



posBotaoY :: Float
posBotaoY = (-(tamanhoY/2)) + tamanhoBotao*2


input :: Event -> GuitarState -> GuitarState
input _ s = s

update :: Float -> GuitarState -> GuitarState
update f s = s



main :: IO ()
main = play window black 60 inicial convertState input update
