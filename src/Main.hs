module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display

import Musica

data GuitarState = State{
  notas :: [NotaBotao],
  pont :: Int,
  pressionado :: [Bool],
  tempo :: Float,
  vel :: Float
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
tamanhoX = 600;

fps :: Int
fps = 120

window :: Display
window = InWindow "Guitar Haskell" (round (tamanhoX),round (tamanhoY)) (10,10)

fixedTime :: Float
fixedTime = 1.0 / (fromIntegral fps);

tempoCair :: Float -> Float
tempoCair x = ((tamanhoY-tamanhoBotao*2)/x)

fx :: Float -> Float
fx x = (-tamanhoX/2)+x

fy :: Float -> Float
fy y = (-tamanhoY/2)+y

posBotaoY :: Float
posBotaoY = (-(tamanhoY/2)) + tamanhoBotao*2

posBotaoX :: Int -> Float
posBotaoX x = (-(tamanhoX)/2)  + (150 + tamanhoBotao + (((tamanhoBotao*2)+ 16) * fromIntegral x))


-- Musica
inicial :: GuitarState
inicial = loadSong wishyou

loadSong :: Musica -> GuitarState
loadSong (Mus vel notas) = State
  { notas = (convertNotaToButton notas vel)
  , pont = 0
  , pressionado = [False,False,False,False,False]
  , tempo = 0
  , vel = vel
}
-- Converte qualquer musica para os objetos dos botoes
convertNotaToButton :: [Nota] -> Float -> [NotaBotao]
convertNotaToButton [] _ = []
convertNotaToButton ((tempo,tipo):xs) vel = (Botao ((tempoCair vel)+tempo,tipo) (-401)) : (convertNotaToButton xs vel)


----------------------------------------------------
--Funções para renderizar algo na tela
--Converte todo o jogo para uma imagem
convertState :: GuitarState -> Picture
convertState (State (notas) (pont) (pressionado) (tempo) vel) = pictures [botoes pressionado,(convertNotas ((pictures [])) notas), drawTempo tempo vel, drawScore pont]

-- Converte os botões de baixo ( que são ativavies para imagem)
botoes :: [Bool] -> Picture
botoes b = pictures
  [
    mkLinha (((posBotaoX 0) - tamanhoBotao - 8)),
    mkBotao (b!!0) 0,
    mkBotao (b!!1) 1,
    mkBotao (b!!2) 2,
    mkBotao (b!!3) 3,
    mkBotao (b!!4) 4
  ]
-- Gera uma linha branca em tal X
mkLinha :: Float -> Picture
mkLinha x = translate x 0 $ color white $ rectangleSolid 2 tamanhoY

-- Gera um botão de apertar Tipo
mkBotao :: Bool -> Int -> Picture
mkBotao pres x = pictures
   [
     translate (posX) (posBotaoY) $ color white $ circleSolid (tamanhoBotao*1.1),
     translate (posX) (posBotaoY) $ color (convertTipoColor x) $ circleSolid tamanhoBotao,
     translate (posX) (posBotaoY) $ color (convertBoolToColor pres x) $ circleSolid (tamanhoBotao*0.5),
     mkLinha (posX + tamanhoBotao + 8)
   ]
   where
     posX = posBotaoX x
--Converte uma nota(bolinhas caindo) para uma imagem
convertNota :: NotaBotao -> Picture
convertNota (Botao (tempo,tipo) posY) = translate (posBotaoX tipo) ((-(tamanhoY/2))+posY) $ color  (light (convertTipoColor tipo)) $ circleSolid tamanhoNota
--Convert todas as bolinhas caindo para uma imagem
convertNotas :: Picture -> [NotaBotao] -> Picture
convertNotas p [] = p
convertNotas p ((Botao x y):xs)
  | y > -100 = convertNotas (pictures [(convertNota (Botao x y)), p]) xs
  |otherwise = convertNotas p xs


drawTempo :: Float -> Float-> Picture
drawTempo tempo vel = color red $ translate (posBotaoX 5) (fy tamanhoY-30) $ scale 0.2 0.2 $ text $ (show tempo)

drawScore :: Int -> Picture
drawScore sc = pictures
  [
    color blue $ translate ((posBotaoX 5)+10) (fy 50) $ scale 0.3 0.3 $ text "Score",
    color white $ translate ((posBotaoX 5)+9) (fy 20) $ scale 0.2 0.2 $ text $ show sc
  ]

convertBoolToColor :: Bool -> Int -> Color
convertBoolToColor b x
    | b =  dim $ cor
    | otherwise = dim $ dim $ dim $ dim $ cor
    where
      cor = (convertTipoColor x)
convertTipoColor :: Int -> Color
convertTipoColor x
  | x == 0 = green
  | x == 1 = red
  | x == 2 = yellow
  | x == 3 = blue
  | otherwise = orange

--------------------------------------------------------------
---- Entradas e updates do jogo
pressiona :: GuitarState -> Int -> Bool -> GuitarState
pressiona (State notas pont pressionado tempo vel) x b
  | b = up x $ newstate
  | otherwise = newstate
    where
       newstate = (State notas pont (setelt x 0 pressionado b) tempo vel)

up :: Int -> GuitarState -> GuitarState
up tipo (State notas pont pres tempo vel)
  | acertouNota tempo notas tipo = (State notas (pont+10) pres tempo vel)
  | otherwise = (State notas (pont-5) pres tempo vel)

acertouNota :: Float -> [NotaBotao] -> Int -> Bool
acertouNota _ [] _ = False
acertouNota tempo ((Botao (ntempo,ntipo) posY):xs) tipo
  | acertou tempo (Botao (ntempo,ntipo) posY) tipo = True
  | otherwise = acertouNota tempo (xs) tipo

acertou :: Float -> NotaBotao -> Int -> Bool
acertou tempo ((Botao (ntempo,ntipo) posY)) tipo = (near tempo ntempo) && (ntipo == tipo)

near :: Float -> Float -> Bool
near x y = x>(y-0.3) && x<(y+0.3)

setelt :: Int -> Int -> [a] -> a -> [a]
set _ _ [] _ = []
setelt x y (k:ks) v
  | x == y = v:ks
  | otherwise = k:(setelt x (y+1) ks v)

input :: Event -> GuitarState -> GuitarState
input (EventKey (Char k) t _ _) s
  | k == 'a' = pressiona s 0 (t==Down)
  | k == 's' = pressiona s 1 (t==Down)
  | k == 'd' = pressiona s 2 (t==Down)
  | k == 'k' = pressiona s 3 (t==Down)
  | k == 'l' = pressiona s 4 (t==Down)
  | otherwise = s
input _ s = s


update :: Float -> GuitarState -> GuitarState
update f (State notas pont pressionado tempo vel) = (State (desceNotas notas vel tempo) pont pressionado (tempo+fixedTime) vel)

desceNotas :: [NotaBotao] -> Float -> Float -> [NotaBotao]
desceNotas [] _ _ = []
desceNotas ((Botao nota posY):xs) vel tempo
  | posY == -401 = (checkDesce (Botao nota posY) tempo vel) : (desceNotas xs vel tempo)
  | posY < 0 = (Botao nota (-404)):(desceNotas xs vel tempo)
  | otherwise = (Botao nota (posY-(fixedTime*vel))):(desceNotas xs vel tempo)

checkDesce :: NotaBotao -> Float -> Float -> NotaBotao
checkDesce (Botao (tnota,tipo) posY) tempo vel
  | ((tempo + (tempoCair vel)) >= tnota) = (Botao (tnota,tipo) tamanhoY)
  | otherwise = (Botao (tnota,tipo) posY)

main :: IO ()
main = play window black fps inicial convertState input update
