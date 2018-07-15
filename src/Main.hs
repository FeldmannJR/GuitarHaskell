module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display

import Musicas.Musicas
import Musicas.Musica

data GuitarState = State{
  notas :: [NotaBotao],
  score :: GameScore,
  pressionado :: [Bool],
  tempo :: Float,
  mus :: Musica
} | PreGame {
  musica :: Musica
} | PosGame {
  score :: GameScore
}
data GameScore = Score{
  pont :: Int,
  streak :: Int,
  erros :: Int,
  perdidas :: Int,
  acertos :: Int
}
data NotaBotao = Botao{
  nota :: Nota,
  posY :: Float
}


tamanhoBotao :: Float
tamanhoBotao = 20;

tamanhoNota :: Float
tamanhoNota = 18;

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
inicial = PreGame
  {
    musica = musicas!!0
  }

loadSong :: Musica -> GuitarState
loadSong (Mus nome vel notas) = State
  { notas = (convertNotaToButton notas vel)
  , score = (Score 0 0 0 0 0)
  , pressionado = [False,False,False,False,False]
  , tempo = 0
  , mus = (Mus nome vel notas)
}
-- Converte qualquer musica para os objetos dos botoes
convertNotaToButton :: [Nota] -> Float -> [NotaBotao]
convertNotaToButton [] _ = []
convertNotaToButton ((Not tempo tipo dur):xs) vel = (Botao (Not ((tempoCair vel)+tempo) tipo dur) (-401)) : (convertNotaToButton xs vel)


----------------------------------------------------
--Funções para renderizar algo na tela
--Converte todo o jogo para uma imagem
convertState :: GuitarState -> Picture
convertState (State (notas) (pont) (pressionado) (tempo) (Mus _ vel _)) = pictures [botoes pressionado,(convertNotas ((pictures [])) notas), drawTempo tempo vel, drawScore pont]
convertState (PreGame (Mus nome vel _)) = pictures
  [
  translate (fx 40) (fy 500) $ color red $ scale 0.2 0.2 $ text (nome),
  translate (fx 40) (fy 450) $ color white $ scale 0.2 0.2 $ text "Use as setinhas para escolher a musica",
  translate (fx 40) (fy 100) $ color white $ scale 0.25 0.25 $ text "Aperte espaco para comecar"
  ]
convertState (PosGame (Score pont _ _ _ _)) = pictures
  [
  translate (fx 40) (fy 500) $ color red $ scale 0.5 0.5 $ text ("Score " ++ (show pont))
  ]


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
     -- Linha do meio
     translate posX 0 $ color (greyN 0.2) $ rectangleSolid 1 tamanhoY,
     -- Borda botao
     translate (posX) (posBotaoY) $ color white $ circleSolid (tamanhoBase*1.1),
     --Botao
     translate (posX) (posBotaoY) $ color (convertTipoColor x) $ circleSolid tamanhoBase,
     --Parte de dentro do botao
     translate (posX) (posBotaoY) $ color (convertBoolToColor pres x) $ circleSolid (tamanhoBase*0.5),
     -- Linha dos lados
     mkLinha (posX + tamanhoBotao + 8)
   ]
   where
     tamanhoBase = (tamanhoBot pres 1.2)
     posX = posBotaoX x
--Converte uma nota(bolinhas caindo) para uma imagem
tamanhoBot :: Bool -> Float -> Float
tamanhoBot b f
  | b = tamanhoBotao*f
  | otherwise = tamanhoBotao

convertNota :: NotaBotao -> Picture
convertNota (Botao (Not tempo tipo dur) posY) = pictures
  [
    translate px py $ color (light (convertTipoColor tipo)) $ circleSolid tamanhoNota,
    translate px py $ color black $ circleSolid (tamanhoNota*0.6),
    translate px py $ color white $ circleSolid (tamanhoNota*0.5)
  ]
  where
    px = (posBotaoX tipo)
    py = ((-(tamanhoY/2))+posY)
--Convert todas as bolinhas caindo para uma imagem
convertNotas :: Picture -> [NotaBotao] -> Picture
convertNotas p [] = p
convertNotas p ((Botao x y):xs)
  | y > -100 = convertNotas (pictures [(convertNota (Botao x y)), p]) xs
  |otherwise = convertNotas p xs


drawTempo :: Float -> Float-> Picture
drawTempo tempo vel = color red $ translate (posBotaoX 5) (fy tamanhoY-30) $ scale 0.2 0.2 $ text $ (show tempo)

drawScore :: GameScore -> Picture
drawScore (Score sc streak _ _ _) = pictures
  [
    color blue $ translate ((posBotaoX 5)+10) (fy 50) $ scale 0.3 0.3 $ text "Score",
    color white $ translate ((posBotaoX 5)+9) (fy 20) $ scale 0.2 0.2 $ text $ show sc,
    color red $ translate ((posBotaoX 5)+10) (fy 150) $ scale 0.3 0.3 $ text "Streak",
    color white $ translate ((posBotaoX 5)+9) (fy 120) $ scale 0.2 0.2 $ text $ show streak
  ]


convertBoolToColor :: Bool -> Int -> Color
convertBoolToColor b x
    | b =  dim $ dim $ dim $ cor
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
up tipo (State notas (Score pont streak acertos erros perdidas) pres tempo vel)
  | acertouNota tempo notas tipo = (State (removeAcertou tempo tipo notas) (Score (pont+15) (streak+1) erros (acertos+1) perdidas) pres tempo vel)
  | otherwise = (State notas (Score (pont-5) (streak) (erros+1) (acertos) perdidas) pres tempo vel)

acertouNota :: Float -> [NotaBotao] -> Int -> Bool
acertouNota _ [] _ = False
acertouNota tempo (x:xs) tipo
  | acertou tempo x tipo = True
  | otherwise = acertouNota tempo (xs) tipo

acertou :: Float -> NotaBotao -> Int -> Bool
acertou tempo ((Botao (Not ntempo ntipo dur) posY)) tipo = (near posY) && (ntipo == tipo)

removeAcertou ::  Float -> Int-> [NotaBotao] -> [NotaBotao]
removeAcertou _ _ [] = []
removeAcertou tempo tipo (b:bs)
  | acertou tempo b tipo = (removeAcertou tempo tipo bs)
  | otherwise = b : (removeAcertou tempo tipo bs)

near :: Float -> Bool
near x = x>((tamanhoBotao*2)-20) && x<((tamanhoBotao*2)+40)

setelt :: Int -> Int -> [a] -> a -> [a]
set _ _ [] _ = []
setelt x y (k:ks) v
  | x == y = v:ks
  | otherwise = k:(setelt x (y+1) ks v)

input :: Event -> GuitarState -> GuitarState
input (EventKey (Char k) t _ _) (State a b c d e)
  | k == 'a' = pressiona (State a b c d e) 0 (t==Down)
  | k == 's' = pressiona (State a b c d e) 1 (t==Down)
  | k == 'd' = pressiona (State a b c d e) 2 (t==Down)
  | k == 'k' = pressiona (State a b c d e) 3 (t==Down)
  | k == 'l' = pressiona (State a b c d e) 4 (t==Down)
  | otherwise = (State a b c d e)
input (EventKey (SpecialKey KeySpace) t _ _) (PreGame mus) = loadSong mus
input (EventKey (SpecialKey KeyRight) Down _ _) (PreGame mus) = (PreGame (nextMusica mus))
input (EventKey (SpecialKey KeyLeft) Down _ _) (PreGame mus) = (PreGame (prevMusica mus))
input (EventKey (SpecialKey KeySpace) t _ _) (PosGame score) = inicial
input _ s = s

ultimaNota :: [NotaBotao] -> Float
ultimaNota [] = 0.0
ultimaNota ((Botao (Not n_tempo n_tipo n_dur) n_y):xs) = max (n_tempo) (ultimaNota xs)

checkFim :: GuitarState -> GuitarState
checkFim (State notas score press tempo vel)
  | ((ultimaNota notas)+3) < tempo = (PosGame score)
  | otherwise = (State notas score press tempo vel)


update :: Float -> GuitarState -> GuitarState
update f (State notas pont pressionado tempo (Mus nome vel m_notas)) = checkFim $ checkPerdidas (State notas pont pressionado tempo (Mus nome vel m_notas)) $ (State (desceNotas notas vel tempo) pont pressionado (tempo+fixedTime) (Mus nome vel m_notas))
update f s = s


checkPerdidas :: GuitarState -> GuitarState -> GuitarState
checkPerdidas (State n1 _ _ _ _) (State n2 (Score pont streak erros acertos perdidas) pres tempo mus) = (State n2 (Score (pont-(perdidas*5)) (getStreak perdidas streak) (erros) acertos (perdidas+1)) pres tempo mus)
  where
    perdidas =((countPerdidas n2)-(countPerdidas n1));

getStreak :: Int -> Int -> Int
getStreak perdidas streak
  | perdidas > 0 = 0
  | otherwise = streak

countPerdidas :: [NotaBotao] -> Int
countPerdidas [] = 0
countPerdidas ((Botao nota posY):xs)
  | posY == -404 = 1 + countPerdidas xs
  | otherwise = countPerdidas xs

desceNotas :: [NotaBotao] -> Float -> Float -> [NotaBotao]
desceNotas [] _ _ = []
desceNotas ((Botao nota posY):xs) vel tempo
  | posY == -401 = (checkDesce (Botao nota posY) tempo vel) : (desceNotas xs vel tempo)
  | posY < -10 = (Botao nota (-404)):(desceNotas xs vel tempo)
  | otherwise = (Botao nota (posY-(fixedTime*vel))):(desceNotas xs vel tempo)

checkDesce :: NotaBotao -> Float -> Float -> NotaBotao
checkDesce (Botao (Not tnota tipo dur) posY) tempo vel
  | ((tempo + (tempoCair vel)) >= tnota) = (Botao (Not tnota tipo dur) tamanhoY)
  | otherwise = (Botao (Not tnota tipo dur) posY)

main :: IO ()
main = play window black fps inicial convertState input update
