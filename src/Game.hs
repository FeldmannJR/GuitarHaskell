module Game where

--
import Control.Monad
import System.Environment
import System.FilePath
import System.Exit
import Data.ByteString

import Control.Concurrent
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Juicy
import Sound.ProteaAudio

import Musicas.Musica
import Musicas.Musicas
import Common


-- ========================== UPDATE
testaSom :: GuitarState -> IO GuitarState
testaSom (State notas pont pressionado tempo (Mus name fname delay vel fn) song) = do
    if ((song==False && tempo >= delay)==False) then
        return  (State notas pont pressionado tempo (Mus name fname delay vel fn) song)
     else do
        playMusica fname;
        return (State notas pont pressionado tempo (Mus name fname delay vel fn) True)
testaSom s = do (return s)

checkPerdidas :: GuitarState -> GuitarState -> GuitarState
checkPerdidas (State n1 _ _ _ _ _) (State n2 (Score pont streak erros acertos perdidas) pres tempo mus song) = (State n2 (Score (pont-(perdidas*10)) (getStreak perdidas streak) (erros) acertos (perdidas+1)) pres tempo mus song)
  where
    perdidas =((countPerdidas n2)-(countPerdidas n1));

getStreak :: Int -> Int -> Int
getStreak perdidas streak
  | perdidas > 0 = 0
  | otherwise = streak

countPerdidas :: [NotaBotao] -> Int
countPerdidas [] = 0
countPerdidas ((Botao nota posY):xs)
  | posY == errou = 1 + countPerdidas xs
  | otherwise = countPerdidas xs

-- Grande
tamanhoBola :: Float -> NotaBotao -> Float
tamanhoBola vel (Botao (Not n_tempo n_tipo n_dur) posY) = (max ((tempoToPx vel n_dur)) 0)

desceNotas :: [NotaBotao] -> Float -> Float -> [NotaBotao]
desceNotas [] _ _ = []
desceNotas ((Botao nota posY):xs) vel tempo
  | posY == praCair = (checkDesce (Botao nota posY) tempo vel) : (desceNotas xs vel tempo)
  | posY == errou =  ((Botao nota posY)): (desceNotas xs vel tempo)
  | (posY+tamanho) < -10 = (Botao nota (errou)):(desceNotas xs vel tempo)
  | otherwise = (Botao nota (posY-(fixedTime*vel))):(desceNotas xs vel tempo)
    where
       tamanho = (tamanhoBola vel (Botao nota posY))

ultimaNota :: [NotaBotao] -> Float
ultimaNota [] = 0.0
ultimaNota ((Botao (Not n_tempo n_tipo n_dur) n_y):xs) = max (n_tempo) (ultimaNota xs)

checkFim :: GuitarState -> GuitarState
checkFim (State notas score press tempo mus song)
  | ((ultimaNota notas)+3) < tempo = (PosGame score mus)
  | otherwise = (State notas score press tempo mus song)

checkDesce :: NotaBotao -> Float -> Float -> NotaBotao
checkDesce (Botao (Not tnota tipo dur) posY) tempo vel
  | ((tempo + (tempoCair vel)) >= tnota) = (Botao (Not tnota tipo dur) tamanhoY)
  | otherwise = (Botao (Not tnota tipo dur) posY)

--
updateGame :: Float -> GuitarState -> IO GuitarState
updateGame f (State notas pont pressionado tempo (Mus nome fnome delay vel m_notas) song) = testaSom ( checkFim $ checkPerdidas (State notas pont pressionado tempo (Mus nome fnome delay vel m_notas) song) $ (State (desceNotas notas vel tempo) pont pressionado (tempo+fixedTime) (Mus nome fnome delay vel m_notas) song))

-- =================================================== GRAPHICS
-- Converte os botões de baixo ( que são ativavies para imagem)
botoes :: [Bool] -> Picture
botoes b = pictures
  [
    mkLinha (((posBotaoX 0) - tamanhoBotao - 8)),
    mkBotao (b!!0) 'A' 0,
    mkBotao (b!!1) 'S' 1,
    mkBotao (b!!2) 'D' 2,
    mkBotao (b!!3) 'K' 3,
    mkBotao (b!!4) 'L' 4
  ]
-- Gera uma linha branca em tal X
mkLinha :: Float -> Picture
mkLinha x = translate x 0 $ color white $ rectangleSolid 2 tamanhoY

-- Gera um botão de apertar Tipo
mkBotao :: Bool -> Char ->  Int -> Picture
mkBotao pres c x = pictures
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
     mkLinha (posX + tamanhoBotao + 8),
     --Letra
     translate (posX-4) (posBotaoY-(tamanhoBotao*1.9)) $ scale 0.09 0.09 $ color white $ text [c]
   ]
   where
     tamanhoBase = (tamanhoBot pres 1.2)
     posX = posBotaoX x

tamanhoBot :: Bool -> Float -> Float
tamanhoBot b f
  | b = tamanhoBotao*f
  | otherwise = tamanhoBotao
--Converte uma nota(bolinhas caindo) para uma imag1em
convertNota :: Float -> NotaBotao -> Picture
convertNota vel (Botao (Not tempo tipo dur) posY) = pictures
  [
    translate px (py+(rastro/2)) $ color black $ rectangleSolid 8 (rastro+2),
    translate px (py+(rastro/2)) $ color (convertTipoColor tipo) $ rectangleSolid 6 rastro,
    translate px py $ color  (convertTipoColor tipo) $ circleSolid tamanhoNota,
    translate px py $ color black $ circleSolid (tamanhoNota*0.6),
    translate px py $ color white $ circleSolid (tamanhoNota*0.5)
  ]
  where
    px = (posBotaoX tipo)
    py = ((-(tamanhoY/2))+posY)
    rastro =  (max ((tempoToPx vel dur)-tamanhoNota-10) 0)

--Convert todas as bolinhas caindo para uma imagem
convertNotas :: Picture -> Float -> [NotaBotao] -> Picture
convertNotas p _ [] = p
convertNotas p vel ((Botao x y):xs)
  | ((y /= errou) && (y /= praCair)) = convertNotas (pictures [(convertNota vel (Botao x y)), p]) vel xs
  |otherwise = convertNotas p vel xs


drawTempo :: Float -> [NotaBotao] -> Picture
drawTempo tempo notas = color red $ translate (posBotaoX 5) (fy tamanhoY-30) $ scale 0.2 0.2 $ text $ (show $ roundN ((ultimaNota notas)-tempo+3) 1)


drawScore :: GameScore -> Picture
drawScore (Score sc streak _ _ _) = pictures
  [
    color blue $ translate ((posBotaoX 5)+10) (fy 50) $ scale 0.3 0.3 $ text "Score",
    color white $ translate ((posBotaoX 5)+9) (fy 20) $ scale 0.2 0.2 $ text $ show sc,
    color red $ translate ((posBotaoX 5)+10) (fy 150) $ scale 0.3 0.3 $ text "Streak",
    color white $ translate ((posBotaoX 5)+9) (fy 120) $ scale 0.2 0.2 $ text $ show streak
  ]

convertGame :: GuitarState -> IO Picture
convertGame (State (notas) (pont) (pressionado) (tempo) (Mus _ _ _ vel _) _) = do return $ pictures [botoes pressionado,(convertNotas ((pictures [])) vel notas), drawTempo tempo notas, drawScore pont]


-- ============================= I/O
pause :: GuitarState -> IO GuitarState
pause a = do
  soundStopAll
  return (Pause a 0 (-1))

pressiona :: GuitarState -> Int -> Bool -> GuitarState
pressiona (State notas pont pressionado tempo vel song) x b
  | b = up x $ newstate
  | otherwise = newstate
    where
       newstate = (State notas pont (setelt x 0 pressionado b) tempo vel song)

up :: Int -> GuitarState -> GuitarState
up tipo (State notas (Score pont streak acertos erros perdidas) pres tempo vel song)
  | acertouNota tempo notas tipo = (State (removeAcertou tempo tipo notas) (Score (pont+10) (streak+1) erros (acertos+1) perdidas) pres tempo vel song)
  | otherwise = (State notas (Score (pont-10) (streak) (erros+1) (acertos) perdidas) pres tempo vel song)

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


getNumber :: Char -> Int
getNumber k
    | k == 'a' = 0
    | k == 's' = 1
    | k == 'd' = 2
    | k == 'k' = 3
    | k == 'l' = 4
    | otherwise = -1
inputGame :: Event -> GuitarState -> IO GuitarState
inputGame (EventKey (SpecialKey KeyEsc) Down _ _) (State a b c d e f) = (pause (State a b c d e f))
  -- Entrada botoes
inputGame (EventKey (Char k) t _ _) (State a b c d e f) = do
        if (getNumber k >= 0) then return (pressiona (State a b c d e f) (getNumber k) (t==Down))
        else do return (State a b c d e f);
inputGame _ s = do return s
