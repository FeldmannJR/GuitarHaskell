module Main where

import Control.Monad
import System.Environment
import System.FilePath
import System.Exit

import Control.Concurrent
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Juicy
import Sound.ProteaAudio
import Data.Cache as C

import Musicas.Musicas
import Musicas.Musica

import Common
import PreGame
import Game
import Pause
import PosGame



----------------------------------------------------
--Funções para renderizar algo na tela
--Converte todo o jogo para uma imagem
convertState ::  Picture  -> GuitarState -> IO Picture
convertState logo (PreGame mus) = convertPreGame logo (PreGame mus)
convertState _ (State (notas) (pont) (pressionado) (tempo) mus song) = convertGame (State (notas) (pont) (pressionado) (tempo) mus song)
convertState _ (Pause state menu volta)  = convertPause (Pause state menu volta)
convertState _ (PosGame sc mus)  = convertPosGame (PosGame sc mus)


--------------------------------------------------------------
---- Entradas e updates do jogo
input :: Event -> GuitarState -> IO GuitarState
-- Caso aperte enter ele trata como se fosse espaço
input (EventKey (SpecialKey KeyEnter) a b c) state = input (EventKey (SpecialKey KeySpace) a b c) state
-- ENtrada do jogo
input e (PreGame mus) = inputPreGame e (PreGame mus)
input ev (State a b c d e f) = (inputGame ev (State a b c d e f))
input ev (Pause state menu volta) = inputPause ev (Pause state menu volta)
input ev (PosGame score mus) = inputPosGame ev (PosGame score mus)


update ::  (Cache String (Sample)) -> Float -> GuitarState -> IO GuitarState
update m f (State notas pont pressionado tempo mus song) = updateGame m f (State notas pont pressionado tempo mus song)
update _ f (Pause state menu volta) = updatePause (Pause state menu volta)
update _ f s = do return s

samples :: [Musica] ->  [(Musica,IO Sample)]
samples [] = []
samples (x:xs) =  (x,(loadSample x)):(samples xs)


insertMusicas :: (Cache String (Sample))  -> [Musica] -> IO ()
insertMusicas _ [] = do return ()
insertMusicas ca ((Mus a b c d e):xs) = do
    load <- (loadSample (Mus a b c d e))
    C.insert ca b load
    insertMusicas ca xs
    return ()


main :: IO ()
main = do
    result <- initAudio 64 44100 1024;
    cache <- newCache Nothing :: IO (Cache String ( Sample))
    insertMusicas (cache) (musicas)
    k <- keys cache
    putStrLn $ show k
    logo <- removeMaybe $ loadJuicyPNG "./img/logo.png"
    playIO window black fps inicial (convertState logo) input (update cache)
