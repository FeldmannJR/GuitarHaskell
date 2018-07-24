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

import Musicas.Musicas
import Musicas.Musica

import Common
import PreGame
import Game
import Pause
import PosGame


removeMaybe:: IO (Maybe a) -> IO a
removeMaybe = (>>= maybe (ioError $ userError "oops") return)

----------------------------------------------------
--Funções para renderizar algo na tela
--Converte todo o jogo para uma imagem
convertState :: GuitarState -> IO Picture
convertState (PreGame mus) = convertPreGame (PreGame mus)
convertState (State (notas) (pont) (pressionado) (tempo) mus song) = convertGame (State (notas) (pont) (pressionado) (tempo) mus song)
convertState (Pause state menu volta) = convertPause (Pause state menu volta)
convertState (PosGame sc mus) = convertPosGame (PosGame sc mus)


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


update :: Float -> GuitarState -> IO GuitarState
update f (State notas pont pressionado tempo mus song) = updateGame f (State notas pont pressionado tempo mus song)
update f (Pause state menu volta) = updatePause (Pause state menu volta)
update f s = do return s



main :: IO ()
main = do
    result <- initAudio 64 44100 1024;
    playIO window black fps inicial convertState input update
