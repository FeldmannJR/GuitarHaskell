
module PosGame where

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
import PreGame




convertPosGame :: GuitarState -> IO Picture
convertPosGame (PosGame (Score pont _ _ _ _) (Mus nome _ _ _ _)) = do
    return $ pictures
        [
        translate (fx 50) (fy 550) $ color green $ scale 0.2 0.2 $ text nome,
        translate (fx 100) (fy 200) $ color red $ scale 0.3 0.3 $ text ("Score " ++ (show $ pont-1))
        ]

-- Io
inputPosGame :: Event -> GuitarState -> IO GuitarState
inputPosGame (EventKey (SpecialKey KeySpace) Up _ _) (PosGame score mus) = do return (PreGame mus)
inputPosGame (EventKey (SpecialKey KeyEsc) Down _ _) (PosGame score mus) = do exitSuccess
inputPosGame _ s = do return s
