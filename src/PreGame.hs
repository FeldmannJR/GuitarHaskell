
module PreGame where

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
import Common as C




-- Carrega musica
loadSong :: Musica -> GuitarState
loadSong (Mus nome fnome delay vel notas) = State
  { notas = (convertNotaToButton notas vel)
  , score = (Score 0 0 0 0 0)
  , pressionado = [False,False,False,False,False]
  , tempo = 0
  , mus = (Mus nome fnome delay vel notas)
  , tocouSong = False
}
-- Converte qualquer musica para os objetos dos botoes
convertNotaToButton :: [Nota] -> Float -> [NotaBotao]
convertNotaToButton [] _ = []
convertNotaToButton ((Not tempo tipo dur):xs) vel
    | dur < 0.3  =  (Botao (Not (tempo) tipo 0) (praCair) False (-1)) : (convertNotaToButton xs vel)
    | otherwise =  (Botao (Not (tempo) tipo dur) (praCair) False (-1)) : (convertNotaToButton xs vel)


-- GLOSS
convertPreGame ::  Picture -> GuitarState -> IO Picture
convertPreGame logo (PreGame (Mus nome fnome _ vel _)) = do
  return $ pictures
       [
       translate 0 300 $ scale 0.5 0.5 $ logo,
       translate (fx 40) (fy (tamanhoY-350)) $ color red $ scale 0.2 0.2 $ text (nome),
       translate (fx 40) (fy (tamanhoY-300)) $ color white $ scale 0.2 0.2 $ text "Use as setinhas para escolher a musica",
       translate (fx 50) (fy 100) $ color white $ scale 0.25 0.25 $ text "Aperte espaco para comecar",
       mkPreBotao "Tecla A" 0,
       mkPreBotao "Tecla S" 1,
       mkPreBotao "Tecla J" 2,
       mkPreBotao "Tecla K" 3,
       mkPreBotao "Tecla L" 4
      ]

-- Botao de ajuda no menu
mkPreBotao :: String -> Int-> Picture
mkPreBotao txt tipo= pictures

  [
       translate (x) (y) $ color white $ circleSolid (tamanhoBotao*1.1),
       translate (x) (y) $ color (convertTipoColor tipo) $ circleSolid tamanhoBotao,
       translate (x) (y) $ color (convertBoolToColor False tipo) $ circleSolid (tamanhoBotao*0.5),
       translate ((tamanhoBotao*2)+x+15) (y-10) $ color white $ scale 0.2 0.2 $ text txt
  ]
  where
    x = (fx 100)
    y = (fy ((tamanhoY-650)+(fromIntegral ((4-tipo)*55))))

-- Io
inputPreGame :: Event -> GuitarState -> IO GuitarState
inputPreGame (EventKey (SpecialKey KeyEsc) Down _ _) (PreGame mus) = do exitSuccess
  -- Space começar jogo
inputPreGame (EventKey (SpecialKey KeySpace) Down _ _) (PreGame mus) = do
    return $ loadSong mus
  -- Seta direito prox musica
inputPreGame (EventKey (SpecialKey KeyRight) Down _ _) (PreGame mus) = do return (PreGame (nextMusica mus))
  -- Seta esquerda musica anterior
inputPreGame (EventKey (SpecialKey KeyLeft) Down _ _) (PreGame mus) = do return (PreGame (prevMusica mus))
inputPreGame _ s = do return s
-- Update
