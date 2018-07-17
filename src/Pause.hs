module Pause where

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

import Musicas.Musicas
import Musicas.Musica

import Common
import Game

opMenu = 3

avancaMenu :: Int -> Int 
avancaMenu m
  | m >= (opMenu-1)  = 0
  | otherwise = m+1

voltaMenu :: Int -> Int
voltaMenu m
    | m == 0  = opMenu-1
    | otherwise = m-1

getCorMenu :: Bool -> Color
getCorMenu b
  | b = yellow
  | otherwise = white

getOpMenu :: String -> Int -> Int -> Picture
getOpMenu nome num sel = translate (fx 40) (fy (tamanhoY-100-((fromIntegral num)*60))) $ color (getCorMenu (num==sel)) $ scale 0.2 0.2 $ text nome


convertPause :: GuitarState -> IO Picture
convertPause (Pause state menu (-1)) = do
  return $ pictures
    [
      getOpMenu "Voltar para o jogo" 0 menu,
      getOpMenu "Sair para o menu" 1 menu,
      getOpMenu "Sair do jogo" 2 menu

    ]
convertPause (Pause state menu volta) = do
  old <- convertGame state;
  return $ pictures
    [
      old,
      translate (fx (tamanhoX/2)) (fy (tamanhoY/2)) $ color white $ scale 0.4 0.4 $ rectangleSolid 300 200,
      translate (fx (tamanhoX/2)-40) (fy (tamanhoY/2)-20) $ color black $ scale 0.4 0.4 $ text ((show (roundN volta 1)))
    ]


inputPause :: Event -> GuitarState -> IO GuitarState
inputPause (EventKey (SpecialKey KeySpace) Down _ _) (Pause state menu volta) = do
  case menu of
    0 -> do return $ (Pause state menu 3)
    1 -> do return $ inicial
    2 -> exitSuccess
-- Seta baixo
inputPause (EventKey (SpecialKey KeyDown) Down _ _) (Pause state menu volta) = do return (Pause state (avancaMenu menu) volta)
  -- Seta cima
inputPause (EventKey (SpecialKey KeyUp) Down _ _) (Pause state menu volta) = do return (Pause state (voltaMenu menu) volta)

inputPause _ s = do return s


updatePause :: GuitarState -> IO GuitarState
updatePause (Pause state menu volta) = do{
    if(volta==(-1)) then return (Pause state menu volta) else
    if ((volta-fixedTime)<=0) then
      return state
    else do return (Pause state menu (volta-fixedTime))
  }
