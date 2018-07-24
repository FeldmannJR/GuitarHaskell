module Musicas.Musicas where

import Musicas.Musica
import Musicas.SeekAndDestroy
import Musicas.RisingFighting
import Musicas.YYZ
import Musicas.DeusEhGay
import Musicas.Satanismo
import Musicas.CalaABoca

musicas :: [Musica]
musicas =[calaaboca,deusEhGay,satanismo,seekAndDestroy,risingFighting,yyz]

nextMusica :: Musica -> Musica
nextMusica s = nextMus s musicas

prevMusica :: Musica -> Musica
prevMusica s = prevMus s musicas


nextMus :: Musica -> [Musica] -> Musica
nextMus s [] = s
nextMus s [a] = musicas!!0
nextMus s (x:xs:xss)
  | x == s = xs
  | otherwise = nextMus s (xs:xss)


prevMus :: Musica -> [Musica] -> Musica
prevMus s [] = musicas!!((length musicas)-1)
prevMus s [a] = musicas!!((length musicas)-1)
prevMus s (x:xs:xss)
  | xs == s = x
  | otherwise = prevMus s (xs:xss)
