module Musica where

  data Nota = N Float Int
    deriving Show

  notas :: [Nota]
  notas = [N 1.0 0, N 2.0 3]