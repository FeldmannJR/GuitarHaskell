module Musicas.Musica where

import qualified Data.ByteString as SB
import Sound.ProteaAudio
import Data.Cache as C

data Nota = Not{
  n_tempo :: Float,
  n_tipo :: Int,
  n_duracao :: Float
}
data Musica = Mus{
  --Quantos px por segundo a bolinha cai
  nome :: String,
  f_name :: String,
  delay :: Float,
  mus_vel :: Float,
  mus_notas :: [Nota]
}
instance Eq Musica where
  (==) (Mus n1_nome _ _ _ _) (Mus n2_nome _ _ _ _) = n1_nome == n2_nome

loadSample :: Musica -> IO Sample
loadSample (Mus _ a _ _ _) = do
    putStrLn ("Carregando 1"++a)
    sample <- (sampleFromFile ("./audio/"++a) 1)
    putStrLn ("Carregando3 "++a)
    return sample
