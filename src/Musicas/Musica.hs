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

loadS :: String -> IO Sample
loadS a = do
    putStrLn ("Carregando 1"++a)
    sample <- (sampleFromFile ("./audio/"++a) 1)
    putStrLn ("Carregando3 "++a)
    return sample

cacheReturn :: String -> IO Sample
cacheReturn s = do
  mus <- (loadS s);
  c <- cache
  (insert c s mus);
  return mus;

loadSample :: String -> IO Sample
loadSample a =  do
  c <- cache
  teste <- (C.lookup c a);
  case teste of
    Just v -> return v
    Nothing -> (cacheReturn a)


cache = newCache Nothing :: IO (Cache String (Sample))
