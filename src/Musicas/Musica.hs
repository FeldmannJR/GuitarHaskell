module Musicas.Musica where

data Nota = Not{
  n_tempo :: Float,
  n_tipo :: Int,
  n_duracao :: Float
}
data Musica = Mus{
  --Quantos px por segundo a bolinha cai
  nome :: String,
  mus_vel :: Float,
  mus_notas :: [Nota]

}
instance Eq Musica where
  (==) (Mus n1_nome _ _) (Mus n2_nome _ _ ) = n1_nome == n2_nome
