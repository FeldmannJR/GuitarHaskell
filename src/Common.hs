module Common  where

  -- Graphics
  import Graphics.Gloss as G
  -- Sound
  import Sound.ProteaAudio

  -- Internal
  import Musicas.Musicas
  import Musicas.Musica

  data GuitarState = State{
    notas :: [NotaBotao],
    score :: GameScore,
    pressionado :: [Bool],
    tempo :: Float,
    mus :: Musica,
    tocouSong :: Bool
  } | PreGame {
    musica :: Musica
  } | PosGame {
    score :: GameScore,
    musica :: Musica
  } | Pause {
    state :: GuitarState,
    menu :: Int,
    volta :: Float
  }
  data GameScore = Score{
    pont :: Int,
    streak :: Int,
    erros :: Int,
    perdidas :: Int,
    acertos :: Int
  }
  data NotaBotao = Botao{
    nota :: Nota,
    posY :: Float,
    nota_acertou :: Bool,
    lastCheck :: Float
  }

  -- State inicial
  inicial :: GuitarState
  inicial = PreGame
    {
      musica = musicas!!0
    }

  tamanhoBotao :: Float
  tamanhoBotao = 20;

  tamanhoNota :: Float
  tamanhoNota = 18;

  tamanhoY :: Float
  tamanhoY = 900;

  tamanhoX :: Float
  tamanhoX = 600;

  fps :: Int
  fps = 120

  --Gambiarra
  praCair :: Float
  praCair = -1000000
  --Gambiarra
  errou :: Float
  errou = -10000100

  fixedTime :: Float
  fixedTime = 1.0 / (fromIntegral fps);

  -- Tempo para a nota cair do topo até o botao
  tempoCair :: Float -> Float
  tempoCair vel = ((tamanhoY-tamanhoBotao*2)/vel)

  -- Converte tempo em pixeis que a nota anda
  tempoToPx :: Float -> Float -> Float
  tempoToPx vel tempo = tempo * vel

  --Converte pixeis da tela para o tempo que a nota passa neles
  pxToTempo :: Float -> Float -> Float
  pxToTempo vel tempo = tempo / vel

  --converte para origin 0 ao em vez de -300
  fx :: Float -> Float
  fx x = (-tamanhoX/2)+x

  --Converte para origem 0 ao em vez de -300
  fy :: Float -> Float
  fy y = (-tamanhoY/2)+y


  --Seta elemento X da lista para outro e retorna a lista modificada
  setelt :: Int -> Int -> [a] -> a -> [a]
  set _ _ [] _ = []
  setelt x y (k:ks) v
    | x == y = v:ks
    | otherwise = k:(setelt x (y+1) ks v)

  posBotaoY :: Float
  posBotaoY = (-(tamanhoY/2)) + tamanhoBotao*2

  posBotaoX :: Int -> Float
  posBotaoX x = (-(tamanhoX)/2)  + (150 + tamanhoBotao + (((tamanhoBotao*2)+ 16) * fromIntegral x))

  roundN :: Float -> Int -> Float
  roundN f n=  (fromInteger $ round $ f * (10^n)) / (10.0^^n)

  playMusica :: String -> IO ()
  playMusica a = do
      soundStopAll
      teste <- loadSample a
      soundPlay teste 1 1 0 1


  convertBoolToColor :: Bool -> Int -> Color
  convertBoolToColor b x
      | b =  dim $ dim $ dim $ cor
      | otherwise = dim $ dim $ dim $ dim $ cor
      where
        cor = (convertTipoColor x)

  convertTipoColor :: Int -> Color
  convertTipoColor x
    | x == 0 = green
    | x == 1 = red
    | x == 2 = yellow
    | x == 3 = blue
    | otherwise = orange

  window :: Display
  window = InWindow "Guitar Haskell" (round (tamanhoX),round (tamanhoY)) (500,10)
