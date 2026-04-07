module Tarefa0_2025 where 

import Data.Char () 

import Tarefa0_geral ( Direcao, Matriz, Posicao )



{-- module Labs2025
      (module Labs2025
      , module Tarefa0_geral
      ) where 
--}
data Terreno 
    = Ar
    | Agua 
    | Terra 
    | Pedra 
    deriving (Eq,Ord, Show, Read, Enum)



type Mapa = Matriz Terreno  

data TipoArma = Jetpack | Escavadora | Bazuca | Mina | Dinamite 
     deriving (Eq,Ord, Show, Read, Enum)

data VidaMinhoca 
    = Viva Int 
    | Morta    
    deriving (Eq,Ord,Show,Read)  

data Minhoca = Minhoca
    -- | Uma posição no mapa. Opcional porque a minhoca pode ter saído do mapa.
    { posicaoMinhoca :: Maybe Posicao
    -- | O estado de saúde da minhoca.
    , vidaMinhoca :: VidaMinhoca
    -- | Munições de @Jetpack@.
    , jetpackMinhoca :: Int
    -- | Munições de @Escavadora@.
    , escavadoraMinhoca :: Int
    -- | Munições de @Bazuca@.
    , bazucaMinhoca :: Int
    -- | Munições de @Mina@.
    , minaMinhoca :: Int
    -- | Munições de @Dinamite@.
    , dinamiteMinhoca :: Int
    }
    deriving (Eq,Ord,Show,Read)

-- | Um tick é a unidade de tempo do jogo.
type Ticks = Int

-- | O índice de uma minhoca na lista de minhocas.
type NumMinhoca = Int

-- | O índice de um objeto na lista de objetos.
type NumObjeto = Int

-- | Um objeto colocado no mapa.
data Objeto
    -- | Um disparo de uma arma.
    = Disparo
        -- | A posição do disparo no mapa.
        { posicaoDisparo :: Posicao
        -- | A direção do disparo.
        , direcaoDisparo :: Direcao
        -- | O tipo de arma do disparo.
        , tipoDisparo :: TipoArma
        -- | O tempo até o disparo explodir. Opcional porque nem todos os disparos de todas as armas têm um tempo pré-definido para explodir.
        , tempoDisparo :: Maybe Ticks
        -- | A minhoca que efetuou o disparo.
        , donoDisparo :: NumMinhoca
        }
    -- | Um barril de pólvora.
    | Barril
        -- | A posição do barril no mapa.
        { posicaoBarril :: Posicao
        -- | Se o barril está prestes a explodir ou não.
        , explodeBarril :: Bool
        }
    deriving (Eq,Ord, Show, Read) 

-- | Estado do jogo.
data Estado = Estado
    -- | O mapa atual.
    { mapaEstado :: Mapa
    -- | Uma lista com os objetos presentes no mapa. Para as funções que vai desenvolver, deve considerar que a ordem dos elementos é irrelevante.
    , objetosEstado :: [Objeto]
    -- | Uma lista com as minhocas no jogo. A ordem dos elementos é relevante, no sentido cada minhoca vai ser identificada pelo seu índice na lista.
    , minhocasEstado :: [Minhoca]
    }
    deriving (Eq,Ord, Show, Read)

-- | Uma jogada que uma minhoca pode efetuar.
data Jogada
    -- | Disparar uma arma numa dada direção.
    = Dispara TipoArma Direcao
    -- | Mover-se numa dada direção.
    | Move Direcao
    deriving  (Eq,Ord, Show, Read)



encontraQuantidadeArmaMinhoca :: TipoArma -> Minhoca -> Int
encontraQuantidadeArmaMinhoca Jetpack m = jetpackMinhoca m
encontraQuantidadeArmaMinhoca Escavadora m = escavadoraMinhoca m
encontraQuantidadeArmaMinhoca Bazuca m = bazucaMinhoca m
encontraQuantidadeArmaMinhoca Mina m = minaMinhoca m
encontraQuantidadeArmaMinhoca Dinamite m = dinamiteMinhoca m




atualizaArma :: TipoArma -> Minhoca -> Int -> Minhoca 
atualizaArma  Jetpack m qtd = m {jetpackMinhoca = qtd}
atualizaArma  Escavadora m qtd = m {escavadoraMinhoca = qtd}
atualizaArma  Bazuca m qtd = m {bazucaMinhoca = qtd}
atualizaArma  Mina m qtd= m {minaMinhoca = qtd}
atualizaArma  Dinamite m qtd = m {dinamiteMinhoca = qtd}


eTerrenoDestrutivel :: Terreno -> Bool 
eTerrenoDestrutivel Ar = False 
eTerrenoDestrutivel Agua = False 
eTerrenoDestrutivel Terra = True 
eTerrenoDestrutivel Pedra = False 

eTerrenoOpaco :: Terreno -> Bool
eTerrenoOpaco terreno = if terreno == Terra || terreno == Pedra then True else False 

ePosicaoMapaLivre :: Posicao -> Mapa -> Bool
ePosicaoMapaLivre _ [] = False 
ePosicaoMapaLivre (x,y) mapa | x < 0 || x >= length (mapa !! y) = False 
                             | y < 0 || y>= length mapa = False 
                             | otherwise = not (eTerrenoOpaco ((mapa !! y) !! x)) 

ePosicaoEstadoLivre :: Posicao -> Estado -> Bool 
ePosicaoEstadoLivre pos (Estado mapa objetos minhocas) = ePosicaoMapaLivre pos mapa && not (posTemMinhoca pos minhocas) && not (posTemBarril pos objetos)
                                                          where posTemMinhoca :: Posicao -> [Minhoca] -> Bool 
                                                                posTemMinhoca _ [] = False 
                                                                posTemMinhoca p (m:resto) = case posicaoMinhoca m of 
                                                                    Just posM -> p == posM || posTemMinhoca p resto
                                                                    Nothing -> posTemMinhoca p resto 
                                                                
                                                                posTemBarril :: Posicao -> [Objeto] -> Bool 
                                                                posTemBarril _ [] = False 
                                                                posTemBarril p (Barril posB _:resto) | p == posB = True 
                                                                                                     | otherwise = posTemBarril p resto

                                                                                    


minhocaTemDisparo :: TipoArma -> NumMinhoca -> [Objeto] -> Bool 
minhocaTemDisparo _ _ [] = False 
minhocaTemDisparo arma numM (Disparo _ _ tipoArma _ dono:resto) | arma == tipoArma && numM == dono = True 
                                                                | otherwise = minhocaTemDisparo arma numM resto 
minhocaTemDisparo arma numM (_:resto) = minhocaTemDisparo arma numM resto                                                                 


destroiPosicao :: Posicao -> Mapa -> Mapa 
destroiPosicao (x,y) mapa | x < 0 || x >= length (mapa !! y) = mapa 
                          | y < 0 || y >= length mapa = mapa 
                          | otherwise = take y mapa ++ [novaLinha] ++ drop (y +1) mapa 
                            where 
                                linha = mapa !! y 
                                novaLinha = take x linha ++ [novoTerreno] ++ drop (x+1) linha 
                                terrenoAtual = linha !! x 
                                novoTerreno = if eTerrenoDestrutivel terrenoAtual then Ar else terrenoAtual


adicionaObjeto :: Objeto -> Estado -> Estado 
adicionaObjeto objeto ( Estado mapa objetos minhocas ) = Estado mapa (objeto:objetos) minhocas 

