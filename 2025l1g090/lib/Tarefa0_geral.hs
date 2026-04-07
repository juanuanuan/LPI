{-|
Module      : Tarefa0_geral
Description : Tipos de dados auxiliares gerais e funções utilitárias

Este módulo contém tipos de dados básicos e funções auxiliares 
utilizadas em todo o projeto Worms.
-}

module Tarefa0_geral where

-- ============================================================================
-- TIPOS DE DADOS
-- ============================================================================

-- | Uma posição no mapa, representada por (linha, coluna)
type Posicao = (Int, Int)

-- | Uma dimensão representada por (linhas, colunas)
type Dimensao = (Int, Int)

-- | Uma matriz genérica de elementos do tipo 'a'
type Matriz a = [[a]]

-- | As 8 direções cardinais e intercardinais possíveis no jogo
data Direcao
    = Norte
    | Nordeste
    | Este
    | Sudeste
    | Sul
    | Sudoeste
    | Oeste
    | Noroeste
    deriving (Eq, Ord, Show, Read, Enum)

-- ============================================================================
-- FUNÇÕES PARA LISTAS
-- ============================================================================

-- | Verifica se um índice é válido numa lista
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido i lista = i >= 0 && i < length lista

-- | Encontra um elemento numa lista pelo seu índice
encontraIndiceLista :: Int -> [a] -> Maybe a
encontraIndiceLista i lista
    | eIndiceListaValido i lista = Just (lista !! i)
    | otherwise = Nothing

-- | Atualiza um elemento numa lista num dado índice
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista _ _ [] = []
atualizaIndiceLista 0 novo (_:resto) = novo : resto
atualizaIndiceLista i novo (x:resto) = x : atualizaIndiceLista (i-1) novo resto

-- ============================================================================
-- FUNÇÕES PARA MATRIZES
-- ============================================================================

-- | Verifica se uma matriz é válida (todas as linhas têm o mesmo tamanho)
eMatrizValida :: Matriz a -> Bool
eMatrizValida [] = False
eMatrizValida (primeiraLinha:restoLinhas) =
    not (null primeiraLinha) &&
    all (\linha -> length linha == length primeiraLinha) restoLinhas

-- | Obtém a dimensão de uma matriz (número de linhas, número de colunas)
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0, 0)
dimensaoMatriz (primeiraLinha:_) = (length (primeiraLinha : [primeiraLinha]), length primeiraLinha)
dimensaoMatriz matriz = (length matriz, length (head matriz))

-- | Verifica se uma posição é válida numa matriz
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool
ePosicaoMatrizValida (linha, coluna) matriz =
    linha >= 0 && linha < length matriz &&
    coluna >= 0 && coluna < length (head matriz)

-- | Encontra um elemento numa matriz numa dada posição
encontraPosicaoMatriz :: Posicao -> Matriz a -> Maybe a
encontraPosicaoMatriz (linha, coluna) matriz
    | ePosicaoMatrizValida (linha, coluna) matriz = 
        Just ((matriz !! linha) !! coluna)
    | otherwise = Nothing

-- | Atualiza um elemento numa matriz numa dada posição
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (linha, coluna) novo matriz =
    atualizaIndiceLista linha linhaAtualizada matriz
  where
    linhaAtualizada = atualizaIndiceLista coluna novo (matriz !! linha)

-- ============================================================================
-- FUNÇÕES PARA POSIÇÕES E DIREÇÕES
-- ============================================================================

-- | Move uma posição numa dada direção
movePosicao :: Direcao -> Posicao -> Posicao
movePosicao Norte (l, c) = (l - 1, c)
movePosicao Nordeste (l, c) = (l - 1, c + 1)
movePosicao Este (l, c) = (l, c + 1)
movePosicao Sudeste (l, c) = (l + 1, c + 1)
movePosicao Sul (l, c) = (l + 1, c)
movePosicao Sudoeste (l, c) = (l + 1, c - 1)
movePosicao Oeste (l, c) = (l, c - 1)
movePosicao Noroeste (l, c) = (l - 1, c - 1)

-- | Move uma posição numa direção, mas mantém dentro de uma janela (dimensão)
movePosicaoJanela :: Dimensao -> Direcao -> Posicao -> Posicao
movePosicaoJanela (maxL, maxC) dir (l, c) =
    let (novaL, novaC) = movePosicao dir (l, c)
        l' = max 0 (min (maxL - 1) novaL)
        c' = max 0 (min (maxC - 1) novaC)
    in (l', c')

-- | Move múltiplas direções a partir de uma posição
moveDirecoesPosicao :: [Direcao] -> Posicao -> Posicao
moveDirecoesPosicao [] pos = pos
moveDirecoesPosicao (dir:resto) pos = moveDirecoesPosicao resto (movePosicao dir pos)

-- | Move uma direção para múltiplas posições
moveDirecaoPosicoes :: Direcao -> [Posicao] -> [Posicao]
moveDirecaoPosicoes dir posicoes = map (movePosicao dir) posicoes

-- | Converte uma posição da origem para o centro de uma dimensão
origemAoCentro :: Dimensao -> Posicao -> Posicao
origemAoCentro (linhas, colunas) (l, c) =
    let centroL = linhas `div` 2
        centroC = colunas `div` 2
    in (l - centroL, c - centroC)

-- | Roda uma posição e direção 45 graus no sentido horário
rodaPosicaoDirecao :: (Posicao, Direcao) -> (Posicao, Direcao)
rodaPosicaoDirecao (pos, dir) =
    let novaDirecao = case dir of
            Norte -> Nordeste
            Nordeste -> Este
            Este -> Sudeste
            Sudeste -> Sul
            Sul -> Sudoeste
            Sudoeste -> Oeste
            Oeste -> Noroeste
            Noroeste -> Norte
        novaPos = movePosicao novaDirecao pos
    in (novaPos, novaDirecao)
