{-|
Module      : Tarefa1
Description : Validação de estados do jogo Worms

Esta tarefa implementa a validação completa de estados do jogo,
verificando se o mapa, objetos e minhocas respeitam todas as regras.
-}

module Tarefa1 where

import Labs2025

-- ============================================================================
-- FUNÇÃO PRINCIPAL
-- ============================================================================

-- | Valida se um estado do jogo é válido segundo todas as regras
validaEstado :: Estado -> Bool
validaEstado (Estado mapa objetos minhocas) =
    validaMapa mapa &&
    validaObjetos mapa objetos minhocas &&
    validaMinhocas mapa objetos minhocas

-- ============================================================================
-- VALIDAÇÃO DO MAPA
-- ============================================================================

-- | Valida se o mapa é válido:
--   1. Não pode ser vazio
--   2. Todas as linhas devem ter o mesmo comprimento (formar uma grelha)
validaMapa :: Mapa -> Bool
validaMapa [] = False  -- Mapa vazio é inválido
validaMapa (primeiraLinha:restoLinhas) =
    not (null primeiraLinha) &&  -- Primeira linha não pode ser vazia
    todasLinhasMesmoTamanho primeiraLinha restoLinhas

-- | Verifica se todas as linhas têm o mesmo tamanho
todasLinhasMesmoTamanho :: [Terreno] -> [[Terreno]] -> Bool
todasLinhasMesmoTamanho primeiraLinha linhas =
    all (\linha -> length linha == length primeiraLinha) linhas

-- ============================================================================
-- FUNÇÕES AUXILIARES PARA POSIÇÕES E TERRENOS
-- ============================================================================

-- | Verifica se uma posição está dentro dos limites do mapa
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (linha, coluna) =
    linha >= 0 && linha < length mapa &&
    coluna >= 0 && coluna < length (head mapa)

-- | Obtém o terreno numa dada posição (se válida)
obtemTerreno :: Mapa -> Posicao -> Maybe Terreno
obtemTerreno mapa (linha, coluna)
    | posicaoValida mapa (linha, coluna) = Just (mapa !! linha !! coluna)
    | otherwise = Nothing

-- | Verifica se um terreno é opaco (Terra ou Pedra)
terrenoOpaco :: Terreno -> Bool
terrenoOpaco Terra = True
terrenoOpaco Pedra = True
terrenoOpaco _ = False

-- | Verifica se uma posição está livre (não é terreno opaco)
posicaoLivre :: Mapa -> Posicao -> Bool
posicaoLivre mapa pos =
    case obtemTerreno mapa pos of
        Just terreno -> not (terrenoOpaco terreno)
        Nothing -> False

-- | Calcula a posição anterior dada uma direção (invertida)
posicaoAnterior :: Posicao -> Direcao -> Posicao
posicaoAnterior (l, c) Norte = (l + 1, c)
posicaoAnterior (l, c) Sul = (l - 1, c)
posicaoAnterior (l, c) Este = (l, c - 1)
posicaoAnterior (l, c) Oeste = (l, c + 1)
posicaoAnterior (l, c) Nordeste = (l + 1, c - 1)
posicaoAnterior (l, c) Noroeste = (l + 1, c + 1)
posicaoAnterior (l, c) Sudeste = (l - 1, c - 1)
posicaoAnterior (l, c) Sudoeste = (l - 1, c + 1)

-- ============================================================================
-- VALIDAÇÃO DE OBJETOS
-- ============================================================================

-- | Valida todos os objetos do estado
validaObjetos :: Mapa -> [Objeto] -> [Minhoca] -> Bool
validaObjetos mapa objetos minhocas =
    all (\(idx, obj) -> validaObjeto mapa obj objetos minhocas idx) (zip [0..] objetos)

-- | Valida um objeto individual (Barril ou Disparo)
validaObjeto :: Mapa -> Objeto -> [Objeto] -> [Minhoca] -> NumObjeto -> Bool
validaObjeto mapa (Barril pos _) objetos minhocas idx =
    -- Barril deve ter posição válida e livre
    posicaoValida mapa pos &&
    posicaoLivre mapa pos &&
    -- Não pode estar na mesma posição que outro barril ou minhoca
    not (posicaoOcupadaPorOutroBarril objetos pos idx) &&
    not (posicaoOcupadaPorMinhoca minhocas pos)

validaObjeto mapa (Disparo pos dir tipo tempo dono) _objetos minhocas _idx =
    -- Validações específicas para disparos
    validaTipoDisparo tipo &&
    validaPosicaoDisparo mapa pos dir tipo &&
    validaTempoDisparo tipo tempo &&
    validaDonoDisparo dono minhocas

-- | Valida tipo de disparo (não pode ser Jetpack ou Escavadora)
validaTipoDisparo :: TipoArma -> Bool
validaTipoDisparo Jetpack = False
validaTipoDisparo Escavadora = False
validaTipoDisparo _ = True

-- | Valida posição de um disparo
validaPosicaoDisparo :: Mapa -> Posicao -> Direcao -> TipoArma -> Bool
validaPosicaoDisparo mapa pos dir Bazuca
    | not (posicaoValida mapa pos) = False
    | posicaoLivre mapa pos = True
    | otherwise = 
        -- Bazuca pode "perfurar" terreno opaco, mas só na superfície
        -- A posição anterior não pode ser opaca
        let posAnt = posicaoAnterior pos dir
        in case obtemTerreno mapa posAnt of
            Just t -> not (terrenoOpaco t)
            Nothing -> True  -- Fora do mapa conta como não opaco

validaPosicaoDisparo mapa pos _ _ =
    -- Outros disparos precisam de posição válida e livre
    posicaoValida mapa pos && posicaoLivre mapa pos

-- | Valida tempo de disparo conforme o tipo de arma
validaTempoDisparo :: TipoArma -> Maybe Ticks -> Bool
validaTempoDisparo Bazuca Nothing = True
validaTempoDisparo Bazuca (Just _) = False
validaTempoDisparo Mina Nothing = True
validaTempoDisparo Mina (Just t) = t >= 0 && t <= 2
validaTempoDisparo Dinamite (Just t) = t >= 0 && t <= 4
validaTempoDisparo Dinamite Nothing = False
validaTempoDisparo _ _ = False

-- | Valida se o dono do disparo existe
validaDonoDisparo :: NumMinhoca -> [Minhoca] -> Bool
validaDonoDisparo dono minhocas = dono >= 0 && dono < length minhocas

-- | Verifica se posição está ocupada por outro barril
posicaoOcupadaPorOutroBarril :: [Objeto] -> Posicao -> NumObjeto -> Bool
posicaoOcupadaPorOutroBarril objetos pos idxAtual =
    any (\(idx, obj) -> idx /= idxAtual && eBarrilNaPosicao obj pos) (zip [0..] objetos)
  where
    eBarrilNaPosicao (Barril p _) pos' = p == pos'
    eBarrilNaPosicao _ _ = False

-- | Verifica se posição está ocupada por alguma minhoca
posicaoOcupadaPorMinhoca :: [Minhoca] -> Posicao -> Bool
posicaoOcupadaPorMinhoca minhocas pos =
    any (\m -> posicaoMinhoca m == Just pos) minhocas

-- ============================================================================
-- VALIDAÇÃO DE MINHOCAS
-- ============================================================================

-- | Valida todas as minhocas
validaMinhocas :: Mapa -> [Objeto] -> [Minhoca] -> Bool
validaMinhocas mapa objetos minhocas =
    all (\(idx, m) -> validaMinhoca mapa objetos minhocas m idx) (zip [0..] minhocas)

-- | Valida uma minhoca individual
validaMinhoca :: Mapa -> [Objeto] -> [Minhoca] -> Minhoca -> NumMinhoca -> Bool
validaMinhoca mapa objetos minhocas m idx =
    validaPosicaoMinhoca mapa (posicaoMinhoca m) (vidaMinhoca m) &&
    not (posicaoMinhocaOcupada objetos minhocas (posicaoMinhoca m) idx) &&
    validaVidaMinhoca (posicaoMinhoca m) mapa (vidaMinhoca m) &&
    validaMunicoes m

-- | Valida posição de minhoca (deve ser válida e livre, se existir)
validaPosicaoMinhoca :: Mapa -> Maybe Posicao -> VidaMinhoca -> Bool
validaPosicaoMinhoca _ Nothing _ = True  -- Sem posição é sempre válido
validaPosicaoMinhoca mapa (Just pos) _ =
    posicaoValida mapa pos && posicaoLivre mapa pos

-- | Verifica se posição da minhoca está ocupada
posicaoMinhocaOcupada :: [Objeto] -> [Minhoca] -> Maybe Posicao -> NumMinhoca -> Bool
posicaoMinhocaOcupada _ _ Nothing _ = False
posicaoMinhocaOcupada objetos minhocas (Just pos) idxAtual =
    -- Não pode estar na mesma posição que um barril
    any (\obj -> case obj of
        Barril p _ -> p == pos
        _ -> False) objetos ||
    -- Não pode estar na mesma posição que outra minhoca
    any (\(idx, m) -> idx /= idxAtual && posicaoMinhoca m == Just pos) (zip [0..] minhocas)

-- | Valida vida da minhoca conforme sua posição
validaVidaMinhoca :: Maybe Posicao -> Mapa -> VidaMinhoca -> Bool
validaVidaMinhoca Nothing _ Morta = True
validaVidaMinhoca Nothing _ (Viva _) = False  -- Sem posição deve estar morta
validaVidaMinhoca (Just _pos) _mapa Morta =
    -- Minhoca morta pode estar em qualquer lugar (incluindo água)
    True
validaVidaMinhoca (Just pos) mapa (Viva pontos) =
    -- Minhoca viva não pode estar na água e deve ter vida válida
    case obtemTerreno mapa pos of
        Just Agua -> False
        _ -> pontos >= 0 && pontos <= 100

-- | Valida munições (todas devem ser >= 0)
validaMunicoes :: Minhoca -> Bool
validaMunicoes m =
    jetpackMinhoca m >= 0 &&
    escavadoraMinhoca m >= 0 &&
    bazucaMinhoca m >= 0 &&
    minaMinhoca m >= 0 &&
    dinamiteMinhoca m >= 0


