{-|
Module      : Tarefa4
Description : Bot que destrói obstáculos em vez de pular
-}
module Tarefa4 where

import Data.Either
import Data.List (sortBy, maximumBy, minimumBy)
import Data.Ord (comparing)

import Labs2025
import Tarefa2 hiding (posicaoValidaMapa)
import Tarefa3 hiding (posicaoValidaMapa)

-- ============================================================================
-- FUNÇÕES PRÉ-DEFINIDAS (não alterar)
-- ============================================================================

tatica :: Estado -> [(NumMinhoca,Jogada)]
tatica e = reverse $ snd $ foldl avancaTatica (e,[]) [0..99]

avancaTatica :: (Estado,[(NumMinhoca,Jogada)]) -> Ticks -> (Estado,[(NumMinhoca,Jogada)])
avancaTatica (e,js) tick = (avancaJogada j e,j:js)
    where j = jogadaTatica tick e

avancaJogada :: (NumMinhoca,Jogada) -> Estado -> Estado
avancaJogada (i,j) e@(Estado _ objetos minhocas) = foldr aplicaDanos e'' danoss''
    where
    e'@(Estado mapa' objetos' minhocas') = efetuaJogada i j e
    minhocas'' = map (avancaMinhocaJogada e') (zip3 [0..] minhocas minhocas')
    (objetos'',danoss'') = partitionEithers $ map (avancaObjetoJogada (e' { minhocasEstado = minhocas''}) objetos) (zip [0..] objetos')
    e'' = Estado mapa' objetos'' minhocas''

avancaMinhocaJogada :: Estado -> (NumMinhoca,Minhoca,Minhoca) -> Minhoca
avancaMinhocaJogada e (i,minhoca,minhoca') = if posicaoMinhoca minhoca == posicaoMinhoca minhoca'
    then avancaMinhoca e i minhoca'
    else minhoca'

avancaObjetoJogada :: Estado -> [Objeto] -> (NumObjeto,Objeto) -> Either Objeto Danos
avancaObjetoJogada e objetos (i,objeto') = if elem objeto' objetos
    then avancaObjeto e i objeto'
    else Left objeto'

-- ============================================================================
-- BOT QUE DESTRÓI OBSTÁCULOS
-- ============================================================================

jogadaTatica :: Ticks -> Estado -> (NumMinhoca,Jogada)
jogadaTatica tick estado =
    let minhocasVivas = minhocasVivasComIndice estado
    in if null minhocasVivas
       then (0, Move Este)
       else if length minhocasVivas == 1
       then let (idx, _) = head minhocasVivas
            in (idx, Move Este)
       else botInteligente tick estado minhocasVivas

-- | Bot que prioriza destruir Terra em vez de pular
botInteligente :: Ticks -> Estado -> [(NumMinhoca, Minhoca)] -> (NumMinhoca, Jogada)
botInteligente tick estado minhocasVivas =
    let indices = [idx | (idx, _) <- minhocasVivas]
        idxEscolhido = indices !! (tick `mod` length indices)
        Just m = lookup idxEscolhido minhocasVivas
    in case posicaoMinhoca m of
        Nothing -> (idxEscolhido, Move Este)
        Just posAtual ->
            let alvos = encontrarAlvos idxEscolhido estado
            in if null alvos
               then (idxEscolhido, Move Este)
               else case encontrarAlvoMaisProximo posAtual alvos of
                   Nothing -> (idxEscolhido, Move Este)
                   Just (_, posAlvo) ->
                       decidirAcao idxEscolhido m posAtual posAlvo estado

-- | Decide ação: Atacar > Escavar > Mover
decidirAcao :: NumMinhoca -> Minhoca -> Posicao -> Posicao -> Estado -> (NumMinhoca, Jogada)
decidirAcao idx m posAtual posAlvo estado =
    let mapa = mapaEstado estado
        dist = distanciaManhattan posAtual posAlvo
        temVisao = temLinhaDeVisao mapa posAtual posAlvo
    in -- PRIORIDADE 1: Pode atacar?
       if podeAtacar m dist temVisao
       then atacar idx m posAtual posAlvo
       -- PRIORIDADE 2: Caminho bloqueado por Terra? Escava!
       else case tentarEscavarNaDirecaoDoAlvo idx m posAtual posAlvo estado of
           Just jogada -> jogada
           -- PRIORIDADE 3: Move (mas NÃO para cima se não aproxima!)
           Nothing -> moverInteligente idx m posAtual posAlvo estado

-- | Verifica se pode atacar (relaxado)
podeAtacar :: Minhoca -> Int -> Bool -> Bool
podeAtacar m dist temVisao =
    (temMunicao m Bazuca || temMunicao m Dinamite) &&
    ((dist <= 20 && temVisao) || dist <= 10)

-- | Ataca o alvo
atacar :: NumMinhoca -> Minhoca -> Posicao -> Posicao -> (NumMinhoca, Jogada)
atacar idx m posAtual posAlvo =
    let dir = calcularDirecaoParaAlvo posAtual posAlvo
        arma = if temMunicao m Bazuca then Bazuca else Dinamite
    in (idx, Dispara arma dir)

-- | NOVO: Tenta escavar Terra na direção do alvo
tentarEscavarNaDirecaoDoAlvo :: NumMinhoca -> Minhoca -> Posicao -> Posicao -> Estado -> Maybe (NumMinhoca, Jogada)
tentarEscavarNaDirecaoDoAlvo idx m posAtual posAlvo estado
    | not (temMunicao m Escavadora) = Nothing
    | otherwise =
        let mapa = mapaEstado estado
            -- Direção geral para o alvo
            (dl, dc) = (fst posAlvo - fst posAtual, snd posAlvo - snd posAtual)
            -- Prioriza direção horizontal primeiro
            direcoesPreferidas = 
                if abs dc > abs dl
                then if dc > 0 then [Este, Sul, Norte, Oeste] else [Oeste, Sul, Norte, Este]
                else if dl > 0 then [Sul, Este, Oeste, Norte] else [Norte, Este, Oeste, Sul]
            
            -- Procura Terra para escavar nessas direções
            terraParaEscavar = [(dir, posAlvo') | 
                                dir <- direcoesPreferidas,
                                Just posAlvo' <- [calculaNovaPosicao (Just posAtual) dir],
                                posicaoValidaMapa mapa posAlvo',
                                mapa !! fst posAlvo' !! snd posAlvo' == Terra]
        in case terraParaEscavar of
            [] -> Nothing
            ((dir, _):_) -> Just (idx, Dispara Escavadora dir)

-- | Move de forma inteligente (EVITA pulos inúteis)
moverInteligente :: NumMinhoca -> Minhoca -> Posicao -> Posicao -> Estado -> (NumMinhoca, Jogada)
moverInteligente idx m posAtual posAlvo estado =
    let distAtual = distanciaManhattan posAtual posAlvo
        (dl, dc) = (fst posAlvo - fst posAtual, snd posAlvo - snd posAtual)
        
        -- Prioriza movimento HORIZONTAL em direção ao alvo
        direcaoPreferida = 
            if abs dc > abs dl
            then if dc > 0 then Este else Oeste
            else if dl > 0 then Sul else Norte
        
        -- Testa movimentos que aproximam (SEM Norte se não estiver bloqueado horizontalmente)
        movimentos = [(d, nova, dist) |
                      d <- [Este, Oeste, Sul, Norte],
                      -- CRÍTICO: Se não está bloqueado horizontalmente, NÃO vai para Norte
                      not (d == Norte && podeIrHorizontalmente idx estado),
                      jogadaValida estado idx (Move d),
                      Just nova <- [calculaNovaPosicao (Just posAtual) d],
                      let dist = distanciaManhattan nova posAlvo]
        
        queAproximam = [(d, nova, dist) | (d, nova, dist) <- movimentos, dist < distAtual]
    in if not (null queAproximam)
       then let (melhorDir, _, _) = minimumBy (comparing (\(_, _, d) -> d)) queAproximam
            in (idx, Move melhorDir)
       else if jogadaValida estado idx (Move direcaoPreferida)
       then (idx, Move direcaoPreferida)
       else case movimentos of
           [] -> (idx, Move Este)  -- Bloqueado completamente
           ((dir, _, _):_) -> (idx, Move dir)

-- | Verifica se pode mover horizontalmente
podeIrHorizontalmente :: NumMinhoca -> Estado -> Bool
podeIrHorizontalmente idx estado =
    jogadaValida estado idx (Move Este) || jogadaValida estado idx (Move Oeste)

-- ============================================================================
-- FUNÇÕES AUXILIARES
-- ============================================================================

posicaoValidaMapa :: Mapa -> Posicao -> Bool
posicaoValidaMapa mapa (l, c) =
    l >= 0 && l < length mapa &&
    c >= 0 && c < length (head mapa)

minhocasVivasComIndice :: Estado -> [(NumMinhoca, Minhoca)]
minhocasVivasComIndice (Estado _ _ minhocas) =
    [(idx, m) | (idx, m) <- zip [0..] minhocas, minhocaEstaViva m]

minhocaEstaViva :: Minhoca -> Bool
minhocaEstaViva m = case (posicaoMinhoca m, vidaMinhoca m) of
    (Just _, Viva _) -> True
    _ -> False

encontrarAlvos :: NumMinhoca -> Estado -> [(NumMinhoca, Minhoca, Posicao)]
encontrarAlvos minhocaAtual estado =
    [(idx, m, pos) |
     (idx, m) <- zip [0..] (minhocasEstado estado),
     idx /= minhocaAtual,
     minhocaEstaViva m,
     Just pos <- [posicaoMinhoca m]]

encontrarAlvoMaisProximo :: Posicao -> [(NumMinhoca, Minhoca, Posicao)] -> Maybe (NumMinhoca, Posicao)
encontrarAlvoMaisProximo _ [] = Nothing
encontrarAlvoMaisProximo posAtual alvos =
    let alvo = minimumBy (comparing (\(_, _, pos) -> distanciaManhattan posAtual pos)) alvos
    in case alvo of
        (idx, _, pos) -> Just (idx, pos)

calcularDirecaoParaAlvo :: Posicao -> Posicao -> Direcao
calcularDirecaoParaAlvo (l1, c1) (l2, c2) =
    let dl = l2 - l1
        dc = c2 - c1
    in case (compare dl 0, compare dc 0) of
        (LT, LT) -> Noroeste
        (LT, EQ) -> Norte
        (LT, GT) -> Nordeste
        (EQ, LT) -> Oeste
        (EQ, GT) -> Este
        (GT, LT) -> Sudoeste
        (GT, EQ) -> Sul
        (GT, GT) -> Sudeste
        (EQ, EQ) -> Este

temLinhaDeVisao :: Mapa -> Posicao -> Posicao -> Bool
temLinhaDeVisao mapa pos1 pos2 =
    let caminho = calcularCaminho pos1 pos2
    in all (\pos -> posicaoValidaMapa mapa pos &&
                    let terreno = mapa !! fst pos !! snd pos
                    in terreno == Ar || terreno == Agua) caminho

calcularCaminho :: Posicao -> Posicao -> [Posicao]
calcularCaminho (l1, c1) (l2, c2) =
    if l1 == l2 && c1 == c2
    then []
    else let dl = signum (l2 - l1)
             dc = signum (c2 - c1)
             proxPos = (l1 + dl, c1 + dc)
         in proxPos : calcularCaminho proxPos (l2, c2)

distanciaManhattan :: Posicao -> Posicao -> Int
distanciaManhattan (l1, c1) (l2, c2) = abs (l1 - l2) + abs (c1 - c2)

jogadaValida :: Estado -> NumMinhoca -> Jogada -> Bool
jogadaValida estado idx jogada =
    let minhocas = minhocasEstado estado
        mapa = mapaEstado estado
    in if idx < 0 || idx >= length minhocas
       then False
       else case minhocas !! idx of
           m -> case (posicaoMinhoca m, vidaMinhoca m) of
               (Nothing, _) -> False
               (_, Morta) -> False
               (Just pos, Viva _) ->
                   case jogada of
                       Move dir ->
                           case calculaNovaPosicao (Just pos) dir of
                               Nothing -> False
                               Just novaPos ->
                                   posicaoValidaMapa mapa novaPos &&
                                   let terreno = mapa !! fst novaPos !! snd novaPos
                                   in terreno == Ar || terreno == Agua
                       Dispara arma dir -> temMunicao m arma
