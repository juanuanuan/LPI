{-|
Module      : Tarefa2
Description : Efetuar jogadas no jogo Worms

Esta tarefa implementa o sistema de jogadas do jogo, permitindo que
as minhocas se movam, usem armas e interajam com o ambiente.
-}

module Tarefa2 where

import Labs2025

-- ============================================================================
-- FUNÇÃO PRINCIPAL
-- ============================================================================

-- | Efetua uma jogada para uma minhoca em específico
efetuaJogada :: NumMinhoca -> Jogada -> Estado -> Estado
efetuaJogada n jogada estado@(Estado _mapa _objetos minhocas)
    | n < 0 || n >= length minhocas = estado  -- Índice inválido
    | not (minhocaPodeJogar (minhocas !! n)) = estado  -- Minhoca não pode jogar
    | otherwise = case jogada of
        Move dir -> efetuaMovimento n dir estado
        Dispara arma dir -> efetuaArma n arma dir estado

-- | Verifica se a minhoca pode jogar (se está viva e tem posição atribuida)
minhocaPodeJogar :: Minhoca -> Bool
minhocaPodeJogar m = case (posicaoMinhoca m, vidaMinhoca m) of
    (Just _, Viva _) -> True
    _ -> False

-- ============================================================================
-- MOVIMENTAÇÃO
-- ============================================================================

-- | Efetua o movimento de uma minhoca
efetuaMovimento :: NumMinhoca -> Direcao -> Estado -> Estado
efetuaMovimento n dir (Estado mapa objetos minhocas) =
    let minhoca = minhocas !! n
        posAtual = posicaoMinhoca minhoca
        novaPos = calculaNovaPosicao posAtual dir
    in if movimentoValido mapa objetos minhocas posAtual novaPos n dir
       then Estado mapa objetos (atualizaMinhoca n minhoca novaPos minhocas)
       else Estado mapa objetos minhocas

-- | Deduz a nova posição baseada na direção do movimento 
calculaNovaPosicao :: Maybe Posicao -> Direcao -> Maybe Posicao
calculaNovaPosicao Nothing _ = Nothing
calculaNovaPosicao (Just (l, c)) dir = Just $ case dir of
    Norte -> (l - 1, c)
    Sul -> (l + 1, c)
    Este -> (l, c + 1)
    Oeste -> (l, c - 1)
    Nordeste -> (l - 1, c + 1)
    Noroeste -> (l - 1, c - 1)
    Sudeste -> (l + 1, c + 1)
    Sudoeste -> (l + 1, c - 1)

-- | Verifica se o movimento é válido
movimentoValido :: Mapa -> [Objeto] -> [Minhoca] -> Maybe Posicao -> Maybe Posicao -> NumMinhoca -> Direcao -> Bool
movimentoValido _ _ _ _ Nothing _ _ = False
movimentoValido _ _ _ Nothing _ _ _ = False
movimentoValido mapa objetos minhocas (Just posAtual) (Just posNova) n dir =
    -- Validações básicas
    posicaoValidaMapa mapa posNova &&
    posicaoLivreMapa mapa posNova &&
    not (posicaoOcupadaObjeto objetos posNova) &&
    not (posicaoOcupadaMinhoca minhocas posNova n)

-- | Verifica se o movimento implica um salto, sem jetpack 
direcaoEnvolveSalto :: Direcao -> Bool
direcaoEnvolveSalto Norte = True
direcaoEnvolveSalto Nordeste = True
direcaoEnvolveSalto Noroeste = True
direcaoEnvolveSalto _ = False

-- | Verifica se a minhoca está no chão ou em cima de algo
minhocaEstaNoChao :: Mapa -> [Objeto] -> [Minhoca] -> Posicao -> NumMinhoca -> Bool
minhocaEstaNoChao mapa objetos minhocas (l, c) n =
    let posInferior = (l + 1, c)
    in if not (posicaoValidaMapa mapa posInferior)
       then True  -- Se está na última linha, considera que está no chão
       else 
           -- Verifica se há terreno sólido por baixo
           not (posicaoLivreMapa mapa posInferior) ||
           -- Ou se há barril por baixo
           posicaoOcupadaObjeto objetos posInferior ||
           -- Ou se há outra minhoca por baixo
           posicaoOcupadaMinhoca minhocas posInferior n

-- | Verifica se a posição está dentro do mapa (posição válida) 
posicaoValidaMapa :: Mapa -> Posicao -> Bool
posicaoValidaMapa mapa (l, c) =
    l >= 0 && l < length mapa &&
    c >= 0 && c < length (head mapa)

-- | Verifica se a posição está livre (não é Terra ou Pedra)
posicaoLivreMapa :: Mapa -> Posicao -> Bool
posicaoLivreMapa mapa (l, c) =
    let terreno = mapa !! l !! c
    in terreno /= Terra && terreno /= Pedra

-- | Verifica se posição está ocupada por qualquer objeto
posicaoOcupadaObjeto :: [Objeto] -> Posicao -> Bool
posicaoOcupadaObjeto objetos pos =
    any (\obj -> case obj of
        Barril p _ -> p == pos
        _ -> False) objetos

-- | Verifica se posição está ocupada por outra minhoca
posicaoOcupadaMinhoca :: [Minhoca] -> Posicao -> NumMinhoca -> Bool
posicaoOcupadaMinhoca minhocas pos n =
    any (\(idx, m) -> idx /= n && posicaoMinhoca m == Just pos) (zip [0..] minhocas)

-- | Atualiza posição de uma minhoca
atualizaMinhoca :: NumMinhoca -> Minhoca -> Maybe Posicao -> [Minhoca] -> [Minhoca]
atualizaMinhoca n minhoca novaPos minhocas =
    take n minhocas ++ [minhoca { posicaoMinhoca = novaPos }] ++ drop (n + 1) minhocas

-- ============================================================================
-- USO DE ARMAS
-- ============================================================================

-- | Efetua uso da arma por uma minhoca
efetuaArma :: NumMinhoca -> TipoArma -> Direcao -> Estado -> Estado
efetuaArma n arma dir estado@(Estado _mapa _objetos minhocas) =
    let minhoca = minhocas !! n
    in if temMunicao minhoca arma
       then case arma of
           Bazuca -> disparaBazuca n dir estado
           Mina -> colocaMina n dir estado
           Dinamite -> colocaDinamite n dir estado
           Jetpack -> usaJetpack n dir estado
           Escavadora -> usaEscavadora n dir estado
       else estado

-- | Verifica se a minhoca tem munição para a arma
temMunicao :: Minhoca -> TipoArma -> Bool
temMunicao m Bazuca = bazucaMinhoca m > 0
temMunicao m Mina = minaMinhoca m > 0
temMunicao m Dinamite = dinamiteMinhoca m > 0
temMunicao m Jetpack = jetpackMinhoca m > 0
temMunicao m Escavadora = escavadoraMinhoca m > 0

-- | Dispara a bazuca
disparaBazuca :: NumMinhoca -> Direcao -> Estado -> Estado
disparaBazuca n dir (Estado mapa objetos minhocas) =
    let minhoca = minhocas !! n
        posDisparo = calculaPosicaoDisparo (posicaoMinhoca minhoca) dir
    in case posDisparo of
        Just pos | posicaoValidaMapa mapa pos ->
            let novoDisparo = Disparo pos dir Bazuca Nothing n
                novosMinhocas = consumeMunicao n Bazuca minhocas
            in Estado mapa (objetos ++ [novoDisparo]) novosMinhocas
        _ -> Estado mapa objetos minhocas

-- | Coloca a mina
colocaMina :: NumMinhoca -> Direcao -> Estado -> Estado
colocaMina n dir (Estado mapa objetos minhocas) =
    let minhoca = minhocas !! n
        posMina = calculaPosicaoDisparo (posicaoMinhoca minhoca) dir
    in case posMina of
        Just pos | posicaoValidaDisparo mapa pos ->
            let novaMina = Disparo pos dir Mina Nothing n
                novosMinhocas = consumeMunicao n Mina minhocas
            in Estado mapa (objetos ++ [novaMina]) novosMinhocas
        _ -> Estado mapa objetos minhocas

-- | Coloca o dinamite
colocaDinamite :: NumMinhoca -> Direcao -> Estado -> Estado
colocaDinamite n dir (Estado mapa objetos minhocas) =
    let minhoca = minhocas !! n
        posDinamite = calculaPosicaoDisparo (posicaoMinhoca minhoca) dir
    in case posDinamite of
        Just pos | posicaoValidaDisparo mapa pos ->
            let novaDinamite = Disparo pos dir Dinamite (Just 4) n
                novosMinhocas = consumeMunicao n Dinamite minhocas
            in Estado mapa (objetos ++ [novaDinamite]) novosMinhocas
        _ -> Estado mapa objetos minhocas

-- | Usa jetpack para o movimento vertical
usaJetpack :: NumMinhoca -> Direcao -> Estado -> Estado
usaJetpack n dir estado@(Estado mapa objetos minhocas)
    | dir `elem` [Norte, Sul] =
        let minhoca = minhocas !! n
            novaPos = calculaNovaPosicao (posicaoMinhoca minhoca) dir
        in if movimentoValidoJetpack mapa objetos minhocas novaPos n
           then let novosMinhocas = consumeMunicao n Jetpack minhocas
                    minhocaAtualizada = (novosMinhocas !! n) { posicaoMinhoca = novaPos }
                in Estado mapa objetos (take n novosMinhocas ++ [minhocaAtualizada] ++ drop (n + 1) novosMinhocas)
           else estado
    | otherwise = estado

-- | Validação específica para o jetpack (sem verificar chão)
movimentoValidoJetpack :: Mapa -> [Objeto] -> [Minhoca] -> Maybe Posicao -> NumMinhoca -> Bool
movimentoValidoJetpack _ _ _ Nothing _ = False
movimentoValidoJetpack mapa objetos minhocas (Just pos) n =
    posicaoValidaMapa mapa pos &&
    posicaoLivreMapa mapa pos &&
    not (posicaoOcupadaObjeto objetos pos) &&
    not (posicaoOcupadaMinhoca minhocas pos n)

-- | Usa escavadora para destruir o terreno
usaEscavadora :: NumMinhoca -> Direcao -> Estado -> Estado
usaEscavadora n dir (Estado mapa objetos minhocas) =
    let minhoca = minhocas !! n
        posEscavar = calculaPosicaoDisparo (posicaoMinhoca minhoca) dir
    in case posEscavar of
        Just pos@(l, c) | posicaoValidaMapa mapa pos ->
            let terreno = mapa !! l !! c
            in if terreno == Terra  -- Só escava se for Terra
               then let novoMapa = escavaTerreno mapa pos
                        -- Primeiro consome munição
                        novosMinhocas = consumeMunicao n Escavadora minhocas
                        -- Depois atualiza a posição da minhoca JÁ com munição consumida
                        minhocaAtualizada = (novosMinhocas !! n) { posicaoMinhoca = Just pos }
                        minhocasFinais = take n novosMinhocas ++ [minhocaAtualizada] ++ drop (n + 1) novosMinhocas
                    in Estado novoMapa objetos minhocasFinais
               else Estado mapa objetos minhocas
        _ -> Estado mapa objetos minhocas

-- | Calcula posição para disparo baseada na posição atual e direção
calculaPosicaoDisparo :: Maybe Posicao -> Direcao -> Maybe Posicao
calculaPosicaoDisparo Nothing _ = Nothing
calculaPosicaoDisparo (Just pos) dir = calculaNovaPosicao (Just pos) dir

-- | Verifica se a posição é válida para o disparo
posicaoValidaDisparo :: Mapa -> Posicao -> Bool
posicaoValidaDisparo mapa pos =
    posicaoValidaMapa mapa pos && posicaoLivreMapa mapa pos

-- | Consome munição de uma arma em específico
consumeMunicao :: NumMinhoca -> TipoArma -> [Minhoca] -> [Minhoca]
consumeMunicao n arma minhocas =
    let minhoca = minhocas !! n
        novaMinhoca = case arma of
            Bazuca -> minhoca { bazucaMinhoca = bazucaMinhoca minhoca - 1 }
            Mina -> minhoca { minaMinhoca = minaMinhoca minhoca - 1 }
            Dinamite -> minhoca { dinamiteMinhoca = dinamiteMinhoca minhoca - 1 }
            Jetpack -> minhoca { jetpackMinhoca = jetpackMinhoca minhoca - 1 }
            Escavadora -> minhoca { escavadoraMinhoca = escavadoraMinhoca minhoca - 1 }
    in take n minhocas ++ [novaMinhoca] ++ drop (n + 1) minhocas

-- | Escava o terreno (transforma Terra em Ar)
escavaTerreno :: Mapa -> Posicao -> Mapa
escavaTerreno mapa (l, c) =
    let linha = mapa !! l
        novaLinha = take c linha ++ [Ar] ++ drop (c + 1) linha
    in take l mapa ++ [novaLinha] ++ drop (l + 1) mapa

