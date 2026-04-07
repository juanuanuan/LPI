{-|
Module      : Tarefa3
Description : Simulação da passagem do tempo no jogo Worms
-}

module Tarefa3 where

import Labs2025
import Tarefa0_geral

-- ============================================================================
-- TIPOS DE DADOS
-- ============================================================================

type Dano = Int
type Danos = [(Posicao, Dano)]

-- ============================================================================
-- FUNÇÃO PRINCIPAL (já vem pré-definida, chama as 3 funções abaixo)
-- ============================================================================

-- | Avança o estado do jogo um tick
avancaEstado estado =
    let -- 1. Processar minhocas PRIMEIRO (gravidade antes das explosões)
        novasMinhocas = map (\(i, m) -> avancaMinhoca estado i m) (zip [0..] (minhocasEstado estado))
        estadoComMinhocas = estado { minhocasEstado = novasMinhocas }
        
        -- 2. Processar objetos e obter danos
        resultados = map (\(i, obj) -> avancaObjeto estadoComMinhocas i obj) (zip [0..] (objetosEstado estadoComMinhocas))
        novosObjetos = [obj | Left obj <- resultados]
        todosDanos = concat [danos | Right danos <- resultados]
        
        -- 3. Aplicar danos
        estadoComDanos = aplicaDanos todosDanos (estadoComMinhocas { objetosEstado = novosObjetos })
        
    in estadoComDanos
-- ============================================================================
-- AVANÇAR MINHOCA (aplicar gravidade)
-- ============================================================================

-- | Avança uma minhoca aplicando gravidade
avancaMinhoca :: Estado -> NumMinhoca -> Minhoca -> Minhoca
avancaMinhoca estado idx minhoca =
    case posicaoMinhoca minhoca of
        Nothing -> minhoca
        Just (l, c) ->
            let posInferior = (l + 1, c)
                mapa = mapaEstado estado
            in if vidaMinhoca minhoca == Morta
               then minhoca
               else if deveCair estado (l, c)
                    then cairMinhoca mapa minhoca posInferior
                    else minhoca

-- | Verifica se minhoca deve cair
deveCair (Estado mapa objs minhs) (l, c) =
    let terrenoAtual = obterTerreno mapa (l, c)
        posInferior = (l + 1, c)
    in if terrenoAtual == Terra
       then True  -- Minhoca em Terra é posição inválida, deve cair
       else (terrenoAtual == Ar || terrenoAtual == Agua) &&
            (not (posicaoValidaMapa mapa posInferior) ||
             (terrenoLivre mapa posInferior && not (temBarrilOuMinhoca objs minhs posInferior)))

-- | Faz a minhoca cair
cairMinhoca :: Mapa -> Minhoca -> Posicao -> Minhoca
cairMinhoca mapa m novaPos =
    if not (posicaoValidaMapa mapa novaPos)
    then m { posicaoMinhoca = Nothing, vidaMinhoca = Morta }
    else case obterTerreno mapa novaPos of
        Agua -> m { posicaoMinhoca = Just novaPos, vidaMinhoca = Morta }
        _ -> m { posicaoMinhoca = Just novaPos }

-- ============================================================================
-- AVANÇAR OBJETO
-- ============================================================================

-- | Avança um objeto e retorna Either (objeto continua) ou (danos da explosão)
avancaObjeto :: Estado -> NumObjeto -> Objeto -> Either Objeto Danos

-- BARRIL
avancaObjeto estado idx (Barril pos True) =
    Right (explosao pos 5)  -- Explode!

avancaObjeto estado idx (Barril pos False) =
    if barrilNoAr estado pos
    then Left (Barril pos True)  -- No ar, vai explodir
    else Left (Barril pos False)  -- No chão, mantém

-- BAZUCA
avancaObjeto estado idx (Disparo pos dir Bazuca tempo dono) =
      case tempo of 
           Just 0 -> Right (explosao pos 5)
           Just t -> Left (Disparo pos dir Bazuca (Just (t-1)) dono)
           Nothing -> 
               let novaPos = movePosicao dir pos
                   mapa = mapaEstado estado 
                   objs = objetosEstado estado 
                   minhs = minhocasEstado estado
               in if not (posicaoValidaMapa mapa novaPos) 
                  then Right []
                  else if terrenoLivre mapa novaPos && not (temBarrilOuMinhoca objs minhs novaPos)
                       then Left (Disparo novaPos dir Bazuca Nothing dono)
                       else Left (Disparo novaPos dir Bazuca (Just 1) dono)
           Just t | t > 0 -> 
              Left (Disparo pos dir Bazuca (Just(t-1)) dono)
           Just 0 -> Right (explosao pos 5) 
           _ -> Right []

-- MINA
avancaObjeto estado idx (Disparo pos dir Mina tempo dono) =
    case tempo of
        Just 0 -> Right (explosao pos 5)  -- Explode com diâmetro 5
        Just n -> Left (Disparo pos dir Mina (Just (n - 1)) dono)  -- tira
        Nothing ->
            if deveAtivarMina estado pos dono
            then Left (Disparo pos dir Mina (Just 2) dono)  -- ativa
            else processarFisicaMina estado pos dir dono


-- DINAMITE
avancaObjeto estado idx (Disparo pos dir Dinamite tempo dono) =
    case tempo of
        Just 0 -> Right (explosao pos 7)  -- Explode!
        Just n ->
            if deveMoverDinamite estado pos
            then let (novaPos, novaDir) = parabola pos dir
                     mapa = mapaEstado estado
                     objs = objetosEstado estado
                     minhs = minhocasEstado estado
                 in if posicaoValidaMapa mapa novaPos && 
                       terrenoLivre mapa novaPos && 
                       not (temBarrilOuMinhoca objs minhs novaPos)
                    then Left (Disparo novaPos novaDir Dinamite (Just (n - 1)) dono)
                    else -- Não consegue mover na parábola, cai na vertical
                         let posVertical = movePosicao Sul pos
                         in if posicaoValidaMapa mapa posVertical
                            then Left (Disparo posVertical Norte Dinamite (Just (n - 1)) dono)
                            else Right []  -- Sai do mapa
            else Left (Disparo pos Norte Dinamite (Just (n - 1)) dono)
        Nothing -> Left (Disparo pos dir Dinamite tempo dono)


avancaObjeto _ _ obj = Left obj  -- Outros casos

-- ============================================================================
-- FUNÇÕES AUXILIARES PARA FÍSICA
-- ============================================================================

-- | física da mina (corrigido: verifica terreno atual e valida posições)
processarFisicaMina :: Estado -> Posicao -> Direcao -> NumMinhoca -> Either Objeto Danos
processarFisicaMina estado pos dir dono =
    let mapa = mapaEstado estado
        terrenoAtual = obterTerreno mapa pos
        posInferior = movePosicao Sul pos
    in if (terrenoAtual == Ar || terrenoAtual == Agua) && elem dir [Sul, Sudeste, Sudoeste]
       then if posicaoValidaMapa mapa posInferior
            then Left (Disparo posInferior Norte Mina Nothing dono)
            else Right []  -- Cai fora do mapa -> eliminado
       else if terrenoAtual /= Ar && terrenoAtual /= Agua
            then Left (Disparo pos Norte Mina Nothing dono) -- No chão aponta para Norte
            else Left (Disparo pos dir Mina Nothing dono)   -- Mantém-se


-- | Verifica se deve ativar mina
deveAtivarMina :: Estado -> Posicao -> NumMinhoca -> Bool
deveAtivarMina estado posMina dono =
    any (\(i, m) -> i /= dono && minhocaPerto m posMina) (zip [0..] (minhocasEstado estado))

-- | Verifica se minhoca está perto
minhocaPerto :: Minhoca -> Posicao -> Bool
minhocaPerto m pos =
    case posicaoMinhoca m of
        Just posM -> distancia posM pos <= 2
        Nothing -> False

-- | Distância entre posições (máximo entre diferenças)
distancia :: Posicao -> Posicao -> Int
distancia (l1, c1) (l2, c2) = max (abs (l1 - l2)) (abs (c1 - c2))

-- | Executa parábola da dinamite
parabola :: Posicao -> Direcao -> (Posicao, Direcao)
parabola pos Norte = (movePosicao Sul pos, Norte)
parabola pos Sul = (movePosicao Sul pos, Norte)
parabola pos dir = (movePosicao dir (movePosicao Sul pos), rodaDireita dir)

-- | Roda direção 45° para a direita
rodaDireita :: Direcao -> Direcao
rodaDireita Norte = Nordeste
rodaDireita Nordeste = Este
rodaDireita Este = Sudeste
rodaDireita Sudeste = Sul
rodaDireita Sul = Sudoeste
rodaDireita Sudoeste = Oeste
rodaDireita Oeste = Noroeste
rodaDireita Noroeste = Norte 

-- | função auxiliar para ver se o barril esta no ar 
barrilNoAr :: Estado -> Posicao -> Bool
barrilNoAr (Estado mapa _ _) pos =
    let terrenoAtual = obterTerreno mapa pos
    in terrenoAtual == Ar || terrenoAtual == Agua

-- | função auxiliar para o moviemento parabolico 
deveMoverDinamite :: Estado -> Posicao -> Bool
deveMoverDinamite (Estado mapa _ _) pos =
    let terrenoAtual = obterTerreno mapa pos
    in terrenoAtual == Ar || terrenoAtual == Agua



-- ============================================================================
-- EXPLOSÕES
-- ============================================================================

-- | Calcula danos de uma explosão
explosao :: Posicao -> Int -> Danos
explosao (l, c) diametro =
    let raio = diametro `div` 2 -- Para diametro=5, raio=2.
        
        -- Gera posições dentro do raio de Manhattan e calcula o dano
        todasPosComDano = [(pos, danoNaPosicao (l, c) pos diametro) 
                           | dl <- [-raio..raio]
                           , dc <- [-raio..raio]
                           , let pos = (l + dl, c + dc)
                           , let dist = abs dl + abs dc
                           , dist <= raio
                          ]
    -- Filtra apenas os pares onde o Dano é estritamente positivo
    in [(pos, dano) | (pos, dano) <- todasPosComDano, dano > 0]

-- | Calcula dano numa posição (corrigido)
danoNaPosicao :: Posicao -> Posicao -> Int -> Dano
danoNaPosicao (l1, c1) (l2, c2) diametro =
    let dist = abs (l1 - l2) + abs (c1 - c2)  -- Distância de Manhattan
        -- Fórmula: DanoMax - 20 * dist. Equivalente a (diametro - 2 * dist) * 10
        danoCalculado = (diametro - (2 * dist)) * 10 
    in max 0 danoCalculado


-- ============================================================================
-- APLICAR DANOS
-- ============================================================================

-- | Aplica lista de danos ao estado, resolvendo reações em cadeia de barris
aplicaDanos :: Danos -> Estado -> Estado
aplicaDanos danos estado =
    let estadoComDanosMinhocas = aplicaDanosMinhocas danos estado
        estadoComDanosBarris = aplicaDanosBarris danos estadoComDanosMinhocas
        estadoComDanosMapa = aplicaDanosMapa danos estadoComDanosBarris
    in estadoComDanosMapa


-- | Aplica danos às minhocas
aplicaDanosMinhocas :: Danos -> Estado -> Estado
aplicaDanosMinhocas danos estado =
    estado { minhocasEstado = map (aplicarDanoMinhoca danos) (minhocasEstado estado) }

-- | Aplica dano a uma minhoca
aplicarDanoMinhoca :: Danos -> Minhoca -> Minhoca
aplicarDanoMinhoca danos m =
    case posicaoMinhoca m of
        Nothing -> m
        Just pos ->
            let danoTotal = sum [d | (p, d) <- danos, p == pos]
            in if danoTotal == 0
               then m
               else case vidaMinhoca m of
                   Morta -> m
                   Viva v -> 
                       let novaVida = v - danoTotal
                       in if novaVida <= 0
                          then m { vidaMinhoca = Morta }
                          else m { vidaMinhoca = Viva novaVida }

-- | Aplica danos aos barris
aplicaDanosBarris :: Danos -> Estado -> Estado
aplicaDanosBarris danos estado =
    estado { objetosEstado = map (aplicarDanoBarril danos) (objetosEstado estado) }

-- | Aplica dano a um barril
aplicarDanoBarril :: Danos -> Objeto -> Objeto
aplicarDanoBarril danos (Barril pos _) =
    let danoTotal = sum [d | (p, d) <- danos, p == pos]
    in if danoTotal > 0
       then Barril pos True
       else Barril pos False
aplicarDanoBarril _ obj = obj

-- | Aplica danos ao mapa (destrói Terra)
aplicaDanosMapa :: Danos -> Estado -> Estado
aplicaDanosMapa danos estado =
    estado { mapaEstado = foldr destruirTerra (mapaEstado estado) danos }

-- | Destrói terra se houver dano
destruirTerra :: (Posicao, Dano) -> Mapa -> Mapa
destruirTerra (pos, dano) mapa =
    if dano > 0 && obterTerreno mapa pos == Terra
    then atualizaPosicaoMatriz pos Ar mapa
    else mapa

-- ============================================================================
-- FUNÇÕES AUXILIARES GERAIS
-- ============================================================================

-- | Verifica se posição é válida no mapa
posicaoValidaMapa :: Mapa -> Posicao -> Bool
posicaoValidaMapa mapa pos = ePosicaoMatrizValida pos mapa

-- | Obtém terreno numa posição
obterTerreno :: Mapa -> Posicao -> Terreno
obterTerreno mapa pos =
    case encontraPosicaoMatriz pos mapa of
        Just t -> t
        Nothing -> Ar

-- | Verifica se terreno é livre (Ar ou Agua)
terrenoLivre :: Mapa -> Posicao -> Bool
terrenoLivre mapa pos =
    let t = obterTerreno mapa pos
    in t == Ar || t == Agua

-- | Verifica se tem barril ou minhoca na posição
temBarrilOuMinhoca :: [Objeto] -> [Minhoca] -> Posicao -> Bool
temBarrilOuMinhoca objs minhs pos =
    any (\(Barril p _) -> p == pos) [b | b@(Barril _ _) <- objs] ||
    any (\m -> posicaoMinhoca m == Just pos) minhs

