module Main where

import Labs2025
import Tarefa2
import Magic

-- ============================================================================
-- MAPA E ESTADOS BASE PARA TESTES
-- ============================================================================

-- Mapa simples 5x5
mapaSimples :: Mapa
mapaSimples = 
    [ [Ar, Ar, Ar, Ar, Ar]
    , [Ar, Ar, Ar, Ar, Ar]
    , [Ar, Ar, Ar, Ar, Ar]
    , [Terra, Terra, Terra, Terra, Terra]
    , [Pedra, Pedra, Pedra, Pedra, Pedra]
    ]

-- Minhoca padrão com munição
minhocaPadrao :: Minhoca
minhocaPadrao = Minhoca
    { posicaoMinhoca = Just (1, 1)
    , vidaMinhoca = Viva 100
    , jetpackMinhoca = 1
    , escavadoraMinhoca = 1
    , bazucaMinhoca = 2
    , minaMinhoca = 1
    , dinamiteMinhoca = 1
    }

-- Estado inicial básico
estadoInicial :: Estado
estadoInicial = Estado
    { mapaEstado = mapaSimples
    , objetosEstado = []
    , minhocasEstado = 
        [ minhocaPadrao
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }

-- ============================================================================
-- TESTES DE MOVIMENTAÇÃO
-- ============================================================================

-- Teste 1: Movimento Norte válido
testeMovimentoNorte :: (NumMinhoca, Jogada, Estado)
testeMovimentoNorte = 
    ( 0
    , Move Norte
    , estadoInicial
    )

-- Teste 2: Movimento Este válido
testeMovimentoEste :: (NumMinhoca, Jogada, Estado)
testeMovimentoEste = 
    ( 0
    , Move Este
    , estadoInicial
    )

-- Teste 3: Movimento bloqueado por terreno
testeMovimentoBloqueado :: (NumMinhoca, Jogada, Estado)
testeMovimentoBloqueado = 
    ( 0
    , Move Sul
    , estadoInicial { minhocasEstado = 
        [ minhocaPadrao { posicaoMinhoca = Just (2, 1) }
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }
    )

-- Teste 4: Movimento bloqueado por outra minhoca
testeMovimentoBloqueadoMinhoca :: (NumMinhoca, Jogada, Estado)
testeMovimentoBloqueadoMinhoca = 
    ( 0
    , Move Sudeste
    , estadoInicial
    )

-- Teste 5: Minhoca morta não se move
testeMinhocaMortaNaoMove :: (NumMinhoca, Jogada, Estado)
testeMinhocaMortaNaoMove = 
    ( 0
    , Move Norte
    , estadoInicial { minhocasEstado = 
        [ minhocaPadrao { vidaMinhoca = Morta }
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }
    )

-- ============================================================================
-- TESTES DE ARMAS
-- ============================================================================

-- Teste 6: Disparar Bazuca
testeDisparaBazuca :: (NumMinhoca, Jogada, Estado)
testeDisparaBazuca = 
    ( 0
    , Dispara Bazuca Norte
    , estadoInicial
    )

-- Teste 7: Colocar Mina
testeColocaMina :: (NumMinhoca, Jogada, Estado)
testeColocaMina = 
    ( 0
    , Dispara Mina Este
    , estadoInicial
    )

-- Teste 8: Colocar Dinamite
testeColocaDinamite :: (NumMinhoca, Jogada, Estado)
testeColocaDinamite = 
    ( 0
    , Dispara Dinamite Oeste
    , estadoInicial
    )

-- Teste 9: Usar Jetpack
testeUsaJetpack :: (NumMinhoca, Jogada, Estado)
testeUsaJetpack = 
    ( 0
    , Dispara Jetpack Norte
    , estadoInicial
    )

-- Teste 10: Usar Escavadora
testeUsaEscavadora :: (NumMinhoca, Jogada, Estado)
testeUsaEscavadora = 
    ( 0
    , Dispara Escavadora Sul
    , estadoInicial { minhocasEstado = 
        [ minhocaPadrao { posicaoMinhoca = Just (2, 1) }
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }
    )

-- Teste 11: Disparar sem munição
testeDisparaSemMunicao :: (NumMinhoca, Jogada, Estado)
testeDisparaSemMunicao = 
    ( 0
    , Dispara Bazuca Norte
    , estadoInicial { minhocasEstado = 
        [ minhocaPadrao { bazucaMinhoca = 0 }
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }
    )

-- Teste 12: Jogada com índice inválido
testeIndiceInvalido :: (NumMinhoca, Jogada, Estado)
testeIndiceInvalido = 
    ( 99
    , Move Norte
    , estadoInicial
    )

-- ============================================================================
-- TESTES COMPLEXOS
-- ============================================================================

-- Teste 13: Múltiplas minhocas e objetos
testeComplexo1 :: (NumMinhoca, Jogada, Estado)
testeComplexo1 = 
    ( 1
    , Move Noroeste
    , Estado
        { mapaEstado = mapaSimples
        , objetosEstado = [Barril (1, 2) False]
        , minhocasEstado = 
            [ minhocaPadrao { posicaoMinhoca = Just (0, 0) }
            , minhocaPadrao { posicaoMinhoca = Just (2, 2) }
            , minhocaPadrao { posicaoMinhoca = Just (2, 4), vidaMinhoca = Viva 50 }
            ]
        }
    )

-- Teste 14: Disparo em posição inválida
testeDisparoPosicaoInvalida :: (NumMinhoca, Jogada, Estado)
testeDisparoPosicaoInvalida = 
    ( 0
    , Dispara Bazuca Sul
    , estadoInicial { minhocasEstado = 
        [ minhocaPadrao { posicaoMinhoca = Just (2, 1) }
        , minhocaPadrao { posicaoMinhoca = Just (2, 3) }
        ]
    }
    )

-- Teste 15: Movimento diagonal
testeMovimentoDiagonal :: (NumMinhoca, Jogada, Estado)
testeMovimentoDiagonal = 
    ( 0
    , Move Nordeste
    , estadoInicial
    )

-- ============================================================================
-- LISTA DE TESTES
-- ============================================================================

testesTarefa2 :: [(NumMinhoca, Jogada, Estado)]
testesTarefa2 = 
    [ testeMovimentoNorte
    , testeMovimentoEste
    , testeMovimentoBloqueado
    , testeMovimentoBloqueadoMinhoca
    , testeMinhocaMortaNaoMove
    , testeDisparaBazuca
    , testeColocaMina
    , testeColocaDinamite
    , testeUsaJetpack
    , testeUsaEscavadora
    , testeDisparaSemMunicao
    , testeIndiceInvalido
    , testeComplexo1
    , testeDisparoPosicaoInvalida
    , testeMovimentoDiagonal
    ]

dataTarefa2 :: IO TaskData
dataTarefa2 = do
    let ins = testesTarefa2
    outs <- mapM (\(i, j, e) -> runTest $ efetuaJogada i j e) ins
    return $ T2 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa2
