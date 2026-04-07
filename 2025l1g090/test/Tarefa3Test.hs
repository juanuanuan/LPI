module Main where

import Labs2025
import Tarefa3
import Magic

-- ============================================================================
-- TESTES DE GRAVIDADE DE MINHOCAS
-- ============================================================================

-- Minhoca no ar deve cair
testeGravidadeMinhoca :: Estado
testeGravidadeMinhoca = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Minhoca cai na água e morre
testeMinhocaCaiAgua :: Estado
testeMinhocaCaiAgua = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Agua, Agua, Agua]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Minhoca cai fora do mapa e morre
testeMinhocaCaiForaMapa :: Estado
testeMinhocaCaiForaMapa = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (1, 1)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES DE BARRIS
-- ============================================================================

-- Barril no ar fica prestes a explodir
testeBarrilNoArExplode :: Estado
testeBarrilNoArExplode = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (0, 1) True
        ]
    , minhocasEstado = []
    }

-- Barril no chão permanece estável
testeBarrilNoChao :: Estado
testeBarrilNoChao = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (0, 1) False
        ]
    , minhocasEstado = []
    }

-- ============================================================================
-- TESTES DE BAZUCA
-- ============================================================================
-- Bazuca avança na sua direção
testeBazucaAvanca :: Estado
testeBazucaAvanca = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Este Bazuca Nothing 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Bazuca bate em terreno e explode
testeBazucaExplode :: Estado
testeBazucaExplode = Estado
    { mapaEstado = 
        [ [Ar, Ar, Terra]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Este Bazuca Nothing 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES DE DINAMITE
-- ============================================================================

-- Dinamite executa parábola
testeDinamiteParabola :: Estado
testeDinamiteParabola = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Este Dinamite (Just 3) 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (1, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Dinamite com tempo 0 explode
testeDinamiteExplode :: Estado
testeDinamiteExplode = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 2) Este Dinamite (Just 0) 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (0, 4)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES DE MINA
-- ============================================================================

-- Mina ativa quando inimigo se aproxima
testeMinaAtiva :: Estado
testeMinaAtiva = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Norte Mina Nothing 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (0, 2)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Mina no ar cai
testeMinaCai :: Estado
testeMinaCai = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Ar, Ar, Ar]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Sul Mina Nothing 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (2, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES DE EXPLOSÕES E DANOS
-- ============================================================================

-- Explosão causa dano a minhoca
testeExplosaoDanaMinhoca :: Estado
testeExplosaoDanaMinhoca = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (0, 2) True
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (0, 3)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Explosão destrói Terra
testeExplosaoDestroiTerra :: Estado
testeExplosaoDestroiTerra = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (1, 2) True
        ]
    , minhocasEstado = []
    }

-- Explosão ativa barril
testeExplosaoAtivaBarril :: Estado
testeExplosaoAtivaBarril = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 1) Norte Dinamite (Just 0) 0
        , Barril (0, 3) False
        ]
    , minhocasEstado = 
        [ Minhoca (Just (1, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES ADICIONAIS
-- ============================================================================

-- Minhoca no chão não cai
testeMinhocaNoChao :: Estado
testeMinhocaNoChao = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar]
        , [Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Múltiplos objetos e minhocas
testeComplexo :: Estado
testeComplexo = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Ar, Ar, Terra, Terra]
        , [Terra, Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (2, 2) False
        , Disparo (0, 3) Sudeste Dinamite (Just 2) 0
        , Disparo (1, 1) Este Bazuca Nothing 1
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 80) 1 1 0 1 0
        , Minhoca (Just (1, 4)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (2, 3)) (Viva 50) 0 0 0 0 0
        ]
    }

-- ============================================================================
-- LISTA DE TESTES PARA O SISTEMA DE FEEDBACK
-- ============================================================================

testesTarefa3 :: [Estado]
testesTarefa3 = 
    [ testeGravidadeMinhoca
    , testeMinhocaCaiAgua
    , testeMinhocaCaiForaMapa
    , testeBarrilNoArExplode
    , testeBarrilNoChao
    , testeBazucaAvanca
    , testeBazucaExplode
    , testeDinamiteParabola
    , testeDinamiteExplode
    , testeMinaAtiva
    , testeMinaCai
    , testeExplosaoDanaMinhoca
    , testeExplosaoDestroiTerra
    , testeExplosaoAtivaBarril
    , testeMinhocaNoChao
    , testeComplexo
    ]

dataTarefa3 :: IO TaskData
dataTarefa3 = do
    let ins = testesTarefa3
    outs <- mapM (runTest . avancaEstado) ins
    return $ T3 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa3
