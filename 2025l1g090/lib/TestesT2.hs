{-|
Module      : TestesT3
Description : Testes para a Tarefa 3 (Passagem do Tempo)
-}

module TestesT3 where

import Labs2025
import Tarefa3

-- ============================================================================
-- LISTA DE TESTES PARA O SISTEMA DE FEEDBACK
-- ============================================================================

testesT3 :: [Estado]
testesT3 = 
    [ testeGravidadeMinhoca
    , testeMinhocaCaiAgua
    , testeMinhocaCaiForaMapa
    , testeBarrilNoArExplode
    , testeBazucaAvanca
    , testeBazucaExplode
    , testeDinamiteParabola
    , testeDinamiteExplode
    , testeMinaAtiva
    , testeMinaCai
    , testeExplosaoDanaMinhoca
    , testeExplosaoDestroiTerra
    , testeExplosaoAtivaBarril
    ]

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
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1  -- No ar, vai cair
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
        [ Minhoca (Just (0, 1)) (Viva 100) 1 1 1 1 1  -- Vai cair na água
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
        [ Minhoca (Just (1, 1)) (Viva 100) 1 1 1 1 1  -- Na última linha, vai cair fora
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
        [ Barril (0, 1) False  -- No ar, vai ficar True no próximo tick
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
        [ Disparo (0, 1) Este Bazuca Nothing 0  -- Avança para Este
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
        [ Disparo (0, 1) Este Bazuca Nothing 0  -- Vai bater na Terra e explodir
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
        [ Disparo (0, 1) Este Dinamite (Just 3) 0  -- Move-se e roda
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
        [ Disparo (0, 2) Este Dinamite (Just 0) 0  -- Vai explodir
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
        [ Disparo (0, 1) Norte Mina Nothing 0  -- Dono é minhoca 0
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1  -- Dono
        , Minhoca (Just (0, 2)) (Viva 100) 1 1 1 1 1  -- Inimigo perto, ativa mina
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
        [ Disparo (0, 1) Sul Mina Nothing 0  -- No ar com direção Sul, cai
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
        [ Disparo (0, 2) Norte Bazuca Nothing 0  -- Vai explodir no chão
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1  -- Longe
        , Minhoca (Just (0, 3)) (Viva 100) 1 1 1 1 1  -- Perto da explosão
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
        [ Barril (1, 2) True  -- Vai explodir e destruir Terra ao redor
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
        [ Disparo (0, 1) Norte Dinamite (Just 0) 0  -- Vai explodir
        , Barril (0, 3) False  -- Perto, vai ser ativado
        ]
    , minhocasEstado = 
        [ Minhoca (Just (1, 0)) (Viva 100) 1 1 1 1 1
        ]
    }

-- ============================================================================
-- TESTES COMPLEXOS
-- ============================================================================

-- Estado com múltiplos objetos e minhocas
testeComplexo1 :: Estado
testeComplexo1 = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Ar, Ar, Terra, Terra]
        , [Terra, Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Barril (2, 2) False  -- No ar, vai cair/explodir
        , Disparo (0, 3) Sudeste Dinamite (Just 2) 0  -- Parábola
        , Disparo (1, 1) Este Bazuca Nothing 1  -- Avança
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 80) 1 1 0 1 0
        , Minhoca (Just (1, 4)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (2, 3)) (Viva 50) 0 0 0 0 0  -- No ar, vai cair
        ]
    }

-- Reação em cadeia de explosões
testeReacaoEmCadeia :: Estado
testeReacaoEmCadeia = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = 
        [ Disparo (0, 3) Norte Dinamite (Just 0) 0  -- Explode
        , Barril (0, 1) False  -- Vai ser ativado
        , Barril (0, 5) False  -- Vai ser ativado
        ]
    , minhocasEstado = 
        [ Minhoca (Just (0, 0)) (Viva 100) 1 1 1 1 1
        , Minhoca (Just (0, 6)) (Viva 100) 1 1 1 1 1
        ]
    }
