module Main where

import Labs2025
import Tarefa4
import Magic

-- ============================================================================
-- TESTES PARA BOT INTELIGENTE - Combate Real!
-- ============================================================================

-- TESTE 1: Arena de Combate - 2 minhocas frente a frente NO CHÃO
estadoCombate1v1 :: Estado
estadoCombate1v1 = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]  -- Chão firme!
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca  -- Minhoca 0 - Lado esquerdo, EM CIMA DA TERRA
            { posicaoMinhoca = Just (2, 2)  -- Linha 2, Terra está na linha 3
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 5
            , escavadoraMinhoca = 10
            , bazucaMinhoca = 20
            , minaMinhoca = 10
            , dinamiteMinhoca = 10
            }
        , Minhoca  -- Minhoca 1 - Lado direito, EM CIMA DA TERRA
            { posicaoMinhoca = Just (2, 9)  -- Linha 2, Terra está na linha 3
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 5
            , escavadoraMinhoca = 10
            , bazucaMinhoca = 20
            , minaMinhoca = 10
            , dinamiteMinhoca = 10
            }
        ]
    }

-- TESTE 2: Battle Royale - 3 minhocas NO CHÃO
estadoBattleRoyale :: Estado
estadoBattleRoyale = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Terra, Terra, Pedra]  -- Plataformas
        , [Pedra, Terra, Terra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Terra, Terra, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Terra, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]  -- Plataforma central
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Terra, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca  -- Esquerda - EM CIMA da plataforma Terra
            { posicaoMinhoca = Just (1, 2)  -- Linha 1, Terra nas linhas 2-3
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 8
            , escavadoraMinhoca = 15
            , bazucaMinhoca = 25
            , minaMinhoca = 12
            , dinamiteMinhoca = 12
            }
        , Minhoca  -- Direita - EM CIMA da plataforma Terra
            { posicaoMinhoca = Just (1, 12)  -- Linha 1, Terra nas linhas 2-3
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 8
            , escavadoraMinhoca = 15
            , bazucaMinhoca = 25
            , minaMinhoca = 12
            , dinamiteMinhoca = 12
            }
        , Minhoca  -- Centro - EM CIMA da plataforma central
            { posicaoMinhoca = Just (5, 7)  -- Linha 5, Terra nas linhas 6-7
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 8
            , escavadoraMinhoca = 15
            , bazucaMinhoca = 25
            , minaMinhoca = 12
            , dinamiteMinhoca = 12
            }
        ]
    }

-- TESTE 3: Labirinto com Terra - Teste de destruição + combate
estadoLabirinto :: Estado
estadoLabirinto = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Terra, Ar, Ar, Ar, Terra, Ar, Ar, Pedra]
        , [Pedra, Terra, Ar, Terra, Ar, Ar, Ar, Terra, Ar, Terra, Pedra]  -- Plataformas
        , [Pedra, Terra, Ar, Terra, Terra, Terra, Terra, Terra, Ar, Terra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Ar, Ar, Ar, Terra, Terra, Terra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = 
        [ Barril (4, 5) False  -- Barril no centro!
        ]
    , minhocasEstado = 
        [ Minhoca  -- EM CIMA da plataforma Terra esquerda
            { posicaoMinhoca = Just (1, 1)  -- Linha 1, coluna 1 = Ar, Terra abaixo
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        , Minhoca  -- EM CIMA da plataforma Terra direita
            { posicaoMinhoca = Just (1, 8)  -- Linha 1, coluna 8 = Ar, Terra abaixo
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        ]
    }

-- TESTE 4: Mapa pequeno - Combate rápido
estadoCombateRapido :: Estado
estadoCombateRapido = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Terra, Ar, Terra, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Terra, Terra, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca
            { posicaoMinhoca = Just (1, 2)
            , vidaMinhoca = Viva 80
            , jetpackMinhoca = 5
            , escavadoraMinhoca = 8
            , bazucaMinhoca = 15
            , minaMinhoca = 8
            , dinamiteMinhoca = 8
            }
        , Minhoca
            { posicaoMinhoca = Just (1, 4)
            , vidaMinhoca = Viva 80
            , jetpackMinhoca = 5
            , escavadoraMinhoca = 8
            , bazucaMinhoca = 15
            , minaMinhoca = 8
            , dinamiteMinhoca = 8
            }
        ]
    }

-- TESTE 5: Guerra Total - 4 minhocas em cantos
estadoGuerraTotal :: Estado
estadoGuerraTotal = Estado
    { mapaEstado = 
        [ [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Terra, Terra, Terra, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Ar, Pedra]
        , [Pedra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Pedra]
        , [Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra, Pedra]
        ]
    , objetosEstado = 
        [ Barril (7, 7) False
        , Barril (7, 8) False
        ]
    , minhocasEstado = 
        [ Minhoca  -- Canto superior esquerdo
            { posicaoMinhoca = Just (1, 2)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        , Minhoca  -- Canto superior direito
            { posicaoMinhoca = Just (1, 13)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        , Minhoca  -- Canto inferior esquerdo
            { posicaoMinhoca = Just (12, 2)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        , Minhoca  -- Canto inferior direito
            { posicaoMinhoca = Just (12, 13)
            , vidaMinhoca = Viva 100
            , jetpackMinhoca = 10
            , escavadoraMinhoca = 20
            , bazucaMinhoca = 30
            , minaMinhoca = 15
            , dinamiteMinhoca = 15
            }
        ]
    }

-- ============================================================================
-- LISTA DE TESTES - APENAS 3 TESTES ESSENCIAIS
-- ============================================================================

testesTarefa4 :: [Estado]
testesTarefa4 = 
    [ estadoCombate1v1        -- Teste 1: Combate direto simples
    , estadoBattleRoyale      -- Teste 2: Múltiplas minhocas
    , estadoLabirinto         -- Teste 3: Combate + Destruição
    ]

dataTarefa4 :: IO TaskData
dataTarefa4 = do
    let ins = testesTarefa4
    outs <- mapM (runTest . tatica) ins
    return $ T4 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa4
