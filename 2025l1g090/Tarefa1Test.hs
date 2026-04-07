module Main where

import Labs2025
import Tarefa1
import Magic

-- ============================================================================
-- TESTES VÁLIDOS
-- ============================================================================

-- Estado válido básico com uma minhoca
estadoValido1 :: Estado
estadoValido1 = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,1)) (Viva 100) 1 1 1 1 1]
    }

-- Estado válido com múltiplas minhocas
estadoValido2 :: Estado
estadoValido2 = Estado
    { mapaEstado = 
        [ [Ar, Ar, Ar, Ar]
        , [Ar, Ar, Ar, Ar]
        , [Terra, Terra, Terra, Terra]
        ]
    , objetosEstado = []
    , minhocasEstado = 
        [ Minhoca (Just (0,0)) (Viva 80) 2 1 3 1 0
        , Minhoca (Just (1,2)) (Viva 100) 1 1 1 1 1
        ]
    }

-- Estado válido com barril
estadoValidoComBarril :: Estado
estadoValidoComBarril = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = [Barril (0,0) False]
    , minhocasEstado = [Minhoca (Just (0,2)) (Viva 100) 1 1 1 1 1]
    }

-- Estado válido com disparo de bazuca
estadoValidoComDisparo :: Estado
estadoValidoComDisparo = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = [Disparo (0,1) Este Bazuca Nothing 0]
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- ============================================================================
-- TESTES INVÁLIDOS - MAPA
-- ============================================================================

-- Mapa vazio (inválido)
estadoInvalidoMapaVazio :: Estado
estadoInvalidoMapaVazio = Estado
    { mapaEstado = []
    , objetosEstado = []
    , minhocasEstado = []
    }

-- Mapa irregular - linhas com tamanhos diferentes (inválido)
estadoInvalidoMapaIrregular :: Estado
estadoInvalidoMapaIrregular = Estado
    { mapaEstado = [[Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = []
    , minhocasEstado = []
    }

-- ============================================================================
-- TESTES INVÁLIDOS - MINHOCAS
-- ============================================================================

-- Minhoca e barril na mesma posição (inválido)
estadoInvalidoMinhocaSobreBarril :: Estado
estadoInvalidoMinhocaSobreBarril = Estado
    { mapaEstado = [[Ar, Ar], [Terra, Terra]]
    , objetosEstado = [Barril (0,0) False]
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- Minhoca viva na água (inválido)
estadoInvalidoMinhocaVivaAgua :: Estado
estadoInvalidoMinhocaVivaAgua = Estado
    { mapaEstado = [[Agua, Ar], [Terra, Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- Minhoca viva sem posição (inválido)
estadoInvalidoMinhocaSemPosicaoViva :: Estado
estadoInvalidoMinhocaSemPosicaoViva = Estado
    { mapaEstado = [[Ar, Ar], [Terra, Terra]]
    , objetosEstado = []
    , minhocasEstado = [Minhoca Nothing (Viva 100) 1 1 1 1 1]
    }

-- ============================================================================
-- TESTES INVÁLIDOS - OBJETOS/DISPAROS
-- ============================================================================

-- Disparo de jetpack não é permitido (inválido)
estadoInvalidoDisparoJetpack :: Estado
estadoInvalidoDisparoJetpack = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = [Disparo (0,1) Este Jetpack Nothing 0]
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- Bazuca não pode ter tempo (inválido)
estadoInvalidoTempoDisparoBazuca :: Estado
estadoInvalidoTempoDisparoBazuca = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = [Disparo (0,1) Este Bazuca (Just 2) 0]
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- Dono do disparo não existe (inválido)
estadoInvalidoDonoInvalido :: Estado
estadoInvalidoDonoInvalido = Estado
    { mapaEstado = [[Ar, Ar, Ar], [Terra, Terra, Terra]]
    , objetosEstado = [Disparo (0,1) Este Bazuca Nothing 5]  -- Dono 5 não existe
    , minhocasEstado = [Minhoca (Just (0,0)) (Viva 100) 1 1 1 1 1]
    }

-- ============================================================================
-- LISTA DE TESTES PARA O SISTEMA DE FEEDBACK
-- ============================================================================

-- | Definir aqui os testes do grupo para a Tarefa 1
testesTarefa1 :: [Estado]
testesTarefa1 = 
    [ estadoValido1
    , estadoValido2
    , estadoValidoComBarril
    , estadoValidoComDisparo
    , estadoInvalidoMapaVazio
    , estadoInvalidoMapaIrregular
    , estadoInvalidoMinhocaSobreBarril
    , estadoInvalidoMinhocaVivaAgua
    , estadoInvalidoMinhocaSemPosicaoViva
    , estadoInvalidoDisparoJetpack
    , estadoInvalidoTempoDisparoBazuca
    , estadoInvalidoDonoInvalido
    ]

dataTarefa1 :: IO TaskData
dataTarefa1 = do
    let ins = testesTarefa1
    outs <- mapM (runTest . validaEstado) ins
    return $ T1 ins outs

main :: IO ()
main = runFeedback =<< dataTarefa1

