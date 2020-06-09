module Lib where
import Text.Show.Functions

laVerdad = True

-- Punto 1)

type Enfermedad = String

data Raton = UnRaton {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    enfermedades :: [Enfermedad]
}deriving (Show)

cerebro :: Raton
cerebro = UnRaton {
    nombre = "Cerebro",
    edad = 9,
    peso = 0.2,
    enfermedades = ["brucelosis","sarampion","tuberculosis"]
}

bicenterrata :: Raton
bicenterrata = UnRaton {
    nombre = "Bicenterrata",
    edad = 256,
    peso = 0.2,
    enfermedades = []
}

huesudo :: Raton
huesudo = UnRaton {
    nombre = "Huesudo",
    edad = 4,
    peso = 10,
    enfermedades = ["alta obesidad", "sinusitis"]
}


-- Punto 2)

type Hierba = Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = edad raton - rejuvenecerRaton ((sqrt.edad) raton) raton}

rejuvenecerRaton :: Float -> Raton -> Float
rejuvenecerRaton años raton = edad raton - años

hierbaVerde :: String -> Hierba
hierbaVerde terminacion raton = raton {enfermedades = filter (noTerminaCon terminacion) (enfermedades raton)}

noTerminaCon :: String -> String -> Bool
noTerminaCon terminacion enfermedad = terminacion /= drop (length enfermedad - length terminacion) enfermedad

alcachofa :: Hierba
alcachofa raton = raton {peso = peso raton - (peso raton) * perderPesoPorcentual raton}

perderPesoPorcentual :: Raton -> Float
perderPesoPorcentual raton | peso raton > 2 = 0.10
                           | otherwise = 0.05

hierbaZort :: Hierba
hierbaZort raton = raton {
    nombre = "Pinky",
    edad = 0,
    enfermedades = []
}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = raton {peso = max 0 (peso raton - 0.1), enfermedades = enfermedadesConMasDe10Letras raton}

enfermedadesConMasDe10Letras :: Raton -> [Enfermedad]
enfermedadesConMasDe10Letras raton = filter ((>10).length) (enfermedades raton)