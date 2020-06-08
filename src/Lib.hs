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