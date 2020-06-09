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


-- Punto 3)

type Medicamento = [Hierba]

pondsAntiAge :: Medicamento
pondsAntiAge = [alcachofa,hierbaBuena,hierbaBuena,hierbaBuena]

reduceFatFast ::Int -> Medicamento
reduceFatFast potencia = [hierbaVerde "Obesisdad"] ++ take potencia (repeat alcachofa)


armarPdepCilina :: [String] -> Medicamento
armarPdepCilina [] = []
armarPdepCilina (cabeza : cola) = [hierbaVerde cabeza] ++ armarPdepCilina cola

pdepCilina :: Medicamento
pdepCilina = armarPdepCilina sufijosInfecciosas

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

-- Punto 4)

cantidadIdeal :: (Num a, Enum a) => (a -> Bool) -> a
cantidadIdeal condicion = head (filter condicion [1..])

tieneSobrePeso :: Raton -> Bool
tieneSobrePeso = (>1).peso

masDe3Enfermedades :: Raton -> Bool
masDe3Enfermedades = (>3).length.enfermedades


lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento ratones = all ratonEstabilizado (aplicarMedicamentoAPoblacion medicamento ratones)

ratonEstabilizado :: Raton -> Bool
ratonEstabilizado raton = (not.tieneSobrePeso) raton && masDe3Enfermedades raton

aplicarMedicamentoAPoblacion :: Medicamento -> [Raton] -> [Raton]
aplicarMedicamentoAPoblacion medicamento  = map (aplicarMedicamento medicamento)

aplicarMedicamento :: Medicamento -> Raton -> Raton
aplicarMedicamento medicamento raton = foldl aplicarHierba raton medicamento

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba raton hierba = hierba raton
