-- 1: Actividades de los personajes

data Personaje = Personaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
} deriving Show

-- Personajes
homero :: Personaje
homero = Personaje "Homero" 100 100
lisa :: Personaje
lisa = Personaje "Lisa" 100 100
skinner :: Personaje
skinner = Personaje "Skinner" 100 100
burns :: Personaje
burns = Personaje "Burns" 100000 100
bart :: Personaje
bart = Personaje "Bart" 6 100

-- Actividades
type Actividad = Personaje -> Personaje 

escuela :: Actividad
escuela personaje
    | nombre personaje == "Lisa" = modificarFelicidad 20 personaje
    | otherwise = modificarFelicidad (-20) personaje

-- Ejemplos de consulta: 
-- escuela bart 
-- Personaje {nombre = "Bart", dinero = 100, felicidad = 80}
-- escuela lisa 
-- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}

comerDonas :: Int -> Actividad
comerDonas cantidad = modificarDinero (-10) . modificarFelicidad (10 * cantidad)

-- Ejemplo de consulta: 
-- comerDonas 12 homero 
-- Personaje {nombre = "Homero", dinero = 90, felicidad = 220}

trabajar :: String -> Actividad
trabajar trabajo = modificarDinero (length trabajo)

-- Ejemplo de consulta: 
-- trabajar "planta nuclear" homero 
-- Personaje {nombre = "Homero", dinero = 114, felicidad = 100}

trabajarDeDirector :: Actividad
trabajarDeDirector = trabajar "Escuela elemental". escuela

-- Ejemplo de consulta: 
-- trabajarDeDirector skinner 
-- Personaje {nombre = "Skinner", dinero = 117, felicidad = 80}


modificarFelicidad :: Int -> Actividad
modificarFelicidad cantidad personaje 
 = personaje {felicidad = max 0 (felicidad personaje + cantidad)}

modificarDinero ::  Int -> Actividad
modificarDinero cantidad personaje 
 = personaje {dinero = dinero personaje + cantidad}


-- 2: Logros

type Logro = Personaje -> Bool

millonario :: Logro
millonario personaje = dinero personaje > dinero burns

alegrarse :: Int -> Logro
alegrarse nivel personaje = felicidad personaje > nivel

verKrusty :: Logro
verKrusty personaje = dinero personaje >= 10

-- A)
decisiva :: Actividad -> Logro -> Personaje -> Bool
decisiva actividad logro personaje = not (logro personaje) && logro (actividad personaje)

-- Ejemplos de consulta: 
-- decisiva (trabajar "mafia") verKrusty bart 
-- True
-- decisiva (trabajar "planta nuclear") millonario homero 
-- False

-- B)
primeraDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
primeraDecisiva personaje _ [] = personaje
primeraDecisiva personaje logro (actividad:actividades)
    | decisiva actividad logro personaje = actividad personaje
    | otherwise = primeraDecisiva personaje logro actividades

-- Ejemplos de consulta: 
-- primeraDecisiva bart verKrusty [trabajar "no", trabajar "mafia"] 
-- Personaje {nombre = "Bart", dinero = 11, felicidad = 100}
-- primeraDecisiva homero millonario [trabajar "mafia", trabajar "planta nuclear"] 
-- Personaje {nombre = "Homero", dinero = 100, felicidad = 100}

-- C)
infinitasActividades :: [Actividad]
infinitasActividades = escuela : infinitasActividades

-- Ejemplos de consulta:
-- primeraActividadDecisiva lisa (alegrarse 105) listaInfinitaActividades 
-- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}
-- Por evaluación diferida, encuentra la primera que es decisiva y la aplica
-- primeraActividadDecisiva bart irAVerAKrosty listaInfinitaActividades 
-- Hasta el momento no encontró ninguna decisiva, por lo que no dio ninguna respuesta, pero sigue buscando...