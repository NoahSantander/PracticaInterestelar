module Library where
import PdePreludat

-- Defino mis alias
type Nombre = String
type X = Number
type Y = Number 
type Z = Number
type Posicion = (X,Y,Z)
type RelacionTiempoTierraAnios = Number -> Number
type EdadTerrestre = Number
type Distancia = Number
type Tiempo = Number
type Velocidad = Number
type Anios = Number
type Nave = Planeta -> Planeta -> Tiempo
type TanquesDeOxigeno = Number
type Viaje = Planeta -> Planeta -> Nave -> Astronauta -> Astronauta
type Astronautas = [Astronauta]

-- Defino mis tipos
data Planeta = UnPlaneta {
    nombrePlaneta :: Nombre,
    posicion :: Posicion,
    relacionTiempoTierraAnios :: RelacionTiempoTierraAnios
} deriving Show

data Astronauta = UnAstronauta {
    nombreAstronauta :: Nombre,
    edadTerrestre :: EdadTerrestre,
    planetaActual :: Planeta
} deriving Show

-- Defino mis funciones auxiliares
coordX (x, _, _) = x
coordY (_, y, _) = y
coordZ (_, _, z) = z

x = coordX.posicion
y = coordY.posicion
z = coordZ.posicion

-- Defino la distancia entre dos planetas
distancia :: Planeta -> Planeta -> Distancia
distancia p1 p2 = sqrt((^2) (x p1 - x p2) + (^2) (y p1 - y p2) + (^2) (z p1 - z p2))

-- Defino el tiempo de viaje de un planeta a otro
tiempoDeViaje :: Planeta -> Planeta -> Velocidad -> Tiempo
tiempoDeViaje p1 p2 velocidad = (/velocidad) (distancia p1 p2)

-- Defino la función pasarTiempo
pasarTiempo :: Anios -> Astronauta -> Astronauta
pasarTiempo anios astronauta = astronauta {edadTerrestre = edadTerrestre astronauta + relacionTiempoTierraAnios (planetaActual astronauta) anios}

-- Defino las naves
naveVieja :: TanquesDeOxigeno -> Nave
naveVieja tanquesDeOxigeno p1 p2 
    | tanquesDeOxigeno < 6 = tiempoDeViaje p1 p2 10
    | otherwise = tiempoDeViaje p1 p2 7

naveFuturista :: Nave
naveFuturista p1 p2 = 0

-- Defino las relaciones
relacionTerrestre :: RelacionTiempoTierraAnios
relacionTerrestre anios = anios

relacionPlanetaDrMann :: RelacionTiempoTierraAnios
relacionPlanetaDrMann = (* 7)

-- Defino los planetas
tierra = UnPlaneta "Tierra" (0,0,0) relacionTerrestre
planetaDrMann = UnPlaneta "Planeta Dr Mann" (54, 79, 104) relacionPlanetaDrMann

-- Defino los astronautas
cooper = UnAstronauta "Cooper" 30 tierra
amelia = UnAstronauta "Amelia" 25 tierra
drMann = UnAstronauta "Dr Mann" 35 planetaDrMann

astronautasRescatistas = [cooper, amelia]

-- Defino que un astronauta pueda viajar a otro planeta usando una nave
realizarViaje :: Viaje
realizarViaje p1 p2 nave astronauta = astronauta {planetaActual = p2, edadTerrestre = (edadTerrestre.(pasarTiempo (nave p1 p2))) astronauta}

-- Defino que un grupo de astronautas rescate a otro
irABuscarlo :: Astronauta -> Nave -> Astronautas -> Astronautas
irABuscarlo astronauta nave astronautas =  map (realizarViaje ((planetaActual.head) astronautas) (planetaActual astronauta) nave) (astronauta:astronautas)

volver :: Planeta -> Nave -> Astronautas -> Astronautas
volver planetaVolver nave astronautas = map (realizarViaje ((planetaActual.head) astronautas) planetaVolver nave) astronautas

rescatarAstronauta :: Astronautas -> Astronauta -> Nave -> Astronautas
rescatarAstronauta astronautas astronauta nave = ((volver ((planetaActual.head) astronautas) nave).(irABuscarlo astronauta nave)) astronautas

-- Inferir tipo de la función
-- Ord t => (t -> a -> t) -> t -> (Number -> a -> Bool) -> [a] -> Bool
f a b c = any ((> b).a b).filter (c 10)