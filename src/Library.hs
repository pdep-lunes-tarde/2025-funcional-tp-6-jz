module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatiVegano | PanIntegral | BaconDeTofu
    deriving (Eq, Show)
precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo = 10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


cuartoDeLibra::Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

listaIngredientes :: Hamburguesa -> [Ingrediente]
listaIngredientes hamburguesa = ingredientes hamburguesa

esCarne :: Ingrediente -> Bool
esCarne ingrediente = ingrediente == Carne

esPollo :: Ingrediente -> Bool
esPollo ingrediente = ingrediente == Pollo

verificarSiEsCarne :: [Ingrediente] -> Bool
verificarSiEsCarne lista = any esCarne lista

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase hamburguesa  
    | any esCarne (ingredientes hamburguesa) = Carne
    | otherwise = Pollo

verificarIncompatibilidad :: Ingrediente -> Hamburguesa -> Bool
verificarIncompatibilidad ingrediente hamburguesa =
  (ingredienteBase hamburguesa == Pollo && esCarne ingrediente) ||
  (ingredienteBase hamburguesa == Carne && esPollo ingrediente)

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
  hamburguesa { 
    ingredientes = ingrediente : ingredientes hamburguesa, 
    precioBase = precioBase hamburguesa + precioIngrediente ingrediente }

agrandar :: Ingrediente -> Hamburguesa -> Hamburguesa
agrandar ingrediente hamburguesa 
    | verificarIncompatibilidad ingrediente hamburguesa = error "Ingredientes incompatibles"
    | otherwise = agregarIngrediente ingrediente hamburguesa


descuento:: Number -> Hamburguesa -> Hamburguesa
descuento numero hamburguesa = hamburguesa {precioBase = numero * precioBase hamburguesa}

aplicarCambios :: Hamburguesa -> Hamburguesa
aplicarCambios = descuento 0.9 . agrandar Panceta . agrandar Panceta hamburguesa


pdepBurguer :: Hamburguesa -> Hamburguesa
pdepBurguer pdepBurga = aplicarCambios pdepBurga

dobleCuarto:: cuartoDeLibra -> Hamburguesa
dobleCuarto cuartoDeLibra = agrandar Carne cuartoDeLibra


bigPdep :: Hamburguesa -> Hamburguesa
bigPdep dobleCuarto = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 0.3 . agregarIngrediente Papas hamburguesa



reemplazarCarnes :: Hamburguesa -> Hamburguesa
reemplazarCarnes = reemplazarIngrediente Carne PatiVegano


reemplazarCheddar :: Hamburguesa -> Hamburguesa
reemplazarCheddar = reemplazarIngrediente Cheddar QuesoDeAlmendras


reemplazarBacon :: Hamburguesa -> Hamburguesa
reemplazarBacon = reemplazarIngrediente Panceta BaconDeTofu

reemplazarToppings :: Hamburguesa -> Hamburguesa
reemplazarToppings = reemplazarBacon . reemplazarCheddar


actualizarPrecio :: Hamburguesa -> Hamburguesa
actualizarPrecio hamburguesa = hamburguesa { precioBase = sum (map precioIngrediente (ingredientes hamburguesa))}

hacerVeggie:: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = actualizarPrecio . reemplazarCarnes . reemplazarToppings hamburguesa


funcion :: Ingrediente -> Ingrediente -> Ingrediente
funcion viejoIngrediente nuevoIngrediente = nuevoIngrediente

reemplazarIngrediente :: Ingrediente -> Ingrediente -> Hamburguesa -> Hamburguesa
reemplazarIngrediente viejoIng nuevoIng hamburguesa =
  hamburguesa { ingredientes = map (reemplazo viejoIng nuevoIng) (ingredientes hamburguesa) }

reemplazo :: Ingrediente -> Ingrediente -> Ingrediente -> Ingrediente
reemplazo viejo nuevo actual
  | actual == viejo = nuevo
  | otherwise       = actual

reemplazarPanes :: Hamburguesa -> Hamburguesa 
reemplazarPanes hamburguesa = reemplazarIngrediente Pan PanIntegral hamburguesa


cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati hamburguesa = actualizarPrecio . reemplazarPanes hamburguesa 

dobleCuartoVegano:: Hamburguesa -> Hamburguesa
dobleCuartoVegano dobleCuarto = actualizarPrecio . hacerVeggie dobleCuarto
