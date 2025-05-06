module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

listaIngredientes :: Hamburguesa -> [Ingrediente]
listaIngredientes hamburguesa = Hamburguesa {ingredientes hamburguesa}
--recorro la lista hasta encontrar carne o pollo

verificarSiEsCarne :: [Ingrediente] -> Bool
verificarSiEsCarne lista = any

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase hamburguesa = 
    |any (verificarSiEsCarne listaIngredientes hamburguesa) listaIngredientes hamburguesa

verificarIncompatibilidad :: Ingrediente -> Hamburguesa -> Bool
verificarIncompatibilidad ingrediente hamburguesa =
  (ingredienteBase hamburguesa == Pollo && ingrediente == Carne) ||
  (ingredienteBase hamburguesa == Carne && ingrediente == Pollo)

-- implementar guardas con una funcion auxiliar
agrandar:: Ingrediente -> Hamburguesa -> Hamburguesa
agrandar ingrediente hamburguesa = 
    |verificarIncompatibilidad ingrediente hamburguesa = error "Ingredientes incompatibles"
    |otherwise = agregarIngrediente ingrediente hamburguesa



agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa =
  hamburguesa { ingredientes = ingrediente : ingredientes hamburguesa }


descuento:: Number -> Hamburguesa -> Hamburguesa
descuento numero hamburguesa = Hamburguesa {precioBase = numero * precioBase hamburguesa}
pdepBurguer:: Hamburguesa -> Hamburguesa

dobleCuarto::
bigPdep ::
delDia::

hacerVeggie::

cambiarPanDePati::

dobleCuartoVegano::
