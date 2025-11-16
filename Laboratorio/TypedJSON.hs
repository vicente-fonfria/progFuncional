{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module TypedJSON where

import AST
import JSONLibrary
import Control.Monad
import Data.List


-- Tipos JSON
data JSONType
 = TyString
  | TyNum
  | TyObject (Object JSONType)
  | TyArray JSONType
  | TyBool
  | TyNull
  deriving (Show, Eq)

-- deja la lista sin elementos repetidos
sinDuplicados :: Eq a => [a] -> [a]
sinDuplicados [] = []
sinDuplicados (x:xs)
  | x `elem` xs = sinDuplicados xs
  | otherwise = x : sinDuplicados xs

-- ayuda: objeto sin claves repetidas
noDupKeys :: Object a -> Bool
noDupKeys o =
  let ks = keysOf o
  in ks = = sinDuplicados ks


-- dado un valor JSON se infiere el tipo. Se devuelve
-- Nothing si el valor está mal tipado
typeOf :: JSON -> Maybe JSONType
typeOf (JString _) = Just TyString
typeOf (JNumber _) = Just TyNum
typeOf (JBoolean _) = Just TyBool
typeOf JNull = Just TyNull
typeOf (JArray xs) = tipoArray xs
typeOf (JObject o)
  | not (noDupKeys o) = Nothing
  | otherwise =
      case tiposCampos o of
        Nothing -> Nothing
        Just camposTipados ->
          Just (TyObject (sortKeys camposTipados))


-- arreglos: no vacíos y homogéneos
tipoArray :: [JSON] -> Maybe JSONType
tipoArray [] = Nothing
tipoArray (x:xs) =
  case typeOf x of
    Nothing -> Nothing
    Just t  ->
      if todosDelMismoTipo t xs
         then Just (TyArray t)
         else Nothing

todosDelMismoTipo :: JSONType -> [JSON] -> Bool
todosDelMismoTipo _ [] = True
todosDelMismoTipo t (y:ys) =
  case typeOf y of
    Nothing  -> False
    Just ty  -> ty = = t && todosDelMismoTipo t ys


-- objeto: tipar cada campo (k, v) -> (k, tipoDeV)
tiposCampos :: [(Key, JSON)] -> Maybe (Object JSONType)
tiposCampos [] = Just []
tiposCampos ((k,v):xs) =
  case typeOf v of
    Nothing -> Nothing
    Just t  ->
      case tiposCampos xs of
        Nothing   -> Nothing
        Just rest -> Just ((k,t) : rest)


-- decide si una lista de claves está ordenada estrictamente
clavesOrdenadas :: [Key] -> Bool
clavesOrdenadas [] = True
clavesOrdenadas [_] = True
clavesOrdenadas (k1:k2:ks) =
  k1 < k2 && clavesOrdenadas (k2:ks)


-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf obj =
  noDupKeys obj && clavesOrdenadas (keysOf obj)

-- decide si todos los tipos dentro de un objeto son bien formados
tiposObjetoWf :: Object JSONType -> Bool
tiposObjetoWf [] = True
tiposObjetoWf ((_, t) : xs) =
  typeWf t && tiposObjetoWf xs

-- decide si todos los tipos objeto contenidos en un tipo JSON
-- están bien formados.
typeWf :: JSONType -> Bool
typeWf TyString = True
typeWf TyNum = True
typeWf TyBool = True
typeWf TyNull = True
typeWf (TyArray t) = typeWf t
typeWf (TyObject obj) =
  objectWf obj && tiposObjetoWf obj

-- dado un valor JSON v, y un tipo t, decide si v tiene tipo t.
hasType :: JSON -> JSONType -> Bool
hasType v t =
  case typeOf v of
    Nothing  -> False
    Just t'  -> t' == t && typeWf t
