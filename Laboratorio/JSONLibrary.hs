{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module JSONLibrary
 (lookupField,
  lookupFieldObj,
  keysOf,
  valuesOf,
  entriesOf,
  leftJoin,
  rightJoin,
  filterArray,
  insertKV,
  sortKeys,
  mkJString, mkJNumber, mkJBoolean, mkJNull, mkJObject, mkJArray,
  fromJString, fromJNumber, fromJBoolean, fromJObject, fromJArray,
  isJString, isJNumber, isJBoolean, isJNull, isJObject, isJArray,
  JSON(),importJSON,
  Object()
 )
where

import AST


{- lookupField:
 Cuando el primer argumento es un objeto y tiene como clave el valor
 dado como segundo argumento, entonces se retorna el valor JSON
 correspondiente (bajo el constructor {\tt Just}). De lo contrario se
 retorna {\tt Nothing}. Si un objeto tiene claves repetidas, se
 retorna el valor de más a la derecha.
-}
lookupField :: JSON -> Key -> Maybe JSON
lookupField (JObject obj) key = lookupFieldObj obj key
lookupField _              _ = Nothing

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj :: Object JSON -> Key -> Maybe JSON
lookupFieldObj obj key = lookup key (reverse obj)

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object a -> [Key]
keysOf obj = map fst obj


-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object JSON -> [JSON]
valuesOf obj = map snd obj


-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object JSON -> [(Key,JSON)]
entriesOf obj = obj

--para usar en funciones
containsKey :: Key -> [Key] -> Bool
containsKey _ [] = False
containsKey k (x:xs)
  | k == x = True
  | otherwise = containsKey k xs


-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del primer objeto.
leftJoin :: Object a -> Object a -> Object a
leftJoin o1 o2 =
  let ks1 = keysOf o1
      onlyRight = filter (\(k, _) -> not (containsKey k ks1)) o2
  in o1 ++ onlyRight


-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del segundo objeto.
rightJoin :: Object a -> Object a -> Object a
rightJoin o1 o2 =
  let ks2 = keysOf o2
      onlyLeft = filter (\(k, _) -> not (containsKey k ks2)) o1
  in o2 ++ onlyLeft

-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) ->  Array -> Array
filterArray fnPredicado arr = filter fnPredicado arr 

-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV kv [] = [kv]
insertKV kv@(k, _) ((k', v') : xs)
  | k <= k' = kv : (k', v') : xs
  | otherwise = (k', v') : insertKV kv xs


-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV kv obj = kv : obj

-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys [] = []
sortKeys (kv : xs) = insertKV kv (sortKeys xs)



-- constructoras
mkJString :: String -> JSON
mkJString s = JString s

mkJNumber :: Integer -> JSON
mkJNumber n = JNumber n

mkJBoolean :: Bool -> JSON
mkJBoolean b = JBoolean b

mkJNull :: () -> JSON
mkJNull () = JNull

mkJArray :: [JSON] -> JSON
mkJArray xs = JArray xs

mkJObject :: [(Key, JSON)] -> JSON
mkJObject campos = JObject campos


-- destructoras
fromJString :: JSON -> Maybe String
fromJString (JString s) = Just s
fromJString _ = Nothing

fromJNumber :: JSON -> Maybe Integer
fromJNumber (JNumber n) = Just n
fromJNumber _ = Nothing

fromJBoolean  :: JSON -> Maybe Bool
fromJBoolean (JBoolean b) = Just b
fromJBoolean _ = Nothing

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject (JObject obj) = Just obj
fromJObject _ = Nothing

fromJArray :: JSON -> Maybe [JSON]
fromJArray (JArray xs) = Just xs
fromJArray _ = Nothing



-- predicados
isJNumber :: JSON -> Bool
isJNumber (JNumber _) = True
isJNumber _ = False

isJNull :: JSON -> Bool
isJNull JNull = True
isJNull _ = False

isJString :: JSON -> Bool
isJString (JString _) = True
isJString _ = False

isJObject :: JSON -> Bool
isJObject (JObject _) = True
isJObject _ = False

isJArray :: JSON -> Bool
isJArray (JArray _) = True
isJArray _ = False

isJBoolean :: JSON -> Bool
isJBoolean (JBoolean _) = True
isJBoolean _ = False
