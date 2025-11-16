{- Grupo: 11
   Integrante(s):
     Fonfría, Vicente, 50889288
-}

module Estudiantes where

import JSONLibrary
import TypedJSON

---------------------------------------------------------------------------------------
-- Importante:
-- Notar que NO se puede importar el módulo AST, que es interno a la biblioteca.
---------------------------------------------------------------------------------------


-- decide si un valor que representa un estudiante esta bien formado
estaBienFormadoEstudiante :: JSON -> Bool
estaBienFormadoEstudiante v =
  hasType v tyEstudiante && cursosBienFormados v


-- tipo de curso (mismo que en los tests)
tyCurso :: JSONType
tyCurso =
  TyObject [ ("anio",     TyNum)
           , ("codigo",   TyNum)
           , ("nombre",   TyString)
           , ("nota",     TyNum)
           , ("semestre", TyNum)
           ]

-- tipo de estudiante (mismo que en los tests)
tyEstudiante :: JSONType
tyEstudiante =
  TyObject [ ("CI",       TyNum)
           , ("apellido", TyString)
           , ("cursos",   TyArray tyCurso)
           , ("nombre",   TyString)
           ]


-- extrae la lista de cursos y chequea que esté bien formada
cursosBienFormados :: JSON -> Bool
cursosBienFormados v =
  case lookupField v "cursos" of
    Nothing   -> False
    Just arr  ->
      case fromJArray arr of
        Nothing      -> False
        Just cursos  -> cursosOrdenados cursos

-- la lista de cursos debe estar en orden:
-- año descendente, semestre descendente, código ascendente
cursosOrdenados :: [JSON] -> Bool
cursosOrdenados []      = False            -- no debería pasar si hasType es True
cursosOrdenados [c]     = esCursoValido c  -- un solo curso siempre respeta el orden
cursosOrdenados (c1:c2:cs) =
  esCursoValido c1 &&
  case (tripleCurso c1, tripleCurso c2) of
    (Just t1, Just t2) ->
      ordenOK t1 t2 && cursosOrdenados (c2:cs)
    _ -> False

esCursoValido :: JSON -> Bool
esCursoValido c =
  case tripleCurso c of
    Just _  -> True
    Nothing -> False

-- (anio, semestre, codigo) del curso, si está bien armado
tripleCurso :: JSON -> Maybe (Integer, Integer, Integer)
tripleCurso c =
  case fromJObject c of
    Nothing   -> Nothing
    Just obj  ->
      case ( lookupFieldObj obj "anio"
           , lookupFieldObj obj "semestre"
           , lookupFieldObj obj "codigo") of
        (Just ja, Just js, Just jc) ->
          case ( fromJNumber ja
               , fromJNumber js
               , fromJNumber jc) of
            (Just a, Just s, Just co) -> Just (a, s, co)
            _                         -> Nothing
        _ -> Nothing

-- orden correcto entre dos cursos:
-- primero años descendentes, luego semestre descendente,
-- y si coinciden, código ascendente
ordenOK :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
ordenOK (a1,s1,c1) (a2,s2,c2)
  | a1 > a2 = True
  | a1 < a2 = False
  | s1 > s2 = True
  | s1 < s2 = False
  | otherwise = c1 <= c2

-- getters
getCI :: JSON -> Maybe Integer
getCI v =
  case lookupField v "CI" of
    Nothing    -> Nothing
    Just jCI   -> fromJNumber jCI

getNombre :: JSON -> Maybe String
getNombre v =
  case lookupField v "nombre" of
    Nothing      -> Nothing
    Just jNombre -> fromJString jNombre

getApellido :: JSON -> Maybe String
getApellido v =
  case lookupField v "apellido" of
    Nothing        -> Nothing
    Just jApellido -> fromJString jApellido

getCursos :: JSON -> Maybe JSON
getCursos v =
  lookupField v "cursos"

-- obtiene arreglo con cursos que fueron aprobados (nota >= 3)
aprobados :: JSON -> Maybe JSON
aprobados est =
  case fromJObject est of
    Nothing    -> Nothing
    Just obj   ->
      case lookupFieldObj obj "cursos" of
        Nothing        -> Nothing
        Just cursosVal ->
          case fromJArray cursosVal of
            Nothing      -> Nothing
            Just cursos  ->
              let cursosAprob   = filter cursoAprobado cursos
                  nuevoCursos   = mkJArray cursosAprob
                  objActualizado =
                    reemplazarCursos nuevoCursos obj
              in Just (mkJObject objActualizado)

-- curso aprobado si tiene nota >= 3
cursoAprobado :: JSON -> Bool
cursoAprobado c =
  case fromJObject c of
    Nothing   -> False
    Just objc ->
      case lookupFieldObj objc "nota" of
        Nothing     -> False
        Just jNota  ->
          case fromJNumber jNota of
            Nothing  -> False
            Just n   -> n >= 3

-- reemplaza el valor del campo "cursos" manteniendo orden y resto igual
reemplazarCursos :: JSON -> Object JSON -> Object JSON
reemplazarCursos nuevos [] = []
reemplazarCursos nuevos ((k,v):xs)
  | k == "cursos" = (k, nuevos) : xs
  | otherwise     = (k, v) : reemplazarCursos nuevos xs

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio anio est =
  case getCursos est of
    Nothing      -> Nothing
    Just jCursos ->
      case fromJArray jCursos of
        Nothing      -> Nothing
        Just cursos  ->
          let cursosEnAnio = filter (cursoEnAnio anio) cursos
          in Just (mkJArray cursosEnAnio)

-- curso pertenece al año dado
cursoEnAnio :: Integer -> JSON -> Bool
cursoEnAnio anio c =
  case fromJObject c of
    Nothing   -> False
    Just obj  ->
      case lookupFieldObj obj "anio" of
        Nothing     -> False
        Just jAnio  ->
          case fromJNumber jAnio of
            Nothing  -> False
            Just n   -> n == anio

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad est =
  case getCursos est of
    Nothing      -> Nothing
    Just jCursos ->
      case fromJArray jCursos of
        Nothing      -> Nothing
        Just cursos  ->
          case notasDeCursos cursos of
            Nothing    -> Nothing
            Just []    -> Nothing
            Just notas ->
              let suma   = sum notas
                  cant   = length notas
              in Just (fromIntegral suma / fromIntegral cant)


-- obtiene todas las notas de una lista de cursos;
-- falla (Nothing) si algún curso no tiene "nota" bien formada
notasDeCursos :: [JSON] -> Maybe [Integer]
notasDeCursos [] = Just []
notasDeCursos (c:cs) =
  case fromJObject c of
    Nothing   -> Nothing
    Just obj  ->
      case lookupFieldObj obj "nota" of
        Nothing    -> Nothing
        Just jNota ->
          case fromJNumber jNota of
            Nothing -> Nothing
            Just n  ->
              case notasDeCursos cs of
                Nothing   -> Nothing
                Just rest -> Just (n : rest)

-- agrega curso a lista de cursos de un estudiante
addCurso :: Object JSON -> JSON -> JSON
addCurso cursoObj est =
  case fromJObject est of
    -- si no es un objeto, lo dejo como está (los tests no pasan por acá)
    Nothing   -> est
    Just obj  ->
      let nuevoCursoJSON = mkJObject cursoObj
          cursosJSON     = lookupFieldObj obj "cursos"
          nuevosCursosJSON =
            case cursosJSON of
              -- si no tenía campo "cursos", creo un array con solo el nuevo
              Nothing      -> mkJArray [nuevoCursoJSON]
              Just jCursos ->
                case fromJArray jCursos of
                  -- si "cursos" no es un array, lo dejo como estaba
                  Nothing      -> jCursos
                  Just cursos  ->
                    mkJArray (insertarCurso nuevoCursoJSON cursos)

          objActualizado = actualizarCursos nuevosCursosJSON obj
      in mkJObject objActualizado
  where
    -- Reemplaza el valor de la clave "cursos" manteniendo las demás igual
    actualizarCursos :: JSON -> Object JSON -> Object JSON
    actualizarCursos nuevos [] = []
    actualizarCursos nuevos ((k,v):xs)
      | k == "cursos" = (k, nuevos) : xs
      | otherwise     = (k, v) : actualizarCursos nuevos xs

    -- Inserta un curso en una lista de cursos ordenada
    insertarCurso :: JSON -> [JSON] -> [JSON]
    insertarCurso c [] = [c]
    insertarCurso c (x:xs) =
      case (tripleCurso c, tripleCurso x) of
        (Just t1, Just t2) ->
          if vieneAntes t1 t2
             -- c debe ir antes que x
             then c : x : xs
             -- sigo buscando más adelante
             else x : insertarCurso c xs
        -- si algo está mal formado, lo encajo adelante y ya
        _ -> c : x : xs

    -- Extrae (anio, semestre, codigo) del JSON de un curso
    tripleCurso :: JSON -> Maybe (Integer, Integer, Integer)
    tripleCurso cursoJSON =
      case fromJObject cursoJSON of
        Nothing       -> Nothing
        Just objCurso ->
          case ( lookupFieldObj objCurso "anio"
               , lookupFieldObj objCurso "semestre"
               , lookupFieldObj objCurso "codigo") of
            (Just jAnio, Just jSem, Just jCod) ->
              case ( fromJNumber jAnio
                   , fromJNumber jSem
                   , fromJNumber jCod) of
                (Just anio, Just sem, Just cod) ->
                  Just (anio, sem, cod)
                _ -> Nothing
            _ -> Nothing

    -- Orden correcto: año desc, semestre desc, código asc
    vieneAntes :: (Integer,Integer,Integer)
               -> (Integer,Integer,Integer)
               -> Bool
    vieneAntes (a1,s1,c1) (a2,s2,c2)
      | a1 > a2 = True
      | a1 < a2 = False
      | s1 > s2 = True
      | s1 < s2 = False
      | otherwise = c1 <= c2
