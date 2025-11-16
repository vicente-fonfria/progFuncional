{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
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
getCI = undefined

getNombre :: JSON -> Maybe String
getNombre = undefined

getApellido :: JSON -> Maybe String
getApellido = undefined

getCursos :: JSON -> Maybe JSON
getCursos = undefined

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados = undefined

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio = undefined

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad = undefined 

-- agrega curso a lista de cursos de un estudiante
addCurso :: Object JSON -> JSON -> JSON
addCurso = undefined
