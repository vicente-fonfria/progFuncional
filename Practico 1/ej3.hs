--  Defina and y or usando expresiones condicionales. Haga lo mismo utilizando pattern matching.

and :: Bool -> Bool -> Bool
and x y = if x then y else False

or :: Bool -> Bool -> Bool
or x y = if x then True else y

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

-- . Defina al conectivo l´ogico implicaci´on como un operador de tipo Bool.
implicacion :: Bool -> Bool -> Bool
implicacion x y = not x || y

-- Supongamos que representamos fechas a traves de una tripla de enteros
-- que corresponden a d´ıa, mes y a˜no. Defina una funci´on edad que dada
-- dos fechas, una representando la fecha de nacimiento de una persona, y la
-- otra representando la fecha actual, calcula la edad en a˜nos de la persona

type Fecha = (Int, Int, Int)

edad :: Fecha -> Fecha -> Int
edad (d1, m1, a1) (d2, m2, a2) = a2 - a1 - if m2 < m1 || (m2 == m1 && d2 < d1) then 1 else 0


-- Se desea procesar informaci´on relativa a estudiantes. Cada estudiante est´a
-- dado por su nombre (cadena de caracteres), CI (entero), a˜no de ingreso
-- (entero) y lista de cursos aprobados. Cada curso est´a dado por el nombre del curso (cadena de caracteres), c´odigo del curso (entero) y nota de
-- aprobaci´on (entero).

-- (a) Represente la informaci´on de cada estudiante a trav´es de tuplas.

curso:: (String, Int, Int)

estudiante:: (String, Int, Int, [curso])

