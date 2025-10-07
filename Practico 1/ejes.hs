-- Programaci´on Funcional - Pr´actico 1
-- 1. Defina una funci´on sumsqrs que tome 3 n´umeros y retorne la suma de los
-- cuadrados de los dos mayores.
square x = x * x;

sumsqrs :: Int -> Int -> Int -> Int;
sumsqrs x y z = x^2 + y^2 + z^2 - (min x (min y z))^2
-- 2. Defina una funci´on analyze :: Int → Int → Int → Bool, que determina si
-- tres enteros positivos son los lados de un tri´angulo.
analyze :: Int -> Int -> Int -> Bool
analyze a b c =
  a > 0 && b > 0 && c > 0 &&
  a + b > c && a + c > b && b + c > a


-- 3. Defina and y or usando expresiones condicionales. Haga lo mismo utilizando pattern matching.
band :: Bool -> Bool -> Bool
band True True = True  
band _ _ = False

bor :: Bool -> Bool -> Bool
bor False False = False
bor _ _ = True 

-- 4. Defina al conectivo l´ogico implicaci´on como un operador de tipo Bool.
implica True True = True
implica False True = True
implica True False = False
implica False False = True

-- 5. Supongamos que representamos fechas a traves de una tripla de enteros
-- que corresponden a d´ıa, mes y a˜no. Defina una funci´on edad que dada
-- dos fechas, una representando la fecha de nacimiento de una persona, y la
-- otra representando la fecha actual, calcula la edad en a˜nos de la persona.

type Fecha = (Dia, Mes, Año)
type Dia = Int
type Mes = Int
type Año = Int

hayAño :: Fecha -> Fecha -> Int
hayAño (d,m,_) (d2,m2,_) = if m2 > m || m == m2 && d2 >= d then 1 else 0

edad :: Fecha -> Fecha -> Int
edad (d,m,a) (d2,m2,a2) = a2 - a - 1 + hayAño (d,m,0) (d2,m2,0) 
-- 6. Se desea procesar informaci´on relativa a estudiantes. Cada estudiante est´a
-- dado por su nombre (cadena de caracteres), CI (entero), a˜no de ingreso
-- (entero) y lista de cursos aprobados. Cada curso est´a dado por el nombre del curso (cadena de caracteres), c´odigo del curso (entero) y nota de
-- aprobaci´on (entero).
-- (a) Represente la informaci´on de cada estudiante a trav´es de tuplas.
-- (b) Escriba una funci´on que dado un estudiante retorne su nombre y CI.
-- (c) Escriba una funci´on que dado un estudiante retorne su a˜no de ingreso.
-- (d) Escriba una funci´on que dado un estudiante y una nota retorne una
-- lista con los c´odigos de los cursos que aprob´o con esa nota. (Sugerencia: use comprensi´on de listas).
-- (e) Escriba una funci´on que dada una lista de estudiantes retorne una
-- lista de pares (nombre, CI) de aquellos estudiantes ingresados en
-- un determinado a˜no dado como par´ametro. (Sugerencia: use comprensi´on de listas).
-- 7. Rehaga el ejercicio anterior usando ahora tipos de datos algebraicos en
-- lugar de tuplas.
-- 8. Deseamos representar pares internamente ordenados, que son pares de
-- n´umeros reales (r, s) tales que r 6 s.
-- 1
-- (a) Defina el tipo de los pares ordenados
-- (b) Defina una funci´on que dado un par de reales cualesquiera retorna
-- un par internamente ordenado.
-- (c) Defina la operaci´on de suma de pares internamente ordenados, que
-- suma las correspondientes componentes de dos pares retornando un
-- nuevo par.
-- (d) Defina la operaci´on de multiplicaci´on por un escalar, que dado un real
-- y un par internamente ordenado multiplica la primera componente
-- del par por el escalar. El resultado debe ser un par internamente ordenado. Si se pierde el orden se deben intercambiar las componentes.
-- 9. Todo n´umero entero x se puede descomponer de manera ´unica en t´erminos
-- de dos n´umeros enteros y y z , tales que:
-- • −5 < y <= 5
-- • x = y + 10 × z.
-- Defina una funci´on que dado un entero x devuelve una tupla con los
-- n´umeros y y z .
-- 10. Deseamos representar n´umeros racionales y operaciones sobre ellos. Los
-- racionales son representados por pares de enteros cuya segunda componente es distinta de cero. Cada racional tiene infinitas representaciones,
-- pero existe la llamada representaci´on can´onica en la que la segunda componente del par de enteros es mayor que cero y ambos enteros son primos
-- entre si.
-- (a) Defina el tipo racional
-- (b) Defina una funci´on que dado un par de enteros, el segundo de los
-- cuales es distinto de cero, retorne un racional en su representaci´on
-- can´onica.
-- (c) Defina las operaciones de suma, resta, multiplicaci´on, y negaci´on de
-- racionales, e int2rac, que convierte un entero en un racional. Dichas
-- operaciones deben devolver representaciones can´onicas como resultado.
-- Nota: Puede usar la funci´on gcd (definida en el Prelude) la cual
-- computa el m´aximo com´un divisor de dos n´umeros.
-- 11. Dado el siguiente tipo para representar tri´angulos:
-- data Triangulo = Equi Int | Iso Int Int | Esca Int Int Int
-- Defina la funci´on mkTriangulo que dados tres enteros positivos, que representan a los lados de un tri´angulo v´alido, retorna un valor de tipo
-- Triangulo.
-- 2

