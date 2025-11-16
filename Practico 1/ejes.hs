import System.Win32 (COORD(yPos))

-- Programacion Funcional - Practico 1
-- 1. Defina una funcion sumsqrs que tome 3 numeros y retorne la suma de los
-- cuadrados de los dos mayores.
square x = x * x;

sumsqrs :: Integer -> Integer -> Integer -> Integer;
sumsqrs x y z = x^2 + y^2 + z^2 - min x (min y z)^2
-- 2. Defina una funcion analyze :: Integer → Integer → Integer → Bool, que determina si
-- tres enteros positivos son los lados de un triangulo.
analyze :: Integer -> Integer -> Integer -> Bool
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

-- 4. Defina al conectivo logico implicacion como un operador de tipo Bool.
implica True True = True
implica False True = True
implica True False = False
implica False False = True

-- 5. Supongamos que representamos fechas a traves de una tripla de enteros
-- que corresponden a dıa, mes y ano. Defina una funcion edad que dada
-- dos fechas, una representando la fecha de nacimiento de una persona, y la
-- otra representando la fecha actual, calcula la edad en anos de la persona.

type Fecha = (Dia, Mes, Año)
type Dia = Integer
type Mes = Integer
type Año = Integer

hayAño :: Fecha -> Fecha -> Integer
hayAño (d,m,_) (d2,m2,_) = if m2 > m || m == m2 && d2 >= d then 1 else 0

edad :: Fecha -> Fecha -> Integer
edad (d,m,a) (d2,m2,a2) = a2 - a - 1 + hayAño (d,m,0) (d2,m2,0) 
-- 6. Se desea procesar informacion relativa a estudiantes. Cada estudiante esta
-- dado por su nombre (cadena de caracteres), CI (entero), ano de ingreso
-- (entero) y lista de cursos aprobados. Cada curso esta dado por el nombre del curso (cadena de caracteres), codigo del curso (entero) y nota de
-- aprobacion (entero).
-- (a) Represente la informacion de cada estudiante a traves de tuplas.

type EstudianteTupla = (Nombre, CI, AñoIngreso, [CursoTupla])
type Nombre = String
type CI = Integer
type AñoIngreso = Integer
type CursoTupla = (Nombre, Codigo, Nota)
type Codigo = Integer
type Nota = Integer


-- (b) Escriba una funcion que dado un estudiante retorne su nombre y CI.
getNombreYCedula :: EstudianteTupla -> (Nombre, CI)
getNombreYCedula (nombre, cedula, _, _) = (nombre, cedula)


-- (c) Escriba una funcion que dado un estudiante retorne su ano de ingreso.
getAñoIngreso :: EstudianteTupla -> AñoIngreso
getAñoIngreso (_,_,añoIngreso,_) = añoIngreso


-- (d) Escriba una funcion que dado un estudiante y una nota retorne una
-- lista con los codigos de los cursos que aprobo con esa nota. (Sugerencia: use comprension de listas).
getCursoTuplasNotas :: EstudianteTupla -> Nota -> [Codigo]
getCursoTuplasNotas (_, _, _, cursos) nota = [codigo | (_, codigo, notaCursoTupla) <- cursos, notaCursoTupla == nota]


-- (e) Escriba una funcion que dada una lista de estudiantes retorne una
-- lista de pares (nombre, CI) de aquellos estudiantes ingresados en
-- un determinado ano dado como parametro. (Sugerencia: use comprension de listas).
getEstudianteTuplasPorAñoIngreso :: [EstudianteTupla] -> AñoIngreso -> [(Nombre, CI)]
getEstudianteTuplasPorAñoIngreso estudiantes añoBuscado = [(nombre, ci) | (nombre, ci, estudianteAñoIngreso, _) <- estudiantes, añoBuscado == estudianteAñoIngreso]

-- Mockup data para probar el ejercicio 6

est1 :: EstudianteTupla
est1 = ("Ana", 12345678, 2020, [("mat1", 1, 8), ("mat2", 2, 9)])

est2 :: EstudianteTupla
est2 = ("Luis", 87654321, 2019, [("mat1", 1, 7), ("mat3", 3, 9)])

est3 :: EstudianteTupla
est3 = ("Maria", 11223344, 2020, [("mat2", 2, 10), ("mat4", 4, 8)])

francescoli = ("Francescoli",50889277,2019,[("mat1",1,9),("mat2",2,9),("mat3",3,9), ("mat4",4,10)])


estudiantes :: [EstudianteTupla]
estudiantes = [est1, est2, est3, francescoli]

-- 7. Rehaga el ejercicio anterior usando ahora tipos de datos algebraicos en
-- lugar de tuplas.

-- (a) Represente la informacion de cada estudiante a datos algebraicos.

data Estudiante = Estudiante Nombre CI AñoIngreso [Curso]
data Curso = Curso Nombre Codigo Nota


-- (b) Escriba una funcion que dado un estudiante retorne su nombre y CI.
data NombreYCI = NombreYCI Nombre CI
  deriving (Show)

getNombreYCedulaAlg :: Estudiante -> NombreYCI
getNombreYCedulaAlg (Estudiante nombre ci _ _) = NombreYCI nombre ci

-- (c) Escriba una funcion que dado un estudiante retorne su ano de ingreso.
getAñoIngresoAlg :: Estudiante -> AñoIngreso
getAñoIngresoAlg (Estudiante _ _ añoIngreso _) = añoIngreso

-- (d) Escriba una funcion que dado un estudiante y una nota retorne una
-- lista con los codigos de los cursos que aprobo con esa nota. (Sugerencia: use comprension de listas).
getCursosPorNota :: Estudiante -> Nota -> [Codigo]
getCursosPorNota (Estudiante _ _ _ cursos) notaBuscada = [codigo | (Curso nombre codigo nota) <- cursos, nota == notaBuscada]

-- (e) Escriba una funcion que dada una lista de estudiantes retorne una
-- lista de pares (nombre, CI) de aquellos estudiantes ingresados en
-- un determinado ano dado como parametro. (Sugerencia: use comprension de listas).
getEstudiantesPorAñoIngreso :: [Estudiante] -> AñoIngreso -> [NombreYCI]
getEstudiantesPorAñoIngreso estudiantes añoIngresoBuscado = [NombreYCI nombre ci | (Estudiante nombre ci añoIngreso _) <- estudiantes, añoIngresoBuscado == añoIngreso]

-- Mockup data para probar el ejercicio 7

estAlg1 :: Estudiante
estAlg1 = Estudiante "Ana" 12345678 2020 [Curso "mat1" 1 8, Curso "mat2" 2 9]

estAlg2 :: Estudiante
estAlg2 = Estudiante "Luis" 87654321 2019 [Curso "mat1" 1 7, Curso "mat3" 3 9]

estAlg3 :: Estudiante
estAlg3 = Estudiante "Maria" 11223344 2020 [Curso "mat2" 2 10, Curso "mat4" 4 8]

francescoliAlg :: Estudiante
francescoliAlg = Estudiante "Francescoli" 50889277 2019 [Curso "mat1" 1 9, Curso "mat2" 2 9, Curso "mat3" 3 9, Curso "mat4" 4 10]

estudiantesAlg :: [Estudiante]
estudiantesAlg = [estAlg1, estAlg2, estAlg3, francescoliAlg]
-- 8. Deseamos representar pares Internamente ordenados, que son pares de
-- numeros reales (r, s) tales que r <= s.
-- 1
-- (a) Defina el tipo de los pares ordenados

data ParesOrdenados = ParesOrdenados Float Float
  deriving (Show, Eq)



-- (b) Defina una funcion que dado un par de reales cualesquiera retorna
-- un par Internamente ordenado.

getParesOrdenados :: Float -> Float -> ParesOrdenados
getParesOrdenados x y = ParesOrdenados (min x y) (max x y)

-- (c) Defina la operacion de suma de pares Internamente ordenados, que
-- suma las correspondientes componentes de dos pares retornando un
-- nuevo par.

sumaParesOrdenados :: ParesOrdenados -> ParesOrdenados -> ParesOrdenados
sumaParesOrdenados (ParesOrdenados x y) (ParesOrdenados x2 y2) = ParesOrdenados (x+x2) (y+y2)

-- (d) Defina la operacion de multiplicacion por un escalar, que dado un real
-- y un par Internamente ordenado multiplica la primera componente
-- del par por el escalar. El resultado debe ser un par Internamente ordenado. Si se pierde el orden se deben Integerercambiar las componentes.

multiplicarParPorEscalar :: ParesOrdenados -> Float -> ParesOrdenados
multiplicarParPorEscalar (ParesOrdenados x y) escalar = getParesOrdenados (x*escalar) (y*escalar)

-- 9. Todo numero entero x se puede descomponer de manera unica en terminos
-- de dos numeros enteros y y z , tales que:
-- • −5 < y <= 5
-- • x = y + 10 × z.
-- Defina una funcion que dado un entero x devuelve una tupla con los
-- numeros y y z .
getYZ :: Integer -> (Integer, Integer)
getYZ x = if mod x 10 > 5 then (-10 + mod x 10,div x 10 + 1) else (mod x 10, div x 10) 


-- 10. Deseamos representar numeros racionales y operaciones sobre ellos. Los
-- racionales son representados por pares de enteros cuya segunda componente es distinta de cero. 
-- Cada racional tiene infinitas representaciones,
-- pero existe la llamada representacion canonica en la que la segunda componente del par de enteros es mayor que cero 
-- y ambos enteros son primos
-- entre si.
-- (a) Defina el tipo racional
type Racional = (Integer, Integer)
-- (b) Defina una funcion que dado un par de enteros, el segundo de los
-- cuales es distinto de cero, retorne un racional en su representacion
-- canonica.

-- abs :: Num -> Num
-- abs x = if x > 0 then x else -x

mcd :: Integer -> Integer -> Integer
mcd x 0 = abs x
mcd x y = mcd y (mod x y)



getRepresentacionCanonica :: Racional -> Racional
getRepresentacionCanonica (x,y) = 
  let d = gcd (abs x) (abs y)
      x' = div x d
      y' = div y d 
  in if y' < 0 then (-x',-y') else (x',y')

-- (c) Defina las operaciones de suma, resta, multiplicacion, y negacion de
-- racionales, e int2rac, que convierte un entero en un racional. Dichas
-- operaciones deben devolver representaciones canonicas como resultado.
-- Nota: Puede usar la funcion gcd (definida en el Prelude) la cual
-- computa el maximo comun divisor de dos numeros.
sumaRacional :: Racional -> Racional -> Racional
sumaRacional (x,y) (x2,y2) = getRepresentacionCanonica (x*y2 + y*x2, y*y2)

restaRacional :: Racional -> Racional -> Racional
restaRacional (x,y) (x2,y2) = getRepresentacionCanonica (x*y2 - y*x2, y*y2)

multiplicacionRacional :: Racional -> Racional -> Racional
multiplicacionRacional (x,y) (x2,y2) = getRepresentacionCanonica (x*x2, y*y2)

negacionRacional :: Racional -> Racional
negacionRacional (x,y) = (-x,y)

int2rac :: Integer -> Racional
int2rac n = getRepresentacionCanonica (n,1)

-- 11. Dado el siguiente tipo para representar triangulos:
data Triangulo = Equi Integer | Iso Integer Integer | Esca Integer Integer Integer
-- Defina la funcion mkTriangulo que dados tres enteros positivos, que representan a los lados de un triangulo valido, 
--retorna un valor de tipo Triangulo.
-- 2

mkTriangulo :: Integer -> Integer -> Integer -> Triangulo
mkTriangulo a b c
  | a == b && b == c = Equi a
  | a == b || b == c || c == a = Iso a b
  | otherwise = Esca a b c


