-- 1. Defina una funcion sumsqrs que tome 3 numeros y retorne la suma de los
-- cuadrados de los dos mayores.
square :: Integer -> Integer
square x = x * x

sumsqrs :: Integer -> Integer -> Integer -> Integer
sumsqrs x y z = if x > y then square x + square (max y z) else square y + square (max x z)

add :: Integer -> Integer -> Integer
add x y = x + y

add' :: (Integer, Integer) -> Integer
add' (x, y) = x + y


addtwo = curry add' 2

