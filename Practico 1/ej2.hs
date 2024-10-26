-- Defina una funcion analyze :: Int → Int → Int → Bool, que determina si
-- tres enteros positivos son los lados de un triangulo.

analyze :: Int -> Int -> Int -> Bool
analyze x y z = x + y > z && x + z > y && y + z > x