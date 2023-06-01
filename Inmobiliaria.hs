 {- Consigna:
Buscar departamentos para alquilar por los medios tradicionales es una tarea compleja, ya que
requiere mucho tiempo de investigación buscando en los clasificados de los diarios y recorriendo
inmobiliarias. Es por eso que hoy en día cada vez son más las personas que dejaron eso atrás
dejando que internet se encargue de buscar las supuestas mejores alternativas para sus
necesidades.

Por eso surge una nueva página para buscar departamentos que permita al usuario personalizar
sus propias búsquedas y de paso eventualmente mandarle mails con las nuevas ofertas inmobiliarias
que podrían ser de su interés a ver si agarra viaje.

Tenemos los departamentos modelados de la siguiente forma:  -}

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto { 
ambientes :: Int,
superficie :: Int,
precio :: Int,
barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
mail :: Mail,
busquedas :: [Busqueda]
}

ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio . filter (not . criterio x)) xs ++ [x] ++ (ordenarSegun criterio . filter (criterio x)) xs

between cotaInferior cotaSuperior valor = valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
Depto 3 80 7500 "Palermo", 
Depto 1 45 3500 "Villa Urquiza", 
Depto 2 50 5000 "Palermo", 
Depto 1 45 5500 "Recoleta"]

{-Se pide desarrollar las siguientes funciones y consultas de modo que se aprovechen tanto como
sea posible los conceptos de orden superior, aplicación parcial y composición.

1. a. Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de evaluarlo sobre el segundo valor respectivamente.
b. Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar una lista de strings en base a su longitud usando ordenarSegun.

2. Definir las siguientes funciones para que puedan ser usadas como requisitos de búsqueda:
a. ubicadoEn que dada una lista de barrios que le interesan al usuario, retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
b. cumpleRango que a partir de una función y dos números, indique si el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

3. a. Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda
se verifican para un departamento dado.
b. Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido.
c. Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:
■ Encontrarse en Recoleta o Palermo
■ Ser de 1 o 2 ambientes
■ Alquilarse a menos de $6000 por mes

4. Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne los mails de las personas que tienen alguna búsqueda que se cumpla para el
departamento dado.-}

{- Respuesta:

 1.a. Para definir las funciones mayor y menor, podemos utilizar la función de orden 
superior "comparar" de Haskell, que compara dos valores y devuelve GT si el primer valor
es mayor, LT si el segundo valor es mayor y EQ si son iguales. Podemos aprovechar esta 
función para comparar el resultado de evaluar una función sobre dos valores. -}

mayor :: (Ord b) => (a -> b) -> a -> a -> Bool
mayor f x y = comparar f x y == GT

menor :: (Ord b) => (a -> b) -> a -> a -> Bool
menor f x y = comparar f x y == LT

{- 1.b. Ahora planteo cómo utilizar la función menor para ordenar una lista de strings en 
base a su longitud usando ordenarSegun.  En este ejemplo, menor length es una función 
parcial que compara la longitud de dos strings. Luego, utilizamos ordenarSegun para ordenar
 la lista de strings en base a esta función parcial.  La lista se ordena de menor a mayor 
 longitud. En caso de haber strings con la misma longitud, se mantiene el orden original. -}

ordenarPorLongitud :: [String] -> [String]
ordenarPorLongitud = ordenarSegun (menor length)

{- 2.a. Para definir la función ubicadoEn, que verifica si un departamento se encuentra en 
alguno de los barrios de una lista dada, podemos utilizar la función elem de Haskell, 
que verifica si un elemento está presente en una lista. -}

ubicadoEn :: [Barrio] -> Depto -> Bool
ubicadoEn barrios depto = elem (barrio depto) barrios

{- 2.b. Para definir la función cumpleRango, que verifica si el valor retornado por una 
función aplicada a un departamento se encuentra entre dos números dados, podemos utilizar la 
función between que se proporciona en el enunciado. -}

cumpleRango :: (Depto -> Int) -> Int -> Int -> Depto -> Bool
cumpleRango f cotaInf cotaSup depto = between cotaInf cotaSup (f depto)

{- 3.a. Para definir la función cumpleBusqueda, que verifica si un departamento cumple con 
todos los requisitos de una búsqueda, podemos utilizar la función all de Haskell, que 
verifica si todos los elementos de una lista cumplen con una condición. -}

cumpleBusqueda :: Busqueda -> Depto -> Bool
cumpleBusqueda requisitos depto = all (\req -> req depto) requisitos

{- 3.b. Para definir la función buscar, que retorna todos los departamentos que cumplen con
 una búsqueda dada, ordenados según un criterio de ordenamiento, podemos utilizar 
 composición y aplicación parcial.-}

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]
buscar requisitos criterioOrdenamiento deptos =
  ordenarSegun criterioOrdenamiento (filter (cumpleBusqueda requisitos) deptos)

{- 3.c. Ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenados por
 mayor superficie, que cumplan con los requisitos dados.  El resultado será una lista de 
 departamentos que cumplen con los requisitos dados, ordenados por mayor superficie. -}

busquedaEjemplo :: Busqueda
busquedaEjemplo = [
  ubicadoEn ["Recoleta", "Palermo"],
  cumpleRango ambientes 1 2,
  cumpleRango precio 0 6000
  ]

resultadoEjemplo :: [Depto]
resultadoEjemplo = buscar busquedaEjemplo (mayor superficie) deptosDeEjemplo

{- 4. Para definir la función mailsDePersonasInteresadas, que retorna los mails de las 
personas que tienen alguna búsqueda que se cumpla para un departamento dado, podemos utilizar
la función concatMap de Haskell para aplicar una función a cada elemento de una lista y 
luego concatenar los resultados. -}

mailsDePersonasInteresadas :: Depto -> [Persona] -> [Mail]
mailsDePersonasInteresadas depto personas =
  concatMap (\persona -> if any (\busqueda -> cumpleBusqueda busqueda depto) (busquedas persona)
                          then [mail persona]
                          else []) personas

{- La función any se utiliza para verificar si alguna búsqueda de la persona se cumple para 
el departamento dado. Si es así, se agrega el mail de la persona a la lista de resultados. -}