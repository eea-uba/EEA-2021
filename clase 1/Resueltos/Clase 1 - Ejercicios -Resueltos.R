#### Resolución de Ejercicios - Clase 1 ####

## Ejercicio 1 ##
 
# 1) Crear un dataframe con el dataset de R: state.x77
df <- state.x77
df

# a) ¿Cuál es la población total de Estados Unidos?
apply(X = as.data.frame(df[,1]), MARGIN = 2 , FUN = sum) # 212321

# b) ¿Cuál es la media de la expectativa de vida?
apply(X = as.data.frame(df[,4]), MARGIN = 2 , FUN = mean) # 70.8786

# c) ¿Cual es la mediana del ingreso en pesos argentinos?
apply(X = as.data.frame(df[,2]), MARGIN = 2 , FUN = function(x) median(x)*71.75) 
# el resultado dependerá de la cotización del dólar que se emplee

# 2) Crear el dataset con las dos columnas
df_indice <- df[,c(3, 5)]
df_indice

# a) Crear el indice
ilit_murd <- apply(X = df_indice, MARGIN = 1, FUN = sum)
ilit_murd

# b) Buscar los máximos y mínimos
ilit_murd[ilit_murd == max(ilit_murd)] # Alabama 17.2
ilit_murd[ilit_murd == min(ilit_murd)] # North y South Dakota 2.2

# 3) Crear:
# a) Un VALOR llamado _OBJETO_ definido como el resultado de la suma: 5+6
OBJETO <- 5+6
OBJETO

# b) un VECTOR _VEC0_ que contenga una muestra aleatoria de numeros del 1 al 10.
VEC0 <- sample(1:10)
VEC0

# c) 3 vectores ( _VEC1_, _VEC2_, _VEC3_) que sean transformaciones del anterior.
VEC1 <- VEC0*2 
VEC2 <- VEC0^2 
VEC3 <- VEC0-2 

# d) 3 vectores con la misma cantidad de elementos que VEC0, pero con variables string (texto) ( _VEC4_, _VEC5_, _VEC6_) 
VEC4 <- c(rep("NO",5), rep("SI",5))
VEC5 <- c(rep("PAGO",7), rep("LIBRE",3))
VEC6 <- c(rep("SAS",2),rep("SPSS",2),rep("R",6))

# e) Un dataframe _DFRAME_ como combinación de todos los vectores creados previamente
DFRAME <- data.frame(VEC0,VEC1,VEC2,VEC3,VEC4,VEC5,VEC6)
DFRAME

# f) Una lista _LA_LISTA_ con el OBJETO, uno de los vectores y el DFRAME
LALISTA <- list(OBJETO,VEC0,DFRAME) 
LALISTA

# 4) Loops
# a) Para todos los valores del vector _VEC0_, imprimir mediante un loop 
# el triple de dichos valores

for(i in VEC0){
  print(i*3)
}

# b) Armar un loop que itere sobre los valores únicos de la variable _VEC6_ del
# dataframe _DFRAME_ e imprima un texto que combine el valor de _VEC6_ y _VEC0_. 
for(i in unique(DFRAME$VEC6)) {
          A <- paste(DFRAME$VEC6[VEC6 == i], DFRAME$VEC0[VEC6 == i])
          print(A)
          }

# c) Reescribir el VEC1 del DATAFRAME para que sus elementos sean el doble de 
# VEC_0 cuando éste sea mayor a 2 o iguales a VEC_0 para el resto de los casos. 

DFRAME$VEC1 <- ifelse(DFRAME$VEC0 > 2, DFRAME$VEC0 * 2, DFRAME$VEC0)
DFRAME

# 5) Funciones 

# a) Crear una función llamada _Hola_Mundo_ que imprima el texto "Hola mundo"
Hola_Mundo <- function(){
  print("Hola mundo")
}

# b) Crear una función devuelva la sumatoria de los numeros enteros comprendidos entre 1 y un parametro x a definir
Sumatoria_enteros<- function(x){
  y <- 1:x 
  sum(y)
}

# c) Crear una función cuyo parámetro/input X sea una matrix y que devuelva: 
# la dimensión de la matriz en cuestión y un texto que diga "El primer elemento
# es par" en caso de que así lo fuera o "El primer elemento no es par" en caso contrario. 

primer_elem_matriz <- function(matriz) {
  # Obtengo las dimensiones
  print(dim(matriz))
  # Chequeo si el primer elemento es par
  primer_elem <- matriz[1,1]
  # indico que imprima el texto en caso de cumplirse la condición
  if (primer_elem %% 2 == 0) {
    print('El primer elemento es par')
  }
  else{print('El primer elemento no es par')}
}

# evaluo la función sobre el dataframe creado previamente
primer_elem_matriz(DFRAME)

## Ejercicio 2 ## 

# 1) 
# a) Levantar la base Individual del 1er trimestre de 2020 de la EPH
individual_T120 <-  data.table::fread("Fuentes/usu_individual_T120.txt",
                              sep=";",
                              dec=",",
                              header = TRUE,
                              fill = TRUE)

# b) Visualizar el contenido
tibble::glimpse(individual_T120) # 51643 filas y 177 columnas

# c) Guardar la base como un archivo de extensión .RDS
saveRDS(individual_T120,"Fuentes/individual_T120.RDS")

# Vuelvo a levantar la base, pero como .RDS y le asigno nombre _BaseRDS_ 
BaseRDS <- readRDS("Fuentes/individual_T120.RDS")

# 2) Crear una función _acumulado_ que calcule el valor acumulado de una 
# variable numérica a designar X en un dataset también a designar df.  
# Es decir, que brinde el valor resultante de acumulado(df, X). 

acumulado <- function(data, variable){
  df <- as.data.frame(data)  
  acum <- sum(df[,variable])
  print (acum)
}

# a) Evalúela para la columna "PONDERA" del dataframe individual_T120.
acumulado(individual_T120, "PONDERA")
# chequeo el resultado
sum(individual_T120[,PONDERA])

# b) Utilizar dicha función para calcular el acumulado de la frecuencia poblacional 
# (variable PONDERA) por Sexo (variable CH04). Sabiendo que 1 = varón, 2 = mujer. 

df_m = individual_T120[individual_T120$CH04 == 2,]
df_v = individual_T120[individual_T120$CH04 == 1,]

acumulado(df_m, "PONDERA")
acumulado(df_v, "PONDERA")

# 3) Modificar la función anterior para que devuelva un vector que contenga 
# la frecuencia poblacional (el acumulado calculado previamente) y 
# la muestral (número de filas del dataset). 

acumulado2 <- function(data, variable){
  df <- as.data.frame(data)  
  tibble('frecuencia_poblacional' = sum(df[,variable]), 'frecuencia_muestral' = nrow(df))
}

acumulado2(individual_T120, "PONDERA")

# a) acumulado2 por sexo
acumulado2(df_m, "PONDERA")
acumulado2(df_v, "PONDERA")

# Funciones que ya existen para obtener resultados similares
# se pueden agrupar distintas variables y resumirlas
# Se muestran ejemplos agrupando por sexo y region

library(dplyr)
individual_T120 %>% 
  group_by(CH04, REGION) %>% 
  summarise(frecuencia_poblacional = sum(PONDERA))

library(purrr)
individual_T120 %>% 
  group_by(CH04, REGION) %>% 
  nest() %>% 
  mutate(frecuencia_poblacional = unlist(map(data,expansion)))

individual_T120 %>% 
  group_by(CH04, REGION) %>% 
  nest() %>% 
  mutate(frecuencias = map(data,expansion)) %>% 
  unnest(frecuencias) #abrimos el dataframe resultante



