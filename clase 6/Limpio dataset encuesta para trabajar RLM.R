
library(tidyverse)

## Levantamos Dataset
encuesta <- read_csv("../Fuentes/encuesta_sueldos_sysarmy_1s2020.csv")
encuesta %>%
  head()

### Limpieza de datos: Aplicamos Filtros
encuesta_ds <- encuesta %>%
  filter(trabajo_de %in% c("Data Scientist / Data Engineer", "BI Analyst / Data Analyst")) %>%
  rename(edad = tengo, 
         salario_bruto = salario_mensual_bruto_en_tu_moneda_local,
         salario_neto = salario_mensual_neto_en_tu_moneda_local)
edad_laboral_minima = 14
limite_inferior_percentil = 20962.50 # percentil 0.05 sueldo_neto
limite_superior_outliers = 137000 # Q3 + 1.5 IQR sueldo_neto
df <- encuesta_ds %>% 
  filter(between(salario_neto, limite_inferior_percentil, limite_superior_outliers), # eliminamos los outliers de acuerdo a los criterios de la clase 3
         sueldo_dolarizado == 0, #Eliminamos los sueldos dolarizados 
         edad - anos_de_experiencia > edad_laboral_minima, # Eliminamos registros inconsistentes con la edad laboral
         salario_bruto > salario_neto, # Inconsistencia en los sueldos
         anos_de_experiencia > anos_en_la_empresa_actual, # Inconsistencia 
         anos_en_la_empresa_actual < 70, # Error de carga
         me_identifico != "Otros") %>%  # elimnamos la categoria otros de me_identifico porque solo tiene 2 valores
  select(-sueldo_dolarizado) # Eliminamos la columna de sueldo dolarizado
df %>%
  glimpse() # 159 obs 13 columnas
# guardo el archivo limpio
write.csv(df, "../Fuentes/encuesta_RLM_limpia.csv")

