library(tidyverse)
library(readxl)
library(janitor)

#

# Lectura de archivo comprimido

#
#unzip( "analisis_datos/sesnsp_monitoreo/data/data_original/Estatal-Víctimas-2015-2022_jun2022.zip", exdir="analisis_datos/sesnsp_monitoreo/data/data_original")

df <- read_excel( 'analisis_datos/sesnsp_monitoreo/data/data_original/Estatal-Víctimas-2015-2022_jun2022.xlsx') 

# Limpiar

df <- clean_names(df)
names(df)
 

# Crear primeros agregados 

df1 <- df %>% rename( anio = ano) %>%
  mutate( anio = as.character(anio),
          clave_ent = as.factor(clave_ent)) %>%
  mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
  filter(tipo_de_delito == 'Feminicidio') %>%
    group_by( anio, entidad  ) %>%
    summarise( sum(total)) 
df1

