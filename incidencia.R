library(tidyverse)
library(janitor)

#

# Lectura de archivo comprimido

df <- read_csv( unz( 'analisis_datos/sesnsp_monitoreo/data/data_original/Municipal-Delitos-2015-2022_jun2022.zip',
              'Municipal-Delitos-2015-2022_jun2022/Municipal-Delitos-2015-2022_jun2022.csv'), 
              locale = readr::locale(encoding = "Latin1"))


# Limpiar

df <- clean_names(df)
#names(df)
 

# Crear primeros agregados 

df1 <- df %>% rename( anio = ano, 
                   cve_mpio = cve_municipio) %>%
  mutate( anio = as.character(anio),
          cve_mpio = as.factor(cve_mpio),
           Clave_Ent = as.factor(clave_ent)) %>%
  mutate(Total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
  filter(tipo_de_delito == 'Feminicidio') %>%
    group_by( anio, municipio,entidad  ) %>%
    summarise( sum(Total)) 
df1

