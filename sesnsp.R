library(tidyverse)
library(janitor)
library(tidyverse)

#

setwd("E:/")

# Lectura de archivo comprimido

df <- read.csv(unz( './sesnsp/data/data_original/Municipal-Delitos-2015-2022_mar2022.zip',
              'Municipal-Delitos-2015-2022_mar2022/Municipal-Delitos-2015-2022_mar2022.csv'), header = T)

# Limpiar

df <- clean_names(df)
names(df)
 
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

# write.csv(s, 'femi_mpio.csv')
  
  