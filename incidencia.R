library(tidyverse)
library(janitor)

setwd ('/home/isaias/analisis_datos/sesnsp_monitoreo/data')

# Lectura de archivo comprimido

file_zip <- unz( '/home/isaias/analisis_datos/sesnsp_monitoreo/data/data_original/Municipal-Delitos-2015-2022_dic2022.zip',
                 'Municipal-Delitos-2015-2022_dic2022/Municipal-Delitos-2015-2022_dic2022.csv')
file_zip

# '~/Municipal-Delitos-2015-2022_jun2022.csv', 
df <- read_csv( file_zip,
              locale = readr::locale(encoding = "Latin1"))

# Limpiar

df <- clean_names(df)
names(df)
 
df <- df %>% rename ( anio = ano, 
                   cve_mpio = cve_municipio) %>%
  mutate ( anio = as.character(anio),
           cve_mpio = ifelse (str_length(cve_mpio)  <5, 
                              paste0 ( '0' , cve_mpio ) , 
                              cve_mpio),
          cve_mpio = as.factor(cve_mpio),
          clave_ent = ifelse (str_length(clave_ent)  == 1, 
                  paste0 ( '0' , clave_ent ) , 
                  clave_ent), 
          clave_ent = as.factor(clave_ent) ) %>%  
  mutate( total = rowSums(select_if(., is.numeric), na.rm = TRUE) )

# df
### Crear el catálogo de delitos  =====

cat_del <- df %>% group_by(bien_juridico_afectado,
                           tipo_de_delito,
                           subtipo_de_delito,
                           modalidad) %>% summarise()
# view(cat_del)

# df %>% filter(anio == 2022) %>% select(diciembre)

### Nivel Nacional

agrupado_nal <- function(df, delito, sub_delito) {
  df <- df %>%
    filter(tipo_de_delito == delito, 
           subtipo_de_delito == sub_delito ) %>%
    group_by( anio) %>%
    summarise( total  =  sum(total))  %>%
    
    return(df)
}

df_nal <- agrupado_nal( df, 'Feminicidio', 'Feminicidio' )
df_nal <- rename(df_nal, 'ci_femi' = 'total' )

df_nal_hom <- agrupado_nal( df, 'Homicidio', 'Homicidio doloso')
df_nal_hom <- rename(df_nal_hom, 'ci_hom_dol' = 'total' )
df_nal_lxs <- agrupado_nal( df, 'Lesiones', 'Lesiones dolosas')
df_nal_lxs <- rename(df_nal_lxs, 'ci_les_dol' = 'total' )

df_nal <- merge(df_nal, df_nal_hom, by = 'anio')
df_nal <- merge(df_nal, df_nal_lxs, by = 'anio')

view(df_nal) 


filename =  '/home/isaias/proyectos/vcmn/entropia_vcmn/datos/nacional/sesnsp_ci_nal.csv'
write_csv(df_nal, filename)

## Nivel estatal

## Agregados, función

agrupado <- function(df, delito, sub_delito) {
  
  df <- df %>%
    filter(tipo_de_delito == delito, 
           subtipo_de_delito == sub_delito ) 
  
  df <- df %>%
    mutate( anio_lab = paste0('ci_', substr(delito, 1, 4) , '_', anio ) ) %>%
    group_by( anio_lab, clave_ent, entidad) %>%
    summarise( total_anio =  sum(total))  %>%
    spread ( key = anio_lab , value = total_anio ) %>% clean_names()
  return(df)
}

df_est <- agrupado( df, 'Feminicidio', 'Feminicidio' )
df_hom <- agrupado( df, 'Homicidio', 'Homicidio doloso')
df_lxs <- agrupado( df, 'Lesiones', 'Lesiones dolosas')

df_est <- merge(df_est, df_hom, by = c('clave_ent', 'entidad'))
df_est <- merge(df_est, df_lxs, by = c('clave_ent', 'entidad'))

#view(df_est) 

filename =  '/home/isaias/proyectos/vcmn/entropia_vcmn/datos/estatal/sesnsp_ci_estatal.csv'
write_csv(df_est, filename)


### Municipal ====

agrupado <- function(df, delito, sub_delito) {
df <- df %>%  filter( tipo_de_delito == delito,
                      subtipo_de_delito == sub_delito ) %>% 
  select( anio, 
          #clave_ent, entidad, 
          cve_mpio, municipio,
          tipo_de_delito,
          subtipo_de_delito,
          total ) %>% 
  mutate( anio_lab = paste0('ci_', delito , '_', anio ) ) %>%
  group_by(anio_lab, 
          # clave_ent, entidad,
           cve_mpio, municipio,
           tipo_de_delito) %>% 
  summarise( total = sum(total))  %>%
  spread( key = anio_lab, value = total) %>% 
  select(- tipo_de_delito) %>% clean_names()

return(df)
}

df_fem <- agrupado( df, 'Feminicidio','Feminicidio' )
df_hom <- agrupado( df, 'Homicidio', 'Homicidio doloso')
df_lxs <- agrupado( df, 'Lesiones', 'Lesiones dolosas')

# Primero obtener todos los municipios

mpios_sesnp <- df %>% group_by( clave_ent, entidad, cve_mpio, municipio) %>% summarise()
#mpios_sesnp

# Unir las tablas

df_mpal <- merge(mpios_sesnp, df_fem, by = c('cve_mpio'), all.x = TRUE)
df_mpal <- merge(df_mpal, df_hom, by = c('cve_mpio'), all.x = TRUE)
df_mpal <- select(df_mpal, - c (municipio.x,municipio.y ) )
df_mpal <- merge(df_mpal, df_lxs, by = c('cve_mpio'), all.x = TRUE)
df_mpal <- df_mpal  %>% select(- c (municipio.x, municipio.y) ) %>% clean_names()
df_mpal[is.na(df_mpal)] <- 0

df_mpal

filename =  '/home/isaias/proyectos/vcmn/entropia_vcmn/datos/municipal/sesnsp_ci_mpal.csv'
write_csv(df_mpal, filename)

###

#### obtener municipios para evaluación

mpios_sesnp <- df %>% group_by( clave_ent, entidad, cve_mpio, municipio) %>% summarise()

mpios_sesnp

# 2474 son menos 5 que están con clave no especificada

filename =  '/home/isaias/analisis_datos/datos_mx_intergacion/datos/diagnostico_datos/sesnsp_mpios.csv'
write_csv(mpios_sesnp, filename)

# 2479
#
