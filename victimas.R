library(tidyverse)
library(readxl)
library(janitor)
# Lectura de archivo comprimido
#unzip( "analisis_datos/sesnsp_monitoreo/data/data_original/Estatal-Víctimas-2015-2022_jun2022.zip", exdir="analisis_datos/sesnsp_monitoreo/data/data_original")

df <- read_excel( '/home/isaias/analisis_datos/sesnsp_monitoreo/data/data_original/Estatal-Víctimas-2015-2022_dic2022.xlsx') 
#unique(df$Año)

# Limpiar

df <- clean_names(df)
names(df)

# limpieza y preparación

str(df)

df <- df %>% rename( anio = ano) %>%
  mutate( anio = as.character(anio),
          clave_ent = ifelse (str_length(clave_ent)  == 1, 
                              paste0 ( '0' , clave_ent ) , 
                              clave_ent), 
          clave_ent = as.factor(clave_ent) ) %>%
  mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE)) 

unique(df$anio)
#head(df)
#view(head(df, 15))
#view(tail(df, 15))

# Nacional


agregado_nal <- function(df, delito, sub_del) {
  suf <- substr(delito, 1, 3)                            
  df <- df %>%
    filter(tipo_de_delito == delito,
           #sexo == 'Mujer',
           subtipo_de_delito == sub_del ) %>%
    group_by( anio, sexo) %>%
    summarise( total = sum(total)) %>%
    spread( key = sexo, value = total) %>%
    rename_at(vars(-anio),function(x) paste0('v', suf, '_', x )) %>%
    clean_names()
  return(df)}

df_nal <- agregado_nal( df, 'Feminicidio', 'Feminicidio' )
df_nal_hom <- agregado_nal( df, 'Homicidio', 'Homicidio doloso')
df_nal_lxs <- agregado_nal( df, 'Lesiones', 'Lesiones dolosas')

df_nal <- merge(df_nal, df_nal_hom, by = 'anio')
df_nal <- merge(df_nal, df_nal_lxs, by = 'anio')

#view(df_nal) 

filename =  '/home/isaias/proyectos/vcmn/entropia_vcmn/datos/nacional/sesnsp_vi_nal.csv'
write_csv(df_nal, filename)

## Estatal

agregado_est <- function(df, delito, sub_del) {
  suf <- substr(delito, 1, 3)                            
  df <- df %>%
    filter(tipo_de_delito == delito,
           #sexo == 'Mujer',
           subtipo_de_delito == sub_del ) %>%
    mutate( anio_lab = paste0(suf, '_', anio ) ) %>%
    group_by(clave_ent, entidad, anio_lab, sexo) %>% 
    summarise( total = sum(total)) %>% mutate( col_lab = paste0('v_',  anio_lab, '_', sexo) ) %>% 
    ungroup() %>%
    select( -c(anio_lab, sexo)) %>% spread( key =  col_lab, value = total) %>% 
    clean_names()
  return(df)}

df_est <- agregado_est( df, 'Feminicidio', 'Feminicidio' )
df_hom <- agregado_est( df, 'Homicidio', 'Homicidio doloso')
df_lxs <- agregado_est( df, 'Lesiones', 'Lesiones dolosas')

df_est <- merge(df_est, df_hom, by = c('clave_ent', 'entidad'))
df_est <- merge(df_est, df_lxs, by = c('clave_ent', 'entidad'))

view(df_est) 

filename =  '/home/isaias/proyectos/vcmn/entropia_vcmn/datos/estatal/sesnsp_vi_estatal.csv'
write_csv(df_est, filename)


