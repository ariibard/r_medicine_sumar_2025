library(agiseR)
library(tidyverse)
library(lubridate)
library(zoo)
library(vroom)
library(pander)
library(zoo)
library(janitor)


path_files_hce <- ("agiserver/agise/proyectos/sumar/archivos_adicionales/datos_historicos/reportes_hsi/")
path_archivos <- ("agiserver/agise/proyectos/sumar/paces/archivos_adicionales/")


# Paso 1: Cargar los nombres de archivo
lista_archivos_total <- list.files(
  path = "agiserver/agise/proyectos/sumar/archivos_adicionales/datos_historicos/reportes_hsi/",
  pattern = "\\.csv$", full.names = FALSE
)

# Paso 2: Crear el dataframe y filtrar por año > 2020
lista_archivos <- tibble(archivo = lista_archivos_total) %>%
  mutate(anio = str_extract(archivo, "\\d{4}"),
         anio = as.numeric(anio)) %>%
  filter(anio > 2020) %>%
  pull(archivo)

# Armo df con las prestaciones de SUMAR
for (i in 1:length(lista_archivos)) {
  tmp_path <- paste0(path_files_hce, lista_archivos[i])
  tmp_prestaciones_hce <-  read.table(tmp_path, sep = ";",
                                      fileEncoding = "ASCII") %>% 
    select(clave_beneficiario, cat_beneficiario, cuie, cod_prestacion,
           fecha_DPH, fecha_prestacion) %>% 
    mutate(cat_beneficiario = as.character(cat_beneficiario),
           clave_beneficiario = as.character(clave_beneficiario),
          # fecha_facturacion = as.Date(fecha_DPH, "%d/%m/%Y"), #ahora fecha_prestacion es la fecha de facturación para evitar subrepresentar SIGEHOS
           origen = "HCE") %>%
    select(-fecha_DPH)
  
  if(i == 1){
    prestaciones_hce <- tmp_prestaciones_hce
  } else {
    prestaciones_hce <- prestaciones_hce %>% 
      bind_rows(tmp_prestaciones_hce)
  }
  
}


grafico <- prestaciones_hce %>% 
  distinct() %>% 
  mutate(fecha_prestacion = as_date(fecha_prestacion),
         fecha_mes = floor_date(fecha_prestacion, "month")) %>% 
  count(fecha_mes)

saveRDS(grafico, paste0(entorno_agiserver(), "/agise/proyectos/sumar/archivos_adicionales/datos_historicos/oct_2020_abril_2025.rds"))
encolar_archivo_productivo(paste0(entorno_agiserver(), "/agise/proyectos/sumar/archivos_adicionales/datos_historicos/oct_2020_abril_2025.rds"))
enviar_archivo_productivo()

install.packages("highcharter")
library(highcharter)

hchart(
  grafico,
  type = "line",
  hcaes(x = fecha_mes, y = n)
) |>
  hc_title(text = "<b>Number of services detected</b><br>by the automation project") |>
  hc_subtitle(text = "From October 2020 to April 2025") |>
  hc_yAxis(title = list(text = "Number of services"),
           labels = list(format = "{value:,.0f}"),
           gridLineWidth = 1) |>
  hc_xAxis(title = list(text = "Date"),
           type = "datetime",
           labels = list(format = "{value:%b %Y}")) |>
  hc_tooltip(pointFormat = "<b>{point.y:,.0f}</b> services<br/>Date: {point.x:%B %Y}") |>
  hc_credits(enabled = TRUE, text = "Own elaboration") |>
  hc_exporting(enabled = TRUE) |>
  hc_chart(zoomType = "x") |>
  hc_add_theme(hc_theme_flat())

