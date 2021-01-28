## =======================
## Actualizar datos ISCIII
## =======================

# Descargar
# --------
# f <- "serie_historica_acumulados.csv"
# download.file(paste0("https://covid19.isciii.es/resources/", f), f, mode="wb")
# Cambio en la dirección web y en el nombre del archivo el 08/05/2020
f <- "agregados.csv"
download.file(paste0("https://cnecovid.isciii.es/covid19/resources/", f), f, mode="wb")

# Importar
# --------
# f <- "agregados.csv"
# Cambio en el archivo el 21/05/2020
# acumulados <- read.csv(f, colClasses = c("character", "character", rep("integer", 7)))
acumulados <- read.csv(f, colClasses = c("character", "character", rep("integer", 6)))
# View(acumulados)

# Verificar variables y seleccionar
# ---------------------------------
# Variables actuales
# dput(names(acumulados))
# var.isciii <- c("CCAA", "FECHA", "CASOS", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")
# Cambio en las variables el 25/04/2020
# var.isciii <- c("CCAA", "FECHA", "CASOS", "PCR.", "TestAc.", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")
# Cambio en las variables el 21/05/2020
var.isciii <- c("CCAA", "FECHA", "CASOS", "PCR.", "TestAc.", "Hospitalizados", "UCI", "Fallecidos")

if (any(names(acumulados) != var.isciii)) stop('Cambios en las variables')
# Seleccionamos los casos que tienen algo en FECHA
inotas <- which(!nzchar(acumulados$FECHA))[1]:nrow(acumulados)
# Combinar todas las notas 
nota.texto <- paste(apply(acumulados[inotas, 1:2], 1, paste, collapse =""), collapse = "\n")
# Cambiar * por \*
nota.texto <- gsub("\\*", "\\\\*", nota.texto)
nota.texto
acumulados <- acumulados[-inotas, var.isciii]
# names(acumulados) <- c("CCAA.ISO", "Fecha", "Casos", "PCR", "TestAc", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")
# Cambio en las variables el 25/04/2020
names(acumulados) <- c("CCAA.ISO", "Fecha", "Casos", "PCR", "TestAc", "Hospitalizados", "UCI", "Fallecidos")

# Verificar niveles factor
# write.csv2(unique(acumulados$CCAA.ISO), file = "levels.csv")
# https://www.iso.org/obp/ui/#iso:code:3166:ES
# El 08/04/2020 se cambió el nivel de Melilla de 'ME' a 'ML' (se mantendrá el anterior en acumula2)
CCAA.ISO <- read.csv2("CCAA.ISO2.csv", colClasses = "character")
if (!all(unique(acumulados$CCAA.ISO) %in% CCAA.ISO$COD_CCAA)) stop('Cambios en CCAA.ISO')
acumulados$CCAA.ISO <- factor(acumulados$CCAA.ISO, levels = CCAA.ISO$COD_CCAA)
iccaa <- match(as.character(acumulados$CCAA.ISO), CCAA.ISO$COD_CCAA)
acumulados <- tibble::add_column(acumulados, CCAA = CCAA.ISO$DESC_CCAA[iccaa], .before = 1)
acumulados$CCAA <- factor(acumulados$CCAA, levels = CCAA.ISO$DESC_CCAA)

# Preparar variables
acumulados$Fecha <- as.Date(acumulados$Fecha, format = "%d/%m/%Y")
range(acumulados$Fecha)

# str(acumulados)

# NA's
# ----
# Debido al cambio del 25/04/2020 ya no se reemplazan los NAs por ceros
# ----
# sapply(acumulados, function(x) sum(is.na(x)))
# which(is.na(acumulados$Fecha))
# acumulados <- acumulados[!is.na(acumulados$Fecha), ]
# acumulados[is.na(acumulados)] <- 0

# plot(acumulados[, -(1:2)])
attr(acumulados, "note") <- nota.texto
attr(acumulados, "url") <- "https://cnecovid.isciii.es/covid19/resources/agregados.csv"
# View(acumulados)
# --------------------------------------
# NOTA: 17/04/2020 La serie histórica de CT se ha eliminado porque 
# está en revisión por dicha comunidad autónoma.Solo se muestra la de casos.
# acumulados[acumulados$CCAA.ISO == "CT", 5:8] <- NA
# Se restauró el 22/04/2020
# --------------------------------------
save(acumulados, file = "acumulados.RData")


## ----------------
# Generar listado de tablas automáticamente y mostrar
# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
browseURL(url = rmarkdown::render("COVID-19-tablas.Rmd", encoding = "UTF-8"))


# acumula2
# --------
library(dplyr, warn.conflicts = FALSE)

acumula2 <- acumulados
# Renombrar variables
names(acumula2) <- tolower(names(acumula2)) 
names(acumula2)[2] <- "iso"
# Cambio en las variables el 25/04/2020
# names(acumula2)[4] <- "confirmados"
# ----
# El 08/05/2020 vuelven a reportar valores en la variable casos,
# y el 09/05/2020 los vuelven a eliminar
# ----
acumula2$confirmados <- with(acumula2, pcr + testac)
# # Debido al cambio del 25/04/2020 se reemplazan los NAs en confirmados por "pcr+ testac"
# # acumula2$confirmados <- with(acumula2, ifelse(is.na(confirmados), pcr, confirmados))
# acumula2$confirmados <- with(acumula2, ifelse(is.na(confirmados), pcr + testac, confirmados))
# # El 03/05/2020 se eliminó de la serie de Andalucía "Casos" y "testac" anteriores al 2020-04-14
# # Se establecen como "pcr" para que no aparezcan NAs (tampoco en nuevos y en España)
acumula2$confirmados <- with(acumula2, ifelse(is.na(confirmados), pcr, confirmados))

# Nuevos
acumula2 <- acumula2 %>% select(-casos, -pcr, -testac) %>% group_by(iso) %>% 
  mutate(nuevos = confirmados - lag(confirmados)) %>% ungroup()
# acumula2$nuevos[is.na(acumula2$nuevos)] <- 0

# Cambio en las variables el 25/04/2020
# var <- c("confirmados", "hospitalizados", "uci", "fallecidos", "recuperados", "nuevos")
var <- c("confirmados", "hospitalizados", "uci", "fallecidos", "nuevos")

# # NOTA: 17/04/2020 La serie histórica de CT se ha eliminado porque 
# # está en revisión por dicha comunidad autónoma.Solo se muestra la de casos.
# # Se añaden los valores anteriores
# load("old.CT.RData")
# # Añadir a los valores anteriores, confirmados y nuevos
# new.CT <- acumula2 %>% filter(iso == "CT") %>% 
#   select(-all_of(var[2:5])) %>% 
#   left_join(select(old.CT, -ccaa, -iso), by = "fecha")
# acumula2 <- bind_rows(filter(acumula2, iso != "CT"), new.CT)

# El 21/05/2020 Cataluña no reporta información
# index <- with(acumula2, fecha == as.Date("2020-05-20") & iso == "CT")
# acumula2[index, var] <- NA

# Total España
# res <- acumula2 %>% group_by(fecha) %>% summarise_at(var, sum, na.rm = TRUE) %>%
res <- acumula2 %>% group_by(fecha) %>% summarise_at(var, sum) %>%
            mutate(ccaa = "España", iso = "ES") 
res <- suppressWarnings(bind_rows(acumula2, res))
res$iso <- factor(res$iso, levels = c("ES", CCAA.ISO$COD_CCAA))
# El 08/04/2020 se cambió el nivel de Melilla de 'ME' a 'ML' (se mantendrá el anterior 'ME' en acumula2)
levels(res$iso) <- gsub("ML", "ME", levels(res$iso))
res$ccaa <- factor(res$ccaa, levels = c("España", CCAA.ISO$DESC_CCAA))
# Ordenar y guardar
acumula2 <- res %>% arrange(fecha, iso)
save(acumula2, file ="acumula2.RData")

# DT::datatable(acumula2, filter = 'top', options = list(pageLength = 19, autoWidth = TRUE))

# NOTA: El 16/05/2020 el ISCIII no reporta datos 
# acumula2 <- acumula2 %>% filter(fecha == as.Date("2020-05-14")) %>%
#     mutate(fecha = fecha + 1) %>%
#     mutate_at(var, function(x) NA) %>%
#     bind_rows(acumula2)


# acumula22
# ---------
library(tidyr)
respuestas <- c("confirmados", "hospitalizados", "uci", 
          "fallecidos", "nuevos")
# Cambio en las variables el 25/04/2020
# acumula22 <- acumula2 %>% select(-ccaa, -recuperados) %>%
acumula22 <- acumula2 %>% select(-ccaa) %>%
    pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
                 names_ptypes = list(respuesta = factor(levels = respuestas)))

# Troceamos por CCAA y respuesta
acumula22 <- split(acumula22, acumula22$iso)
acumula22 <- lapply(acumula22, function(d) split(d, d$respuesta))
save(acumula22, file ="acumula22.RData")


## =======================
## acumula2_hist.RData
## =======================
# fecha.txt <- format(Sys.Date(), format = "%m_%d")
fecha.txt <- format(max(acumula2$fecha) + 1, format = "%m_%d")
file.copy("acumula2.RData", paste0("./acumula2_hist/acumula2_",fecha.txt,".RData"), overwrite = TRUE)
source("acumula2_hist/acumula2_hist.R", chdir = TRUE)


## ================
## Datos MSCBS
## ================

# Descargar
# https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm

ifile <-  as.numeric(Sys.Date() - as.Date("2020-01-30"))
file <- sprintf("Actualizacion_%i_COVID-19.pdf", ifile)
download.file(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/", 
              file), file, mode="wb")


# ==========================================================================================
# ==========================================================================================
## A PARTIR DE AQUI SIN ACTUALIZAR DESDE 2020-04-23
# ==========================================================================================
# ==========================================================================================


# ==========================================================================================
## Actualizar MSCBS por CCAA
# ==========================================================================================

old.data <- new.env()
load("COVID-19.RData", envir = old.data)
# str(old.data$tables)

files <- dir(pattern = '*.pdf')
tables <- vector(mode = "list", length = length(files))

iold <- match(files, old.data$files)
tables <- old.data$tables[iold]

inew <- which(is.na(iold))
inew <- inew[1]
inew
# inew <- length(files)

## Procesar files[inew]
## --------------------

file <- files[inew]
file
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]


library("tabulizer")

process_table_a8 <- function(file, page = 1, table = 1, nhead = 3) { 
# page = 1; table = 1; nhead = 3; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -(1:2)]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  values <- gsub("[^0-9.-]", "", tabla) # Eliminar caracteres no numéricos
  values <- apply(values, 2, as.numeric)
  # head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  head <- c("Casos PCR", "Nuevos PCR", "Incremento %", "IA (14 d.)")
  colnames(values) <- head
  # rnames <- tabla[-ihead, 1]
  # rownames(values) <- rnames[nzchar(rnames)]
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    


table_a <- process_table_a8(files[inew])
# View(table_a)

# El 08/04/2020 se dejó de calcular el total de España de hospitalizados y UCI


## ----------------
library(pdftools)

dates <- sapply(files[inew], function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables)[inew] <- dates


save(files, tables, file = "COVID-19.RData")


# ==========================================================================================
# Actualizar Tablas por sexo y grupo de edad
# ==========================================================================================

# files <- dir(pattern = '*.pdf')
# inew <- length(files)
# file <- files[inew]
# file

process_table_edadsexo5 <- function(file, page = 4, table = 1 ) { # nhead = 5
    # page = 4; table = 1
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[4:14, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    # View(tabla)
    tabla[c(16, 34, 52), ncol(tabla)] <- "100 -1" # Anadir -1 en lugar de Letalidad del total

    # Totales
    values <- tabla[6:16, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    # knitr::kable(values)
    total <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Mujeres
    values <- tabla[24:34, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    mujeres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Hombres
    values <- tabla[42:52, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    hombres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    edadsexo <- dplyr::bind_rows(list(Total = total, Mujeres = mujeres, Hombres = hombres), .id = "sexo")
    edadsexo$sexo <- factor(edadsexo$sexo, levels = c("Mujeres", "Hombres", "Total")) # Order matters...
    edadsexo$edad <- as.factor(edadsexo$edad)
    return(edadsexo)
}


edadsexo <- process_table_edadsexo4(file, page = 4)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")
# Pendiente añadir etiquetas variables
# View(edadsexo)
save(edadsexo, file = "edadsexo.RData")

## ----------------
# Generar listado de tablas automáticamente y mostrar

# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
browseURL(url = rmarkdown::render("COVID-19-tablas.Rmd", encoding = "UTF-8"))

browseURL(url = rmarkdown::render("COVID-19-MSCBS.Rmd", encoding = "UTF-8"))

