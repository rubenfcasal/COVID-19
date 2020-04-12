## =======================
## Actualizar datos ISCIII
## =======================

# Importar
# --------

f <- "serie_historica_acumulados.csv"
acumulados <- read.csv(f, colClasses = c("character", "character", rep("integer", 5)))
# View(acumulados)

# Verificar variables y seleccionar
# ---------------------------------
# Variables actuales
# dput(names(acumulados))
var.isciii <- c("CCAA", "FECHA", "CASOS", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")
if (any(names(acumulados) != var.isciii)) stop('Cambios en las variables')
# Seleccionamos los casos que tienen algo en FECHA
inotas <- which(!nzchar(acumulados$FECHA))[1]:nrow(acumulados)
# Combinar todas las notas 
nota.texto <- paste(apply(acumulados[inotas, 1:2], 1, paste, collapse =""), collapse = "\n")
nota.texto
acumulados <- acumulados[-inotas, var.isciii]
names(acumulados) <- c("CCAA.ISO", "Fecha", "Casos", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")

# Verificar niveles factor
# El 08/04/2020 se cambió el nivel de Melilla de 'ME' a 'ML' (se mantendrá el anterior en acumula2)
CCAA.ISO <- read.csv2("CCAA.ISO2.csv", colClasses = "character")
if (!all(unique(acumulados$CCAA.ISO) %in% CCAA.ISO$COD_CCAA)) stop('Cambios en CCAA.ISO')
acumulados$CCAA.ISO <- factor(acumulados$CCAA.ISO, levels = CCAA.ISO$COD_CCAA)
iccaa <- match(as.character(acumulados$CCAA.ISO), CCAA.ISO$COD_CCAA)
acumulados <- tibble::add_column(acumulados, CCAA = CCAA.ISO$DESC_CCAA[iccaa], .before = 1)
acumulados$CCAA <- factor(acumulados$CCAA, levels = CCAA.ISO$DESC_CCAA)

# Preparar variables
acumulados$Fecha <- as.Date(acumulados$Fecha, format = "%d/%m/%Y")
# write.csv2(unique(acumulados$CCAA.ISO), file = "levels.csv")
# https://www.iso.org/obp/ui/#iso:code:3166:ES

str(acumulados)

# NA's
# ----
sapply(acumulados, function(x) sum(is.na(x)))
# which(is.na(acumulados$Fecha))
# acumulados <- acumulados[!is.na(acumulados$Fecha), ]
acumulados[is.na(acumulados)] <- 0

# plot(acumulados[, -(1:2)])
attr(acumulados, "note") <- nota.texto
attr(acumulados, "url") <- "https://covid19.isciii.es/resources/serie_historica_acumulados.csv"
# View(acumulados)
save(acumulados, file = "acumulados.RData")

# acumula2
# --------
library(dplyr)

acumula2 <- acumulados
# Renombrar variables
names(acumula2) <- tolower(names(acumula2)) 
names(acumula2)[2] <- "iso"
names(acumula2)[4] <- "confirmados"
# Nuevos
acumula2 <- acumula2 %>% group_by(iso) %>% mutate(nuevos = confirmados - lag(confirmados)) %>% ungroup()
acumula2$nuevos[is.na(acumula2$nuevos)] <- 0
# Total España
var <- c("confirmados", "hospitalizados", "uci", "fallecidos", "recuperados", "nuevos")
res <- acumula2 %>% group_by(fecha) %>% summarise_at(var, sum, na.rm = TRUE) %>%
            mutate(ccaa = "España", iso = "ES") 
res <- suppressWarnings(bind_rows(acumula2, res))
res$iso <- factor(res$iso, levels = c("ES", CCAA.ISO$COD_CCAA))
# El 08/04/2020 se cambió el nivel de Melilla de 'ME' a 'ML' (se mantendrá el anterior 'ME' en acumula2)
levels(res$iso) <- gsub("ML", "ME", levels(res$iso))
res$ccaa <- factor(res$ccaa, levels = c("España", CCAA.ISO$DESC_CCAA))
# Ordenar y guardar
acumula2 <- res %>% arrange(fecha, iso)
save(acumula2, file ="acumula2.RData")


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

library("tabulizer")


process_table <- function(file, page = 1, table = 1, nhead = 5) { 
# page = 1; table = 1; nhead = 5; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  values <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  values <- gsub(',', '.', values)       # Cambiar comas por puntos
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 2, as.numeric)
  # values[is.na(values)] <- 0 # Reemplazar NAs por 0 
  if (any(is.na(values))) warning("Hay datos faltantes...")
  head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  colnames(values) <- head
  rownames(values) <- tabla[-ihead, 1]
  return(values)
}    


file <- files[inew]
file
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[inew]] <- process_table(files[inew], nhead = 4)
knitr::kable(tables[[inew]])
# View(tables[[inew]])


# El 08/04/2020 se dejó de calcular el total de España de hospitalizados y UCI
tables[[inew]][nrow(tables[[inew]]), 3:4] <- colSums(tables[[inew]][-nrow(tables[[inew]]), 3:4], na.rm = TRUE)

## ----------------
library(pdftools)

dates <- sapply(files[inew], function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables)[inew] <- dates


save(files, tables, file = "COVID-19.RData")



# ==========================================================================================
# Actualizar Tablas por sexo y grupo de edad
# ==========================================================================================

process_table_edadsexo3 <- function(file, page = 2, table = 1 ) { # nhead = 5
    # page = 2; table = 1
    # str(extract_tables(file, page = page, encoding = "UTF-8"))
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)",
              "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    rownms <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69",
                "70-79", "80-89", "90 y +", "Total")
    tabla <- gsub("%", "", tabla)   # Eliminar %
    
    # Totales
    values <- tabla[15:25, ]
    # Eliminar " " inicial 
    values[10] <-gsub("90 y \\+", "90y+", values[10])
    # Anadir -1 en lugar de Letalidad del total
    values[11] <- paste(values[11], -1)
    # Trocear y combinar
    values <- simplify2array(strsplit(values, " "))
    # Tansformar valores
    values <- values[-1, ]
    values <- apply(values, 1, as.numeric)
    # Calcular letalidad del total
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    # knitr::kable(values)
    total <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Mujeres
    values <- tabla[33:43, ]
    # Eliminar " " inicial 
    values[10] <-gsub("90 y \\+", "90y+", values[10])
    # Anadir -1 en lugar de Letalidad del total
    values[11] <- paste(values[11], -1)
    # Trocear y combinar
    values <- simplify2array(strsplit(values, " "))
    # Tansformar valores
    values <- values[-1, ]
    values <- apply(values, 1, as.numeric)
    # Calcular letalidad del total
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    # knitr::kable(values)
    mujeres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Hombres
    values <- tabla[51:61, ]
    # Eliminar " " inicial 
    values[10] <-gsub("90 y \\+", "90y+", values[10])
    # Anadir -1 en lugar de Letalidad del total
    values[11] <- paste(values[11], -1)
    # Trocear y combinar
    values <- simplify2array(strsplit(values, " "))
    # Tansformar valores
    values <- values[-1, ]
    values <- apply(values, 1, as.numeric)
    # Calcular letalidad del total
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    # knitr::kable(values)
    hombres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    edadsexo <- dplyr::bind_rows(list(Total = total, Mujeres = mujeres, Hombres = hombres), .id = "sexo")
    edadsexo$sexo <- factor(edadsexo$sexo, levels = c("Mujeres", "Hombres", "Total")) # Order matters...
    edadsexo$edad <- as.factor(edadsexo$edad)
    return(edadsexo)
}


edadsexo <- process_table_edadsexo3(file)
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
