## ================
## Instalar paquetes
## ================

# Instalar Java Runtime Environment de 64 bits para Windows
#   https://www.java.com/es/download/windows-64bit.jsp
# Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241') # No debería ser necesario
# if(!require("rJava")) install.packages("rJava")

# https://docs.ropensci.org/tabulizer
# if(!require("devtools")) install.packages("devtools")
# devtools::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"), INSTALL_opts = "--no-multiarch")

# https://docs.ropensci.org/pdftools
# if(!require("pdftools")) install.packages("pdftools")


## ================
## Procesar
## ================
# Si no se quieren procesar todos ir a "Actualizar"

library("tabulizer")

files <- dir(pattern = '*.pdf')
tables <- vector(mode = "list", length = length(files))

# La tabla por CCAA comienza en Actualizacion_35_COVID-19.pdf - 3.03.2020 (tabla 3, no se importa)
# La tabla por CCAA completa comienza en Actualizacion_36_COVID-19.pdf
# Hay cambios en los formatos de las tablas y de los archivos

# file <- files[5]  # "Actualizacion_35_COVID-19.pdf" # No encuentra tablas
# ?locate_areas
 
# file <- files[6]  # "Actualizacion_36_COVID-19.pdf"
# file <- files[13] # "Actualizacion_43_COVID-19.pdf"

process_table0 <- function(file, page = 2, table = 1) {
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    values <- gsub("\\.", "", tabla[-1, -1]) # Eliminar puntos
    values <- gsub(',', '.', values) # Cambiar comas por puntos
    values <- apply(values, 2, as.numeric)
    values[is.na(values)] <- 0 # Reemplazar NAs por 0 
    colnames(values) <- tabla[1, -1]
    rownames(values) <- tabla[-1, 1]
    return(values)
}    

for (i in 6:13) {
  tables[[i]] <- process_table0(files[i])
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}
  
# file <- files[14] # "Actualizacion_44_COVID.pdf"
# file <- files[15] # "Actualizacion_45_COVID.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

process_table2 <- function(file, page = 1, table = 1) {
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    values <- gsub("\\.", "", tabla[6:24, 3:4]) # Eliminar puntos
    values <- gsub(',', '.', values) # Cambiar comas por puntos
    values <- apply(values, 2, as.numeric)
    values[is.na(values)] <- 0 # Reemplazar NAs por 0 
    colnames(values) <- tabla[5, 3:4]
    rownames(values) <- tabla[6:24, 2]
    return(values)
}  

for (i in 14:15) {
  tables[[i]] <- process_table2(files[i])
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}

# file <- files[16] # "Actualizacion_46_COVID-19.pdf"

tables[[16]] <- process_table0(files[16])
knitr::kable(tables[[16]])

# file <- files[17] # "Actualizacion_47_COVID-19.pdf"
# file <- files[20] # "Actualizacion_52_COVID-19.pdf"

for (i in 17:20) {
  tables[[i]] <- process_table0(files[i], page = 1)
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}

# file <- files[21] # "Actualizacion_51_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

process_table3 <- function(file, page = 1, table = 1) {
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    values <- gsub("\\.", "", tabla[4:23, 2:7]) # Eliminar puntos
    values <- gsub(',', '.', values) # Cambiar comas por puntos
    values <- apply(values, 2, as.numeric)
    values[is.na(values)] <- 0 # Reemplazar NAs por 0 
    colnames(values) <- c(tabla[2, 2:6], "Nuevos")
    rownames(values) <- tabla[4:23, 1]
    return(values)
}  

tables[[21]] <- process_table3(files[21])
knitr::kable(tables[[21]])

# file <- files[22] # "Actualizacion_52_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[22]] <- process_table3(files[22], table = 2)
knitr::kable(tables[[22]])

# file <- files[23] # "Actualizacion_53_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[23]] <- process_table0(files[23], page = 1, table = 1)
knitr::kable(tables[[23]])

# file <- files[24] # "Actualizacion_54_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[24]] <- process_table0(files[24], page = 1, table = 1)
knitr::kable(tables[[24]])

## Fechas
## ----------------

# file <- files[6]  # "Actualizacion_36_COVID-19.pdf" 2020-03-04
# file <- files[22] # "Actualizacion_52_COVID-19.pdf" 2020-03-22
# seq(as.Date("2020/3/4"), Sys.Date(), by = 1)
# extract_text(file, pages = 1)

library(pdftools)

dates <- sapply(files, function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables) <- dates

save(files, tables, file = "COVID-19.RData")


## ================
## Actualizar
## ================

old.data <- new.env()
load("COVID-19.RData", envir = old.data)
# str(old.data$tables)

files <- dir(pattern = '*.pdf')
tables <- vector(mode = "list", length = length(files))

iold <- match(files, old.data$files)
tables <- old.data$tables[iold]

inew <- which(is.na(iold))
inew

## Procesar files[inew]
## --------------------

library("tabulizer")


process_table <- function(file, page = 1, table = 1, nrows.head = 5) { 
# page = 1; table = 1; nrows.head = 5; ncol.labels = 1
  ihead <- seq_len(nrows.head)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  values <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  values <- gsub(',', '.', values) # Cambiar comas por puntos
  values <- apply(values, 2, as.numeric)
  values[is.na(values)] <- 0 # Reemplazar NAs por 0 
  head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  colnames(values) <- head
  rownames(values) <- tabla[-ihead, 1]
  return(values)
}    

inew[1]

file <- files[inew[1]]
file
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[inew[1]]] <- process_table(files[inew[1]])
knitr::kable(tables[[inew[1]]])


## ----------------
library(pdftools)

dates <- sapply(files[inew], function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables)[inew] <- dates

save(files, tables, file = "COVID-19.RData")

## ----------------
## Tablas por sexo y grupo de edad
## ----------------

# file <- files[24] # "Actualizacion_54_COVID-19.pdf"

file <- files[25] # "Actualizacion_55_COVID-19.pdf"
tabla <- extract_tables(file, page = 2, encoding = "UTF-8")[[1]]
tabla <- gsub("\\.", "", tabla) # Eliminar puntos
tabla <- gsub(",", ".", tabla) # Cambiar comas por puntos
tabla <- gsub("%", "", tabla) # Eliminar %

# dput(apply(tabla[1:3, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
head <- c("Confirmados", "Hospitalizados", "Hospitalizados(% total)", 
          "UCI", "UCI(% total)", "Fallecidos", "Fallecidos(% total)", "Letalidad(% edad)")

rownms <- tabla[4:13, 1] # Eliminamos las filas de totales 
# (PENDIENTE: anadir espacio en última columna? eliminar %?) 
tabla[c(14, 30), 9]

# Totales
values <- tabla[4:13, -1] # Eliminamos la fila de totales
values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
values <- apply(values, 1, as.numeric)
colnames(values) <- head
rownames(values) <- rownms
knitr::kable(values)

# Mujeres
values <- tabla[20:29, -1] # Eliminamos la fila de totales
values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
values <- apply(values, 1, as.numeric)
colnames(values) <- head
rownames(values) <- rownms
knitr::kable(values)

# Hombres
values <- tabla[36:45, -1] # Eliminamos la fila de totales
values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
values <- apply(values, 1, as.numeric)
colnames(values) <- head
rownames(values) <- rownms
knitr::kable(values)

## ----------------
# Generar listado de tablas automáticamente y mostrar

# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
browseURL(url = rmarkdown::render("COVID-19-tablas.Rmd", encoding = "UTF-8"))

