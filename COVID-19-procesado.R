## Instalar paquetes

# Instalar Java Runtime Environment de 64 bits para Windows
#   https://www.java.com/es/download/windows-64bit.jsp
# Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_241') # No debería ser necesario
# if(!require("rJava")) install.packages("rJava")

# https://docs.ropensci.org/tabulizer
# if(!require("devtools")) install.packages("devtools")
# devtools::install_github(c("ropenscilabs/tabulizerjars", "ropenscilabs/tabulizer"), INSTALL_opts = "--no-multiarch")

# https://docs.ropensci.org/pdftools
# if(!require("pdftools")) install.packages("pdftools")

## Procesar

library("tabulizer")

files <- dir(pattern = '*.pdf')
tables <- vector(mode = "list", length = length(files))

# La tabla por CCAA comienza en Actualizacion_35_COVID-19.pdf - 3.03.2020 (tabla 3, no se importa)
# La tabla por CCAA completa comienza en Actualizacion_36_COVID-19.pdf
# Hay cambios en los formatos de los archivos

file <- files[5]  # "Actualizacion_35_COVID-19.pdf" # No encuentra tablas

file <- files[6]  # "Actualizacion_36_COVID-19.pdf"
file <- files[13] # "Actualizacion_43_COVID-19.pdf"

process_table <- function(file, page = 2, table = 1) {
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
  tables[[i]] <- process_table(files[i])
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}
  
file <- files[14] # "Actualizacion_44_COVID.pdf"
file <- files[15] # "Actualizacion_45_COVID.pdf"
#  tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

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

file <- files[16] # "Actualizacion_46_COVID-19.pdf"
tables[[16]] <- process_table(files[16])
knitr::kable(tables[[16]])

file <- files[17] # "Actualizacion_47_COVID-19.pdf"
file <- files[20] # "Actualizacion_52_COVID-19.pdf"

for (i in 17:20) {
  tables[[i]] <- process_table(files[i], page = 1)
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}

file <- files[21] # "Actualizacion_51_COVID-19.pdf"
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

file <- files[22] # "Actualizacion_52_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

tables[[22]] <- process_table3(files[22], table = 2)
knitr::kable(tables[[22]])



## Fechas

# file <- files[6]  # "Actualizacion_36_COVID-19.pdf" 2020-03-04
# file <- files[22] # "Actualizacion_52_COVID-19.pdf" 2020-03-22
# seq(as.Date("2020/3/4"), Sys.Date(), by = 1)
# extract_text(file, pages = 1)

library(pdftools)

dates <- sapply(files, function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables) <- dates

save(files, tables, file = "COVID-19.RData")
