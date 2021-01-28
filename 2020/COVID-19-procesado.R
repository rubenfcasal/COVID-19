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
# NOTA: ESTE ARCHIVO ESTA SIENDO USADO UNICAMENTE
# COMO HISTORICO DE LOS PROCESAMIENTOS REALIZADOS
# VER COVID-19-actualizar.R PARA EL ULTIMO
# PROCEDIMIENTO REALIZADO

library("tabulizer")

files <- dir(pattern = '*.pdf')
tables <- vector(mode = "list", length = length(files))

# La tabla por CCAA comienza en Actualizacion_35_COVID-19.pdf - 3.03.2020 (tabla 3, no se importa)
# La tabla por CCAA completa comienza en Actualizacion_36_COVID-19.pdf
# Hay cambios en los formatos de las tablas y de los archivos

# file <- files[5]  # "Actualizacion_35_COVID-19.pdf" # No encuentra tablas
# ?locate_areas
# ?extract_text

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

# file <- files[25] # "Actualizacion_55_COVID-19.pdf"
# file <- files[38] # "Actualizacion_68_COVID-19.pdf"
# tabla <- extract_tables(file, page = 1, encoding = "UTF-8")[[1]]

process_table4 <- function(file, page = 1, table = 1, nhead = 5) { 
# page = 1; table = 1; nhead = 5; ncol.labels = 1
  ihead <- seq_len(nhead)
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

for (i in 25:38) {
  tables[[i]] <- process_table(files[i], page = 1)
  cat("\nfile: ", files[i])
  print(knitr::kable(tables[[i]]))
}

## ----------------
# file <- files[39] # "Actualizacion_69_COVID-19.pdf"
## ----------------




process_table5 <- function(file, page = 1, table = 1, nhead = 5) { 
# page = 2; table = 1; nhead = 4; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  values <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  values <- gsub(',', '.', values)       # Cambiar comas por puntos
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 2, as.numeric)
  ina <- is.na(values[, 1])
  values <- values[!ina, ]
  # values[is.na(values)] <- 0 # Reemplazar NAs por 0 
  if (any(is.na(values))) warning("Hay datos faltantes...")
  head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
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


## ----------------
# file <- files[48] # "Actualizacion_78_COVID-19.pdf"
## ----------------

process_table_a <- function(file, page = 1, table = 1, nhead = 3) { 
# page = 1; table = 1; nhead = 3; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  values <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  values <- gsub(',', '.', values)       # Cambiar comas por puntos
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 2, as.numeric)
  # head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  head <- c("Casos", "Nuevos", "PCR", "Test rápidos", "IA (14 d.)")
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


process_table_b <- function(file, page = 2, table = 1, nhead = 4) { 
# page = 2; table = 1; nhead = 3; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Trocear
  tabla <- gsub("¥", " NA", tabla)
  values <- apply(tabla[-nrow(tabla),], 1, function(x) unlist(strsplit(x, " ")))
  values <- suppressWarnings(apply(values, 1, as.numeric))
  values <- rbind(values, colSums(values))
  head <- c("Casos que han precisado hospitalización", "Hosp. nuevos", 
            "Casos que han ingresado en UCI", "UCI nuevos", 
            "Fallecidos", "Fall. nuevos", "Curados", "Cur. Nuevos")
  colnames(values) <- head
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    

table_a <- process_table_a(files[inew], nhead = 1)
# View(table_a)

process_table_b2 <- function(file, page = 2, table = 1, nhead = 3) { 
# page = 2; table = 1; nhead = 3
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Corregir notas
  i2d <- which(tabla == "¥", arr.ind = TRUE)
  i2d2 <- i2d
  i2d2[, 1] <- i2d2[, 1] + 1
  tabla[i2d2] <- paste(tabla[i2d2], "NA")
  tabla <- tabla[-unique(i2d[, 1]), ]
  # Arreglar a mano los que faltan
  i2d2 <- matrix(c(2,2, 9,1, 9,2, 15,1), ncol = 2, byrow = TRUE)
  tabla[i2d2] <- paste(tabla[i2d2], "NA")
  # Trocear
  values <- apply(tabla[-nrow(tabla),], 1, function(x) unlist(strsplit(x, " ")))
  values <- suppressWarnings(apply(values, 1, as.numeric))
  values <- rbind(values, colSums(values))
  head <- c("Casos que han precisado hospitalización", "Hosp. nuevos", 
            "Casos que han ingresado en UCI", "UCI nuevos", 
            "Fallecidos", "Fall. nuevos", "Curados", "Cur. Nuevos")
  colnames(values) <- head
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    

table_b <- process_table_b2(files[inew])
# View(table_b)

tables[[inew]] <- cbind(table_a, table_b)
knitr::kable(tables[[inew]])

## ----------------
# file <- files[49] # "Actualizacion_79_COVID-19.pdf"
## ----------------

  

process_table_a2 <- function(file, page = 1, table = 1, nhead = 3) { 
# page = 1; table = 1; nhead = 3; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  i2d <- which(tabla[, 1:2] == "", arr.ind = TRUE)
  tabla[i2d] <- "NA NA"
  # Arreglar a mano los que faltan
  i2d2 <- matrix(c(5,2, 13,2, 14,2, 15,2), ncol = 2, byrow = TRUE)
  tabla[i2d2] <- paste(tabla[i2d2], "NA")  
  i2d2 <- matrix(c(9,2), ncol = 2, byrow = TRUE)
  tabla[i2d2] <- paste("NA", tabla[i2d2])  
  values <- apply(tabla[, 1:2], 1, function(x) unlist(strsplit(x, " ")))
  
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 1, as.numeric)
  values <- cbind(values, apply(tabla[, -(1:2)], 2, as.numeric))
  # head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  head <- c("Casos", "Nuevos", "Confirmados PCR", "Confirmados test rápidos", "IA (14 d.)",
            "Positivos test rápidos", "Total positivos")
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



table_a <- process_table_a2(files[inew])
# View(table_a)

# El 08/04/2020 se dejó de calcular el total de España de hospitalizados y UCI

process_table_b3 <- function(file, page = 2, table = 1, nhead = 3) { 
# page = 2; table = 1; nhead = 3
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Corregir notas
  tabla <- gsub("¥", ' NA', tabla)
  # Arreglar a mano los que faltan
  i2d2 <- matrix(c(8,2), ncol = 2, byrow = TRUE)
  tabla[i2d2] <- paste(tabla[i2d2], "NA")
  # Trocear
  values <- apply(tabla[-nrow(tabla), 1:3], 1, function(x) unlist(strsplit(x, " ")))
  values <- suppressWarnings(apply(values, 1, as.numeric))
  values <- rbind(values, colSums(values))
  values <- cbind(values, apply(tabla[, -(1:3)], 2, as.numeric))
  
  head <- c("Casos que han precisado hospitalización", "Hosp. nuevos", 
            "Casos que han ingresado en UCI", "UCI nuevos", 
            "Fallecidos", "Fall. nuevos", "Curados", "Cur. Nuevos")
  colnames(values) <- head
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    

table_b <- process_table_b3(files[inew])
# View(table_b)

tables[[inew]] <- cbind(table_a, table_b)
knitr::kable(tables[[inew]])
# View(tables[[inew]])


## ----------------
# file <- files[50] # "Actualizacion_80_COVID-19.pdf"
## ----------------


process_table_a3 <- function(file, page = 1, table = 1, nhead = 3) { 
# page = 1; table = 1; nhead = 3; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Arreglar a mano
  i2d2 <- matrix(c(5,2, 13,2, 15,2), ncol = 2, byrow = TRUE)
  tabla[i2d2] <- paste(tabla[i2d2], "NA")  
  values <- apply(tabla[, 1:2], 1, function(x) unlist(strsplit(x, " ")))
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 1, as.numeric)
  values <- cbind(values, apply(tabla[, c(3, 5, 6)], 2, as.numeric))
  # head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  head <- c("Casos", "Nuevos", "Confirmados PCR", "Confirmados test anticuerpos", "IA (14 d.)",
            "Asintomáticos pos. test anticuerpos", "Total positivos")
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



table_a <- process_table_a3(files[inew])
# View(table_a)

# El 08/04/2020 se dejó de calcular el total de España de hospitalizados y UCI

process_table_b4 <- function(file, page = 2, table = 1, nhead = 3) { 
# page = 2; table = 1; nhead = 3
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Corregir notas
  tabla <- gsub("¥", ' NA', tabla)
  # # Arreglar a mano los que faltan
  # i2d2 <- matrix(c(8,2), ncol = 2, byrow = TRUE)
  # tabla[i2d2] <- paste(tabla[i2d2], "NA")
  # Trocear
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(tabla[-nrow(tabla), 1:3], 1, function(x) unlist(strsplit(x, " ")))
  values <- apply(values, 1, as.numeric)
  values <- rbind(values, colSums(values))
  values <- cbind(values, apply(tabla[, -(1:3)], 2, as.numeric))
  head <- c("Casos que han precisado hospitalización", "Hosp. nuevos", 
            "Casos que han ingresado en UCI", "UCI nuevos", 
            "Fallecidos", "Fall. nuevos", "Curados", "Cur. Nuevos")
  colnames(values) <- head
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    

table_b <- process_table_b4(files[inew])
# View(table_b)

tables[[inew]] <- cbind(table_a, table_b)
knitr::kable(tables[[inew]])
# View(tables[[inew]])


process_table_a7 <- function(file, page = 1, table = 1, nhead = 5) { 
# page = 1; table = 1; nhead = 5; ncol.labels = 1
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  tabla <- tabla[tabla[, 1] != "", ]
  values <- apply(tabla[, 1:2, drop = FALSE], 1, function(x) unlist(strsplit(x, " ")))
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 1, as.numeric)
  values <- cbind(values, apply(tabla[, 3:5], 2, as.numeric))
  # head <- apply(tabla[ihead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" "))
  head <- c("Casos", "Nuevos", "Confirmados PCR", "Confirmados test anticuerpos", "IA (14 d.)",
            "Asintomáticos pos. test anticuerpos", "Total positivos")
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


table_a <- process_table_a7(files[inew])
# View(table_a)

# El 08/04/2020 se dejó de calcular el total de España de hospitalizados y UCI

process_table_b8 <- function(file, page = 2, table = 1, nhead = 3) { 
# page = 2; table = 1; nhead = 3
  ihead <- seq_len(nhead)
  tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
  tabla <- gsub("\\.", "", tabla[-ihead, -1]) # Eliminar puntos
  tabla <- gsub(',', '.', tabla)       # Cambiar comas por puntos
  # Corregir notas
  tabla <- gsub("¥", "", tabla)
  # Arreglar a mano los que faltan
  tabla[14, 1]  <- "7077 NA 981"
  tabla[, 2]<- gsub("", "NA", tabla[, 2])
  values <- apply(tabla[-nrow(tabla), ], 1, function(x) unlist(strsplit(x, " ")))
  values <- gsub("[^0-9.-]", "", values) # Eliminar caracteres no numéricos
  values <- apply(values, 1, as.numeric)
  values <- rbind(values, colSums(values))
  head <- c("Casos que han precisado hospitalización", "Hosp. nuevos", 
            "Casos que han ingresado en UCI", "UCI nuevos", 
            "Fallecidos", "Fall. nuevos", "Curados", "Cur. Nuevos")
  colnames(values) <- head
  rnames <- c("Andalucía", "Aragón", "Asturias", "Baleares", "Canarias", 
    "Cantabria", "Castilla La Mancha", "Castilla y León", "Cataluña", 
    "Ceuta", "C. Valenciana", "Extremadura", "Galicia", "Madrid", 
    "Melilla", "Murcia", "Navarra", "País Vasco", "La Rioja", "ESPAÑA")
  rownames(values) <- rnames
  return(values)
}    

table_b <- process_table_b8(files[inew])
# View(table_b)

tables[[inew]] <- cbind(table_a, table_b)
knitr::kable(tables[[inew]])
# View(tables[[inew]])



## Fechas
## ----------------

library(pdftools)

dates <- sapply(files, function(file) format(pdf_info(file)$created, format = "%Y-%m-%d"))
names(tables) <- dates

save(files, tables, file = "COVID-19.RData")


# ==========================================================================================
## Actualizar
# ==========================================================================================
# VER COVID-19-actualizar.R 


## ----------------
## Tablas por sexo y grupo de edad
## ----------------


library("tabulizer")
files <- dir(pattern = '*.pdf')

# file <- files[24] # "Actualizacion_54_COVID-19.pdf"
file <- files[26] # "Actualizacion_56_COVID-19.pdf"

tabla <- extract_tables(file, page = 2, encoding = "UTF-8")[[1]]
tabla <- gsub("\\.", "", tabla) # Eliminar puntos
tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos

tabla <- gsub("%", "", tabla)   # Eliminar %
tabla[c(14, 30, 46), 9] <- "100 ." # Anadir espacio en última columna del total

# dput(apply(tabla[1:3, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")

rownms <- tabla[4:14, 1] 


# Totales
values <- tabla[4:14, -1] # Eliminamos la fila de totales
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

# ------------
file <- files[27] # "Actualizacion_57_COVID-19.pdf"
# ------------

process_table_edadsexo1 <- function(file, page = 2, table = 2) { # nhead = 7
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:7, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    rownms <- tabla[8:18, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    
    # Totales
    values <- tabla[8:18, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    colnames(values) <- head
    rownames(values) <- rownms
    # knitr::kable(values)
    total <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Mujeres
    values <- tabla[25:35, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    colnames(values) <- head
    rownames(values) <- rownms
    mujeres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Hombres
    values <- tabla[41:51, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    colnames(values) <- head
    rownames(values) <- rownms
    hombres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    edadsexo <- dplyr::bind_rows(list(Total = total, Mujeres = mujeres, Hombres = hombres), .id = "sexo")
    edadsexo$sexo <- factor(edadsexo$sexo, levels = c("Mujeres", "Hombres", "Total")) # Order matters...
    edadsexo$edad <- as.factor(edadsexo$edad)
    return(edadsexo)
}

edadsexo <- process_table_edadsexo1(file)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")
# Pendiente añadir etiquetas variables

# ------------
# file <- files[28] # "Actualizacion_58_COVID-19.pdf"
file <- files[34] # "Actualizacion_64_COVID-19.pdf"
# ------------

process_table_edadsexo2 <- function(file, page = 2, table = 1 ) { # nhead = 5
    # page = 2; table = 2
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[6:16, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    # View(tabla)
    tabla[c(16, 34, 51), ncol(tabla)] <- "100 -1" # Anadir -1 en lugar de Letalidad del total

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
    values <- tabla[41:51, -1] # Eliminamos la fila de totales
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

edadsexo <- process_table_edadsexo2(file)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")

# ------------
file <- files[35] # "Actualizacion_65_COVID-19.pdf"
# ------------

process_table_edadsexo3 <- function(file, page = 2, table = 2 ) { # nhead = 5
    # page = 2; table = 2
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[6:16, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    # View(tabla)
    tabla[c(16, 33, 50), ncol(tabla)] <- "100 -1" # Anadir -1 en lugar de Letalidad del total

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
    values <- tabla[23:33, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    mujeres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Hombres
    values <- tabla[40:50, -1] # Eliminamos la fila de totales
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

edadsexo <- process_table_edadsexo2(file)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")

# ------------
# file <- files[36] # "Actualizacion_66_COVID-19.pdf"
file <- files[38] # "Actualizacion_68_COVID-19.pdf"
# ------------

process_table_edadsexo2 <- function(file, page = 2, table = 1 ) { # nhead = 5
    # page = 2; table = 2
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[6:16, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    # View(tabla)
    tabla[c(16, 34, 51), ncol(tabla)] <- "100 -1" # Anadir -1 en lugar de Letalidad del total

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
    values <- tabla[41:51, -1] # Eliminamos la fila de totales
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


edadsexo <- process_table_edadsexo2(file)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")


# ------------
# file <- files[39] # "Actualizacion_69_COVID-19.pdf"
file <- files[45] # "Actualizacion_74_COVID-19.pdf"
# ------------

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



process_table_edadsexo4 <- function(file, page = 2, table = 1 ) { # nhead = 5
    # page = 2; table = 1
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[6:16, 1] 
    
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


process_table_edadsexo5 <- function(file, page = 3, table = 1 ) { # nhead = 5
    # page = 3; table = 1
    tabla <- extract_tables(file, page = page, encoding = "UTF-8")[[table]]
    tabla <- gsub("\\.", "", tabla) # Eliminar puntos
    tabla <- gsub(",", ".", tabla)  # Cambiar comas por puntos
    # View(tabla)
    # dput(apply(tabla[1:nhead, -1], 2, function(x) paste(x[nchar(x)>0], collapse=" ")))
    # c("Confirmados n", "Hospitalizados totales n %", "n", "Total UCI %", 
    # "n", "", "", "Fallecidos % Letalidad(%)")
    head <- c("Casos", "Hospitalizados", "Hospital. (% sexo)", 
          "UCI", "UCI (% sexo)", "Fallecidos", "Fallec. (% sexo)", "Letalidad (% edad)")
    
    rownms <- tabla[6:16, 1] 
    
    tabla <- gsub("%", "", tabla)   # Eliminar %
    # View(tabla)
    tabla[c(16, 33, 50), ncol(tabla)] <- "100 -1" # Anadir -1 en lugar de Letalidad del total

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
    values <- tabla[23:33, -1] # Eliminamos la fila de totales
    values <- apply(values, 1, function(x) unlist(strsplit(x, " ")))
    values <- apply(values, 1, as.numeric)
    values[11, 8] = round(100 * values[11, 6] / values[11, 1], 1)
    colnames(values) <- head
    rownames(values) <- rownms
    mujeres <- tibble::rownames_to_column(as.data.frame(values), var = "edad")

    # Hombres
    values <- tabla[40:50, -1] # Eliminamos la fila de totales
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


edadsexo <- process_table_edadsexo4(file)
attr(edadsexo, "file") <- file
attr(edadsexo, "date") <- format(pdftools::pdf_info(file)$created, format = "%Y-%m-%d")
# Pendiente añadir etiquetas variables
# View(edadsexo)
save(edadsexo, file = "edadsexo.RData")

# ==========================================================================================
# Actualizar
# ==========================================================================================
# VER COVID-19-actualizar.R 


## ----------------
# Generar listado de tablas automáticamente y mostrar

# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
browseURL(url = rmarkdown::render("COVID-19-tablas.Rmd", encoding = "UTF-8"))
