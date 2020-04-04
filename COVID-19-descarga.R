## ================
## Datos MSCBS
## ================

# Descargar
# https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm

# ini <- 31
# last <- 43
# Parece que el 44 no está disponible
# En el 45 se cambió el nombre del archivo
# ini <- last <- 45
# files <- sprintf("Actualizacion_%i_COVID.pdf", ini:last)
# ini <- 46
# last <- 54
ini <- last <- 55
files <- sprintf("Actualizacion_%i_COVID-19.pdf", ini:last)

for (f in files)
  download.file(paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/", f),
                f, mode="wb")

## ================
## Datos ISCIII
## ================

f <- "serie_historica_acumulados.csv"
download.file(paste0("https://covid19.isciii.es/resources/", f), f, mode="wb")

# Importar
# --------

f <- "serie_historica_acumulados.csv"
acumulados <- read.csv(f, colClasses = c("character", "character", rep("integer", 5)))

nota.texto <- acumulados[nrow(acumulados), 1]
nota.texto
acumulados <- acumulados[-nrow(acumulados), ]

acumulados$Fecha <- as.Date(acumulados$Fecha, format = "%d/%m/%Y")
names(acumulados)[1] <- "CCAA.ISO"
# write.csv2(unique(acumulados$CCAA.ISO), file = "levels.csv")
# https://www.iso.org/obp/ui/#iso:code:3166:ES
CCAA.ISO <- read.csv2("CCAA.ISO.csv", colClasses = "character")
acumulados$CCAA.ISO <- factor(acumulados$CCAA.ISO, levels = CCAA.ISO$COD_CCAA)
iccaa <- match(as.character(acumulados$CCAA.ISO), CCAA.ISO$COD_CCAA)
acumulados <- tibble::add_column(acumulados, CCAA = CCAA.ISO$DESC_CCAA[iccaa], .before = 1)
acumulados$CCAA <- factor(acumulados$CCAA, levels = CCAA.ISO$DESC_CCAA)

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


