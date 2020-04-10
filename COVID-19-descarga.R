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

# Descargar
# --------
f <- "serie_historica_acumulados.csv"
download.file(paste0("https://covid19.isciii.es/resources/", f), f, mode="wb")

# Importar
# --------
# Ver COVID-19-actualizar.R

