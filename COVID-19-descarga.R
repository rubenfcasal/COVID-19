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

f <- "serie_historica_acumulados.csv"
acumulados <- read.csv(f, colClasses = c("character", "character", rep("integer", 5)))
# PENDIENTE: Combinar todas las notas a partir de nzchar(acumulados$FECHA)
nota.texto <- acumulados[nrow(acumulados), 1]
nota.texto

# Verificar variables y seleccionar
# ---------------------------------
# Variables actuales
# dput(names(acumulados))
var.isciii <- c("CCAA", "FECHA", "CASOS", "Hospitalizados", "UCI", "Fallecidos", "Recuperados")
if (any(names(acumulados) != var.isciii)) stop('Cambios en las variables')
# Seleccionamos los casos que tienen algo en FECHA
acumulados <- acumulados[nzchar(acumulados$FECHA), var.isciii]
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
