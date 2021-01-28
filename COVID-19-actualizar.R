
# Verificar sin descargar
# -----------------------

library(httr)
f <- "casos_hosp_uci_def_sexo_edad_provres.csv"
r <- HEAD(paste0("https://cnecovid.isciii.es/covid19/resources/", f))
# r$headers$`last-modified`
# r$headers$`content-length`
unlist(r$headers[c("last-modified", "content-length")])
#                  last-modified                  content-length 
# "Wed, 27 Jan 2021 09:31:23 GMT"                      "18388064" 


# Descargar 
# --------------------

f <- "casos_hosp_uci_def_sexo_edad_provres.csv"
download.file(paste0("https://cnecovid.isciii.es/covid19/resources/", f), f, mode="wb") 


# Procesar
# --------------------
# Cuidado: `na.strings = ""` para evitar que `"NA"` de Navarra se interprete como missing
# Pendiente: iso Navarra "NC" en iso_ccaa_provincia.csv es el estándar, puede dar lugar a confusión,
#            cambiar a "NA" como en provincia_iso?
#            Esto permitiría tener en cuenta "No Consta" al trocear?
# En caso de duda: https://es.wikipedia.org/wiki/ISO_3166-2:ES#Provincias
# Cuidado: Melilla no cumple el estándar (utiliza ME en lugar de ML)
# Pendiente: EA: Ceuta y Melilla.

library(dplyr)

f <- "casos_hosp_uci_def_sexo_edad_provres.csv"
casos <- read.csv(f, sep = ",", na.strings = "", 
                  colClasses = c(rep("factor", 3), "character", rep("integer", 4)))
casos$fecha <- as.Date(casos$fecha, format = "%Y-%m-%d") 
range(casos$fecha)

# Verificar nombre de las variables
var.isciii <- c("provincia_iso", "sexo", "grupo_edad", "fecha", "num_casos", 
                "num_hosp", "num_uci", "num_def")
if (any(names(casos) != var.isciii)) stop('Cambios en las variables')

# Listado de provincias y CCAA 
provincias <- read.csv2(file = "iso_ccaa_provincia.csv", na.strings = "", 
                        colClasses = "character", header = TRUE) 
# Convertir a factor manteniendo el orden
provincias <- provincias %>% mutate(across(.fns =  ~ factor(.x, levels = unique(.x)))) # mutate_if

# Verificar niveles de provincia_iso
if (!all(levels(casos$provincia_iso) %in% levels(provincias$provincia_iso))) 
    stop('Cambios en provincia_iso')

# Añadir "provincia", "iso" (CCAA) y "ccaa"
casos <- casos %>% left_join(provincias, by="provincia_iso")

# Verificar y cambiar niveles de sexo 
if (!all(levels(casos$sexo) == c("H", "M", "NC"))) 
    stop('Cambios en niveles del factor sexo')
levels(casos$sexo) <- c("Hombres", "Mujeres", "NA")

# Pendiente: calcular totales por factor?
attr(casos, "url") <- "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv.csv"

save(casos, file = "casos.RData")


# Casos por CCAA
# -------------------
# Pendiente: mantener factores sexo y edad
# Pendiente: agregar incidencia acumulada a 14 días y a 7 días

casos_ccaa <- casos %>% group_by(fecha, iso, ccaa) %>% 
  summarise(casos=sum(num_casos), hospitalizados=sum(num_hosp), uci=sum(num_uci), 
            fallecidos=sum(num_def), .groups = 'drop') # %>% relocate(fecha)

# Se crean las variables para el total de España
var <- c("casos", "hospitalizados", "uci", "fallecidos")

res <- casos_ccaa %>% group_by(fecha) %>% summarise(across(all_of(var), sum), .groups = 'drop') %>%
  mutate(ccaa = "España", iso = "ES") 
res <- suppressWarnings(bind_rows(casos_ccaa, res))
res$iso <- factor(res$iso, levels = c("ES", levels(provincias$iso)))
res$ccaa <- factor(res$ccaa, levels = c("España", levels(provincias$ccaa)))

# # Guardar niveles CCAA
# ccaas <- levels(res$ccaa)
# names(ccaas) <- levels(res$iso)
# save(ccaas, file = "ccaas.RData")

# Ordenar y guardar
casos_ccaa <- res %>% arrange(fecha, iso)
# View(casos_ccaa)
save(casos_ccaa, file ="casos_ccaa.RData")

# Acumulados por CCAA
# -------------------
# NOTAS: 
# En la nueva versión emplearemos acumulados en lugar acumula2
# Se elimina la variable nuevos 
# Se supone que casos_ccaa está ordenado por fecha
# Emplear casos en lugar de confirmados?

# Calcular acumulados
acumulados <- casos_ccaa %>% group_by(iso) %>% 
  mutate(casos = cumsum(casos), hospitalizados = cumsum(hospitalizados), 
         uci = cumsum(uci), fallecidos = cumsum(fallecidos)) %>%
  ungroup() %>% rename(confirmados = casos)
# View(acumulados)

# Guardar
save(acumulados, file ="acumulados.RData")


# acumula22
# ---------
# Datos troceados por CCAA y respuesta (lista anidada)
#     acumula22[[ccaa]][[respuesta]] data frame
#     variables: c("fecha", "iso", "respuesta", "observado")
# Pendiente: Cambiar orden [[respuesta]][[ccaa]]?
# Pendiente: Conservar No Consta?
# Pendiente: Eliminar "iso" y "respuesta" del resultado final

library(tidyr)
respuestas <- c("confirmados", "hospitalizados", "uci", "fallecidos")
acumula22 <- acumulados %>% select(-ccaa) %>%
  pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
               names_ptypes = list(respuesta = factor(levels = respuestas)))

# Troceamos por CCAA y respuesta
acumula22 <- split(acumula22, acumula22$iso)
acumula22 <- lapply(acumula22, function(d) split(d, d$respuesta))
save(acumula22, file ="acumula22.RData")


# Histórico
# -------------------

# Se guardan los ficheros por "fecha de descarga" en ./acumula2_hist2

# Fecha descarga
fecha.txt <- format(max(acumulados$fecha) + 1, format = "%y_%m_%d")
file.copy("acumulados.RData", paste0("./acumula2_hist2/acumula2_",fecha.txt,".RData"), overwrite = TRUE)
file.copy("acumula22.RData", paste0("./acumula2_hist2/acumula22_",fecha.txt,".RData"), overwrite = TRUE)

# Pendiente: Informe acumula2_hist2
