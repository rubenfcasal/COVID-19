library(dplyr)
library(tidyr)

# Listado de provincias y CCAA 
provincias <- read.csv2(file = "iso_ccaa_provincia.csv", na.strings = "", 
                        colClasses = "character", header = TRUE) 
# Convertir a factor manteniendo el orden
provincias <- provincias %>% mutate(across(.fns =  ~ factor(.x, levels = unique(.x)))) # mutate_if


# BUCLE
# --------------------
# CUIDADO: Set Working Directory > To Source File Location
dirs <- list.dirs("./historico_csv", recursive = FALSE) 

files <- file.path(dirs, "casos_hosp_uci_def_sexo_edad_provres.csv")

for (f in files) {
  
cat("\nProcesando: ", f)  
  
# Procesar
# --------------------
# Cuidado: `na.strings = ""` para evitar que `"NA"` de Navarra se interprete como missing
# Cuidado: iso Navarra "NC" en iso_ccaa_provincia.csv es el estándar, puede dar lugar a confusión,
#     Cambiar a "NA" como en provincia_iso? Esto permitiría tener en cuenta "No Consta" al trocear?
# En caso de duda: https://es.wikipedia.org/wiki/ISO_3166-2:ES#Provincias
# Cambios:
#   El 02/03/2021 se cambió el nivel de Melilla de 'ME' a 'ML' (coincidiendo con el estándar)
#     Hasta 24/02/2021 se utilizaba ME en lugar de ML
# Pendiente: EA: Ceuta y Melilla, código ISO 3166-1 alfa-2 
# Pendiente: renombrar casos como confirmados
# Pendiente: mantener factores sexo y edad
# Pendiente: añadir población y nuevas variables
#     Mantener acumula2 y acumula22 en formato predicción cooperativa
#     Guardar solo acumula22 en acumula2_hist2

# f <- "casos_hosp_uci_def_sexo_edad_provres.csv"
casos <- read.csv(f, sep = ",", na.strings = "", 
                  colClasses = c(rep("factor", 3), "character", rep("integer", 4)))
casos$fecha <- as.Date(casos$fecha, format = "%Y-%m-%d") 
range(casos$fecha)

# Verificar nombre de las variables
var.isciii <- c("provincia_iso", "sexo", "grupo_edad", "fecha", "num_casos", 
                "num_hosp", "num_uci", "num_def")
if (any(names(casos) != var.isciii)) stop('Cambios en las variables')

# Antes del 02/03/2021 se empleaba 'ME' en lugar de 'ML' para Melilla
levels(casos$provincia_iso) <- gsub("ME", "ML", levels(casos$provincia_iso))
# Verificar niveles de provincia_iso
if (!all(levels(casos$provincia_iso) %in% levels(provincias$provincia_iso))) 
    stop('Cambios en provincia_iso: ',
         setdiff(levels(casos$provincia_iso), levels(provincias$provincia_iso)))


# Añadir "provincia", "iso" (CCAA) y "ccaa"
casos <- casos %>% left_join(provincias, by="provincia_iso")

# Verificar y cambiar niveles de sexo 
if (!all(levels(casos$sexo) == c("H", "M", "NC"))) 
    stop('Cambios en niveles del factor sexo')
levels(casos$sexo) <- c("Hombres", "Mujeres", "NA")

# Pendiente: calcular totales por factor?
attr(casos, "url") <- "https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv"

# save(casos, file = "casos.RData")
# dput(names(casos))
# c("provincia_iso", "sexo", "grupo_edad", "fecha", "num_casos", 
# "num_hosp", "num_uci", "num_def", "provincia", "iso", "ccaa")

# Casos por CCAA
# -------------------
# Pendiente: agregar incidencia acumulada a 14 días y a 7 días

casos_ccaa <- casos %>% rename(edad = grupo_edad) %>%
  group_by(fecha, iso, ccaa, sexo, edad) %>% 
  summarise(confirmados=sum(num_casos), hospitalizados=sum(num_hosp), uci=sum(num_uci), 
            fallecidos=sum(num_def), .groups = 'drop')

# Se añaden totales para España
# https://stackoverflow.com/questions/46126610/appeding-summary-row-of-each-factor-level-using-dplyr-in-r
casos_ccaa <- casos_ccaa %>% bind_rows( 
      group_by(., fecha, sexo, edad) %>% 
      summarise(iso = "ES", ccaa = "España",
                across(confirmados:fallecidos, sum), .groups = 'drop') 
  , .) # Añade anterior al final       
casos_ccaa$iso <- factor(casos_ccaa$iso, levels = c("ES", levels(provincias$iso)))
casos_ccaa$ccaa <- factor(casos_ccaa$ccaa, levels = c("España", levels(provincias$ccaa)))

# # Guardar niveles CCAA
# ccaas <- levels(casos_ccaa$ccaa)
# names(ccaas) <- levels(casos_ccaa$iso)
# save(ccaas, file = "ccaas.RData")

# Se añade total de sexo
levels.sexo <- c("Total", levels(casos_ccaa$sexo))
casos_ccaa <- casos_ccaa %>% bind_rows( 
      group_by(., fecha, iso, ccaa, edad) %>% 
      summarise(sexo = "Total", 
                across(confirmados:fallecidos, sum), .groups = 'drop') 
  , .) # Añade anterior al final     
casos_ccaa$sexo <- factor(casos_ccaa$sexo, levels = levels.sexo)

# Se añade total de edad
levels.edad <- c("Total", levels(casos_ccaa$edad))
casos_ccaa <- casos_ccaa %>% bind_rows( 
      group_by(., fecha, iso, ccaa, sexo) %>% 
      summarise(edad = "Total", 
                across(confirmados:fallecidos, sum), .groups = 'drop') 
  , .) # Añade anterior al final     
casos_ccaa$edad <- factor(casos_ccaa$edad, levels = levels.edad)


# Ordenar y guardar
casos_ccaa <- casos_ccaa %>% arrange(fecha, iso, sexo, edad)
# View(casos_ccaa)
# save(casos_ccaa, file ="casos_ccaa.RData")

# Acumulados por CCAA
# -------------------
# NOTAS: 
# Se supone que casos_ccaa está ordenado por fecha

# Calcular acumulados
acumulados <- casos_ccaa %>%  
  group_by(iso, sexo, edad) %>% 
  mutate(across(confirmados:fallecidos, cumsum)) %>%
  ungroup() 
# View(acumulados)

# Guardar
save(acumulados, file ="acumulados.RData")

# acumula2
# ---------
# Mantenemos acumula2 compatible con la versión anterior
#   Sin sexo y edad
# Se elimina la variable nuevos 
acumula2 <- acumulados %>% filter(sexo == "Total", edad == "Total") %>% 
  select(-sexo, -edad) 
# View(acumula2)

# Guardar
# save(acumula2, file ="acumula2.RData")


# acumula22
# ---------
# Datos troceados por CCAA y respuesta (lista anidada)
#     acumula22[[ccaa]][[respuesta]] data frame
#     variables: c("fecha", "iso", "respuesta", "observado")
# Se elimina "iso" y "respuesta" del resultado final
# Pendiente: Cambiar orden [[respuesta]][[ccaa]]?
# Pendiente: Conservar No Consta?

library(tidyr)
respuestas <- c("confirmados", "hospitalizados", "uci", "fallecidos")
acumula22 <- acumula2 %>% select(-ccaa) %>%
  pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
               names_ptypes = list(respuesta = factor(levels = respuestas)))

# Troceamos por CCAA y respuesta
iso <- acumula22$iso
acumula22 <- split(select(acumula22, -iso), iso)
acumula22 <- lapply(acumula22, function(d) {
  respuesta <- d$respuesta
  split(select(d, -respuesta), respuesta)
})
# View(acumula22[[1]][[1]])
save(acumula22, file ="acumula22.RData")


# Histórico
# -------------------
# Se guardan los ficheros por "fecha de descarga" en ./acumula2_hist2
#   Actualmente solo "acumula22.RData"

# Fecha descarga
fecha.txt <- format(max(acumulados$fecha) + 1, format = "%y_%m_%d")
# file.copy("acumula2.RData", paste0("./acumula2_hist2/acumula2_",fecha.txt,".RData"), overwrite = TRUE)
file.copy("acumula22.RData", paste0("./acumula2_hist2/acumula22_",fecha.txt,".RData"), overwrite = TRUE)

} # for (f in files)
cat("\nTerminado.\n")  
