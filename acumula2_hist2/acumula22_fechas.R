library(dplyr, warn.conflicts = FALSE)
library(tidyr)

files <- dir(pattern = paste0('^acumula2_\\d{2}_\\d{2}_\\d{2}\\.RData'))
fechas.txt <- substr(files, 10, 17)

var <- c( "fecha", "iso", "confirmados", "hospitalizados", "uci", 
          "fallecidos")
respuestas <- c("confirmados", "hospitalizados", "uci", "fallecidos")

for (i in seq_along(files)) {
  
  load(files[i])

  acumula22 <- acumulados %>% select(all_of(var)) %>%
  pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
               names_ptypes = list(respuesta = factor(levels = respuestas)))

  # Troceamos por CCAA y respuesta
  acumula22 <- split(acumula22, acumula22$iso)
  acumula22 <- lapply(acumula22, function(d) split(d, d$respuesta))
  save(acumula22, file =paste0("acumula22_",fechas.txt[i],".RData"))  
  
}