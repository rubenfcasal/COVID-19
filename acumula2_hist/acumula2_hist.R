# -------------------------------------------
# -------------------------------------------
# acumula2_hist.RData
# acumula22_hist.RData
# -------------------------------------------
# -------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# dput(names(acumula2))
# Cambio en las variables el 25/04/2020
# var <- c( "ccaa", "iso", "fecha", "confirmados", "hospitalizados", "uci", 
#           "fallecidos", "recuperados", "nuevos")
var <- c( "ccaa", "iso", "fecha", "confirmados", "hospitalizados", "uci", 
          "fallecidos", "nuevos")
# PENDIENTE: Cambiar el orden de las respuestas
# respuestas <- c("uci", "hospitalizados", "fallecidos", "nuevos", "confirmados")
respuestas <- c("confirmados", "hospitalizados", "uci", "fallecidos", "nuevos")

# -------------------------------------------
# changes$change = 1
# -------------------------------------------
#' Se constuye `acumula2_hist.RData` de forma incremental, añadiendo los nuevos valores reportados a los anteriores.
#' Si en los valores correspondientes a la última fecha del histórico 
#' hay un cambio en porcentaje superior a `tol=1`, `100*abs(old - new)/pmax(old, 1) > tol`,
#' se considera que hubo un cambio. 
#' Se asigna `NA` a los valores que se añaden y se guarda el cambio en `changes`.
#' De esta forma se ignorarán las predicciones a ese horizonte en el cálculo de errores.
# -------------------------------------------

files <- dir(pattern = paste0('^acumula2_\\d{2}_\\d{2}\\.RData'))

load(files[1])
# Cambio en las variables el 25/04/2020
# acumula2 <- acumula2 %>% select(-ccaa, -recuperados) %>%
acumula2 <- acumula2 %>% select(all_of(var[-1])) %>%
    pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
                 names_ptypes = list(respuesta = factor(levels = respuestas)))
fecha.last <- max(acumula2$fecha)
last <- acumula2 %>% filter(fecha == fecha.last) %>%
          arrange(iso, respuesta)
changes <- NULL

# Solo consideramos un cambio si hay una diferencia 
# superior a tol% y ninguno es NA
fcambio <- function(old, new, tol = 1) {
  # return(old != new)
  index <- 100*abs(old - new)/pmax(old, 1) > tol
  index[is.na(index)] <- FALSE
  return(index)
}

# PENDIENTE: Considerar cambio tipo 1 en España solo si lo hay en alguna CCAA?
# Se considera cambio tipo 1 en nuevos solo si lo hay en confirmados

for (f in files[-1]) { # f <- files[2]
# for (f in files[2:45]) { # f <- files[46]
  new.data <- new.env()
  load(f, envir = new.data)
  if (!all(var %in% names(new.data$acumula2))) stop('Cambios en los nombres de las variables')
  # Cambio en las variables el 25/04/2020
  # new.acumula2 <- new.data$acumula2 %>% select(-ccaa, -recuperados) %>%
  new.acumula2 <- new.data$acumula2 %>% select(all_of(var[-1])) %>%
      pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
                 names_ptypes = list(respuesta = factor(levels = respuestas))) %>%
      filter(fecha >= fecha.last)%>%
      arrange(fecha, iso, respuesta)
  # Verificar si hubo un salto en la fecha
  if (max(new.acumula2$fecha) > fecha.last + 1) {
    acumula2 <- bind_rows(acumula2, filter(new.acumula2, fecha > fecha.last))  
    fecha.last <- max(new.acumula2$fecha)
    last <- new.acumula2 %>% filter(fecha == fecha.last) %>%
          arrange(iso, respuesta)
    next
  }
  new.acumula2 <- split(new.acumula2, new.acumula2$fecha)
  # Comparar old y new
  new.last <- new.acumula2[[1]] %>% select(-fecha) %>% 
    rename(new = observado)
  last <- last %>% rename(old = observado) %>% 
    left_join(new.last,by = c("iso", "respuesta")) %>%
    mutate(index = fcambio(new, old))
  # Reemplazar index de nuevos por index de confirmados
  # View(last %>% filter(respuesta %in% c("confirmados", "nuevos")))
  inuevos <- last$respuesta == "nuevos"
  last[inuevos, "index"] <- last[inuevos, ] %>% select(iso) %>%
    left_join( last %>% filter(respuesta == "confirmados"), by = "iso") %>% 
    select(index)
  index <- last$index
  if(any(index)) {
    new.changes <- last %>% filter(index) %>% select(-index)
    changes <- bind_rows(changes, new.changes)
    last <- new.acumula2[[2]] %>% 
      left_join(select(last, iso, respuesta, index), 
          by = c("iso", "respuesta")) %>%
      mutate(observado = replace(observado, index, NA)) %>%
      select(-index)
  } else last <- new.acumula2[[2]]
  # Actualizar
  fecha.last <- max(last$fecha)
  acumula2 <- bind_rows(acumula2, last)
}

changes <- changes %>% 
  mutate(change = 1, pchange = 100*abs(old - new)/pmax(old, 1))


# -------------------------------------------
# changes$change = 2
# -------------------------------------------
#' Adicionalmente se asignó `NA` a `confirmados` y `nuevos` el 2020-04-24 y 2020-04-25
#' y se estableció como cambio tipo 2. 
# Eliminar a mano los problemas del 24 y 25
# load("acumula2_04_26.RData") # load("acumula2_04_25.RData")
# index <- with(acumula2, fecha %in% as.Date(c("2020-04-24", "2020-04-25")))
# acumula2[index,  c("confirmados", "nuevos")] <- NA
# save(acumula2, file = "acumula2_04_26.RData") # save(acumula2, file = "acumula2_04_25.RData")
# -------------------------------------------
# load("acumula2_hist.RData")
# dput(names(acumula2))
# c("iso", "fecha", "respuesta", "observado")

#' Para medir cambios respecto al día anterior se emplea 
#' "symmetric absolute percentage increment":
sapinc <- function(old, new) 200*abs(old - new)/pmax(abs(old) + abs(new), 20)
#' a partir del 2020-04-01.

# Considerar cambio en España solo si lo hay en alguna CCAA
# Considerar cambio en nuevos solo si lo hay confirmados
# Cambios en CCAA
changes2 <- acumula2 %>% filter(respuesta != "nuevos", iso != "ES", fecha > as.Date("2020-04-01")) %>% 
  group_by(iso, respuesta) %>% 
  mutate(sapinc = sapinc(lag(observado), observado)) %>%
  filter(sapinc > 15) %>%
  ungroup()

# Cambios en nuevos de CCAA
changes2 <- acumula2 %>% filter(respuesta == "nuevos", iso != "ES")  %>% 
  group_by(iso) %>% 
  mutate(sapinc = sapinc(lag(observado), observado)) %>%
  ungroup() %>% right_join(
      changes2 %>% filter(respuesta == "confirmados") %>% select(fecha, iso), 
      by = c("fecha", "iso")) %>% 
  bind_rows(changes2)

# Cambios en España
changes2 <- acumula2 %>% filter(iso == "ES") %>% 
  group_by(respuesta) %>% 
  mutate(sapinc = sapinc(lag(observado), observado)) %>%
  ungroup() %>% semi_join(
      changes2 %>% select(fecha, respuesta), 
      by = c("fecha", "respuesta")) %>% 
  bind_rows(changes2) 

# Añadir cambios tipo 2 el 2020-04-24 y 2020-04-25 en confirmados y nuevos
changes2 <- acumula2 %>% filter(fecha %in% as.Date(c("2020-04-24", "2020-04-25")), 
      respuesta %in% c("confirmados", "nuevos")) %>%
      mutate(sapinc = NA) %>%
      bind_rows(changes2)

# index <- with(acumula2, fecha %in% as.Date(c("2020-04-24", "2020-04-25")))
# acumula2[index,  c("confirmados", "nuevos")] <- NA

# Añadir cambios tipo 2 el 2020-05-15 en todas las variables
changes2 <- acumula2 %>% filter(fecha == as.Date("2020-05-15")) %>%
      mutate(sapinc = NA) %>%
      bind_rows(changes2)



# -------------------------------------------
# changes
# -------------------------------------------
changes <- bind_rows(changes, 
          changes2 %>% rename(pchange = sapinc, old = observado) %>% 
              mutate(change = 2)) %>%
      arrange(fecha, respuesta, iso)
# dput(names(changes))
# c("iso", "fecha", "respuesta", "old", "new", "change", "pchange")

# Añadir NAs a acumula2_hist
acumula2 <- acumula2 %>% left_join( 
                changes %>% mutate(fecha = fecha + (change == 1)) %>%
                  select(iso, fecha, respuesta, change),
                by = c("iso", "fecha", "respuesta")) %>%
          mutate(change = replace_na(change, 0),
                 observado = replace(observado, which(change!=0), NA))

# DT::datatable(acumula2, filter = 'top', options = list(pageLength = 19, autoWidth = TRUE))
# View(acumula2)
# dput(names(acumula2))
# c("iso", "fecha", "respuesta", "observado")
# dput(names(changes))
# c("iso", "fecha", "respuesta", "old", "new")
save(acumula2, changes, file ="acumula2_hist.RData")


# -------------------------------------------
# acumula22_hist.RData
# -------------------------------------------

acumula22 <- split(acumula2, acumula2$iso)
acumula22 <- lapply(acumula22, function(d) split(d, d$respuesta))

changes2 <- split(changes, changes$iso)
changes2 <- lapply(changes2, function(d) split(d, d$respuesta, drop = TRUE))
save(acumula22, changes2, file ="acumula22_hist.RData")


# -------------------------------------------
# filter_pred  
# -------------------------------------------

# Filtro horizontes
h <- 7:1
fh <- unlist(lapply(h, function(l) l:7))
filterh <- data.frame(joint = 1, ipred = - rep(h, 7-h+1), horizonte = fh)

# Filtro predicciones
# load("acumula2_hist.RData") # acumula2, changes
filter_pred <- changes %>% mutate(joint = 1) %>% 
    full_join(filterh, by = "joint") %>% 
    mutate(fecha.pred = fecha + (change == 1) + ipred) %>%
    select(-(old:ipred), change)
# dput(names(filter_pred))
# c("iso", "fecha", "respuesta", "horizonte", "fecha.pred", "change")
# # Convertir horizonte en factor
# horizontes <- as.character(1:7) 
# filter_pred$horizonte <- factor(filter_pred$horizonte, levels = horizontes)
# DT::datatable(filter_pred, filter = 'top', options = list(pageLength = 19, autoWidth = TRUE))

save(filter_pred, file ="filter_pred.RData")
  
    
# -------------------------------------------
# Generar informe  
# -------------------------------------------

# Generar informe automáticamente y mostrar
fecha <- fecha.last
# fecha <- Sys.Date() - 1

# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")
browseURL(url = rmarkdown::render("Informe_acumula2_hist.Rmd", # predicciones_cp2_new.RData
                  output_file = "Informe_acumula2_hist.html",                
                  params = list(fecha = fecha),
                  envir = new.env(), 
                  encoding = "UTF-8"
  ))


