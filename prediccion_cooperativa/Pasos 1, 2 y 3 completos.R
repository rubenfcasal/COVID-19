# ===============================
# ===============================
# Establecer directorio actual como directorio de trabajo
#   setwd("C:/Dropbox (GMODES)/Prediccion_cooperativa/Predicciones")
# -------------------------
# Actualizar acumulados ISCII modificado "acumula2.RData" 
# -------------------------
# Se crean los historicos de nuevo, combinando todas las 
# predicciones, de los grupos y cooperativas, hasta la fecha:
fecha.last <- Sys.Date() - 2
# fecha.last <- as.Date("20-05-2020", format = "%d-%m-%Y")
fecha.ini <- as.Date("01-04-2020", format = "%d-%m-%Y")
fechas <- seq(fecha.ini, fecha.last, by = 1)
fecha.hist <- fecha.ini + 8 # Fecha inicio histórico de predicciones cooperativas
# ===============================
# ===============================


# ===============================
# PASO 0 (13h): ANTES DE CUALQUIERA DE LOS PASOS
# ===============================

# library(openxlsx) # install.packages('openxlsx')
library(readxl) # install.packages('readxl')
# read_excel() reads both xls and xlsx files and detects the format from the extension.
library(dplyr, warn.conflicts = FALSE)    # install.packages('dplyr')
library(tidyr)    # install.packages('tidyr')

# Variables
variables <- c("horizonte", "ccaa", "iso", "uci", "hospitalizados", "fallecidos", "nuevos", "confirmados")
respuestas <- c("confirmados", "hospitalizados", "uci", "fallecidos", "nuevos")

# CCAA
iso.ccaa <- read.csv2("iso.ccaa.csv", colClasses = "character")
iso <- iso.ccaa$iso
ccaa <- iso.ccaa$ccaa
names(ccaa) <- iso

# Grupos
# CUIDADO: Solo directorios con predicciones de los grupos
dirs <- list.dirs(recursive = FALSE) 
# https://regexr.com (cuidado con \\ en lugar de \)
dirs <- grep("^\\.\\/\\d{2}-.*", dirs, value = TRUE)

grupos <- paste0('P', substring(dirs, 3, 4))
grupname <- substring(dirs, 6)

# Horizontes
horizontes <- as.character(1:7) 

# Listas
list.fechas <- vector(mode = "list", length = length(fechas))
names(list.fechas) <- fechas
list.resp <- vector(mode = "list", length = length(respuestas))
names(list.resp) <- respuestas
list.iso <- vector(mode = "list", length = length(iso))
names(list.iso) <- iso
list.grupos <- vector(mode = "list", length = length(grupos))
names(list.grupos) <- grupos
list.horizontes <- vector(mode = "list", length = length(horizontes))
names(list.horizontes) <- horizontes

# =========================
# PASOS 1 y 2
# Crear histórico predicciones individuales
# =========================

# Histórico
# ------------
# Se crea una lista por fechas que después se combinan por filas
lpredicciones <- list.fechas

# Establecer warnings como errores
options(warn = 2)

# file.notas = "" # Salida por consola
file.notas = "Info_predicciones.txt"
cat('\nImportando predicciones\n', file = file.notas)

for (f in names(lpredicciones)) { # f <- "2020-04-01"

    fpred <- as.Date(f)
    fpred.txt <- format(fpred, format = "%d_%m")
    cat('\nImportando predicciones', fpred.txt, '\n')
    cat('\nPredicciones', fpred.txt, '\n',
            file = file.notas, append = TRUE)
    
    # Predicciones 
    # ------------
    # Se crea una lista por grupos que después se combinan por filas
    predicciones <- list.grupos

    # Importar predicciones
    for (i in seq_along(dirs)) { # i <- 3
      # glob2rx("*.xls*") == "^.*\\.xls.$"
      file <- dir(dirs[i], pattern = paste0('^.*',fpred.txt, '\\.xls')) 
      if (!length(file)) {
        cat('El grupo', grupname[i], 'no tiene datos en', fpred.txt, '\n',
            file = file.notas, append = TRUE)
        next
      }  
      # datos <- readWorkbook(file.path(dirs[i], file))
      datos <- read_excel(file.path(dirs[i], file), col_types = "text") 
      nmdatos <- tolower(names(datos))
      names(datos) <- nmdatos
      # Revisar variables
      stopifnot(all(nmdatos %in% variables)) # nombres de variables correctas
      # Convertir a numéricas
      ivar <- nmdatos %in% c("horizonte", respuestas)  
      datos[nmdatos[ivar]] <- lapply(datos[nmdatos[ivar]], as.numeric) 
      ires <- respuestas %in% nmdatos
      if (!all(ires))
          cat('El grupo', grupname[i], 'no tiene:', respuestas[!ires], '\n',
            file = file.notas, append = TRUE)
      # Revisar factores
      if (!all(index <- datos$iso %in% iso)) {
           print(datos[!index, ])
           stop('Problemas con iso en', file)
      }
      if (!all(index <- datos$ccaa %in% ccaa)) {
           # print(datos[!index, ])
           cat('  Se corrige ccaa en', file, '...\n')
           datos$ccaa <- ccaa[as.character(datos$iso)]
      }
      # Respuestas en filas
      predicciones[[grupos[i]]] <- datos %>% 
          pivot_longer(all_of(respuestas[ires]), names_to = "respuesta", values_to = "prediccion", values_drop_na = TRUE) %>%
          mutate(fecha.pred = fpred, fecha.horiz = fpred + horizonte)
    } # for (i in seq_along(dirs)) 
    
    # Grupos sin datos (TRUE)
    res <- sapply(predicciones, is.null)
    if(all(res)) {
      cat('\nAVISO: No hay grupos con datos en', f, ':', sum(res), '\n',
            file = file.notas, append = TRUE)
      predicciones <- NULL
      next
    }
    cat('\nNumero de grupos sin datos en', f, ':', sum(res), '\n',
            file = file.notas, append = TRUE)
    # Combinar grupos
    # View(bind_rows(predicciones, .id = "grupo"))
    predicciones <- bind_rows(predicciones, .id = "grupo")
    # Convertir variables categóricas en factores
    predicciones$grupo <- factor(predicciones$grupo, levels = grupos)
    # predicciones$horizonte <- factor(predicciones$horizonte, levels = horizontes)
    predicciones$ccaa <- factor(predicciones$ccaa, levels = ccaa)
    predicciones$iso <- factor(predicciones$iso, levels = iso)
    predicciones$respuesta <- factor(predicciones$respuesta, levels = respuestas)

    # Añadir distancia y filtro a predicciones
    predicciones <- predicciones %>% group_by(respuesta, iso, horizonte) %>%
      mutate(q1 = quantile(prediccion, 0.25), q3 = quantile(prediccion, 0.75)) %>%
      ungroup() %>% mutate(iqrd = pmin(abs(prediccion - q1), abs(prediccion - q3))/pmax(q3 - q1, 1)) %>% 
      group_by(respuesta, iso, grupo) %>% mutate(miqrd = mean(iqrd)) %>%
      ungroup() %>% mutate(filtro = miqrd < 4) %>% select(-q1, -q3) 
    
    # Imprimir filtrado
    cat('\nEn los predictores cooperativos Bates/Granger (mod) se filtrarían:\n',
            file = file.notas, append = TRUE)    
    cat(paste(knitr::kable(predicciones %>% filter(!filtro) %>% group_by(grupo, iso, respuesta) %>%
            summarise(miqrd = first(miqrd))), collapse ="\n"),
        '\n', file = file.notas, append = TRUE)

    lpredicciones[[f]] <- predicciones
} # for (f in names(lpredicciones))

# Restablecer warnings 
options(warn = 0)

# Crear histórico
historico <- bind_rows(lpredicciones)


# Acumulados ISCII modificado
# ---------------------------
# load("hist_acumula2.RData")  
# https://covid19.isciii.es/resources/serie_historica_acumulados.csv
# acumula2 <- acumula2 %>% select(-recuperados, -ccaa) %>%
#     pivot_longer(all_of(respuestas), names_to = "respuesta", values_to = "observado",
#                  names_ptypes = list(respuesta = factor(levels = respuestas)))

load("acumula2_hist.RData")
if (!all(levels(acumula2$respuesta) == respuestas)) {
  # Recodificar el factor respuesta
  acumula2$respuesta <- factor(acumula2$respuesta, levels = respuestas)
}

# Rango de fechas
range(acumula2$fecha)

# Combinar
# ----------
historico <- historico %>% 
    left_join(acumula2, by = c("fecha.horiz" = "fecha", "iso", "respuesta"))


# Filtrar
# ----------
load("filter_pred.RData")
# dput(names(filter_pred))
# c("iso", "fecha", "respuesta", "horizonte", "fecha.pred", "change")

historico <- historico %>% select(-change) %>% 
    left_join(select(filter_pred, -fecha), 
        by = c("fecha.pred", "horizonte", "iso", "respuesta")) %>%
          mutate(change = replace_na(change, 0),
                 observado = replace(observado, which(change!=0), NA))
# View(filter(historico, change !=0))

# str(historico)
# Rango de fechas
range(historico$fecha.pred)
# Datos con observaciones
table(!is.na(historico$observado))

# Calcular errores
# ----------
# tolerance.pe <- 1
historico <- historico %>% mutate(
        ac.e = observado - prediccion,
        ac.pe = ac.e/pmax(observado, 1) * 100,
        # ac.pe = ac.e/pmax(observado, 1),
        ac.se = ac.e^2,
        ac.ae = abs(ac.e),
        ac.ape = abs(ac.pe)
      )

# View(historico)
# dput(names(historico))
#     c("grupo", "horizonte", "ccaa", "iso", "respuesta", 
#     "prediccion", "fecha.pred", "fecha.horiz", "iqrd", "miqrd", "filtro", 
#     "observado", "ac.e", "ac.pe", "ac.se", "ac.ae", "ac.ape", "change")

# Guardar
# ----------
save(historico, file = "historico.RData")


# ================================
# PASO 3 APRENDIZAJE Y PREDICCIÓN
# ================================

CP.labels <- c( 
      CP01 = "Simple Average",
      CP02 = "Median",
      CP03 = "Trimmed Mean",
      CP04 = "Winsorized Mean",
      CP05 = "Bates/Granger (mod)",
      # CP05 = "Winkler/Makridakis (mod)",
      CP06 = "Lowess",
      CP07 = "Loess + Bates/Granger (mod)"
      # CP07 = "Loess + Winkler/Makridakis (mod)"
  )  


# -----------------------------------
# Inicio bucle fechas predicciones cooperativas
# -----------------------------------
lpredicciones_cp <- list.fechas[fechas >= fecha.hist]
laccuracy <- lpredicciones_cp
# Establecer warnings como errores
# getOption("warn")
options(warn = 2)
# -----------------------------------
for (f in names(lpredicciones_cp)) {
# -----------------------------------
  # f <- "2020-05-03"  
  f.pred <- as.Date(f) # fecha.pred
  predicciones <- lpredicciones[[f]]
  if (is.null(predicciones)) {
    cat("\n AVISO: No hay predicciones en", f, "\n")
    laccuracy[[f]] <- NULL
    lpredicciones_cp[[f]] <- NULL
    next
  }  
  cat("\n Calculando predicciones cooperativas en", f, "\n")  
  range(predicciones$fecha.pred)
  
  # Accuracy
  # ------------
  # Error de aprendizaje
  error <- "ac.mae"   # Bates/Granger
  # error <- "ac.wmae"   # Winkler/Makridakis
  # Parámetro ponderación temporal (discounting factor)
  # theta <- 1  # Bates/Granger
  theta <- 0.9 # Winkler/Makridakis

  # Se evalúan las predicciones hasta f.pred - 1 
  accuracy <- historico %>% filter (!is.na(observado), fecha.horiz < f.pred) %>%
    mutate(w = theta^as.numeric(f.pred - fecha.horiz - 1)) %>%
    group_by(respuesta, iso, horizonte, grupo) %>%
    summarize( 
          fecha.hist = f.pred - 1,
          ac.me = mean(ac.e),
          ac.mpe = mean(ac.pe),
          ac.rmse = sqrt(mean(ac.se)),
          ac.mae = mean(ac.ae),
          ac.mape = mean(ac.ape),
          ac.wme = weighted.mean(ac.e, w),
          ac.wmpe = weighted.mean(ac.pe, w),
          ac.wrmse = sqrt(weighted.mean(ac.se, w)),
          ac.wmae = weighted.mean(ac.ae, w),
          ac.wmape = weighted.mean(ac.ape, w),
          nhist = n()
      ) %>%
    ungroup()
  
  # dput(names(accuracy))
  # c("respuesta", "iso", "horizonte", "grupo", "fecha.hist", 
  # "ac.me", "ac.mpe", "ac.mse", "ac.mae", "ac.mape", 
  # "ac.wme", "ac.wmpe", "ac.wmse", "ac.wmae", "ac.wmape", "nhist")
  
  # Predictores
  # -------------------------
  predictores <- lapply(list.resp, function(x) list.iso)
  dim.pre <- list(respuestas, iso, horizontes) 
  npredictores <- array(0, dim = sapply(dim.pre, length), 
                        dimnames = dim.pre)
  npre.min <- 3 # Número mínimo predictores
  
  # Historicos
  # -------------------------
  predhist <- predictores
  # npredhist <- npredictores
  
  # Eliminamos la variable ccaa (redundante) y troceamos
  predicciones2 <- split(select(predicciones, -ccaa), predicciones$respuesta, 
                         drop = TRUE) # drop = TRUE no debería ser necesario...
  # Seleccionar error accuracy y trocear
  accuracy2 <- accuracy %>% rename(acerr = all_of(error)) %>%
    select(-fecha.hist, -starts_with('ac.')) %>%
    split(accuracy2$respuesta, drop = TRUE) 
  
  predicciones_cp2 <- list.resp
  accu.pesos <- list.resp
  
  # Bucle por respuestas
  for (r in names(predicciones2)) { # r <- "confirmados"
  
    predicciones2[[r]] <- split(predicciones2[[r]], predicciones2[[r]]$iso, drop = TRUE) 
    accuracy2[[r]] <- split(accuracy2[[r]], accuracy2[[r]]$iso, drop = TRUE) 
    predicciones_cp2[[r]] <- list.iso
    accu.pesos[[r]] <- list.iso
    
    # Bucle por CCAA
    for (ca in names(predicciones2[[r]])) { # ca <- "ES"

      predicciones2[[r]][[ca]] <- split(predicciones2[[r]][[ca]], 
                                        predicciones2[[r]][[ca]]$horizonte, drop = TRUE) 
      accuracy2[[r]][[ca]] <- split(accuracy2[[r]][[ca]], 
                                    accuracy2[[r]][[ca]]$horizonte, drop = TRUE) 
      accu.pesos[[r]][[ca]] <- list.horizontes
  
      # Número de predictores individuales
      res <- sapply(predicciones2[[r]][[ca]], nrow) # Podría no haber en todos los saltos
      npredictores[r, ca, names(res)] <- res 
      # Eliminamos los horizontes con menos de npre.min predictores
      ipre <- npredictores[r, ca, ] < npre.min 
      predicciones2[[r]][[ca]][names(ipre)[ipre]] <- NULL # Podría no haber en algún salto
      accuracy2[[r]][[ca]][names(ipre)[ipre]] <- NULL
      pred_cp <- list.horizontes[!ipre]
      # Listado de predictores por salto
      predictores[[r]][[ca]] <- lapply(predicciones2[[r]][[ca]], function(x) x$grupo)
      # Verificar si hay algún borizonte a predecir
      if(!length(predicciones2[[r]][[ca]])) {
        predicciones2[[r]][[ca]] <- NULL
        predicciones_cp2[[r]][[ca]] <- NULL
        accu.pesos[[r]][[ca]] <- NULL 
        next
      }
  
      # Bucle por horizontes
      for (h in names(predicciones2[[r]][[ca]])) { # h <- "1" 
        # NOTA: Hay que tener cuidado con niveles de factores e índices 
        # (mejor emplear texto para horizontes)
        
        pred <- predicciones2[[r]][[ca]][[h]]
        
        # Calcular predicciones cooperativas sin entrenamiento
        pred.val <- pred$prediccion
        pred_cp[[h]] <- pred[1, c("horizonte", "iso", "respuesta", 
                                  "fecha.pred", "fecha.horiz")]
        pred_cp[[h]]$CP01 = round(mean(pred.val, na.rm = TRUE)) 
        pred_cp[[h]]$CP02 = round(median(pred.val, na.rm = TRUE))
        pred_cp[[h]]$CP03 = round(mean(pred.val, trim = 0.2, na.rm = TRUE))
        pred_cp[[h]]$CP04 = round(psych::winsor.mean(pred.val, trim = 0.2, na.rm = TRUE))
        pred_cp[[h]]$npre = npredictores[r, ca, h] # Número de predictores individuales
        # Calcular errores y pesos
        if(is.null(accuracy2[[r]][[ca]][[h]])) {
          nphis <- ndhis <- 0
        } else {
          accu <- accuracy2[[r]][[ca]][[h]] %>% 
            filter(grupo %in% pred$grupo) # Filtramos info
          # Número de predictores en histórico
          nphis <- nrow(accu)
          # Máximo número de observaciones en histórico
          ndhis <- if (nphis) max(accu$nhist) else 0 
        }
        pred_cp[[h]]$nphis <- nphis
        pred_cp[[h]]$ndhis <- ndhis
        if(!nphis | !ndhis) {
          # Se calculará la media filtrada en CP05
          pesos <- pred$filtro
        } else {
          max.accu <- max(accu$acerr) # Empleada en la penalización
          accu$acerr <- with(accu, nhist*acerr + (ndhis - nhist)*max.accu)
          igrupo <- match(pred$grupo, accu$grupo)
          errores <- accu$acerr[igrupo]
          names(errores) <- pred$grupo
          errores[is.na(errores)] <- max(errores, na.rm = TRUE)
          # Calcular pesos (filtrando predictores)
          pesos <- pred$filtro/pmax(errores, 0.01)
        }
        # Reescalar
        if (sum(pesos) < 100*.Machine$double.eps) is.na(pesos) <- TRUE
        pesos <- pesos/sum(pesos)
        pred_cp[[h]]$CP05 <- round(sum(pesos*pred.val))
        # Grupos en histórico
        predhist[[r]][[ca]][[h]] <- accu$grupo
        # Almacenar
        predicciones2[[r]][[ca]][[h]] <- pred
        accu.pesos[[r]][[ca]][[h]] <- pred %>%
              select(respuesta, iso, horizonte, grupo) %>%
              mutate(peso = pesos, peso.rel = pesos*npredictores[r, ca, h]) 
      }  # for (h in names(predicciones2[[r]][[ca]]))
      # Fin bucle por horizontes
      # Recopilar
      # ------------
      predicciones2[[r]][[ca]] <- bind_rows(predicciones2[[r]][[ca]]) 
      predicciones_cp2[[r]][[ca]] <- bind_rows(pred_cp)
      accu.pesos[[r]][[ca]] <- bind_rows(accu.pesos[[r]][[ca]]) 
      # Lowess
      # x <- as.numeric(predicciones2[[r]][[ca]]$horizonte)
      x <- predicciones2[[r]][[ca]]$horizonte
      y <- predicciones2[[r]][[ca]]$prediccion
      fit <- lowess(x, y)
      xout <- predicciones_cp2[[r]][[ca]]$horizonte
      predicciones_cp2[[r]][[ca]]$CP06 <- round(approx(fit, xout = xout, ties = mean)$y)
      # Loess + Bates/Granger | Winkler/Makridakis
      w <- accu.pesos[[r]][[ca]]$peso
      fit <- loess(y ~ x, weights = w)
      predicciones_cp2[[r]][[ca]]$CP07 <- round(predict(fit, newdata = xout))
      predicciones_cp2[[r]][[ca]] <- predicciones_cp2[[r]][[ca]] %>%
          pivot_longer(starts_with("CP"), names_to = "predictor", values_to = "prediccion", 
              names_ptypes = list(predictor = factor(levels = names(CP.labels))), 
              values_drop_na = TRUE) 
    } # for (ca in names(predicciones2[[r]]))
    # Fin bucle por CCAA
  } # for (r in names(predicciones2))
  # Fin bucle por respuestas  
  
  # Recopilar pesos
  # ---------------
  accu.pesos <- bind_rows(lapply(accu.pesos, bind_rows))
  accuracy <- accuracy %>% 
        right_join(accu.pesos, by = c("respuesta", "iso", "horizonte", "grupo")) %>%
        arrange(respuesta, iso, horizonte, grupo)
  laccuracy[[f]] <- accuracy
  
  
  # Crear predicciones_cp
  # -------------------------
  predicciones_cp <- bind_rows(lapply(predicciones_cp2, bind_rows))
  lpredicciones_cp[[f]] <- predicciones_cp
  
} # for (f in names(lpredicciones_cp))
# -----------------------------------
# Fin bucle fechas predicciones cooperativas
# -----------------------------------
# Restablecer warnings 
options(warn = 0)

# Crear histórico de accuracy de predicciones individuales
# -------------------------
historico_accu <- bind_rows(laccuracy)
save(historico_accu, file = "historico_accu.RData")

# Crear histórico de predicciones cooperativas
# -------------------------
historico_cp <- bind_rows(lpredicciones_cp)

# Combinar
historico_cp <- historico_cp %>% 
    left_join(acumula2, by = c("fecha.horiz" = "fecha", "iso", "respuesta"))

# Filtrar
historico_cp <- historico_cp %>% select(-change) %>% 
    left_join(select(filter_pred, -fecha), 
        by = c("fecha.pred", "horizonte", "iso", "respuesta")) %>%
          mutate(change = replace_na(change, 0),
                 observado = replace(observado, which(change!=0), NA))
# View(filter(historico, change !=0))

# Rango de fechas
range(historico_cp$fecha.pred)
# Datos con observaciones
table(!is.na(historico_cp$observado))

# Calcular errores
# ----------
# tolerance.pe <- 1
historico_cp <- historico_cp %>% mutate(
        ac.e = observado - prediccion,
        ac.pe = ac.e/pmax(observado, 1) * 100,
        # ac.pe = ac.e/pmax(observado, 1),
        ac.se = ac.e^2,
        ac.ae = abs(ac.e),
        ac.ape = abs(ac.pe)
      )
# View(historico_cp)

# Guardar
# ----------
save(historico_cp, file = "historico_cp.RData")
# dput(names(historico_cp))
# c("horizonte", "iso", "respuesta", "fecha.pred", "fecha.horiz", 
# "npre", "nphis", "ndhis", "predictor", "prediccion", "observado", 
# "ac.e", "ac.pe", "ac.se", "ac.ae", "ac.ape")
# load("historico_cp.RData")

accuracy_cp <- historico_cp %>% filter(!is.na(observado), fecha.horiz < fecha.last) %>%
  mutate(w = theta^as.numeric(fecha.last - fecha.horiz - 1)) %>%
  group_by(respuesta, iso, horizonte, predictor) %>%
  summarize( 
        # fecha.hist = fecha.last - 1,
        ac.me = mean(ac.e),
        ac.mpe = mean(ac.pe),
        ac.rmse = sqrt(mean(ac.se)),
        ac.mae = mean(ac.ae),
        ac.mape = mean(ac.ape),
        ac.wme = weighted.mean(ac.e, w),
        ac.wmpe = weighted.mean(ac.pe, w),
        ac.wrmse = sqrt(weighted.mean(ac.se, w)),
        ac.wmae = weighted.mean(ac.ae, w),
        ac.wmape = weighted.mean(ac.ape, w),
        nhist = n()
    ) %>%
  ungroup()

save(accuracy, file = "accuracy_new.RData")
save(accuracy_cp, file = "accuracy_cp_new.RData")

save(predicciones2, predictores, npredictores, 
     file = 'predicciones2_new.RData')  
save(predicciones_cp2, predictores, npredictores, CP.labels, 
     file = 'predicciones_cp2_new.RData')  
save(predicciones_cp, file = "predicciones_cp_new.RData") # Necesario para informe

# -----------------------------------
# Generar y mostrar informes 
# -----------------------------------
fecha.txt <- format(as.Date(f), format = "%d_%m")
# fecha.txt <- format(Sys.Date() - 1, format = "%d_%m")
# Si no se emplea RStudio:
# Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

browseURL(url = rmarkdown::render("Informe_cp.Rmd", # predicciones_cp2_new.RData
                  output_file = paste0('Informe_cp_', fecha.txt, '.html'),                
                  params = list(fecha_txt = fecha.txt),
                  envir = new.env(), 
                  encoding = "UTF-8"))

browseURL(url = rmarkdown::render("Informe_foto_fija.Rmd",
                  output_file = paste0('Informe_foto_fija_', fecha.txt, '.html'),
                  params = list(fecha_txt = fecha.txt),
                  envir = new.env(),
                  encoding = "UTF-8"))

browseURL(url = rmarkdown::render("Informe_accuracy_cp.Rmd", 
                  output_file = paste0('Informe_accuracy_cp_', fecha.txt, '.html'), 
                  params = list(fecha_txt = fecha.txt),
                  envir = new.env(), 
                  encoding = "UTF-8"))

# =========================
# Predicciones individuales actuales (newpreds) 
# =========================
# predicciones2[[respuesta]][[ccaa]] data frame
#   Contiene el "objetivo (actual)" a predecir (con num grupos >= npre.min)
#   Variables predicciones2[[respuesta]][[ccaa]]:
#     c("grupo", "horizonte", "iso", "respuesta", "prediccion", "fecha.pred", 
#     "fecha.horiz", "iqrd", "miqrd", "filtro", "pesos")
# predictores[[respuesta]][[ccaa]][[horizonte]]
#   Contiene el listado de grupos (con num grupos >= npre.min)
# npredictores[respuesta, ccaa, horizonte]
#   Contiene el número de grupos (0 si num grupos < npre.min)
# =========================
# Predicciones cooperativas
# =========================
# predicciones_cp data frame
#   Variables
#     c("horizonte", "iso", "respuesta", "fecha.pred", "fecha.horiz", 
#     "npre", "nphis", "ndhis", "predictor", "prediccion")
# predicciones_cp2[[respuesta]][[ccaa]] data frame
#   Variables predicciones_cp2[[respuesta]][[ccaa]]
#     c("horizonte", "iso", "respuesta", "fecha.pred", "fecha.horiz", 
#     "npre", "nphis", "ndhis", "predictor", "prediccion")
# -------------------------------
# Predicciones actuales troceadas (para informes):
#   predicciones_cp2_new.RData
#   predicciones2_new.RData
# -------------------------------
# Precisiones actuales (para informes):
#   accuracy_new.RData: Incluye pesos aprendizaje
#       c("respuesta", "iso", "horizonte", "grupo", "fecha.hist", 
#       "ac.wme", "ac.wmpe", "ac.wmse", "ac.wmae", "ac.wmape", "nhist",
#       "peso", "peso.rel")
#   accuracy_cp_new.RData
#       c("respuesta", "iso", "horizonte", "predictor", 
#       "ac.me", "ac.mpe", "ac.mse", "ac.mae", "ac.mape", "nhist")
# -------------------------------
# Históricos:
#   historico.RData
#       c("grupo", "horizonte", "ccaa", "iso", "respuesta", 
#       "prediccion", "fecha.pred", "fecha.horiz", "iqrd", "miqrd", "filtro", 
#       "observado", "ac.e", "ac.pe", "ac.se", "ac.ae", "ac.ape")
#   historico_cp.RData
#       c("horizonte", "iso", "respuesta", "fecha.pred", "fecha.horiz", 
#       "npre", "nphis", "ndhis", "predictor", "prediccion", "observado", 
#       "ac.e", "ac.pe", "ac.se", "ac.ae", "ac.ape")
#   historico_accu.RData: historico evolución de precisiones



