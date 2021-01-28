
# Datos oficiales de COVID-19 en España

<!-- 
Readme.md is generated from Readme.Rmd. 
Please edit that file 
-->

El objetivo principal de [este
repositorio](https://github.com/rubenfcasal/COVID-19) es facilitar el
acceso a los datos oficiales del COVID-19 en España a los que pueden
estar interesados en analizarlos empleando R. Además se incluye una
pequeña recopilación de enlaces a recursos que pueden ser de interés.

Esta es la nueva versión del repositorio, con datos actualizados a fecha
***2021-01-28***. La versión anterior dejó de actualizarse el 2020-05-21
por problemas con la fuente de datos, aunque todavía es posible acceder
a ella a través de este
[enlace](https://rubenfcasal.github.io/COVID-19/2020/Readme.html) (puede
resultar de interés como histórico y para obtener información sobre los
problemas que hubo con los datos).

En esta versión se emplean los datos oficiales disponibles en la pestaña
*Documentación y Datos* de la web [Situación de COVID-19 en
España](https://cnecovid.isciii.es/covid19) (aplicación shiny) del
[Instituto de Salud Carlos III (ISCIII)](https://www.isciii.es). Se
considera únicamente el archivo:

  - [casos\_hosp\_uci\_def\_sexo\_edad\_provres.csv](https://cnecovid.isciii.es/covid19/resources/casos_hosp_uci_def_sexo_edad_provres.csv):
    Número de casos, hospitalizaciones, ingresos en UCI y defunciones
    por sexo, edad, provincia de residencia y fecha de diagnóstico (ver
    [metadata](https://cnecovid.isciii.es/covid19/resources/metadata_diag_ccaa_decl_prov_edad_sexo.pdf).

Los datos publicados proceden de la declaración individualizada de casos
COVID-19 a la Red Nacional de Vigilancia Epidemiológica (RENAVE) a
través de la aplicación informática SiViEs y se están actualizando de
forma continua. Además de que pueden contener errores, hay un retraso al
ir añadiendo los nuevos casos en las fechas de diagnóstico. Por este
motivo los valores correspondientes a las últimas fechas no están
consolidados (lamentablemente en el caso de algunas CCAA también ocurre
en fechas anteriores; ver histórico) y no deberían ser empleados en los
análisis (salvo que se corrijan).

## Archivos

  - [casos.RData](casos.RData): Número de casos, hospitalizaciones,
    ingresos en UCI y defunciones por sexo, edad, provincia de
    residencia y fecha de diagnóstico (datos de prevalencia).

  - [casos\_ccaa.RData](casos_ccaa.RData): Número de casos,
    hospitalizaciones, ingresos en UCI y defunciones por fecha de
    diagnóstico, CCAA y para el total de España (datos de prevalencia).

  - [acumulados.RData](acumulados.RData): Evolución de los valores
    acumulados por CCAA y para el total de España (formato adecuado para
    el proyecto [Predicción Cooperativa](#forecoop).

<!-- 
En [COVID-19-tablas.html](COVID-19-tablas.html) se pueden consultar tablas con los datos actuales por CCAA (informe generado automáticamente a partir de [COVID-19-tablas.Rmd](COVID-19-tablas.Rmd)). 
-->

El archivo [COVID-19-actualizar.R](COVID-19-actualizar.R) contiene el
código necesario para descargar e importar los datos a R.

En el directorio
[acumula2\_hist2](https://github.com/rubenfcasal/COVID-19/tree/master/acumula2_hist2)
se están almacenando un histórico de los datos publicados el ISCIII
(reportados por las CCAA). Puede ser de utilidad para estudiar como se
van consolidando los datos de la serie. Por ejemplo se incluye un
[informe](acumula2_hist2/acumula22_hist.html) con la evolución de los
datos reportados por las distintas CCAA. Lamentablemente, es muy triste
constatar que después de tanto tiempo todavía siguen los problemas…

## Proyecto de predicción cooperativa

Otro objetivo de este repositorio es proporcionar datos que puedan
servir como base para el [llamamiento del Comité Español de
Matemáticas](http://matematicas.uclm.es/cemat/covid19/2020/04/01/llamamiento-para-compartir-aportaciones-y-poder-crear-un-meta-predictor-a-corto-plazo-basado-en-las-mismas)
para la creación de un predictor cooperativo.

El proyecto “Predicción Cooperativa” surgió dentro de la iniciativa
“[Matemáticas contra el
coronavirus](http://matematicas.uclm.es/cemat/covid19)” impulsada por el
Comité Español de Matemáticas
([CEMat](http://matematicas.uclm.es/cemat/es/)). Como resultado, se
desarrolló un sitio web interactivo utilizando R
(<https://covid19.citic.udc.es>) para monitorear y predecir en el corto
plazo variables relevantes en la propagación de Covid-19. Esta web
proporcionaba “predicciones cooperativas” (metapredicciones), en
horizontes de 1 a 7 días por cada comunidad autónoma y variable de
interés, combinando predicciones basadas en diferentes métodos que
regularmente proporcionaban un gran número de grupos de investigación de
forma independiente. Para más detalles ver [Vilar-Fernández et
al. (2020)](http://www.seio.es/BBEIO/BEIOVol36Num2/files/assets/common/downloads/publication.pdf#page=15).

El proyecto estuvo vigente desde el 2 de abril hasta el 26 de mayo de
2020, ya que la falta de datos oficiales obligó a su paralización.
Actualmente se está intentando volver a ponerlo en marcha…

El código principal, empleado durante el primer periodo del proyecto
para el cálculo de las predicciones cooperativas y la generación de
informes, está disponible en el directorio
[prediccion\_cooperativa](https://github.com/rubenfcasal/COVID-19/tree/master/prediccion_cooperativa).

El archivo
[historico\_cp.RData](prediccion_cooperativa/historico_cp.RData) en la
carpeta
[prediccion\_cooperativa](https://github.com/rubenfcasal/COVID-19/tree/master/prediccion_cooperativa)
contiene las predicciones cooperativas obtenidas durante el primer
periodo del proyecto (un compromiso de confidencialidad impide
proporcionar las predicciones individuales de los grupos participantes).

El directorio
[acumula2\_hist](https://github.com/rubenfcasal/COVID-19/tree/master/acumula2_hist)
contiene el código necesario para generar el histórico de valores
reportados del ISCIII durante el primer periodo del proyecto:
[acumula2\_hist.RData](acumula2_hist/acumula2_hist.RData), empleado para
la evaluación de los predictores. También incluye un
[informe](acumula2_hist/Informe_acumula2_hist.html) con más detalles
(puede servir también para ver algunos de los problemas de los datos
reportados por las distintas CCAA en el pasado, pocas se salvan…).

## Enlaces

### Datos

  - Web del *Ministerio de Sanidad, Consumo y Bienestar Social* (se
    puede descargar un pdf con la situación actual):
    <https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/situacionActual.htm>

  - [Instituto de Salud Carlos III (ISCIII)](https://cnecovid.isciii.es)
    
      - [Situación de COVID-19 en
        España](https://cnecovid.isciii.es/covid19)
    
      - [agregados.csv](https://cnecovid.isciii.es/covid19/resources/agregados.csv)
        ***Actualmente no disponible***
    
      - [datos\_ccaas.csv](https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv)
    
      - [datos\_provincias.csv](https://cnecovid.isciii.es/covid19/resources/datos_provincias.csv)
    
      - [Informes COVID-19 del Centro Nacional de
        Epidemiología](https://www.isciii.es/QueHacemos/Servicios/VigilanciaSaludPublicaRENAVE/EnfermedadesTransmisibles/Paginas/InformesCOVID-19.aspx)

  - Otras fuentes de datos:
    
      - Escovid19data: <https://lab.montera34.com/covid19>
    
      - Galicia: <https://galiciancovid19.info>
    
      - Castilla y León:
        <https://analisis.datosabiertos.jcyl.es/pages/coronavirus/descarga-de-datasets#situacin-actual>
    
      - País Vasco:
        <https://opendata.euskadi.eus/catalogo/-/evolucion-del-coronavirus-covid-19-en-euskadi>
    
      - Andalucía:
        <https://www.juntadeandalucia.es/institutodeestadisticaycartografia/salud/index.htm>,
        <https://pakillo.github.io/COVID19-Andalucia/evolucion-coronavirus-andalucia.html>,
        <https://github.com/Pakillo/COVID19-Andalucia>
    
      - Aragón:
        <https://opendata.aragon.es/datos/catalogo/dataset/publicaciones-y-anuncios-relacionados-con-el-coronavirus-en-aragon>
    
      - Comunidad Valenciana:
        <https://dadesobertes.gva.es/va/dataset?q=&sort=views_recent+desc>
    
      - Pruebas PCR por CCAA:
        <https://datos.civio.es/dataset/pcr-coronavirus-covid19-espana-comunidades-autonomas>

<br>

## Proyecto de predicción cooperativa

  - [Acción Matemática contra el
    Coronavirus](http://matematicas.uclm.es/cemat/covid19)

  - [Web Investigadores UDC (Aplicación
    Shiny)](https://covid19.citic.udc.es), que contiene las predicciones
    cooperativas (pestaña Predicción cooperativa: Resultados).

  - [Código para el cálculo de las predicciones cooperativas y la
    generación de
    informes](https://github.com/rubenfcasal/COVID-19/tree/master/prediccion_cooperativa).

  - Vilar-Fernández, J.A., Fernández-Casal, R. y Fernandez-Lozano, C.
    (2020). [Covid-19 projections for Spain using forecast
    combinations](http://www.seio.es/BBEIO/BEIOVol36Num2/files/assets/common/downloads/publication.pdf#page=15).
    BEIO, 36 (2), 99-125.

  - [Presentación VII Xornada de Usuarios de R en
    Galicia](https://rubenfcasal.github.io/COVID-19/prediccion_cooperativa/vii_xornadasr.html)

### COVID-19 y R

  - [Top 25 R resources on COVID-19
    Coronavirus](https://www.statsandr.com/blog/top-r-resources-on-covid-19-coronavirus)

  - [Covid-19 interactive map (using R with shiny, leaflet and
    dplyr)](http://r-posts.com/covid-19-interactive-map-using-r-with-shiny-leaflet-and-dplyr)

  - [COVID-19 epidemiology with
    R](https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r)

### Epidemiología (y áreas relacionadas) con R

  - <https://www.repidemicsconsortium.org>

  - [Model-based Geostatistics: Methods and Applications in Global
    Public Health
    (book)](https://www.crcpress.com/Model-based-Geostatistics-for-Global-Public-Health-Methods-and-Applications/Diggle-Giorgi/p/book/9781138732353)
    by P.J. Diggle and E. Giorgi (2019), [código
    R](https://sites.google.com/view/mbgglobalhealth/r-scripts?authuser=0)

  - [Spatio-Temporal Statistics with R
    (book)](https://spacetimewithr.org) by C.K. Wikle, A. Zammit-Mangion
    and N. Cressie (2019), [código R](https://spacetimewithr.org/code)
    (por si alguien se anima con modelos Bayesianos…)

  - [Forecasting: Principles and Practice
    (book)](https://otexts.com/fpp2), 2ª ed., by R.J. Hyndman and G.
    Athanasopoulos (2018).

  - [Epicalc\_Book](https://cran.r-project.org/doc/contrib/Epicalc_Book.pdf)

### Paquetes de R

Paquetes y otras herramientas…

  - [COVID19](https://github.com/emanuele-guidotti/COVID19),
    [CRAN](https://cran.r-project.org/package=COVID19): Coronavirus
    COVID-19 (2019-nCoV) Epidemic Datasets

  - [cdccovidview](https://cinc.rud.is/web/packages/cdccovidview/):
    Weekly Surveillance Summary of U.S. COVID-19 Activity

  - [coronavirus](https://github.com/RamiKrispin/coronavirus),
    [CRAN](https://cran.r-project.org/package=coronavirus) : The 2019
    Novel Coronavirus COVID-19 (2019-nCoV) Dataset

  - [nCov2019](https://github.com/GuangchuangYu/nCov2019): An R package
    with real-time data, historical data and Shiny app

  - [forecast](https://pkg.robjhyndman.com/forecast): Forecasting
    Functions for Time Series and Linear Models

Se puede realizar una búsqueda en <https://rseek.org>…

## Colabora

Si quieres puedes ayudar a través de GitHub o enviando un correo a
<rubenfcasal@gmail.com>.
