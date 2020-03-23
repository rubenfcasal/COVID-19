#' ---
#' title: "Tablas COVID-19 por CCAA"
#' date: "`r Sys.Date()`"
#'
#' output:
#'   html_document:
#'      toc: true
#'      toc_float: true
#'   pdf_document: 
#'      toc: true
#' ---

print(load("COVID-19.RData"))

#+ results = "asis"
dates <- names(tables)
for (i in seq_along(files)) {
  cat("\n\n## ", dates[i])
  cat("\n\n File: [", files[i], "](", files[i], ")")
  if (!is.null(tables[[i]])) print(knitr::kable(tables[[i]]))
}


