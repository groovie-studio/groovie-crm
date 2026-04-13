packages <- c(
  "dplyr",
  "readr",
  "lubridate",
  "stringr",
  "ggplot2",
  "cluster",
  "factoextra",
  "renv",
  "writexl",
  "BTYD",
  "arules"
)

installed <- packages %in% rownames(installed.packages())

if(any(!installed)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)