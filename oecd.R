library(tidyverse)
library(ofce)
init_qmd()
dataset <- "OECD.ECO.MAD,DSD_EO@DF_EO,1.2"
query <- "NLD+EA17+USA+GBR+ESP+ITA+DEU+FRA+CHE.GDPVD_CAP.A"

oecd <- OECD::get_dataset(dataset, query) |>
  transmute(year = as.numeric(TIME_PERIOD),
            value = as.numeric(ObsValue),
            country = REF_AREA) |>
  mutate(country = ifelse(country=="EA17", "EUZ", country))

