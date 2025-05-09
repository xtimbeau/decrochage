library(tidyverse)
library(glue)
library(MetricsWeighted)
library(ofce)

dir.create("/tmp/wid")
curl::curl_download("https://wid.world/bulk_download/wid_all_data.zip",
                    destfile = "/tmp/wid/wid.zip")
unzip("/tmp/wid/wid.zip", exdir = "/tmp/wid")
dpercentile <- str_c(str_c("p", 0:99),  str_c("p", 1:100))

fr <-  vroom::vroom(glue("/tmp/wid/WID_data_FR.csv")) |>
    filter(
      variable== "aptincj992"|
        variable== "adiincj992"|
        variable== "tptincj992"|
        variable== "tdiincj992",
      percentile %in% c("p0p50", "p50p100", "p0p90", "p0p100", dpercentile)) |>
  filter(year==2019)

fr_detail  <-  fr |>
  filter(percentile%in%dpercentile)

fr_aggr <-  fr |>
  filter(!percentile%in%dpercentile)

fr_da <- fr_detail |>
  group_by(variable) |>
  summarize(p0p50 = mean(value[value<=quantile(value, 0.5)]),
            p0p90 = mean(value[value<=quantile(value, 0.9)]),
            p0p100= mean(value)) |>
  pivot_longer(c(p0p50, p0p90, p0p100), names_to = "percentile") |>
  pivot_wider(names_from = variable)

fr_da
