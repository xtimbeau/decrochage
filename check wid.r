library(tidyverse)
library(glue)
library(MetricsWeighted)
library(ofce)

dir.create("/tmp/wid")
curl::curl_download("https://wid.world/bulk_download/wid_all_data.zip",
                    destfile = "/tmp/wid/wid.zip")
unzip("/tmp/wid/wid.zip", exdir = "/tmp/wid")
dpercentile <- str_c(str_c("p", 0:99),  str_c("p", 1:100))

apercentile <- c("p0p50", "p0p90", "p90p100")

fr <-  vroom::vroom(glue("/tmp/wid/WID_data_FR.csv")) |>
    filter(
      variable |> str_detect("ptinc")) |>
  filter(year==2019) |>
  mutate(pp = str_remove(percentile, "^p")) |>
  separate(pp, into = c("pp1", "pp2"), sep = "p") |>
  mutate(
    pp1 = as.numeric(pp1),
    pp2 = as.numeric(pp2),
    type = str_sub(variable, 1,1),
    variable = str_sub(variable, 2, 6)) |>
 pivot_wider(names_from = type, values_from = value)

fr_detail  <-  fr |>
  filter(percentile %in% dpercentile) |>
  select(variable, pp1, pp2, a, t, s) |>
  group_by(variable) |>
  arrange(variable, pp1, pp2) |>
  mutate(
    cums = cumsum(s),
    cuma = cumsum(a)/(pp2),
    ratio = a/s / mean(a[s>0]/s[s>0])) |>
  ungroup()

fr_aggr <-  fr |>
  filter(percentile%in%apercentile) |>
  select(variable, percentile, year, a,s,b,t)


fr_da <- fr_detail |>
  group_by(variable) |>
  summarize(p0p50 = mean(a[a<=quantile(a, 0.5)]),
            p0p90 = mean(a[a<=quantile(a, 0.9)]),
            p0p100= mean(a)) |>
  pivot_longer(c(p0p50, p0p90, p0p100), names_to = "percentile", values_to = "a")

fr_dt <- fr_detail |>
  group_by(variable) |>
  summarize(p0p50 = max(a[a<=quantile(a, 0.5)]),
            p0p90 = max(a[a<=quantile(a, 0.9)]),
            p0p100= max(a)) |>
  pivot_longer(c(p0p50, p0p90, p0p100), names_to = "percentile", values_to = "t")

fr_da
