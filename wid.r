library(tidyverse)
library(glue)
library(MetricsWeighted)
library(ofce)

dir.create("/tmp/wid")
curl::curl_download("https://wid.world/bulk_download/wid_all_data.zip",
                    destfile = "/tmp/wid/wid.zip")
unzip("/tmp/wid/wid.zip", exdir = "/tmp/wid")

pays <- c("FR", "DE", "IT", "ES", "NL", "AT", "BE", "FI", "GR", "PT", "IE", "US", "GB")
euz <- c("FR", "DE", "IT", "ES", "NL", "AT", "BE", "FI", "GR", "PT", "IE")
dpercentile <- str_c(str_c("p", 0:99),  str_c("p", 1:100))
raw <- map_dfr(pays, ~{
  vroom::vroom(glue("/tmp/wid/WID_data_{.x}.csv")) |>
    filter(
      variable== "aptincj992"|
        variable== "adiincj992"|
        variable== "acaincj992"|
        variable == "xlceupi999"|
        variable=="npopuli992",
      percentile %in% c("p0p50", "p0p90", "p0p100", dpercentile)) |>
    mutate(pays = .x, age = as.numeric(age))
})

pop <- raw |>
  filter(variable == "npopuli992") |>
  select(country, year, pop=value)

ppp <- raw |>
  filter(variable == "xlceupi999", year == 2023) |>
  select(country, ppp=value)

ptinc <- raw |>
  filter(variable ==  "aptincj992", percentile %in% c("p0p50", "p0p90", "p0p100")) |>
  left_join(ppp, by="country") |>
  mutate(ptinc.ppp = value/ppp) |>
  select(country, year, percentile, ptinc.ppp)

data.euz <- raw |>
  filter(variable %in% c("aptincj992", "adiincj992"),
         percentile %in% dpercentile,
         country %in% euz,
         year>=1980) |>
  select(variable, country, year, percentile, value) |>
  left_join(ppp, by= "country") |>
  left_join(pop, by = c("country", "year")) |>
  mutate(value.ppp = value/ppp,
         pop = pop/100) |>
  group_by(year, variable) |>
  arrange(year, value.ppp) |>
  mutate(qp0p50 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.5),
         qp0p90 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.9),
         qp0p100 = TRUE) |>
  summarize(
    p0p50 = sum(value.ppp[qp0p50] * pop[qp0p50])/sum(pop[qp0p50]),
    p0p90 = weighted_mean(value.ppp[qp0p90], w = pop[qp0p90]),
    p50p90 = weighted_mean(value.ppp[!qp0p50&qp0p90], w = pop[!qp0p50&qp0p90]),
    p90p100 = weighted_mean(value.ppp[!qp0p90], w = pop[!qp0p90]),
    p0p100 = weighted_mean(value.ppp, w = pop),
    pop = sum(pop),
    countries = list(unique(country)),
    .groups = "drop") |>
  select(variable, year, p0p50, p0p90, p50p90, p90p100, p0p100) |>
  pivot_longer(cols = c(p0p50, p0p90, p50p90, p90p100, p0p100),
               names_to = "percentile", values_to = "value.ppp") |>
  pivot_wider(names_from = variable, values_from = value.ppp) |>
  mutate(country = "EUZ")

data.oth2 <- raw |>
  filter(variable %in% c("aptincj992", "adiincj992"),
         percentile %in% dpercentile,
         year>=1980) |>
  select(variable, country, year, percentile, value) |>
  left_join(ppp, by= "country") |>
  left_join(pop, by = c("country", "year")) |>
  mutate(value.ppp = value/ppp,
         pop = pop/100) |>
  group_by(year, variable, country) |>
  arrange(year, value.ppp) |>
  mutate(qp0p50 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.5),
         qp0p90 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.9),
         qp0p100 = TRUE) |>
  summarize(
    p0p50 = sum(value.ppp[qp0p50] * pop[qp0p50])/sum(pop[qp0p50]),
    p0p90 = weighted_mean(value.ppp[qp0p90], w = pop[qp0p90]),
    p50p90 = weighted_mean(value.ppp[!qp0p50&qp0p90], w = pop[!qp0p50&qp0p90]),
    p90p100 = weighted_mean(value.ppp[!qp0p90], w = pop[!qp0p90]),
    p0p100 = weighted_mean(value.ppp, w = pop),
    pop = sum(pop),
    .groups = "drop") |>
  select(variable, country, year, p0p50, p0p90, p50p90, p90p100, p0p100) |>
  pivot_longer(cols = c(p0p50, p0p90, p50p90, p90p100, p0p100),
               names_to = "percentile", values_to = "value.ppp") |>
  pivot_wider(names_from = variable, values_from = value.ppp)

data.oth <- raw |>
  filter(variable %in% c("aptincj992", "adiincj992"),
         percentile %in% c("p0p50", "p0p90", "p0p100")) |>
  left_join(ppp, by="country") |>
  mutate(value.ppp = value/ppp) |>
  select(variable, country, year, percentile, value.ppp) |>
  pivot_wider(names_from = variable, values_from = value.ppp) |>
  filter(year>1980)

dina <- bind_rows(data.euz, data.oth2)

ggplot(dina |> filter(percentile %in% c("p0p50", "p0p100"))) +
  geom_line(aes(x=year, y=adiincj992, color = percentile))+
  scale_y_log10() +
  facet_wrap(vars(country)) +
  theme_ofce()
data <- dina |>
  filter(country%in%c("US", "GB", "FR", "EUZ"),
         percentile %in% c("p0p50", "p50p90", "p90p100")) |>
  pivot_longer(cols = c(adiincj992, aptincj992), names_to = "variable", values_to = "eurppp")

ggplot(data) +
  geom_line(aes(x=year, y=eurppp, color = country, linetype = percentile),
            linewidth = 0.75)+
  facet_grid(
    cols = vars(percentile),
    rows = vars(variable))+
  scale_y_log10(
    labels = scales::label_number(scale=1/1000, suffix = "k"),
    breaks = c(20000, 30000, 40000, 50000, 100000, 150000, 200000),
    guide  = guide_axis_logticks()) +
  PrettyCols::scale_color_pretty_d("Joyful") +
  theme_ofce()
