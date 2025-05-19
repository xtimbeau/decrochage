library(tidyverse)
library(glue)
library(MetricsWeighted)
library(ofce)
library(ggrepel)
library(ggiraph)

dir.create("/tmp/wid")

curl::curl_download("https://wid.world/bulk_download/wid_all_data.zip",
                    destfile = "/tmp/wid/wid.zip")
unzip("/tmp/wid/wid.zip", exdir = "/tmp/wid")

eurozone <- eurostat::ea_countries |> pull(code) |> str_replace("EL","GR")

pays <- c(eurozone, "US", "GB", "CH")
euz <- eurozone
dpercentile <- str_c(str_c("p", 0:99),  str_c("p", 1:100))

raw <- map_dfr(pays, ~{
  vroom::vroom(glue("/tmp/wid/WID_data_{.x}.csv")) |>
    filter(
      variable== "aptincj992"|
        variable== "adiincj992"|
        variable== "acaincj992"|
        variable == "xlceupi999"|
        variable=="npopuli992",
      percentile %in% c("p0p50", "p0p90", "p0p99", "p0p100", dpercentile)) |>
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
         qp0p99 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.99),
         qp0p100 = TRUE) |>
  summarize(
    p0p50 = sum(value.ppp[qp0p50] * pop[qp0p50])/sum(pop[qp0p50]),
    p0p90 = weighted_mean(value.ppp[qp0p90], w = pop[qp0p90]),
    p50p90 = weighted_mean(value.ppp[!qp0p50&qp0p90], w = pop[!qp0p50&qp0p90]),
    p50p99 = weighted_mean(value.ppp[!qp0p50&qp0p99], w = pop[!qp0p50&qp0p99]),
    p90p100 = weighted_mean(value.ppp[!qp0p90], w = pop[!qp0p90]),
    p90p99 = weighted_mean(value.ppp[!qp0p90&qp0p99], w = pop[!qp0p90&qp0p99]),
    p99p100 = weighted_mean(value.ppp[!qp0p99], w = pop[!qp0p99]),
    p0p100 = weighted_mean(value.ppp, w = pop),
    pop = sum(pop),
    countries = list(unique(country)),
    .groups = "drop") |>
  select(variable, year, p0p50, p0p90, p50p90, p50p99, p90p100, p90p99, p50p99, p99p100, p0p100) |>
  pivot_longer(cols = c(p0p50, p0p90, p50p90, p90p100, p50p99, p90p99, p99p100, p0p100),
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
         qp0p99 = value.ppp <= weighted_quantile(value.ppp, w=pop, probs = 0.99),
         qp0p100 = TRUE) |>
  summarize(
    p0p50 = sum(value.ppp[qp0p50] * pop[qp0p50])/sum(pop[qp0p50]),
    p0p90 = weighted_mean(value.ppp[qp0p90], w = pop[qp0p90]),
    p50p90 = weighted_mean(value.ppp[!qp0p50&qp0p90], w = pop[!qp0p50&qp0p90]),
    p50p99 = weighted_mean(value.ppp[!qp0p50&qp0p99], w = pop[!qp0p50&qp0p99]),
    p90p100 = weighted_mean(value.ppp[!qp0p90], w = pop[!qp0p90]),
    p90p99 = weighted_mean(value.ppp[!qp0p90&qp0p99], w = pop[!qp0p90&qp0p99]),
    p99p100 = value.ppp[percentile=="p99p100"],
    p0p100 = weighted_mean(value.ppp, w = pop),
    pop = sum(pop),
    .groups = "drop") |>
  select(variable, country, year, p0p50, p0p90, p50p90, p90p100, p50p99, p90p99, p99p100, p0p100) |>
  pivot_longer(cols = c(p0p50, p0p90, p50p90, p50p99, p90p100, p90p99, p99p100, p0p100),
               names_to = "percentile", values_to = "value.ppp") |>
  pivot_wider(names_from = variable, values_from = value.ppp)

data.oth <- raw |>
  filter(variable %in% c("aptincj992", "adiincj992"),
         percentile %in% c("p0p50", "p0p90", "p0p100", "p90p99", "p50p99", "p99p100")) |>
  left_join(ppp, by="country") |>
  mutate(value.ppp = value/ppp) |>
  select(variable, country, year, percentile, value.ppp) |>
  pivot_wider(names_from = variable, values_from = value.ppp) |>
  filter(year>1980)


dina <- bind_rows(data.euz, data.oth2) |>
  filter(
    percentile %in% c("p0p50", "p50p90", "p90p100", "p99p100", "p90p99", "p50p99")) |>
  pivot_longer(cols = c(adiincj992, aptincj992), names_to = "variable", values_to = "eurppp") |>
  group_by(year, percentile, variable) |>
  mutate(usrel = eurppp/eurppp[country=="US"]-1) |>
  group_by(percentile, country, variable) |>
  mutate(
    label = ifelse(year==max(year), str_c(country), NA_character_),
    label = ifelse(str_detect(label, "^US"), "US", label)
  ) |>
  ungroup() |>
  mutate(
    clabel = countrycode::countrycode(country, "iso2c", "country.name.fr"),
    clabel = ifelse(country=="EUZ", "Zone euro", clabel),
    vlabel = case_match(variable,
                        "aptincj992" ~ "Pré-taxation",
                        "adiincj992" ~ "Ajusté après taxation et redistribution"),
    variable= factor(variable, c("aptincj992", "adiincj992"))) |>
  group_by(year, country, percentile) |>
  mutate(
    lline = glue("<b>{round(eurppp/1000,1)} k€</b> (2023, ppp)/an/adulte, {vlabel}"),
    rline = ifelse(country=="US", "", glue(" ({ifelse(usrel>0, '+','')}{round(usrel*100)}% US)")),
    ept = eurppp[variable=="adiincj992"]/eurppp[variable=="aptincj992"] - 1,
    eline = glue("Le revenu après taxes et redis. est {ifelse(ept>0, 'augmenté', 'diminué')} de <b>{round(100*abs(ept))}%</b> par rapport au revenu pré taxation"),
    tooltip = glue("<b>{clabel}</b>
                   {year}
                   {lline[variable=='aptincj992']}{rline[variable=='aptincj992']}
                   {lline[variable=='adiincj992']}{rline[variable=='adiincj992']}
                   {eline}")
  ) |> ungroup() |>
  select(country, year, eurppp, label, percentile, variable, tooltip) |>
  group_by(variable, year, percentile) |>
  mutate(us1 = eurppp/eurppp[country=="US"]) |>
  ungroup() |>
  pivot_longer(cols = c(eurppp, us1), names_to = "type", values_to = "revenu") |>
  group_by(country, year, percentile, type) |>
  mutate(prepost = revenu[variable=="adiincj992"]/revenu[variable=="aptincj992"]) |>
  ungroup() |>
  mutate(year = year + 0.5)

dmer <- cross_join(tibble(percentile = dpercentile), tibble(mil = str_c("m", 1:10))) |>
  transmute(percentile, dmil = str_c(percentile, mil))

raw.dper <- raw |>
  filter(variable %in% c("aptincj992", "adiincj992"),
         percentile %in% dpercentile,
         country %in% c(euz,"US", "GB", "CH"),
         year>=1980) |>
  select(country, year, percentile,value, variable) |>
  left_join(ppp, by= "country") |>
  left_join(pop, by = c("country", "year")) |>
  mutate(value.ppp = value/ppp,
         pop = pop/100,
         country2 = ifelse(country%in%c("US", "GB","CH"), country, "EUZ"),
         country3 = case_when(
           country=="US" ~ "US",
           country=="GB" ~ "GB",
           country=="CH" ~ "CH",
           country=="DE" ~ "DE",
           country=="FR" ~ "FR",
           country=="IT" ~ "IT",
           country=="ES" ~ "ES",
           country=="NL" ~ "NL",
           TRUE ~ "Autres\nEUZ" )) |>
  mutate(
    country3 = factor(country3, c("Autres\nEUZ", "NL", "ES", "IT", "FR", "DE", "CH", "GB", "US"))
  ) |>
  left_join(dmer, by="percentile", relationship = "many-to-many") |>
  mutate(popm = pop/10,
         revenu = value.ppp*exp(rnorm(n(), sd=0.1)))

dec_year <- raw.dper  |>
  group_by(year, variable) |>
  reframe(seuil = weighted_quantile(revenu, w=popm, probs = 0:10/10),
          decile = str_c("d", 0:10)) |>
  group_by(year, variable) |>
  transmute(year, variable, decile, seuilm = lag(seuil), seuilp = seuil) |>
  ungroup() |>
  mutate(seuilm = ifelse(seuilm==min(seuilp), -1, seuilm)) |>
  drop_na(seuilm) |>
  mutate(decile = factor(decile, str_c("d", 1:10)))

full <- raw.dper |>
  left_join(dec_year, join_by(year, variable, revenu >= seuilm, revenu <= seuilp)) |>
  group_by(year, variable) |>
  mutate(popr = pop/sum(pop)) |>
  ungroup()

full_dec <- full |>
  group_by(country3, year, decile, variable) |>
  summarise(popr = sum(popr),
            revenu = sum(value.ppp*pop)/sum(pop)) |>
  ungroup() |>
  left_join(dec_year, by=c("year", "decile", "variable"))

return(list(dina=dina, fdec = full_dec))
