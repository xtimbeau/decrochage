library(fredr)
library(eurostat)
library(tidyverse)

ofce::init_qmd()

api_key <- Sys.getenv('fred_key')
set_fred_key(api_key)

median_us <- fred(medianusa_r = "MEHOINUSA672N") |>
  as_tibble() |>
  mutate(date = as.Date(date))

average_us <- fred(b6n = "A067RX1A020NBEA", pop = "B230RC0A052NBEA") |>
  mutate(b6n_vph = b6n/pop)

mav_us <- median_us |>
  left_join(average_us, by = c("date")) |>
  transmute(date, geo = "us", average = b6n_vph, median = medianusa_r)

macro_i <- get_eurostat("naida_10_gdp", filters = list(geo = c("EA20", "FR", "DE", "IT", "NL", "ES", "US"),
                                                       unit = c("CLV_I10", "CP_MEUR"),
                                                       na_item = c("B1GQ", "P3"))) |>
  select(date = time, unit, geo, na_item, values) |>
  drop_na() |>
  pivot_wider(names_from = c(na_item, unit), values_from = values) |>
  mutate(p3_p = P3_CP_MEUR/P3_CLV_I10,
         gdp_p = B1GQ_CP_MEUR/B1GQ_CLV_I10) |>
  group_by(geo) |>
  mutate(p3_p = p3_p/p3_p[date=="2010-01-01"],
         gdp_p = gdp_p/gdp_p[date=="2010-01-01"])

macro_ea <- get_eurostat("nama_10_gdp",
                         filters = list(geo = c("EA19", "FR", "DE", "IT", "NL", "ES"),
                                        unit = c("CLV_I10", "CP_MEUR", "PD10_EUR"),
                                        na_item = c("B1GQ", "P3"))) |>
  mutate(geo = ifelse(str_detect(geo, "^EA"), "EA", geo)) |>
  select(date = time, unit, geo, na_item, values) |>
  drop_na() |>
  pivot_wider(names_from = c(na_item, unit), values_from = values) |>
  mutate(p3_p = P3_CP_MEUR/P3_CLV_I10,
         gdp_p = B1GQ_CP_MEUR/B1GQ_CLV_I10) |>
  group_by(geo) |>
  mutate(p3_p = p3_p/p3_p[date=="2010-01-01"],
         gdp_p = gdp_p/gdp_p[date=="2010-01-01"])

sectors_ea <- get_eurostat("nasa_10_nf_tr", filters = list(sector = "S14_S15", na_item = "B6N", unit = "CP_MEUR", direct = "PAID",
                                                        geo = c("EA20", "FR", "DE", "IT", "NL", "ES"))) |>
  mutate(geo = ifelse(str_detect(geo, "^EA"), "EA", geo))|>
  mutate(date = time) |>
  select(date, geo, b6n = values) |>
  left_join(macro_ea |> select(p3_p, date, geo), by = c("date", "geo")) |>
  drop_na()

pop_ea <- get_eurostat("demo_pjan", filters = list(geo =  c("EA19", "FR", "DE", "IT", "NL", "ES"), sex = "T", age = "TOTAL")) |>
  mutate(geo = ifelse(str_detect(geo, "^EA"), "EA", geo)) |>
  mutate(pop = values/1000,
         date = time) |>
  drop_na() |>
  select(date, geo, pop)  |>
  mutate(geo = ifelse(str_detect(geo, "^EA"), "EA", geo))

average_ea <- sectors_ea |>
  left_join(pop_ea, by = c("geo", "date")) |>
  mutate(b6n_vph = b6n/pop/p3_p)


# SILC/revenu médian
#
median_ea <- get_eurostat("ilc_di03",
                          filters = list(
                            geo = c("EA20", "EA19", "FR", "DE", "IT", "NL", "ES"),
                            indic_il = c("MED_E", "MEI_E"),
                            sex = "T",
                            unit = c("EUR", "PPS"),
                            age = "TOTAL") ) |>
  mutate(date = as.Date(time)) |>
  select(date, unit, indic_il, geo, values) |>
  pivot_wider(names_from = geo, values_from = values) |>
  group_by(indic_il, unit) |>
  mutate(EA = ifelse(is.na(EA20), EA19/EA19[date == "2014-01-01"]*EA20[date == "2014-01-01"], EA20)) |>
  ungroup() |>
  select(-EA19, -EA20) |>
  pivot_longer(cols = -c(date, unit, indic_il), names_to = "geo", values_to = "values") |>
  drop_na() |>
  pivot_wider(names_from = c(unit, indic_il), values_from = values) |>
  left_join(macro_ea |> select(p3_p, date, geo), by = c("date", "geo")) |>
  mutate(across(-c(date, geo, p3_p), ~.x/p3_p))

mav <- average_ea |>
  filter(date>="2008-01-01") |>
  select(date, geo, average = b6n_vph) |>
  left_join(median_ea |> select(date, geo, median = EUR_MED_E), by = c("date", "geo")) |>
  bind_rows(mav_us |> filter(date>="2008-01-01")) |>
  group_by(geo) |>
  mutate(across(c(average, median), ~.x /.x[date=="2008-01-01"], .names = "i_{.col}")) |>
  ungroup() |>
  pivot_longer(cols = c(average, median, i_average, i_median), names_to = "measure", values_to = "b6nh")

mav_c <- mav |> filter(geo!="us") |>
  left_join(mav |> filter(geo=="us") |> select(date, measure, us = b6nh), by = c("date", "measure"))
ggplot(mav_c |> filter(measure |> str_starts("i_"))) +
  ggbraid::geom_braid(aes(x=date, ymin=b6nh, ymax = us, fill = us>b6nh), alpha = 0.2) +
  scale_fill_manual(values = c(darkgreenish, yelish)) +
  geom_line(aes(x =date, y = b6nh, col = geo)) +
  geom_point(aes(x =date, y = b6nh, col = geo)) +
  geom_line(aes(x =date, y = us), col = redish) +
  scale_color_pays(format = "iso2", aesthetics = "color") +
  geom_point(aes(x =date, y = us), col = redish) +
  facet_grid(col = vars(geo), row = vars(measure)) +
  scale_y_log10() +
  theme_ofce()

# Base  nama_10_gdp : Gross domestic product and main components
base_pib_ea <- get_eurostat("nama_10_gdp", time_format = "raw", stringsAsFactors = F, cache = F)

median_ea <- base_revenu_ea %>%
  filter(age=="TOTAL", sex=="T", unit=="EUR", indic_il=="MED_E",
         geo %in% c("EA20","EA19"))
median_ea <- median_ea[,-c(1:5)] %>%
  pivot_wider(names_from="geo",
              values_from = "values") %>%
  mutate(year=seq(as.Date("2004/1/1"), as.Date("2022/1/1"), by = "year")) # Création d'une colone de date pour décalage des années
median_ea['date'] = year(median_ea$year)
median_ea <- median_ea[,-c(1,4)] %>%
  mutate(date=as.character(date))

median_ea$ea19new <- vector("double",nrow(median_ea))
# for (i in seq_along(median_ea$date)) {
#   median_ea$ea19new[[i]] <- ifelse(is.na(median_ea$EA19[[i]]),
#                                    median_ea$ea19new[[i-1]]*median_ea$EA20[[i]]/median_ea$EA20[[i-1]],
#                                    median_ea$EA19[[i]])

revenu_ea_us <- full_join(revenu_us,revenu_ea,by="year")
revenu_ea_us <- revenu_ea_us %>% select(year,medianusa_r,medianea_r) %>%
  filter(year>=2004,year<=2024)

revenu_av <- get_eurostat("naida_10_gdp", filters = list(geo = c("EA20", "US"), unit = "CLV_I10", na_item = "B1GQ")) |>
  transmute(year = year(time), value = values, geo, variable  = "average") |>
  mutate(geo = ifelse(geo=="US", "usa", "ea")) |>
  drop_na()

data <- revenu_ea_us  |>
  select(year,medianusa_r,medianea_r) |>
  rename(usa = medianusa_r, ea = medianea_r) |>
  drop_na() |>
  pivot_longer(cols = c(usa, ea), names_to = "geo") |>
  mutate(variable = "median") |>
  bind_rows(revenu_av) |>
  group_by(geo, variable) |>
  mutate(indice = value/value[year==2004]) |>
  filter(year>=2004)
dataw <- data |>
  select(-value) |>
  pivot_wider(names_from = geo, values_from = indice)

gg <- ggplot(dataw) +
  facet_wrap(vars(variable), ncol = 1) +
  ggbraid::geom_braid(aes(x = year, ymin = ea, ymax = usa, fill = usa>ea), alpha=0.4, show.legend = FALSE) +
  geom_line(data = data, aes(x=year, y=indice, color = geo)) +
  theme_ofce() +
  xlab(NULL)+ylab("1 en 2004")+
  scale_y_log10() +
  PrettyCols::scale_colour_pretty_d("Joyful", name = NULL) +
  PrettyCols::scale_fill_pretty_d("Joyful")


