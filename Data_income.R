## Charger les packages utiles
library(questionr)
library(eurostat)
library(eFRED)
library(ecb)
library(tidyverse)
library(haven)
library(lubridate)
library(stringr)
library(ggthemes)
library(ggplot2)
library(httr)
library(ofce)
library(devtools)
library(rsdmx)
library(ggrepel)
library(ggpubr)
library(openxlsx)
library(readxl)
library(zoo)
library(tframePlus)
library(seasonal)

api_key <- "ed5c6db135c9996757c242231bf28c0e"
set_fred_key(api_key)


## RECUPERE LES DONNEES ZONE EURO SOUS EUROSTAT ###

# Base  ilc_di03 : Mean and median income by age and sex
base_revenu_ea <- get_eurostat("ilc_di03", time_format = "raw", stringsAsFactors = F, cache = F)

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
for (i in seq_along(median_ea$date)) {
  median_ea$ea19new[[i]] <- ifelse(is.na(median_ea$EA19[[i]]),
                                    median_ea$ea19new[[i-1]]*median_ea$EA20[[i]]/median_ea$EA20[[i-1]],
                                    median_ea$EA19[[i]])
}

deflateur_ea <- base_pib_ea %>%
  filter(na_item=="P31_S14", geo=="EA20",
         unit %in% c("CP_MEUR","CLV10_MEUR")) %>%
  rename(date="TIME_PERIOD")
deflateur_ea <- deflateur_ea[,-c(1,3:4)] %>%
 pivot_wider(names_from="unit",
             values_from = "values") %>%
  mutate(deflateur_ea = CP_MEUR/CLV10_MEUR)

revenu_ea <- full_join(deflateur_ea,median_ea,by="date") %>%
  rename(year = "date")%>%
  mutate(medianea_r=ea19new/deflateur_ea, year=as.numeric(year))


## RECUPERE LES DONNEES USA A PARTIR DE FRED ###

median_us <- fred(medianusa = "MEHOINUSA646N", medianusa_r = "MEHOINUSA672N")
median_us$date <- median_us$date %>% lubridate::ymd()
median_us$year <- year(median_us$date)
median_us <- median_us[,-c(1)]

quintile_us <- fred(q1usa = "CXUINCAFTTXLB0102M", q2usa = "CXUINCAFTTXLB0103M", q3usa="CXUINCAFTTXLB0104M", q4usa = "CXUINCAFTTXLB0105M", q5usa = "CXUINCAFTTXLB0106M")
quintile_us$date <- quintile_us$date %>% lubridate::ymd()
quintile_us$year <- year(quintile_us$date)
quintile_us <- quintile_us[,-c(1)]

cpe <- fred(cpe_usa ="PCECTPI")
cpe$date <- cpe$date %>% lubridate::ymd()
cpe$year <- year(cpe$date)

cpe_annual <- cpe[,-c(1)] %>%
  group_by(year) %>%
  summarise_all(mean)

cpi <- fred(cpi_usa ="CPIAUCSL")
cpi$date <- cpi$date %>% lubridate::ymd()
cpi$year <- year(cpi$date)

cpi_annual <- cpi[,-c(1)] %>%
  group_by(year) %>%
  summarise_all(mean)

revenu_us <- full_join(cpe_annual,median_us,by="year")
revenu_us <- full_join(revenu_us,cpi_annual,by="year")
revenu_us <- full_join(revenu_us,quintile_us,by="year")
revenu_us <- revenu_us %>% filter(year>"1983") %>%
  mutate(rmedian_verif=medianusa/(cpe_usa/100)) %>%
  mutate(rmedian_verif2=medianusa/(cpi_usa/100)) %>%
  mutate(cpi_u_rs = medianusa/medianusa_r) %>%
  mutate(inf_u_rs = 100*(cpi_u_rs/lag(cpi_u_rs,1)-1)) %>%
  mutate(q1usa_r = q1usa/cpi_u_rs,q2usa_r = q2usa/cpi_u_rs,q3usa_r = q3usa/cpi_u_rs,
         q4usa_r = q4usa/cpi_u_rs,q5usa_r = q5usa/cpi_u_rs) %>%
  mutate(rapportq1q5 = 100*(q1usa/q5usa), rapportq2q5 = 100*(q2usa/q5usa), rapportq3q5 = 100*(q3usa/q5usa), rapportq4q5 = 100*(q4usa/q5usa))

data1_gra_us <- revenu_us %>% select(year,q1usa_r,q2usa_r,q3usa_r,q4usa_r,q5usa_r) %>%
  filter(year>"2014") %>%
  pivot_longer(cols=-c("year"),
             names_to="revenu",
             values_to = "value")%>%
  mutate(value=as.numeric(value))

gra1_usa <-ggplot(data=data1_gra_us,aes(x=year,y=value,color=revenu)) +
  geom_line()+
#  scale_y_log10() +
  labs(
    subtitle="Revenus médians et par quintile",
    y="En $ (2023)",
    x=NULL,
    caption = "Census Bureau") +
    theme_ofce()+
  scale_y_log10() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
gra1_usa

revenu_ea_us <- full_join(revenu_us,revenu_ea,by="year")
revenu_ea_us <- revenu_ea_us %>% select(year,medianusa_r,medianea_r) %>%
  filter(year>2004,year<2024)

gra_medianus <- ggplot(revenu_ea_us, aes(x=year,y=medianusa_r,color="")) +
  geom_line()+
  ylim(65000,85000)+
  labs(title="Revenu médian",subtitle = "USA",y="$",x=NULL,caption="", color=NULL) +
  theme_ofce()+
  scale_y_log10() +
  scale_color_manual(values=c("black"))+
  theme(legend.position = "")
gra_medianus

#  ylim(96,116)+

gra_medianea <- ggplot(revenu_ea_us, aes(x=year,y=medianea_r,color="")) +
  geom_line()+
  ylim(15000,19000)+
  labs(subtitle="EA",y="€",x=NULL,caption="", color=NULL) +
  theme_ofce()+
  scale_y_log10() +
  scale_color_manual(values=c("blue"))+
  theme(legend.position = "")

gra_combine <- ggarrange(gra_medianus,gra_medianea,
                          labels = c("", ""), nrow = 2)
gra_combine

## essai 2

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

saveRDS(gg, file = "graphiques/income/inc_us_ea.ggplot")

data_gra2_us <- revenu_us[,-c(2:18)] %>%
  filter(year>"1994") %>%
  pivot_longer(cols=-c("year"),
               names_to="revenu",
               values_to = "value")%>%
  mutate(value=as.numeric(value))

gra2_usa <-ggplot(data=data_gra2_us,aes(x=year,y=value,color=revenu)) +
  geom_line()+
  labs(
    subtitle="Revenus en % du Q5",
    y="En %",
    x=NULL,
    caption = "Census Bureau") +
  theme_ofce()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")
gra2_usa

base_ea <- get_eurostat("prc_hicp_midx", time_format = "raw", stringsAsFactors = F, cache = F)

cpi_ea <- base_ea %>%
  filter(geo == "EA19",
         unit == "I15",
         coicop == "CP00") %>%
  rename(date = "TIME_PERIOD")
cpi_ea <- cpi_ea[,-c(1:4)]
cpi_ea$date <- cpi_ea$date %>% lubridate::ym()
cpi_ea$year <- year(cpi_ea$date)

cpiea_annual <- cpi_ea[,-c(1)] %>%
  group_by(year) %>%
  summarise_all(mean) %>%
  rename(cpi_ea = "values") %>%
  mutate(ea = 100*(cpi_ea/lag(cpi_ea,1)-1))

cpi_comp <- full_join(cpi_annual,cpiea_annual,by="year") %>%
  mutate(us = 100*(cpi_usa/lag(cpi_usa,1)-1)) %>%
  filter(year<"2025", year>"1996")

cpi_comp <- cpi_comp[,-c(2:3)] %>%
  pivot_longer(cols=-c("year"),
                names_to="pays",
                values_to = "value")%>%
                 mutate(value=as.numeric(value))

gra_inf <-ggplot(data=cpi_comp,aes(x=year,y=value,color=pays)) +
  geom_line()+
  labs(
    subtitle="Inflation",
    y="En %",
    x=NULL,
    caption = "BLS, Eurostat") +
  theme_ofce()+
  scale_colour_manual(values = c("black","blue"))+
  theme(legend.title = element_blank(),
        legend.position = "bottom")
gra_inf

stats <- cpi_comp %>%
  group_by(pays) %>%
  summarise(mean=mean(value))

stats_2021 <- cpi_comp %>%
  filter(year>"2020") %>%
  group_by(pays) %>%
  summarise(mean=mean(value))
