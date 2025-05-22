library(tidyverse)
library(gganimate)

full_dec <- ofce::source_data("wid.R")$fdec |>
  mutate(
    country4 = case_when(
      country3 == "US" ~ "US",
      country3%in%c("CH", "GB") ~ "EnEU",
      TRUE ~ "EUZ"))

fnum <- function(x) formatC(x, big.mark =" ", decimal.mark=",", format = "fg", digits=2, width=1)

deciles <- full_dec |>
  group_by(decile, year, country3, variable) |>
  summarise(
    revenu_moy = sum(popr*revenu)/sum(popr),
    seuilp = first(seuilp),
    seuilm = first(seuilm)
  ) |>
  ungroup() |>
  mutate(
    seuilm = ifelse(seuilm<0, 0, seuilm),
    label = str_c("[", fnum(seuilm), "->", fnum(seuilp), "]") ) |>
  select(variable, decile, year, country3, label, revenu_moy)

pops <- full_dec |>
  filter(variable %in% "aptincj992") |>
  group_by(year, country3) |>
  summarize(spopr = sum(popr),
            .groups = "drop") |>
  arrange(desc(country3))

breaks <- pops |>
  filter(year=="2023") |>
  mutate(cpopr = cumsum(spopr)/10) |>
  pull(cpopr, name = country3)

full_dec <- full_dec |>
  left_join(deciles, by=c("country3", "decile", "year", "variable")) |>
  left_join(pops, by = c("country3", "year")) |>
  mutate(year = as.integer(year))

gga <- ggplot(full_dec) +
  aes(x = decile, y = popr, fill = country4)+
  facet_wrap( vars(variable))+
  geom_col(
    alpha=0.7, color = "white", linewidth =0.1) +
  geom_text(aes(label=if_else(decile=="d1", country4, "")),
            position = position_stack(vjust=0.5),
            size = 2.5, col="white") +
  scale_y_continuous(
    breaks = breaks,
    labels = c("4%", "5%", "", "6%", "7%", "8%", "", "9%", "10%")) +
  ylab(NULL) +
  xlab(NULL) +
  # theme_ofce(
  #   marquee=FALSE,
  #   panel.grid.major.x = element_blank(),
  #   panel.grid.major.y = element_line(color = "grey", linewidth = 0.5)  )+
  PrettyCols::scale_fill_pretty_d("Joyful") +
  #scale_fill_manual(values = c(PrettyCols::prettycols("Teals", 6), "orchid3", "royalblue4")) +
  guides(fill="none") +
  theme_void() +
  labs(
    caption = "World Inequality Database (WID), code @github.com/xtimbeau/decrochage",
    title = 'Year: {frame_time}') +
  transition_states(year, state_length = 2, wrap = FALSE) +
  enter_fade() +
  exit_fade() +
  ease_aes('linear')

animate(gga, height = 4, width = 4, units = "cm", res = 400)
