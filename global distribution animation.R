library(tidyverse)
library(gganimate)
library(showtext)
showtext_auto()
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
  mutate(year = as.integer(year)) |>
  arrange(year, variable, country3, decile)

ffd <- full_dec |>
  mutate(year = factor(year)) |>
  group_by(country4, year, variable, decile) |>
  summarize(popr = sum(popr)) |>
  group_by(year) |>
  mutate(yl = ifelse(row_number()==1, as.character(year), NA_character_))


gga <- ggplot(ffd) +
  aes(x = decile, y = popr, fill = country4)+
  facet_wrap( vars(variable), labeller = as_labeller(c(adiincj992 = "Après redistribution",aptincj992 = "Avant redistribution")))+
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
  PrettyCols::scale_fill_pretty_d("Summer") +
  #scale_fill_manual(values = c(PrettyCols::prettycols("Teals", 6), "orchid3", "royalblue4")) +
  guides(fill="none") +
  geom_text(aes(x=0, y=.11, label=year))+
  theme_void() +
  theme(
    plot.subtitle = element_text(margin=margin()),
    strip.text = element_text(margin=margin()),
    plot.margin = margin(),
    panel.spacing = unit(3, "pt")) +
  labs(
    caption = "World Inequality Database (WID), code @github.com/xtimbeau/decrochage") +
  transition_states(year) +
  enter_fade() +
  exit_fade() +
  ease_aes('linear')

animate(gga, height = 4, width = 6, device="ragg_png", units = "cm", res = 400, rewind=FALSE, end_pause=3, start_pause=3, fps=5)


full_dec |>
  filter(year%in%c("1980", "2023")) |>
  group_by(variable, country3, year) |>
  summarize(d5 = sum(popr[decile%in%c("d1", "d2")])) |>
  ungroup() |>
  left_join(pops, by=c("year", "country3")) |>
  ggplot() +
  facet_wrap(vars(country3))+
  scale_y_log10()+
  geom_point(aes(x=factor(year), y=d5/spopr*10, color = variable))+
  ofce::theme_ofce()

full_dec |>
  group_by(variable, country3, year) |>
  summarize(d5 = sum(popr[decile%in%c("d9", "d10")])) |>
  ungroup() |>
  left_join(pops, by=c("year", "country3")) |>
  ggplot() +
  facet_wrap(vars(country3))+
  scale_y_log10()+
  geom_line(aes(x=year, y=d5/spopr*10, color = variable))
