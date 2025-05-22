library(knitr)
opts_chunk$set(
  fig.pos="H",
  out.extra="",
  dev="ragg_png",
  dev.args = list(bg = "transparent"),
  out.width="100%",
  fig.showtext=TRUE,
  message = qmd_message,
  warning = qmd_warning,
  echo = qmd_echo)

library(tidyverse, quietly = TRUE)
library(ofce, quietly = TRUE)
library(showtext, quietly = TRUE)
library(gt, quietly = TRUE)
library(readxl, quietly = TRUE)
library(ggiraph, quietly = TRUE)
library(curl, quietly = TRUE)
library(ggrepel, quietly = TRUE)
library(gt, quietly = TRUE)
library(scales, quietly = TRUE)
library(glue, quietly = TRUE)
library(patchwork, quietly = TRUE)
library(downloadthis, quietly = TRUE)
library(lubridate, quietly = TRUE)
library(insee, quietly = TRUE)
library(ggh4x, quietly = TRUE)
library(PrettyCols, quietly = TRUE)
library(cli, quietly = TRUE)
library(quarto, quietly = TRUE)
library(qs, quietly = TRUE)
library(devtools, quietly = TRUE)
library(conflicted, quietly = TRUE)
library(marquee, quietly = TRUE)
library(readxl, quietly = TRUE)
library(tictoc)

options(
  ofce.marquee = TRUE,
  ofce.base_size = 12,
  ofce.background_color = "transparent",
  ofce.caption.ofce = FALSE,
  ofce.marquee = TRUE,
  ofce.caption.srcplus = "pr\u00e9vision OFCE avril 2025",
  ofce.caption.ofce = TRUE,
  ofce.caption.wrap = 0,
  sourcoise.src_in = "file",
  sourcoise.init_fn = ofce::init_qmd)

margin_download <- function(data, output_name = "donnees", label = "donn\u00e9es") {
  if(knitr::is_html_output()) {
    if(lobstr::obj_size(data)> 1e+5)
      cli::cli_alert("la taille de l'objet est sup\u00e9rieure à 100kB")
    fn <- str_c("ofce-prev2503-", tolower(output_name))
    downloadthis::download_this(
      data,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label,
      output_name = fn)
  } else
    return(invisible(NULL))
}

prev_download <- function(data, output_name = "donnees", label = "donn\u00e9es", margin = TRUE) {

  if(knitr::is_html_output()) {
    if(lobstr::obj_size(data)> 1e+5)
      cli::cli_alert("la taille de l'objet est sup\u00e9rieure à 100kB")
    fn <- str_c("ofce-prev2503-", tolower(output_name))

    dwn <- downloadthis::download_this(
      data,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label,
      output_name = fn)

    cat(str_c("::: {.column-margin} \n" ))
    dwn |> htmltools::tagList() |> print()
    cat("\n")
    cat(":::\n")

  } else
    return(invisible(NULL))
}

country_lbl <- function(x, format=NULL) {
  if(is.null(format))
    if(is.null(dim(x)))
      fmt <- ifelse(max(stringr::str_length(x))==2, "eurostat", "iso3c")
  else
    fmt <- ifelse(max(stringr::str_length(x[,1]))==2, "eurostat", "iso3c")
  else
    fmt <- format
  if(is.null(dim(x)))
    return(countrycode::countrycode(x, fmt, "country.name.fr"))
  x |>
    mutate(across(1, ~countrycode::countrycode(.x, fmt, "country.name.fr")))
}

margin_link <- function(data, output_name = "donnees", label = "données") {
  if(knitr::is_html_output()) {
    link <- stringr::str_c("dnwld/", output_name, ".csv")
    vroom::vroom_write(data, link, delim = ";")
    downloadthis::download_link(
      link,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label)
  } else
    return(invisible(NULL))
}

showtext_opts(dpi = 120)
showtext_auto()
options(cli.ignore_unknown_rstudio_theme = TRUE)
tooltip_css  <-
  "font-family:Open Sans;
  background-color:snow;
  border-radius:5px;
  border-color:gray;
  border-style:solid;
  border-width:0.5px;
  font-size:9pt;
  padding:4px;
  box-shadow: 2px 2px 2px gray;
  r:20px;"

gdtools::register_gfont("Open Sans")

girafe_opts <- function(x, ...) girafe_options(
  x,
  opts_hover(css = "stroke-width:1px;", nearest_distance = 60),
  opts_tooltip(css = tooltip_css, delay_mouseover = 10, delay_mouseout = 3000)) |>
  girafe_options(...)

girafy <- function(plot, r=2.5, o = 0.5, out=TRUE,  ...) {
  if(out)
    graph2prev(plot)
  if(knitr::is_html_output()| interactive()) {
    return(
      girafe(ggobj = plot) |>
        girafe_options(
          opts_hover_inv(css = glue("opacity:{o};")),
          opts_hover(css = glue("r:{r}px;"), nearest_distance = 10),
          opts_tooltip(css = tooltip_css)) |>
        girafe_options(...)
    )
  }

  if(knitr::is_latex_output()) {
    if("patchwork" %in% class(plot))
      return( plot & theme_ofce(base_size = 8))
    return( plot + theme_ofce(base_size = 8))
  }

  plot
}

milliards <- function(x, n_signif = 3L) {
  stringr::str_c(
    format(
      x,
      digits = n_signif,
      big.mark = " ",
      decimal.mark = ","),
    " milliards d'euros")
}

if(.Platform$OS.type=="windows")
  Sys.setlocale(locale = "fr_FR.utf8") else
    Sys.setlocale(locale = "fr_FR")
locale <- Sys.getlocale()
ccsummer <- function(n=4) PrettyCols::prettycols("Summer", n=n)
ccjoy <- function(n=4) PrettyCols::prettycols("Joyful", n=n)

bluish <- ccjoy()[1]
redish <- ccjoy()[2]
yelish <- ccsummer()[2]
greenish <- ccsummer()[4]
darkgreenish <- ccsummer()[3]
darkbluish <- ccjoy()[4]

pays_long <- c(FRA = "France", EUZ = "Zone euro", DEU = "Allemagne", ESP = "Espagne", GBR = "Royaume-Uni",
               USA = "Etats-Unis d'Am\u00e9rique",
               BRA = "Br\u00e9sil", CHI = "Chine",
               PECO = "Pays d'Europe centrale et orientale", NLD = "Pays-Bas", CHE = "Suisse",
               NOR = "Norv\u00e8ge", GRC = "Gr\u00e8ce", SWE  = "Su\u00e8de", ITA = "Italie", AUT = "Autriche",
               FIN = "Finlande", AUS = "Australie",
               BEL  = "Belgique", DEN = "Danemark", PRT = "Portugal",
               CAN ="Canada", MEX = "Mexique", IND = "Inde", JPN= "Japon",
               RUS = "Russie", ZAF = "Afrique du Sud", CHN = "Chine")

pays_long2 <- c(FRA = "France", EUZ = "Zone euro", DEU = "Allemagne", ESP = "Espagne", GBR = "Royaume-Uni",
                USA = "Etats-Unis",
                BRA = "Br\u00e9sil", CHI = "Chine",
                PECO = "Pays d'Europe centrale et orientale", NLD = "Pays-Bas", CHE = "Suisse",
                NOR = "Norv\u00e8ge", GRC = "Gr\u00e8ce", SWE  = "Su\u00e8de", ITA = "Italie", AUT = "Autriche",
                FIN = "Finlande", AUS = "Australie",
                BEL  = "Belgique", DEN = "Danemark", PRT = "Portugal",
                CAN ="Canada", MEX = "Mexique", IND = "Inde", JPN= "Japon",
                RUS = "Russie", ZAF = "Afrique du Sud", CHN = "Chine")


## pour les tableaux

tableau_labels <- c("En %", "T1", "T2", "T3", "T4", "T1", "T2", "T3", "T4", "", "", "")
tableau.font.size <- 12
prev_tab_options <- function(data, ...) {
  tab_options(data,
              footnotes.font.size = "90%",
              source_notes.font.size = "100%",
              quarto.disable_processing= TRUE,
              table.font.size = tableau.font.size,
              table_body.hlines.style = "none",
              column_labels.padding = 3,
              data_row.padding = 2,
              footnotes.multiline = FALSE,
              footnotes.padding = 5,
              source_notes.padding =  2,
              table.border.bottom.style = "none",
              row_group.padding = 2) |>
    opt_footnote_marks("letters") |>
    tab_options(...)
}

prev_cols_fill <- function(data, columns) {
  tab_style(data, style = cell_fill(color = prev_color),
            locations = cells_body(columns = {{columns}}))
}

prev_row_bold <- function(data, row) {
  tab_style(data, style = cell_text(weight = "bold"),  locations = cells_body(rows = {{ row }}))
}

prev_spanners_bold <- function(data) {
  tab_style(data, style = cell_text(weight = "bold"),  locations = gt::cells_column_spanners())
}

prev_row_italic <- function(data, row) {
  tab_style(data, style = cell_text(style = "italic"),  locations = cells_body(rows = {{row}}))
}

prev_align_decimals <- function(data, columns = NULL) {
  if(is.null(columns))
    cols_align_decimal(data, columns = where(is.numeric), dec_mark = ",")
  else
    cols_align_decimal(data, columns = {{columns}}, dec_mark = ",")
}

prev_fmt_decimal <- function(data, columns = NULL, rows = everything(), decimals = 1) {
  if(is.null(columns))
    fmt_number(data, columns = where(is.numeric), rows = {{rows}}, dec_mark = ",", decimals = decimals, sep_mark = " ")
  else
    fmt_number(data, columns = {{columns}}, rows = {{rows}}, dec_mark = ",", decimals = decimals, sep_mark = " ")
}

prev_background_enc <- function(data, color = enc_color) {
  tab_style(
    data = data,
    style = cell_fill(enc_color),
    locations = list(
      cells_body(),
      cells_column_labels(),
      cells_column_spanners(),
      cells_footnotes(),
      cells_row_groups(),
      cells_source_notes()
    ))
}

out_graphes <- if(Sys.getenv("OUTGRAPHS") == "TRUE") TRUE else FALSE

fmt_val <- function(x, digits=1) {
  formatC(x=x, digits = digits, big.mark = " ", decimal.mark = ",", format = "f")
}

date_trim <- function(date) {
  str_c("T", lubridate::quarter(date), " ", lubridate::year(date))
}

date_mois <- function(date) {
  str_c(lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

date_jour <- function(date) {
  str_c(lubridate::day(date), " ", lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

date_prev_start <- "2025-01-01"
date_prev_end <- "2026-12-31"
prev_color <- colorspace::lighten("#ED1524", 0.9)
prev_color_a5 <- colorspace::lighten("#ED1524", 0.8)
prev_color_f5 <- colorspace::lighten("#ED1524", 0.3)
enc_color <- "#F8F8F8"

annotate_prevision <- function(posy = 1, xstart = date_prev_start, xend = date_prev_end, ymin = -Inf, ymax = Inf, size = 3) {
  midx <- (as.Date(xend) - as.Date(xstart))/2 + as.Date(xstart)
  list(
    ggplot2::annotate(
      "rect",
      xmin = as.Date(xstart), xmax = as.Date(xend),
      ymin = ymin, ymax = ymax, alpha = 0.5, fill = prev_color_a5),
    ggplot2::annotate(
      "text",
      x=midx, y = posy,
      label="Pr\u00e9visions", size = size,
      color="grey20", hjust = 0.5))
}

annotate_prevision_year <-function(posy = 1, xstart = year(date_prev_start), xend = year(date_prev_end), ymin = -Inf, ymax = Inf, size = 3) {
  list(
    ggplot2::annotate(
      "rect",
      xmin = xstart, xmax = xend,
      ymin = ymin, ymax = ymax, alpha = 0.5, fill = prev_color_a5),
    ggplot2::annotate(
      "text",
      x=.5*(xstart+xend), y = posy,
      label="Pr\u00e9visions", size= size,
      color="grey20", hjust = 0.5))
}

if(Sys.getenv("QUARTO_PROJECT_DIR") == "") {
  safe_find_root <- purrr::safely(rprojroot::find_root)
  root <- safe_find_root(rprojroot::is_quarto_project | rprojroot::is_r_package | rprojroot::is_rstudio_project)
  if(is.null(root$error))
    ofce.project.root <- root$result
} else {
  ofce.project.root <- Sys.getenv("QUARTO_PROJECT_DIR")
}

graph2prev <- function(graph, label=NULL, chunk = knitr::opts_current$get(), document=knitr::current_input()) {

  if(Sys.getenv("OUTGRAPHS") != "TRUE")
    return(graph)

  if(!knitr::is_html_output())
    return(graph)
  # if(rlang::is_interactive())
  #   return(graph)

  if(is.null(document)|document=="")
    return(graph)

  ratio <- chunk$fig.width/chunk$fig.height
  document <- document |> str_remove("\\..+")
  label <- chunk$label

  if(is.null(label)|label=="")
    return(graph)

  if(Sys.getenv("QUARTO_DOCUMENT_PATH")=="")
    return(graph)

  partie <- Sys.getenv("QUARTO_DOCUMENT_PATH") |>
    fs::path_rel(Sys.getenv("QUARTO_PROJECT_DIR")) |>
    as.character() |>
    str_replace("/", "-")

  if(!(partie  |> str_detect("^france|^fiches|^inter")))
    return(graph)

  partie <- partie |>
    str_c("-", document)

  rep <- fs::path_join(c(ofce.project.root, "_sav_graphes"))

  dir.create(rep, recursive = TRUE)
  fn <- stringr::str_c(rep, "/", partie, "-", tolower(label))

  saveRDS(object = graph, file = str_c(fn, ".ggplot"))

  return(graph)
}

load_graphe <- function(graphe) {
  readRDS(fs::path_join(c(ofce.project.root,"_sav_graphes", graphe)) |> fs::path_ext_set("ggplot"))
}

tbl2prev <- function(tbl, label=NULL, chunk = knitr::opts_current$get(), document=knitr::current_input()) {

  if(Sys.getenv("OUTGRAPHS") != "TRUE")
    return(tbl)

  if(!knitr::is_html_output())
    return(tbl)
  # if(rlang::is_interactive())
  #   return(tbl)

  if(is.null(document)|document=="")
    return(tbl)

  document <- document |> str_remove("\\..+")
  label <- chunk$label

  if(is.null(label)|label=="")
    return(tbl)

  if(Sys.getenv("QUARTO_DOCUMENT_PATH")=="")
    return(tbl)

  partie <- Sys.getenv("QUARTO_DOCUMENT_PATH") |>
    fs::path_rel(Sys.getenv("QUARTO_PROJECT_DIR")) |>
    as.character() |>
    str_replace("/", "-")

  if(!(partie  |> str_detect("^france|^fiches|^inter")))
    return(tbl)

  partie <- partie |>
    str_c("-", document)

  rep <- fs::path_join(c(ofce.project.root, "_sav_graphes"))

  dir.create(rep, recursive = TRUE)
  fn <- stringr::str_c(rep, "/", partie, "-", tolower(label))

  saveRDS(object = tbl, file = str_c(fn, ".gt"))

  cli::cli_inform("tbl {label} -> sav")

  return(tbl)
}

load_tbl <- function(tbl) {
  readRDS(fs::path_join(c(ofce.project.root,"_sav_graphes", tbl)) |> fs::path_ext_set("gt"))
}

girafyJS <- function(plot){
  plot
}

conflicted::conflicts_prefer(dplyr::filter, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::select, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::lag, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::year, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::month, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::first, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::last, .quiet = TRUE)
conflicted::conflicts_prefer(dplyr::between, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::quarter, .quiet = TRUE)
conflicted::conflicts_prefer(rlang::set_names)
trk_dp <- str_c("fiches/data_pays/", c("USA", "FRA", "GBR", "JPN", "ITA", "ESP", "EUZ", "DEU"), ".xlsx")

payswizdata <- set_names(c("USA", "FRA", "GBR", "JPN", "ITA", "ESP", "EUZ", "DEU"))
data_pays_rep <- fs::path_join(c(ofce.project.root, "fiches/data_pays"))

data_pays <- function(pays=payswizdata) {
  data <- map(pays, ~{
    fn <- fs::path_join(c(data_pays_rep, .x)) |> fs::path_ext_set("xlsx")
    trim <- readxl::read_xlsx(fn, sheet="trim") |>
      select(-ends_with("_o"), -any_of(c("C1", "C2"))) |>
      mutate(across(-date, ~ifelse(is.na(.x)|.x=="NA", NA_real_, .x)),
             date = as.Date(date),
             pays = .x)
    an <- readxl::read_xlsx(fn, sheet="an") |>
      select(-ends_with("_o")) |>
      mutate(across(-date, ~ifelse(is.na(.x)|.x=="NA", NA_real_, .x)),
             date = as.Date(date),
             pays = .x)
    list(trim = trim, an = an)
  })
  data <- purrr::transpose(data)
  data$trim <- bind_rows(data$trim)
  data$an <- bind_rows(data$an)

  return(data)
}

pathify <- function(path, root = ofce.project.root) {
  if(stringr::str_detect(path, "^/"))
    return(fs::path_join(c(root, path)))
  return(path)
}

tabsetize <- function(list, facety = TRUE, cap = TRUE, girafy = TRUE, asp = NULL, r = 1.5) {
  if(knitr::is_html_output()&!interactive()) {
    chunk <- knitr::opts_current$get()
    label <- knitr::opts_current$get()$label
    if(cap) {
      if(is.null(label))
        return(list)
      cat(str_c(":::: {#", label, "} \n\n" ))
    }
    ids <- 1:length(list) |> set_names(names(list))
    cat("::: {.panel-tabset} \n\n")
    purrr::iwalk(list, ~{
      cat(paste0("### ", .y," {.tabset} \n\n"))

      if(is(.x, "ggplot")) {
        id <- str_c(digest::digest(.x, algo = "crc32"), "-", ids[[.y]])
        if(!is.null(asp))
          asp_txt <- glue(", fig.asp={asp}")
        else
          asp_txt <- ""
        lbl <- glue("'{id}'")
        if(girafy) {
          plot <- girafy(.x, r=r)
          lib <- "library(ggiraph)\n"
        }
        else {
          plot <- .x
          lib <- ""}
        rendu <- knitr::knit(
          text = str_c("```{r ", lbl, asp_txt," }\n", lib, "plot \n```"),
          quiet=TRUE)
        cat(rendu, sep="\n")
      }

      if(is(.x, "character")) {
        cat("![](", .x, "){fig-align='center'}")
      }

      cat("\n\n")
    })
    cat(":::\n\n")
    if(cap) {
      cat(chunk$fig.cap)
      cat("\n\n")
      cat("::::\n\n")
    }
  } else {

    ids <- 1:length(list) |> set_names(names(list))
    label <- knitr::opts_current$get()$label

    purrr::iwalk(list, ~{
      id <- ids[[.y]]
      if(!is.null(asp))
        asp_txt <- glue(", fig.asp={asp}")
      else
        asp_txt <- ""
      lbl <- glue("'{label} {id}'")
      if(is(.x, "ggplot")) {
        plot <- .x
        rendu <- knitr::knit(
          text = str_c("```{r ", lbl, asp_txt," }\nplot \n```"),
          quiet=TRUE)
      }
      cat(rendu, sep="\n")
    })
  }
}

tabsetize2 <- function(list, facety = TRUE, cap = TRUE, girafy = FALSE, asp=NULL, r=1.5) {

  if(knitr::is_html_output()) {
    chunk <- knitr::opts_current$get()
    label <- knitr::opts_current$get()$label

    if(cap) {
      if(is.null(label))
        return(list)
      cat(str_c("::::: {#", label, "} \n\n" ))
    }

    cat(":::: {.panel-tabset} \n\n")
    purrr::iwalk(list, ~{
      cat(paste0("### ", .y," {.tabset} \n\n"))
      tabsetize(.x, facety=FALSE, cap = FALSE, girafy = girafy, asp = asp, r = r)
      cat("\n\n")
    })
    cat("::::\n\n")

    if(cap) {
      cat(chunk$fig.cap)
      cat("\n\n")
      cat(":::::\n\n")
    }
  } else {
    purrr::iwalk(list, ~{
      tabsetize(.x, facety=FALSE, cap = FALSE, girafy = girafy, asp = asp, r = r)
      cat("\n\n")
    })
  }
}

download_margin <- function(data, output_name = "donnees", label = "donn\u00e9es", link = FALSE) {

  if(knitr::is_html_output()) {

    fn <- tolower(output_name)

    if(link) {
      if(!fs::dir_exists("dnwld"))
        dir.create("dnwld")
      link <- stringr::str_c("dnwld/", output_name, ".csv.gz")
      vroom::vroom_write(data, link, delim = ";")

      dwn <- downloadthis::download_link(
        link,
        icon = "fa fa-download",
        class = "dbtn",
        button_type = "default",
        has_icon = TRUE,
        button_label  = label)
    }

    if(!link) {
      if(lobstr::obj_size(data)> 1e+5)
        cli::cli_alert("la taille de l'objet est sup\u00e9rieure à 100kB")
      dwn <- downloadthis::download_this(
        data,
        icon = "fa fa-download",
        class = "dbtn",
        button_label  = label,
        output_name = fn)
    }

    rendu <- knitr::knit(
      text=str_c("```{r ", round(runif(1)*100000) ,", include=FALSE}\nfontawesome::fa_html_dependency()\n```"),
      quiet=TRUE)
    cat(rendu)
    cat("\n\n")
    cat(str_c("::: {.column-margin} \n" ))
    dwn |> htmltools::tagList() |> print()
    cat("\n")
    cat(":::\n")

  } else
    return(invisible(NULL))
}

tbl2word <- function(tbl, chunk = knitr::opts_current$get(), document=knitr::current_input()) {
  if(knitr::is_html_output())
    return(tbl)
  if(rlang::is_interactive())
    return(tbl)

  rep <- Sys.getenv("QUARTO_DOCUMENT_PATH")
  if(rep=="")
    return(tbl)
  label <- chunk$label
  if(is.null(label)|label=="")
    return(tbl)
  if(is.null(document)|document=="")
    return(tbl)
  document <- document |> str_remove("\\..+")
  svg_rep <- fs::path_join(c(rep, "/img"))
  dir.create(svg_rep)
  fn <- stringr::str_c(svg_rep, "/", tolower(label), ".png")
  gt::gtsave(tbl, filename = fn, zoom = 4)
  knitr::include_graphics(fn)
}

grph2word <- function(grph, chunk = knitr::opts_current$get(), document=knitr::current_input()) {
  if(knitr::is_html_output())
    return(grph)
  if(rlang::is_interactive())
    return(grph)

  rep <- Sys.getenv("QUARTO_DOCUMENT_PATH")
  if(rep=="")
    return(grph)
  label <- chunk$label
  ratio <- chunk$fig.width/chunk$fig.height

  if(is.null(label)|label=="")
    return(grph)
  if(is.null(document)|document=="")
    return(grph)
  document <- document |> str_remove("\\..+")
  svg_rep <- fs::path_join(c(rep, "/img"))
  dir.create(svg_rep)
  fn <- stringr::str_c(svg_rep, "/", tolower(label))
  graph2svg(grph, file = fn, rep = "", ratio = ratio)
  knitr::include_graphics(fn |> fs::path_ext_set("svg"))
}

cols_hide_pdf <- function(tbl, col) {
  if(knitr::is_latex_output())
    return(gt::cols_hide(data = tbl, columns = {{ col }} ))
  return(tbl)
}

prev <- source_data("fiches/data_pays/data_vars.R")

`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!is.ggplot(plot)) {
    stop('Need a plot on the left side')
  }
  plot$layers = c(layer, plot$layers)
  plot
}
