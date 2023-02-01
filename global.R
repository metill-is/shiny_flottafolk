library(dplyr)
library(shiny)
library(tidyr)
library(metill)
library(bslib)
library(thematic)
library(shinycssloaders)
library(arrow)
library(here)
library(purrr)
library(plotly)
library(thematic)

theme_set(theme_metill())
thematic_on()
shinyOptions(plot.autocolor = TRUE)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")
code_files <- here("R") |> list.files()
code_files <- here("R", code_files)
for (file in code_files) source(file)


# Data --------------------------------------------------------------------

d <- here("Data/Data.parquet") |> 
    read_parquet()


# Theming -----------------------------------------------------------------

bs_global_theme(
    bootswatch = "flatly"
)

bs_global_add_variables(
    primary = "#484D6D",
    secondary = "#969696",
    success = "#969696",
    # danger = "#FF8CC6",
    # info = "#FF8CC6",
    light = "#faf9f9",
    dark = "#484D6D",
    bg = "#faf9f9",
    fg = "#737373",
    "body-bg" = "#faf9f9",
    base_font = "Lato",
    heading_font = "Segoe UI",
    "navbar-brand-font-family" = "Playfair Display",
    code_font = "SFMono-Regular"
)


# Sidebar Info ------------------------------------------------------------

sidebar_info <- paste0(
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<a href='https://github.com/metill-is/shiny_flottafolk' target='_top'> Kóði og gögn </a><br>")
)
