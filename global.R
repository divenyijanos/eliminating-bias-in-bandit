library(data.table)
library(magrittr)
library(glue)
library(stringr)
library(purrr)
library(parallel)
library(moments)
library(ggplot2)
library(ggrepel)
library(xtable)
library(knitr)
library(kableExtra)

invisible(sapply(list.files("R", full.names = TRUE), source))

options(scipen = 99)
options(datatable.CJ.names = TRUE)
options("mc.cores" = 3)
options(knitr.kable.NA = '')

SDS <- c(1, 2, 5, 10, 15, 20, 25, 30)
BATCH_SIZES <- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)
LIMITS <- c(0.005, 0.01, 0.02, 0.05, 0.1, 0.15, 0.2, 0)

SIMULATION_FOLDER <- "data/simulations"
INTERIM_RESULT_FOLDER <- "data/interim_results"

MAIN_COLOR <- "#042037"
SECONDARY_COLOR <- "#D4A86A"
TWO_COLOR_SCHEME <- c(MAIN_COLOR, SECONDARY_COLOR)
DARK_SECONDARY_COLOR <- "#805315"
THIRD_COLOR <- "#3F88C5"
GRADIENT_LIMITS <- c("#DBB785", "#031523")

mytheme <- function (base_size = 11, base_family = "", base_line_size = base_size/22,
    base_rect_size = base_size/22) {
    theme_minimal(base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
        theme(
            panel.border = element_rect(fill = NA, colour = "grey20"),
            panel.grid = element_line(colour = "grey92"),
            panel.grid.minor = element_line(size = rel(0.5))
        )
}
theme_set(mytheme())
