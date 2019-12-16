source("global.R")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) {
    stop("Please specify one file where the simulation patterns are listed.")
}

patterns_to_aggregate <- readLines(args)

walk(patterns_to_aggregate, ~{
    aggregateSimulations(pattern = .)
})
