#!/usr/bin/env Rscript

source("global.R")

jtpa <- fread('data/jtpa_sample.csv') %>%
    .[, Y := earnings_30m - (treated == "trainee") * 774]
itt <- jtpa[assignment == "treatment", mean(Y)] - jtpa[assignment == "control", mean(Y)]
sd <- jtpa[, sd(Y)]

jtpa_batch_sizes <- c(BATCH_SIZES[-length(BATCH_SIZES)], nrow(jtpa))

# participants <- jtpa[, .(Y0 = Y - (assignment == "treatment") * itt, Y1 = Y + (assignment == "control") * itt)]
participants <- jtpa[, .(assignment = ifelse(assignment == "treatment", 1, 0), Y)]

n_sim <- 10000
n_sim_split <- 1000

params <- CJ(itt, sd = sd, batch_size = jtpa_batch_sizes, limit = c(0, 0.05)) %>%
    .[batch_size * limit == 0 | batch_size * limit >= 1] %>%
    .[!(batch_size == nrow(jtpa) & limit > 0)]

pwalk(params, ~with(list(...), {
    cat(glue("Simulating JTPA with batch_size={batch_size}, limit={limit}... "))
    filename <- generateNamePatternFromParams(
        list(batch_size = batch_size, limit = limit),
        regex = FALSE
    )
    if (file.exists(glue("{SIMULATION_FOLDER}/jtpa/{filename}full.csv.gz"))) {
        cat("skipped.\n")
    } else {
        n_split <- ceiling(n_sim / n_sim_split)
        walk(seq(n_split), ~{
            cat("\n", glue("---Simulating split {.x}/{n_split}... "), sep = "")
            if (file.exists(glue("{SIMULATION_FOLDER}/jtpa/{filename}{.x}.csv.gz"))) {
                cat("skipped.")
            } else {
                batch_results <- runSimulations(
                    run_from = (.x - 1) * n_sim_split + 1, run_to = .x * n_sim_split,
                    participants, batch_size, sd, limit, distribution = "empirical"
                )
                cat("done.\n", glue("---Checking number of runs... "), sep = "")
                if (batch_results[, uniqueN(run)] == n_sim_split) {
                    cat("correct. Writing into file... ")
                    fwrite(batch_results, glue("{SIMULATION_FOLDER}/jtpa/{filename}{.x}.csv"))
                    cat("done. Zipping... ")
                    system(glue("gzip {SIMULATION_FOLDER}/jtpa/{filename}{.x}.csv"))
                    cat("done.")
                } else {
                    cat("missing runs :(")
                }
            }
        })
        cat("\n---Aggregating splitted simulations... ")
        aggregateSimulations(pattern = filename, distribution = "jtpa")
        system(glue("rm {SIMULATION_FOLDER}/jtpa/{filename}[0-9]*.csv.gz"))
	cat("done.\n")
    }
}))
