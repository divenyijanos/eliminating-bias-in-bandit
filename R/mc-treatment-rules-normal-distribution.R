#!/usr/bin/env Rscript

source("global.R")

te <- 1
n_treatment <- 1
n <- 10000

get_n_sim <- function(sd) {
    if (sd < 10) {
        10000
    } else if (sd < 20) {
        20000
    } else {
        50000
    }
}
n_sim_split <- 1000

params <- CJ(te, sd = SDS, batch_size = BATCH_SIZES, limit = LIMITS) %>%
    .[batch_size * limit == 0 | batch_size * limit >= 1] %>%
    .[!(batch_size == n & limit > 0)]

pwalk(params, ~with(list(...), {
    cat(glue("Simulating {n_treatment} treatment with te={te}, sd={sd}, batch_size={batch_size}, limit={limit}... "))
    filename <- generateNamePatternFromParams(
        list(te = te, n = n, sd = sd, batch_size = batch_size, limit = limit),
        regex = FALSE
    )
    if (file.exists(glue("{SIMULATION_FOLDER}/{filename}full.csv.gz"))) {
        cat("skipped.\n")
    } else {
        n_sim <- get_n_sim(sd)
        participants <- simulateParticipants(n = n, te = te, sd = sd)
        n_split <- ceiling(n_sim / n_sim_split)
        walk(seq(n_split), ~{
            cat("\n", glue("---Simulating split {.x}/{n_split}... "), sep = "")
            if (file.exists(glue("{SIMULATION_FOLDER}/{filename}{.x}.csv.gz"))) {
                cat("skipped.")
            } else {
                batch_results <- runSimulations(
                    run_from = (.x - 1) * n_sim_split + 1, run_to = .x * n_sim_split,
                    participants, batch_size, sd, limit, n_treatment
                )
                cat("done.\n", glue("---Checking number of runs... "), sep = "")
                if (batch_results[, uniqueN(run)] == n_sim_split) {
                    cat("correct. Writing into file... ")
                    fwrite(batch_results, glue("{SIMULATION_FOLDER}/{filename}{.x}.csv"))
                    cat("done. Zipping... ")
                    system(glue("gzip {SIMULATION_FOLDER}/{filename}{.x}.csv"))
                    cat("done.")
                } else {
                    cat("missing runs :(")
                }
            }
        })
        cat("\n---Aggregating splitted simulations... ")
        aggregateSimulations(pattern = filename)
        system(glue("mv {SIMULATION_FOLDER}/{filename}[0-9]*.csv.gz {SIMULATION_FOLDER}/to-remove/"))
	cat("done.\n")
    }
}))
