#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 1 & args[1] == "redo") {
    message("Recalculating each summary...")
    redo <- TRUE
} else {
    message("Calculating summaries that do not exist yet...")
    redo <- FALSE
}

Sys.sleep(1)

source("global.R")

# walk(DISTRIBUTIONS, ~{
    params <- CJ(te = 1, n = 2000, sd = 10, batch_size = BATCH_SIZES)
    mclapply(seq(nrow(params)), function(run) {
        try(generateInterimResults(
            as.list(params[run]),
            c(
                "calculateTEBySetups",
                "calculateMeansBySetups",
                "calculateTreatedShareBySetups",
                "calculateWelfareBySetups",
                "calculateWelfareAccumulationBySetups"
            ),
            distribution = "normal",
            redo
        ))
    })
# })
