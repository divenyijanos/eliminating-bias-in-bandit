source("global.R")


calculateRequiredSampleSize <- function(sd, power = .8, significance = .05) {
    round(4 * (qnorm(power) + qnorm(1 - significance))^2 * sd^2)
}

sapply(SDS, calculateRequiredSampleSize) %>% setNames(SDS)


simulateETCWithBatchSize <- function(sd, batch_size, te = 1, n = 10000) {
    params <- list(te = te, n = n, sd = sd, batch_size = batch_size, limit = 0)
    mean_estimates_by_batch <- readInSimulationFiles(params = params)
    if (nrow(mean_estimates_by_batch) > 0) {
        mean_estimates_by_batch[batch == 1, minY := min(mean_Y), run] %>%
            .[mean_Y == minY, loserTreatment := assignment] %>%
            .[, loserTreatment := max(loserTreatment, na.rm = TRUE), run] %>%
            .[batch > 1 & assignment == loserTreatment, mean_Y := mean_Y + (-1)^loserTreatment] %>%
            .[batch > 1, `:=`(assignment = as.integer(1 - loserTreatment), batch = 2)] %>%
            .[, .(mean_Y = weighted.mean(mean_Y, w = size), size = sum(size)), .(n, sd, batch_size, run, batch, assignment)]
    } else {
        message("simulation file does not exist")
        data.table()
    }
}

n <- 10000
sd_values <- SDS

walk(sd_values, ~{
    sd <- .x
    etc_results <- map_df(BATCH_SIZES, ~{
        simulateETCWithBatchSize(sd = sd, batch_size = .x, n = n)
    })

    etc_results[, .(mean_estimate = weighted.mean(mean_Y, w = size)), .(n, sd, batch_size, run, assignment)] %>%
        calculateTE() %>%
        summarizeTE(by = c("n", "sd", "batch_size")) %>%
        fwrite(glue("{INTERIM_RESULT_FOLDER}/etc-n{n}-sd{sd}-TEBySetups.csv"))
    calculateWelfare(etc_results) %>%
        summarizeWelfare(by = c("n", "sd", "batch_size")) %>%
        fwrite(glue("{INTERIM_RESULT_FOLDER}/etc-n{n}-sd{sd}-WelfareBySetups.csv"))
})
