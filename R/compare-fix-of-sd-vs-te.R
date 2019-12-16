source("global.R")

batch_results <- readInSimulationFiles(pattern = "te0.05-sd1-|te1-sd20-")

# identical setups
calculateWelfare(batch_results, by = c("te", "sd", "batch_size")) %>%
    .[, share_to_ideal := welfare / (10000 * te)] %>%
    summarizeMeasure("share_to_ideal", is_te = FALSE, by = c("te", "sd", "batch_size")) %>%
    .[order(batch_size)]
