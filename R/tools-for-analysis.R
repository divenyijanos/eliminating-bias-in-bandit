summarizeMeasure <- function(dt, variable,
                             by = c("n", "sd", "batch_size", "limit"),
                             additional_aggregations = NULL) {
    x <- enexpr(variable)
    dt[, c(n_run = .N, standardAggregations(eval(x)), eval(additional_aggregations)), by]
}

standardAggregations <- function(x) {
    list(
        mean = mean(x),
        min = min(x),
        q5 = quantile(x, 0.05),
        q25 = quantile(x, 0.25),
        median = median(x),
        q75 = quantile(x, 0.75),
        q95 = quantile(x, 0.95),
        max = max(x),
        stdev = sd(x),
        skewness = skewness(x),
        kurtosis = kurtosis(x)
    )
}

summarizeTE <- function(dt, variable = TE, te = 1, by = c("n", "sd", "batch_size", "limit")) {
    x <- enexpr(variable)
    additional_aggr <- expr(list(
        sign_pct = mean(!!x < 0) * 100,
        mse = mean((!!x - !!te)^2)
    ))
    summarizeMeasure(dt, !!x, by, additional_aggr)
}

summarizeWelfare <- function(dt, variable = welfare,
                             by = c("n", "sd", "batch_size", "limit")) {
    x <- enexpr(variable)
    additional_aggr <- expr(list(
        inferior_pct = mean(round(!!x) < (size / 2)) * 100,
        stuck_pct = mean(round(!!x) < (size / 100)) * 100,
        best_possible_pct = mean(round(!!x) == (size - batch_size / 2)) * 100
    ))
    summarizeMeasure(dt, !!x, by, additional_aggr)
}

calculateTE <- function(mean_estimates, mean_variable = "mean_estimate",
                        te = 1, by = c("n", "sd", "batch_size", "limit")) {
    id_vars <- c("run", intersect(names(mean_estimates), by))
    formula <- paste(glue_collapse(id_vars, "+"), "assignment", sep = "~")
    mean_estimates %>%
        dcast(formula, value.var = mean_variable) %>%
        .[, TE := `1` - `0`] %>%
        .[, bias_in_TE := TE - te] %>%
        .[]
}

calculateWelfare <- function(mean_estimates, by = c("n", "sd", "batch_size", "limit")) {
    mean_estimates[,
        .(welfare = sum(size * mean_Y), size = sum(size)),
        c("run", intersect(names(mean_estimates), by))
    ]
}

calculateTEBySetups <- function(mean_estimates_by_batch) {
    te_by_limits <- mean_estimates_by_batch[,
        .(mean_estimate = weighted.mean(mean_Y, w = size)),
        .(n, sd, batch_size, limit, run, assignment)
    ] %>%
        calculateTE() %>%
        summarizeTE() %>% .[, method := "TE"]

    ipwe_by_limits <- mean_estimates_by_batch[,
        .(batch_avg = mean(mean_Y)),
        by = .(n, sd, batch_size, limit, run, assignment)
    ] %>%
        calculateTE(mean_variable = "batch_avg") %>%
        summarizeTE() %>% .[, method := "IPWE"]

    first_batch_te <- mean_estimates_by_batch[batch == 1 & limit == 0] %>%
        when(
            nrow(.) > 0 ~ {
                calculateTE(., mean_variable = "mean_Y") %>%
                summarizeTE() %>% .[, method := "FBTE"]
            },
            data.table()
        )
    rbindlist(list(te_by_limits, ipwe_by_limits, first_batch_te))
}

calculateMeansBySetups <- function(mean_estimates_by_batch) {
    avg <- mean_estimates_by_batch[,
        .(mean_estimate = weighted.mean(mean_Y, w = size)),
        .(n, sd, batch_size, limit, run, assignment)
    ] %>%
        summarizeMeasure(
            mean_estimate,
            by = c("n", "sd", "batch_size", "limit", "assignment")
        ) %>% .[, method := "TE"]

    batch_avg <- mean_estimates_by_batch[,
        .(batch_avg = mean(mean_Y)),
        by = .(n, sd, batch_size, limit, run, assignment)
    ] %>%
        summarizeMeasure(
            batch_avg,
            by = c("n", "sd", "batch_size", "limit", "assignment")
        ) %>% .[, method := "IPWE"]

    rbindlist(list(avg, batch_avg))
}


calculateWelfareBySetups <- function(mean_estimates_by_batch) {
    calculateWelfare(mean_estimates_by_batch) %>%
    summarizeWelfare()
}


calculateWelfareAccumulationBySetups <- function(mean_estimates_by_batch) {
    mean_estimates_by_batch %>%
        .[, batch1000 := batch * batch_size / 1000] %>%
        calculateWelfare(by = c("n", "sd", "batch_size", "limit", "batch1000")) %>%
        .[,
            `:=`(accumulated_welfare = cumsum(welfare), size = cumsum(size)),
            .(sd, batch_size, limit, run)
        ] %>%
        summarizeWelfare(accumulated_welfare, by = c("n", "sd", "batch_size", "limit", "batch1000"))
}

calculateTreatedShareBySetups <- function(mean_estimates_by_batch) {
    mean_estimates_by_batch[,
        .(treated_share = sum(assignment * size) / sum(size)),
        .(n, sd, batch_size, limit, run, batch)
    ] %>%
        summarizeMeasure(treated_share, by = c("n", "sd", "batch_size", "limit", "batch"))
}
