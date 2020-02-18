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

putTogetherSetup <- function(n, sd, te = 1) {
    tes_by_setups <- collectInterimResults("TEBySetups", n = n, sd = sd)
    welfare_by_setups <- collectInterimResults("WelfareBySetups", n = n, sd = sd)

    merge(
        tes_by_setups[, .(sd, TE = mean, bias = mean - te, mse, sign_pct, method, batch_size, limit)],
        welfare_by_setups[, .(welfare = mean, batch_size, limit)],
        by = c("batch_size", "limit")
    ) %>%
    .[, setup := paste0(ifelse(limit == 0, "", "limited "), method)] %>%
    .[, setup_detailed := paste(setup, limit)]
}

putTogetherETCSetup <- function(n, sd, te = 1) {
    etc_tes <- fread(glue("{INTERIM_RESULT_FOLDER}/etc-n{n}-sd{sd}-TEBySetups.csv"))
    etc_welfare <- fread(glue("{INTERIM_RESULT_FOLDER}/etc-n{n}-sd{sd}-WelfareBySetups.csv"))

    merge(
        etc_tes[, .(sd, TE = mean, bias = mean - te, mse, batch_size, limit = 0, setup = "ETC", setup_detailed = "ETC")],
        etc_welfare[, .(welfare = mean, batch_size)],
        by = c("batch_size")
    )
}

putTogetherSetupsWithETC <- function(n, sd_values, te = 1) {
    setup_comparison <- map(sd_values, ~{
        rbind(
            putTogetherSetup(n = n, sd = .x),
            putTogetherETCSetup(n = n, sd = .x),
            fill = TRUE
        )
    }) %>%
    rbindlist() %>%  # map_df causes annoying .internal.selfref warning
    .[, alpha := I(pmax(1 - 2 * abs(bias), 0))] %>%
    .[order(sd, batch_size, setup)]
}

createLatexTableForScenarios <- function(table, caption, footnote, result_file, by, by_name, digits = 0) {
    kable(table, "latex", booktabs = TRUE, longtable = TRUE, caption = caption, digits = digits) %>%
    add_header_above(c(" ", "Batch size" = 10)) %>%
    kable_styling(latex_options = c("repeat_header")) %>%
    pack_rows(
        index = map_dbl(seq_along(by), ~nrow(table)/length(by)) %>% setNames(map_chr(by, ~glue("${by_name} = {.x}$"))),
        escape = FALSE, latex_gap_space = "0.5em"
    ) %>%
    footnote(footnote, threeparttable = TRUE, general_title = "", escape = FALSE) %>%
    writeLines(glue("text/{result_file}.txt"))
}

createTableForSetup <- function(n, sd, measure, by_variable) {
    cucc <- putTogetherSetupsWithETC(n = n, sd = sd) %>%
        .[!(setup == "limited TE")] %>%
        .[, .(sd, batch_size, limit, setup, method, welfare, bias, mse)] %>%
        .[order(limit, -setup)] %>%
        .[, allocation := ifelse(
            setup == "ETC",
            "ETC",
            ifelse(limit == 0, "TS", str_remove(glue("LTS-{scales::percent(limit)}"), "\\.0"))
        )] %>%
        .[, strategy := ifelse(
            allocation == "TS" & setup == "FBTE",
            "TS-FB",
            ifelse(allocation == "TS" & setup == "IPWE", "TS-IPW", allocation)
        )] %>%
        .[, `:=`(
            allocation = factor(allocation, levels = unique(allocation)),
            strategy = factor(strategy, levels = unique(strategy))
        )] %>%
        .[, .SD, .SDcols = c("batch_size", "limit", measure, by_variable)] %>%
        unique() %>%
        dcast(reformulate("batch_size", by_variable), value.var = measure) %>%
        .[, (length(.)) := map(.SD, ~ifelse(is.na(.), min(., na.rm = TRUE), .)), .SDcols = length(.)]
}
