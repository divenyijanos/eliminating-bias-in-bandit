source("global.R")

# Equivalence of setups: higher n with higher sigma is the same
# --> increasing n is the same as decreasing the noise

setups <- list(
    list(n = 10000, sd = 5, batch_size = 500),
    list(n = 40000, sd = 10, batch_size = 2000)
)

welfare_accumulation <- map_df(
    setups,
    ~do.call(collectInterimResults, c(.x, result_name = "WelfareAccumulationBySetups"))
)

welfare_accumulation[, limit := factor(limit, labels = c("0%", "0.5%", "1%", "2%", "5%", "10%", "15%", "20%"))]
ggplot(welfare_accumulation, aes(batch1000 * 1000 / n, mean / (batch1000 * 1000), color = factor(n))) +
    geom_line(size = 1) +
    facet_wrap(~ limit) +
    scale_color_manual(values = TWO_COLOR_SCHEME) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "process", y = "achieved relative welfare") +
    theme(
        legend.title = element_blank(),
        legend.position = c(0.8, 0.15)
    )

tes <- map_df(
    setups,
    ~do.call(collectInterimResults, c(.x, result_name = "TEBySetups"))
)

dcast(tes[method != "FBTE"], limit ~ method + n, value.var = "mean")

tes[limit == 0.2, label := as.character(n)]
plotXYBy(tes[limit > ], "limit", "mean", by = "n") +
    facet_wrap(~ method)


# Analysis by horizon ---------------------------------------------------------

NS <- c(2000, 10000, 20000, 40000)

welfare0 <- map_df(NS, ~collectInterimResults("WelfareBySetups", sd = 10, n = .x)) %>% .[limit == 0]

etc_welfare <- map_df(NS, ~{
    datafile <- glue("{INTERIM_RESULT_FOLDER}/etc-n{.x}-sd10-WelfareBySetups.csv")
    if (file.exists(datafile)) {
        fread(datafile)
    } else {
        data.table()
    }
}) %>% .[batch_size %in% c(1000, 2000)] %>% .[n == 40000, label := paste0("ETC-", batch_size)]

welfare0[batch_size <= 2000] %>%
    .[n == 40000 & batch_size %in% c(10, 100, 500, 1000, 2000), label := batch_size] %>%
    ggplot(aes(n, n - mean, group = factor(batch_size), color = factor(batch_size))) +
    geom_line(size = 1) +
    geom_line(data = etc_welfare, linetype = "dashed", color = THIRD_COLOR) +
    geom_label_repel(
        data = etc_welfare,
        aes(label = label), color = THIRD_COLOR,
        direction = "y", xlim = c(NA, 35000),
        segment.size = 0.2, na.rm = TRUE
    ) +
    geom_label_repel(
        aes(label = label),
        direction = "y", nudge_x = 100, box.padding = 0.1,
        segment.size = 0.2, na.rm = TRUE
    ) +
    scale_color_manual(values = generateGradientColors(BATCH_SIZES[BATCH_SIZES <= 2000]), guide = FALSE) +
    scale_x_continuous(breaks = NS, minor_breaks = NS) +
    labs(y = "Expected regret")
saveChart("expected-regret-by-horizon")

welfare0[, welfare_share := mean / n]
welfare0[, max_welfare := max(welfare_share), n]
welfare0[batch_size <= 500] %>%
    .[batch_size == 500, label := n] %>%
    plotXYBy("batch_size", "welfare_share", "n", box_padding = 0.05) +
    geom_point(data = welfare0[welfare_share == max_welfare]) +
    labs(y = "Share of optimal welfare") +
    scale_x_continuous(breaks = c(10, 100, 200, 500), minor_breaks = BATCH_SIZES)
saveChart("welfare-by-batch-size-across-horizon")

# Summary tables --------------------------------------------------------------

opts_current$set(label = "welfare-summary-horizon")
map(NS, ~createTableForSetup(n = ., sd = 10, "welfare", "allocation")) %>%
    rbindlist(fill = TRUE) %>%
    createLatexTableForScenarios(
        caption = "Expected welfare for different strategies ($\\sigma = 10$)",
        footnote = "TS: Thompson sampling, ETC: Explore-then-commit, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation. Expected welfare is calculated as the average of the sum of outcomes ($\\\\sum_{i=1}^n Y$) across the simulation runs. Number of simulations = $20,000$ for $n = 10,000$, and $10,000$ otherwise.",
        result_file = "table-welfare-horizon",
        by = NS, by_name = "n"
    )

opts_current$set(label = "bias-summary-horizon")
map(NS, ~createTableForSetup(n = ., sd = 10, "bias", "strategy")) %>%
    rbindlist(fill = TRUE) %>%
    createLatexTableForScenarios(
        caption = "Bias for different strategies ($\\sigma = 10$)",
        footnote = "TS: Thompson sampling with $\\\\hat \\\\tau_0$, TS-IPW: Thompson sampling with $\\\\hat \\\\tau_{IPW}$, TS-FB: Thompson sampling with $\\\\hat \\\\tau_{FB}$, ETC: Explore-then-commit with $\\\\hat \\\\tau_0$, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation and $\\\\hat \\\\tau_{IPW}$. Number of simulations = $20,000$ for $n = 10,000$, and $10,000$ otherwise.",
        result_file = "table-bias-horizon",
        digits = 3, by = NS, by_name = "n"
    )

opts_current$set(label = "mse-summary-horizon")
map(NS, ~createTableForSetup(n = ., sd = 10, "mse", "strategy")) %>%
    rbindlist(fill = TRUE) %>%
    createLatexTableForScenarios(
        caption = "MSE for different strategies ($\\sigma = 10$)",
        footnote = "TS: Thompson sampling with $\\\\hat \\\\tau_0$, TS-IPW: Thompson sampling with $\\\\hat \\\\tau_{IPW}$, TS-FB: Thompson sampling with $\\\\hat \\\\tau_{FB}$, ETC: Explore-then-commit with $\\\\hat \\\\tau_0$, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation and $\\\\hat \\\\tau_{IPW}$. Number of simulations = $20,000$ for $n = 10,000$, and $10,000$ otherwise.",
        result_file = "table-mse-horizon",
        digits = 3, by = NS, by_name = "n"
    )
