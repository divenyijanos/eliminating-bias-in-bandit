source("global.R")

tes_by_setups <- collectInterimResults("TEBySetups")
welfare_by_setups <- collectInterimResults("WelfareBySetups")

# Outcome ---------------------------------------------------------------------

welfare0 <- welfare_by_setups[limit == 0] %>%
    .[batch_size == 500 & sd %in% c(1, 10, 20, 30), label := sd]
welfare0[, max_welfare := max(mean), by = sd]

welfare0[batch_size <= 5000] %>%
    plotXYBy("batch_size", "mean", by = "sd", box_padding = 0) +
    geom_line(aes(y = 10000 - batch_size / 2), linetype = "dashed") +
    geom_point(data = welfare0[mean == max_welfare], size = 2) +
    labs(y = "Expected welfare")
saveChart("welfare-by-batch-size")


# Traditional mean estimates---------------------------------------------------

mean_estimates0 <- collectInterimResults("MeansBySetups") %>%
    .[limit == 0] %>%
    .[batch_size == 500 & sd %in% c(1, 10, 30), label := sd] %>%
    .[, bias_in_outcome := mean - assignment]

mean_estimates0[method == "TE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_outcome", by = "sd", label_nudge_x = 0, box_padding = 0.01) +
    labs(y = "Bias") +
    facet_grid(. ~ assignment, labeller = labeller(assignment = c(`0` = "control", `1` = "treatment")))
saveChart("bias-in-means", width = 8)


tes0 <- tes_by_setups %>%
    .[limit == 0] %>%
    .[batch_size == 1000 & sd %in% c(1, 10, 20, 30), label := sd] %>%
    .[, bias_in_te := mean - 1]

tes0[method == "TE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_te", by = "sd", label_nudge_x = 0, box_padding = 0.3) +
    labs(y = "Bias in TE")
saveChart("bias-in-te")

# amount of learning in first batch
treated_shares <- collectInterimResults("TreatedShareBySetups") %>%
    .[limit == 0 & batch > 1]

treated_shares[batch == 2] %>%
    .[batch_size == 1000 & sd %in% c(1, 5, 10, 20, 30), label := as.character(sd)] %>%
    plotXYBy("batch_size", "mean", by = "sd", label_nudge_x = 0, box_padding = 0.05) +
    labs(y = "Average treated share in 2nd batch")
saveChart("avg-treated-share")

# Avg of avgs -----------------------------------------------------------------

mean_estimates0[method == "IPWE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_outcome", by = "sd", box_padding = 0.01) +
    facet_grid(. ~ assignment, labeller = label_both) +
    labs(y = "Bias")
saveChart("bias-in-ipwe-means-grouped-by-bs", width = 8)


# Avg of avgs with limits -----------------------------------------------------

ipwe_by_setups <- tes_by_setups[method == "IPWE" & limit > 0]
ipwe_by_setups[, label := ""]
ipwe_by_setups[, min_mse_by_sd := min(mse), sd]

ipwe_by_setups[batch_size <= 5000 & limit %in% c(0.005, 0.02, 0.1, 0.2)] %>%
    plotXYBy("batch_size", "mse", by = "sd") +
    labs(y = "MSE") +
    facet_wrap(~ limit)

ipwe_by_setups %>%
    plotXYBy("limit", "mse", by = "sd") +
    labs(y = "MSE") +
    facet_wrap(~ batch_size)

# Best batch_size by limit ----------------------------------------------------

welfare_by_setups[, max_welfare := max(mean), .(sd, limit)]

welfare_by_setups[batch_size <= 500 & limit > 0] %>%
    .[batch_size == 500, label := as.character(limit)] %>%
    plotXYBy("batch_size", "mean", by = "limit", size = 0.5) +
    geom_point(data = welfare_by_setups[mean == max_welfare & limit > 0]) +
    facet_wrap(~ sd)

welfare_by_setups %>%
    plotXYBy("limit", "mean", by = "batch_size", size = 0.5) +
    labs(y = "Expected welfare") +
    facet_wrap(~ sd)


# Best combinations

welfare_by_setups[limit > 0, max_welfare_by_sd := max(mean), sd]
welfare_by_setups[mean >= max_welfare_by_sd * 0.999, .N, keyby = sd]

welfare_by_setups[limit > 0 & batch_size < 2000 & sd %in% c(1, 5, 10, 15, 20, 30)] %>%
    .[order(mean)] %>%
    ggplot(aes(
        x = factor(limit, levels = LIMITS, labels = map_chr(LIMITS, ~scales::percent(.x, accuracy = .x*100))),
        y = factor(batch_size),
        fill = mean / max_welfare_by_sd,
        color = (mean >= max_welfare_by_sd * 0.99)
    )) +
    geom_tile(size = 0.5) +
    scale_fill_gradient(
        low = GRADIENT_LIMITS[2], high = GRADIENT_LIMITS[1],
        guide = guide_legend(title = "Welfare \nrelative \nto the best", reverse = TRUE, order = 1)
    ) +
    scale_color_manual(
        breaks = TRUE, values = c(NA, MAIN_COLOR), labels = "",
        guide = guide_legend(title = "Within 1% \nof the best", override.aes = list(fill = NA))
    ) +
    theme(legend.title = element_text(size = rel(0.8))) +
    labs(x = "Limit", y = "Batch size") +
    facet_wrap(~ sd, labeller = label_bquote(sigma == .(sd)))
saveChart("best-welfare-combinations", width = 8)




ipwe_by_setups[limit > 0 & batch_size < 2000 & sd %in% c(1, 5, 10, 15, 20, 30)] %>%
    ggplot(aes(
        x = factor(limit, levels = LIMITS, labels = map_chr(LIMITS, ~scales::percent(.x, accuracy = .x*100))),
        y = factor(batch_size),
        fill = mse
    )) +
    geom_tile(size = 0.5) +
    scale_fill_gradient(
        low = GRADIENT_LIMITS[1], high = GRADIENT_LIMITS[2],
        breaks = c(0.1, 0.3, 0.5, 1, 10),
        guide = guide_legend(title = "MSE", reverse = TRUE)
    ) +
    theme(legend.title = element_text(size = rel(0.8))) +
    labs(x = "Limit", y = "Batch size") +
    facet_wrap(~ sd, labeller = label_bquote(sigma == .(sd)))
saveChart("best-mse-combinations", width = 8)


# SUMMARY TABLES --------------------------------------------------------------

opts_current$set(label = "welfare-summary")
map(SDS, ~createTableForSetup(n = 10000, ., "welfare", "allocation")) %>%
    rbindlist() %>%
    createLatexTableForScenarios(
        caption = "Expected welfare for different strategies ($n = 10,000$)",
        footnote = "TS: Thompson sampling, ETC: Explore-then-commit, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation. Expected welfare is calculated as the average of the sum of outcomes ($\\\\sum_{i=1}^n Y$) across the simulation runs. Number of simulations = $10,000$ for $\\\\sigma < 10$, $20,000$ for $10 \\\\geq \\\\sigma < 20$ and $50,000$ for $\\\\sigma \\\\geq 20$.",
        result_file = "table-welfare",
        by = SDS, by_name = "\\\\sigma"
    )

opts_current$set(label = "bias-summary")
map(SDS, ~createTableForSetup(n = 10000, ., "bias", "strategy")) %>%
    rbindlist() %>%
    createLatexTableForScenarios(
        caption = "Bias for different strategies ($n = 10,000$)",
        footnote = "TS: Thompson sampling with $\\\\hat \\\\tau_0$, TS-IPW: Thompson sampling with $\\\\hat \\\\tau_{IPW}$, TS-FB: Thompson sampling with $\\\\hat \\\\tau_{FB}$, ETC: Explore-then-commit with $\\\\hat \\\\tau_0$, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation and $\\\\hat \\\\tau_{IPW}$. Number of simulations = $10,000$ for $\\\\sigma < 10$, $20,000$ for $10 \\\\geq \\\\sigma < 20$ and $50,000$ for $\\\\sigma \\\\geq 20$.",
        result_file = "table-bias",
        digits = 3, by = SDS, by_name = "\\\\sigma"
    )


opts_current$set(label = "mse-summary")
map(SDS, ~createTableForSetup(n = 10000, ., "mse", "strategy")) %>%
    rbindlist() %>%
    createLatexTableForScenarios(
        caption = "MSE for different strategies ($n = 10,000$)",
        footnote = "TS: Thompson sampling with $\\\\hat \\\\tau_0$, TS-IPW: Thompson sampling with $\\\\hat \\\\tau_{IPW}$, TS-FB: Thompson sampling with $\\\\hat \\\\tau_{FB}$, ETC: Explore-then-commit with $\\\\hat \\\\tau_0$, LTS-X\\\\%: Limited Thompson sampling with X\\\\% limitation and $\\\\hat \\\\tau_{IPW}$. Number of simulations = $10,000$ for $\\\\sigma < 10$, $20,000$ for $10 \\\\geq \\\\sigma < 20$ and $50,000$ for $\\\\sigma \\\\geq 20$.",
        result_file = "table-mse",
        digits = 3, by = SDS, by_name = "\\\\sigma"
    )
