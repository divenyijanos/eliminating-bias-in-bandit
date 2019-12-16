source("global.R")

CHART_FORMAT <- "eps"

tes_by_setups <- collectInterimResults("TEBySetups")
welfare_by_setups <- collectInterimResults("WelfareBySetups")

# Outcome ---------------------------------------------------------------------

welfare0 <- welfare_by_setups[limit == 0] %>%
    .[batch_size == 1000, label := sd]
welfare0[, max_welfare := max(mean), by = sd]

welfare0[batch_size <= 5000] %>%
    plotXYBy("batch_size", "mean", by = "sd") +
    geom_line(aes(y = 10000 - batch_size / 2), linetype = "dashed") +
    geom_point(data = welfare0[mean == max_welfare], size = 2) +
    labs(y = "Expected welfare")
saveChart("welfare-by-batch-size")
saveChart("welfare-by-batch-size-wide", width = 8)


# Traditional mean estimates---------------------------------------------------

mean_estimates0 <- collectInterimResults("MeansBySetups") %>%
    .[limit == 0] %>%
    .[batch_size == 500 & sd %in% c(1, 10, 30), label := sd] %>%
    .[, bias_in_outcome := mean - assignment]

mean_estimates0[method == "TE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_outcome", by = "sd", label_nudge_x = +10, box_padding = 0.01) +
    labs(y = "Bias") +
    facet_grid(. ~ assignment, labeller = labeller(assignment = c(`0` = "control", `1` = "treatment")))
saveChart("bias-in-means", width = 8)


tes0 <- tes_by_setups %>%
    .[limit == 0] %>%
    .[batch_size == 1000 & sd %in% c(1, 5, 10, 20, 30), label := sd] %>%
    .[, bias_in_te := mean - 1]

tes0[method == "TE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_te", by = "sd", label_nudge_x = 0, box_padding = 0.3) +
    labs(y = "Bias in TE")
saveChart("bias-in-te")

# amount of learning in first batch
treated_shares <- collectInterimResults("TreatedShareBySetups") %>%
    .[limit == 0 & batch > 1]

treated_shares[batch == 2] %>%
    .[batch_size == 1000, label := as.character(sd)] %>%
    plotXYBy("batch_size", "mean", by = "sd", box_padding = 0.05) +
    labs(y = "Average treated share in 2nd batch")
saveChart("avg-treated-share")

# Avg of avgs -----------------------------------------------------------------

mean_estimates0[method == "IPWE" & batch_size <= 5000] %>%
    plotXYBy("batch_size", "bias_in_outcome", by = "sd", box_padding = 0.01) +
    facet_grid(. ~ assignment, labeller = label_both) +
    labs(y = "Bias")
saveChart("bias-in-means-grouped-by-bs", width = 8, format = "png")


# Best batch_size by limit ----------------------------------------------------

welfare_by_setups[, max_welfare := max(mean), .(sd, limit)]

welfare_by_setups[batch_size <= 500 & limit > 0] %>%
    .[batch_size == 500, label := as.character(limit)] %>%
    plotXYBy("batch_size", "mean", by = "limit", size = 0.5) +
    geom_point(data = welfare_by_setups[mean == max_welfare & limit > 0]) +
    facet_wrap(~ sd)

welfare_by_setups[limit > 0][order(-mean), .SD[1:3, .(batch_size, limit, mean)], sd]
welfare_by_setups[limit > 0][order(-mean), .SD[1:3, .(batch_size, limit)], sd] %>%
    xtable(digits = c(0, 0, 0, 3)) %>%
    print(booktabs = TRUE, include.rownames = FALSE)


minimum_batch_sizes <- data.table(
    limit = LIMITS,
    min_batch_size = sapply(
        LIMITS,
        function(limit) ifelse(
            limit > 0,
            min(BATCH_SIZES[which(BATCH_SIZES * limit >= 1)]),
            10
        )
    )
)

dt_to_plot <- welfare_by_setups[order(-mean), .SD[1, .(batch_size, mean)], .(sd, limit)] %>%
    merge(minimum_batch_sizes, by = "limit", all.x = TRUE) %>%
    .[, limit := factor(limit, labels = c("0%", "0.5%", "1%", "2%", "5%", "10%", "15%", "20%"))]
ggplot(dt_to_plot, aes(sd, batch_size, color = limit)) +
    geom_hline(aes(yintercept = min_batch_size), linetype = "dashed") +
    geom_line(size = 1) +
    scale_color_manual(values = generateGradientColors(dt_to_plot[, unique(limit)]), guide = FALSE) +
    scale_x("sd") +
    scale_y_continuous(breaks = c(10, 100, 200, 500), minor_breaks = BATCH_SIZES) +
    facet_wrap(~ limit, nrow = 2) +
    labs(x = bquote(sigma), y = "Best batch size")
saveChart("best-batch-sizes", width = 8)
