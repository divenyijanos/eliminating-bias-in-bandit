source("global.R")

distribution_colors <- setNames(c(MAIN_COLOR, THIRD_COLOR, SECONDARY_COLOR, DARK_SECONDARY_COLOR), DISTRIBUTIONS)
n <- 10000
sd <- 10
te <- 1

map_df(DISTRIBUTIONS, ~{
    simulateParticipants(sd = 10, distribution = .x) %>% .[, distribution := .x]
}) %>%
    .[, distribution := factor(distribution, levels = DISTRIBUTIONS)] %>%
    ggplot(aes(Y0, color = distribution)) +
    geom_density() +
    scale_color_manual(values = distribution_colors) +
    theme(legend.position = "bottom")
saveChart("distribution-densities", format = "png")

results <- map_df(DISTRIBUTIONS, ~{
    tes_by_setups <- collectInterimResults("TEBySetups", n = n, sd = sd, distribution = .x)
    welfare_by_setups <- collectInterimResults("WelfareBySetups", n = n, sd = sd, distribution = .x)

    merge(
        tes_by_setups[, .(sd, method, batch_size, limit, distribution = .x, Bias = mean - te, MSE = mse)],
        welfare_by_setups[, .(batch_size, limit, Welfare = mean)],
        by = c("batch_size", "limit")
    )
}) %>%
    .[batch_size < 10000] %>%
    .[, strategy := fcase(
            method == "TE" & limit == 0, "TS",
            method == "IPWE" & limit == 0.05, "LTS-5%-IPW"
    )] %>%
    .[!is.na(strategy)]

results[, .(
    strategy = ordered(ifelse(strategy == "TS", "TS", "LTS-5%"), levels = c("TS", "LTS-5%")),
    distribution, batch_size, Welfare
)] %>%
    ggplot(aes(batch_size, Welfare, color = distribution)) +
    geom_line(size = 1) +
    scale_color_manual(values = distribution_colors, limits = c("chisq", "normal", "t", "negchisq")) +
    scale_x("batch_size") +
    labs(x = "Batch size (log scale)") +
    facet_grid(~ strategy) +
    theme(
        legend.position = c(.05, .05),
        legend.justification = c("left", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color="white", size=2)
    )
saveChart("welfare-by-distribution", width = 8)


results[, .(
    strategy = ordered(strategy, levels = c("TS", "LTS-5%-IPW")),
    distribution, batch_size, Bias, MSE
)] %>%
    melt(measure.vars = c("Bias", "MSE")) %>%
    ggplot(aes(batch_size, value, color = distribution)) +
    geom_line(size = 1) +
    scale_color_manual(values = distribution_colors, limits = c("negchisq", "t", "normal", "chisq")) +
    scale_x("batch_size") +
    labs(x = "Batch size (log scale)", y = "") +
    facet_grid(variable ~ strategy, scale = "free") +
    theme(
        legend.position = c(.05, .52),
        legend.justification = c("left", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color="white", size=2)
    )
saveChart("estimation-by-distribution", width = 8, height = 8)

results[, .(
    strategy = ordered(strategy, levels = c("TS", "LTS-5%-IPW")),
    distribution, batch_size, MSE
)] %>%
    ggplot(aes(batch_size, MSE, color = distribution)) +
    geom_line(size = 1) +
    scale_color_manual(values = distribution_colors, limits = c("negchisq", "t", "normal", "chisq")) +
    scale_x("batch_size") +
    labs(x = "Batch size (log scale)", y = "") +
    facet_grid(. ~ strategy, scale = "free") +
    theme(
        legend.position = c(.25, .42),
        legend.justification = c("left", "bottom"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.box.background = element_rect(color="white", size=2)
    )
saveChart("mse-by-distribution", width = 8)
