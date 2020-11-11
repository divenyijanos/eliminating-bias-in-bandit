source("global.R")

te <- 1
sd <- 10
set.seed(sd * 10)
participants <- simulateParticipants(n = 10000, te = te, sd = sd)

# Check required sample size calculation was correct --------------------------
effect <- map(1:1000, ~{
    participants <- simulateParticipants(n = 10000, te = te, sd = sd)

    sample_rct_assignment <- copy(participants) %>%
        .[, assignment := rep(c(0, 1), each = .N/2)] %>%
        .[, Y := assignment * Y1 + (1 - assignment) * Y0]
    t.test(Y ~ assignment, data = sample_rct_assignment, alternative = "less")
})

no_effect <- map(1:1000, ~{
    participants <- simulateParticipants(n = 10000, te = 0, sd = sd)

    sample_rct_assignment <- copy(participants) %>%
        .[, assignment := rep(c(0, 1), each = .N/2)] %>%
        .[, Y := assignment * Y1 + (1 - assignment) * Y0]
    t.test(Y ~ assignment, data = sample_rct_assignment, alternative = "less")
})

# reject
mean(map_dbl(effect, ~{.x$p.value}) < 0.05)
mean(map_dbl(no_effect, ~{.x$p.value}) < 0.05)

# -----------------------------------------------------------------------------

# Simulated assignments
params <- list(n = 10000, sd = sd, batch_size = 1000, limit = 0)
mean_estimates_by_batch <- readInSimulationFiles(params = params)

# assignment is adaptive
mean_estimates_by_batch[,
    .(treated_share = sum(size * assignment) / sum(size)), .(run, batch)] %>%
    summarizeMeasure(treated_share, by = "batch") %>%
    ggplot(aes(x = batch)) +
    geom_line(aes(y = mean), color = MAIN_COLOR, size = 1) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = MAIN_COLOR, alpha = 0.5) +
    geom_hline(aes(yintercept = 0.5), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 10, 2), minor_breaks = seq(10)) +
    labs(x = "Batch", y = "Share of assigned to treatment")

# by runs
mean_estimates_by_batch[, .(treated_share = sum(size * assignment) / sum(size)), .(sd, batch, run)] %>%
    ggplot(aes(batch, treated_share, group = run)) +
    geom_line(color = MAIN_COLOR, alpha = 0.01) +
    scale_x_continuous(breaks = seq(0, 10, 2), minor_breaks = seq(10)) +
    labs(x = "Batch", y = "Share of assigned to treatment")
saveChart("illustration-adaptive-assignment", width = 8)


# Outcome
welfare <- calculateWelfare(mean_estimates_by_batch)

ggplot(welfare, aes(welfare)) +
    geom_density(fill = MAIN_COLOR, color = NA, alpha = 0.5) +
    labs(x = "Expected welfare") +
    geom_vline(aes(xintercept = 5000), linetype = "dashed") +
    coord_cartesian(xlim = c(4500, 10000)) +
    scale_x_continuous(breaks = seq(5000, 10000, 1000))
saveChart("illustration-gain-on-outcome")

# Treatment effect estimation - biased!
mean_estimates <- mean_estimates_by_batch[,
    .(mean_estimate = weighted.mean(mean_Y, w = size)),
    .(run, assignment)
]

collectInterimResults("TEBySetups", sd = 10, batch_size = 10000)

tes <- calculateTE(mean_estimates)
tes_means <- summarizeTE(tes, by = NULL) %>% .[, lapply(.SD, round, 2)]
tes_means

ggplot(tes, aes(TE)) +
    geom_density(fill = MAIN_COLOR, color = NA, alpha = 0.5) +
    geom_text(
        data = tes_means,
        aes(
            label = paste(
                "mean =", mean, "\n", "skewness =", skewness, "\n",
                "sd = ", stdev, "\n", "MSE = ", mse),
            x = q95, y = 0
        ),
        color = MAIN_COLOR, size = 12*0.35, vjust = -1, hjust = 0.5
    ) +
    geom_vline(xintercept = te, linetype = "dashed") +
    expand_limits(x = c(0, NA)) +
    labs(x = "Estimated treatment effect (TE)")
saveChart("illustration-te")

mean_estimates_means <- summarizeMeasure(
    mean_estimates, mean_estimate, by = 'assignment',
    additional_aggregations = expr(list(
        mse = mean((mean_estimate - assignment)^2)
    ))
)

# Intuition for bias ----------------------------------------------------------

effect_of_first_batch_estimate <- mean_estimates_by_batch[
    batch == 1, first_batch_estimate := mean_Y
][,
    .(
        mean_outcome = weighted.mean(mean_Y, w = size),
        size = sum(size),
        first_batch_estimate = max(first_batch_estimate, na.rm = TRUE)
    ),
    by = .(run, assignment)
]

effect_of_first_batch_estimate %>%
    ggplot(aes(first_batch_estimate, mean_outcome)) +
    geom_hex(bins = 100) +
    scale_fill_gradient(low = GRADIENT_LIMITS[2], high = GRADIENT_LIMITS[1], guide = FALSE, trans = "log") +
    geom_point(aes(x = assignment, y = assignment), shape = 3) +
    labs(x = "First batch mean estimate", y = "Overall mean estimate") +
    facet_grid(. ~ assignment, labeller = labeller(assignment = c(`0` = "control", `1` = "treatment")))
saveChart("illustration-first-batch-mean-vs-overall", width = 8)

# ANIMATION -------------------------------------------------------------------

sampling_animation <- effect_of_first_batch_estimate[method == "TS"][
    run < 300 & assignment == 0, .(run, first_batch_estimate, size, mean_outcome)
] %>%
    melt(measure.vars = c("size", "mean_outcome")) %>%
    .[, variable := factor(variable, labels = c("#Controls", "Overall estimate"))] %>%
    ggplot(aes(first_batch_estimate, value, color = variable, group = run)) +
    geom_point(alpha = 0.7) +
    scale_color_manual(values = TWO_COLOR_SCHEME, guide = FALSE) +
    xlim(-2, 2) +
    facet_wrap(~ variable, scales = "free_y") +
    labs(x = "First batch estimate") +
    transition_states(run, transition_length = 0, state_length = 0.1) +
    shadow_mark()
animate(sampling_animation, nframes = 300, rewind = FALSE, width = 800, height = 400)
anim_save("figure/effect-of-first-batch.gif")
animate(sampling_animation, nframes = 300, rewind = FALSE, width = 800, height = 400, renderer = av_renderer())
anim_save("figure/effect-of-first-batch.avi")

effect_of_first_batch_estimate[
    run < 300 & assignment == 0, .(run, first_batch_estimate, size, mean_outcome)
] %>%
    melt(measure.vars = c("size", "mean_outcome")) %>%
    .[, variable := factor(variable, labels = c("#Controls", "Overall estimate"))] %>%
    ggplot(aes(first_batch_estimate, value, color = variable, group = run)) +
    geom_point(alpha = 0) +
    scale_color_manual(values = TWO_COLOR_SCHEME, guide = FALSE) +
    xlim(-2, 2) +
    facet_wrap(~ variable, scales = "free_y") +
    labs(x = "First batch estimate")
saveChart("effect-of-first-batch", width = 8)

# IPWE ------------------------------------------------------------------------

# Average of batch averages (same as IPW)
batch_avgs <- mean_estimates_by_batch[,
    .(batch_avg = mean(mean_Y), mean_estimate = weighted.mean(mean_Y, w = size), n_batch = .N),
    by = .(run, assignment)
]
batch_avgs_means <- summarizeMeasure(
    batch_avgs, batch_avg, by = 'assignment',
    additional_aggregations = expr(list(
        mse = mean((batch_avg - assignment)^2)
    ))
) %>% .[, lapply(.SD, round, 2)]
batch_avgs_means
# biased...

ggplot(batch_avgs, aes(batch_avg)) +
    geom_density(fill = MAIN_COLOR, color = NA, alpha = 0.5) +
    geom_text(
        data = batch_avgs_means,
        aes(
            label = paste("avg =", mean, "\n", "skewness =", skewness),
            x = -0.5, y = 0
        ),
        size = 12*0.35, vjust = -1, hjust = 0.5
    ) +
    geom_vline(data = batch_avgs_means, aes(xintercept = assignment), linetype = 'dashed') +
    labs(x = "Average of batch averages") +
    coord_cartesian(xlim = c(-1.5, 1.5)) +
    facet_wrap(~ assignment, labeller = label_both, scales = "free_x")
saveChart("batch-avg-density", width = 8)

ipwe <- calculateTE(batch_avgs, "batch_avg")
ipwe_means <- summarizeTE(ipwe, by = NULL) %>% .[, lapply(.SD, round, 2)]
ipwe_means

ggplot(ipwe, aes(TE)) +
    geom_density(fill = MAIN_COLOR, color = NA, alpha = 0.5) +
    geom_text(
        data = ipwe_means,
        aes(
            label = paste(
                "mean =", mean, "\n", "skewness =", skewness, "\n",
                "sd =", stdev, "\n", "MSE = ", mse),
            x = q95, y = 0
        ),
        color = MAIN_COLOR, size = 12*0.35, vjust = -1, hjust = 0.5
    ) +
    geom_vline(xintercept = te, linetype = "dashed") +
    coord_cartesian(xlim = c(-10, 10)) +
    labs(x = "Batch average treatment effect (IPWE)")
saveChart("illustration-ipwe")


gradient_colors <- generateGradientColors(1:10)
map_df(seq(10), ~{
    mean_estimates_by_batch[assignment == 0] %>%
    .[batch <= .x, .(batch_avg = mean(mean_Y), latest_batch = .x), run]
}) %>%
    ggplot() +
    geom_density(aes(batch_avg, color = factor(latest_batch))) +
    coord_cartesian(xlim = c(-5, 5)) +
    scale_color_manual(values = gradient_colors, guide = FALSE) +
    annotate(
        "label", x = 0, y = 0.75, label = "after 1st batch",
        color = gradient_colors[1], hjust = 0
    ) +
    annotate(
        "label", x = 1, y = 0.25, label = "after last batch",
        color = gradient_colors[10], hjust = 0
    ) +
    labs(x = "Average of batch averages (control)")
saveChart("control-batch-avg-through-batches")

mean_estimates_by_batch[assignment == 0] %>%
    ggplot() +
    geom_density(aes(mean_Y, color = factor(batch))) +
    coord_cartesian(xlim = c(-5, 5)) +
    scale_color_manual(values = gradient_colors, guide = FALSE) +
    annotate(
        "label", x = 0, y = 0.75, label = "1st batch",
        color = gradient_colors[1], hjust = 0
    ) +
    annotate(
        "label", x = 2.5, y = 0.1, label = "last batch",
        color = gradient_colors[10], hjust = 0
    ) +
    labs(x = "Batch average (control)")
saveChart("control-batch-avg-by-batch")


# avg of unbiased averages is biased because of selection of batches with control assignees
summarizeMeasure(batch_avgs[assignment == 0], batch_avg, by = "n_batch") %>%
    ggplot(aes(n_batch, mean)) +
    geom_line(size = 1, color = MAIN_COLOR) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = MAIN_COLOR, color = NA, alpha = 0.5) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    scale_x_continuous(breaks = seq(0, 10, 2), minor_breaks = seq(10)) +
    labs(x = "Number of batches with controls", y = "Average of batch control averages")
saveChart("ipwe-selection", width = 8)

batch_avgs[, n_batch_both := min(n_batch), run] %>%
    calculateTE("batch_avg", by = c("run", "n_batch_both")) %>%
    summarizeMeasure(TE, by = "n_batch_both", round = 4) %>%
    .[, .(n_batch_both, mean, n_run / 20000)] %>%
    t() %>%
    xtable() %>%
    print(booktabs = TRUE)


calculateTE(batch_avgs) %>% summarizeTE(by = NULL)
