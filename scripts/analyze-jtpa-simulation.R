source("global.R")

jtpa <- fread('data/jtpa_sample.csv') %>%
    .[, Y := earnings_30m - (treated == "trainee") * 774]
itt <- jtpa[assignment == "treatment", mean(Y)] - jtpa[assignment == "control", mean(Y)]
actual_welfare <- jtpa[, sum(Y)]

jtpa_sim_results <- readInSimulationFiles(pattern = "", distribution = "jtpa")

 # Benchmark #1: Bootstrap actual assignment
participants <- jtpa[, .(assignment = ifelse(assignment == "treatment", 1, 0), Y)]
n_treatment <- participants[, sum(assignment)]
n_control <- nrow(participants) - n_treatment
n_sim <- 10000

actual_share_bootstrapped <- mclapply(seq(n_sim), function(run) {
    set.seed(run)
    addBootstrappedAssignees(createEmptyAssignees(), c(`0` = n_control, `1` = n_treatment), participants, batch = 1) %>%
        .[,
            .(run = run, mean_Y = mean(Y), size = .N),
            by = .(batch, assignment)
        ]
}) %>% rbindlist()


# Benchmark #2: ETC
# first_batch_results <- jtpa_sim_results[batch == 1 & limit == 0]

# jtpa_etc <- rbind(
#     first_batch_results,
#     dcast(first_batch_results, run + batch_size ~ assignment, value.var = "mean_Y") %>%
#         .[, .(batch_size, run, assignment = max.col(.SD) - 1, size = nrow(jtpa) - batch_size), .SDcols = c("0", "1")] %>%
#         .[, .(
#             batch_size, run, assignment, size, batch = 2, limit = 0,
#             mean_Y = pmap_dbl(., ~with(list(...), {
#                 bootstrapFrom(participants, assignment, size)[, mean(Y)]
#             }))
#         )]
# ) %>% na.omit()
# fwrite(jtpa_etc, glue("{INTERIM_RESULT_FOLDER}/jtpa/etc.csv"))
jtpa_etc <- fread(glue("{INTERIM_RESULT_FOLDER}/jtpa/etc.csv"))

# Bandit simulations

group <-  c("batch_size", "limit")

welfare_results <- jtpa_sim_results %>%
    calculateWelfare(by = group) %>%
    summarizeWelfare(by = group)

random_split_welfare <- welfare_results[batch_size > 10000 & limit == 0, mean]

# welfare_results[batch_size < 10000] %>%
#     .[batch_size == 50 & limit == 0, label := "TS"] %>%
#     .[batch_size == 200 & limit == 0.05, label := "LTS-5%"] %>%
#     plotXYBy("batch_size", "mean", "limit", colors = c(SECONDARY_COLOR, DARK_SECONDARY_COLOR), label_nudge_x = 0) +
#     labs(y = "Expected welfare") +
#     scale_y_continuous(label = scales::label_number_si(accuracy = 0.1)) +
#     geom_hline(yintercept = welfare, linetype = "dashed") +
#     annotate(x = 50, y = actual_welfare, geom = "label", label = "actual welfare", label.size = 0) +
#     geom_hline(yintercept = random_split_welfare, linetype = "dashed") +
#     annotate(x = 50, y = random_split_welfare, geom = "label", label = "random-split welfare", label.size = 0)
# saveChart("jtpa-welfare-gain")

# # Improvement in welfare
# welfare_without_treatment <- jtpa[assignment == "control", mean(Y)] * nrow(participants)
# welfare_results[, .(batch_size, limit, gain = (mean - welfare_without_treatment) / (random_split_welfare - welfare_without_treatment))]
# welfare_results[, .(batch_size, limit, gain = (mean - welfare_without_treatment) / (welfare - welfare_without_treatment))]

# Estimation
estimation_results <- rbind(
    jtpa_sim_results[limit == 0][,
        .(mean_estimate = weighted.mean(mean_Y, w = size), strategy = "TS"),
        .(batch_size, limit, run, assignment)
    ],
    jtpa_sim_results[limit > 0][,
        .(mean_estimate = mean(mean_Y), strategy = "LTS"),
        by = .(batch_size, limit, run, assignment)
    ]
) %>%
    calculateTE(te = itt, by = c(group, "strategy")) %>%
    summarizeTE(te = itt, by = c(group, "strategy"))

simulated_mse_with_actual_share <- actual_share_bootstrapped[, .(mean_estimate = weighted.mean(mean_Y, w = size)), .(run, assignment)] %>%
    calculateTE(te = itt, by = NULL) %>%
    summarizeTE(te = itt, by = NULL) %>%
    .[, mse]

random_split_mse <- estimation_results[batch_size > 10000, mse]

# estimation_results[batch_size < 10000, .(batch_size, strategy, `Relative Bias` = (mean - itt) / itt, MSE = mse)] %>%
#     melt(id.vars = c("batch_size", "strategy")) %>%
#     .[, variable := ordered(variable, levels = c("Relative Bias", "MSE"))] %>%
#     .[batch_size == 200, label := strategy] %>%
#     ggplot(aes(x = batch_size, y = value, color = strategy)) +
#         geom_hline(yintercept = 0, color = "gray20", size = rel(0.25)) +
#         geom_line(size = 1) +
#         geom_label_repel(
#             aes(label = label),
#             direction = "y", nudge_x = 0, hjust = 0,
#             segment.size = 0.2, box.padding = 0.1, na.rm = TRUE
#         ) +
#         geom_hline(
#             aes(yintercept = value),
#             data = data.table(variable = "MSE", value = simulated_mse_with_actual_share),
#             linetype = "dashed"
#         ) +
#         geom_label(
#             aes(label = "MSE with actual share"),
#             data = data.table(batch_size = 50, variable = "MSE", value = simulated_mse_with_actual_share, strategy = "TS"),
#             label.size = 0, color = "black"
#         ) +
#         geom_blank(data = data.table(variable = "MSE", batch_size = 20, value = -25000, strategy = "TS")) +
#         scale_color_manual(values = c(SECONDARY_COLOR, DARK_SECONDARY_COLOR), guide = FALSE) +
#         scale_x("batch_size") +
#         labs(x = "Batch size (log scale)", y = "") +
#         facet_wrap(~ variable, scales = "free")
# saveChart("jtpa-estimation", width = 8)


# Welfare-estimation comparison

etc_results <- merge(
    etc_estimation <- jtpa_etc[,
        .(mean_estimate = mean(mean_Y), strategy = "ETC"),
        by = .(batch_size, limit, run, assignment)
    ] %>%
        calculateTE(te = itt, by = c(group, "strategy")) %>%
        summarizeTE(te = itt, by = c(group, "strategy")) %>%
        .[, .(TE = mean, bias = mean - itt, mse, strategy, batch_size, limit)],
    etc_welfare <-  jtpa_etc %>%
        calculateWelfare(by = group) %>%
        summarizeWelfare(by = group) %>%
        .[, .(welfare = mean, batch_size, limit)]
)

welfare_estimation_jtpa <- merge(
    estimation_results[, .(TE = mean, bias = mean - itt, mse, strategy, batch_size, limit)],
    welfare_results[, .(welfare = mean, batch_size, limit)]
) %>%
    rbind(etc_results) %>%
    rbind(., .[batch_size > 10000][, strategy := "LTS"]) %>%
    rbind(data.table(batch_size = 2000, strategy = "actual", bias = 0, welfare = actual_welfare, mse = simulated_mse_with_actual_share), fill = TRUE)

convex_hull_LTS <- rbind(
    welfare_estimation_jtpa[strategy == "LTS"],
    data.table(
        mse = c(welfare_estimation_jtpa[, min(mse)], 9.9*10^5, 10^6),
        welfare = c(welfare_estimation_jtpa[, min(welfare)], welfare_estimation_jtpa[, max(welfare)], welfare_estimation_jtpa[, min(welfare)]),
        strategy = "LTS",
        bias = 0
    ),
    fill = TRUE
) %>%
.[, .SD[chull(mse, welfare)]] %>%
.[order(mse)]

copy(welfare_estimation_jtpa)[batch_size == 2000, label := strategy] %>%
    .[strategy == "TS" & batch_size > 10000, label := "random split"] %>%
    .[, alpha := I(pmax((itt - 2 * abs(bias)) / itt, 0))] %>%
    .[strategy != "LTS"] %>%
    .[order(strategy, batch_size)] %>%
    ggplot(aes(welfare, mse, color = strategy, alpha = alpha)) +
    geom_path(aes(group = strategy), size = 1) +
    geom_point(aes(shape = (bias / itt < 0.02)), size = 2) +
    geom_polygon(
        data = convex_hull_LTS,
        fill = DARK_SECONDARY_COLOR, color = NA, alpha = 0.3
    ) +
    geom_path(
        data = convex_hull_LTS,
        linetype = "dashed", color = DARK_SECONDARY_COLOR, alpha = 1
    ) +
    annotate(
        "text", x = 173 * 10^6, y = 300000, label = "LTS-IPW", color = DARK_SECONDARY_COLOR
    ) +
    geom_label_repel(aes(label = label), segment.size = 0.2, point.padding = 0.5, nudge_y = 1, show.legend = FALSE) +
    scale_shape_manual(values = c(21, 16), guide = FALSE) +
    scale_color_manual(guide = FALSE, values = c(
        LTS = DARK_SECONDARY_COLOR,
        TS = SECONDARY_COLOR,
        actual = "black",
        ETC = MAIN_COLOR)
    ) +
    scale_y_reverse(label = scales::label_number_si()) +
    scale_x_continuous(label = scales::label_number_si(accuracy = 0.1)) +
    coord_cartesian(ylim = c(750000, 100000), xlim = c(random_split_welfare, 174.5 * 10^6)) +
    labs(x = "Welfare ($)", y = "MSE")
saveChart("welfare-vs-mse-jtpa")
