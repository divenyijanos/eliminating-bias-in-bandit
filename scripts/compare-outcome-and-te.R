source("global.R")

SETUP_COLORS <- c(ETC = MAIN_COLOR, IPWE = DARK_SECONDARY_COLOR, `limited IPWE` = DARK_SECONDARY_COLOR, FBTE = THIRD_COLOR, TE = SECONDARY_COLOR)

labelSetups <- function(setup_comparison, type = c("limit", "method")) {
    setup_comparison[
        batch_size == dplyr::nth(unique(batch_size), -2) & setup == "limited IPWE",
        label := scales::percent(limit, accuracy = limit * 100),
        .(sd, setup_detailed)
    ]
}

createConvexHullData <- function(setup_comparison, chosen_setup, n = 10000) {
    rbind(
        setup_comparison[setup == chosen_setup],
        data.table(
            mse = c(setup_comparison[, min(mse)], 9.9, 10),
            welfare = c(n / 2, setup_comparison[, max(welfare)], n / 2),
            setup = chosen_setup,
            bias = 0
        ),
        fill = TRUE
    ) %>%
    .[, .SD[chull(mse, welfare)]] %>%
    .[mse <= 10] %>%
    .[order(mse)]
}


plotWelfareVsMSE <- function(setups, n = 10000, colors = SETUP_COLORS, mse_limits, legend_position) {
    ggplot(mapping = aes(welfare, mse, color = setup))  +
    geom_path(
        data = setups[setup != "limited IPWE"],
        aes(alpha = alpha), size = 1
    ) +
    geom_polygon(
        data = createConvexHullData(setups, "limited IPWE", n),
        fill = SETUP_COLORS["IPWE"], color = NA, alpha = 0.3
    ) +
    geom_path(
        data = createConvexHullData(setups, "limited IPWE", n),
        linetype = "dashed", alpha = 1
    ) +
    scale_color_manual(values = colors, guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = mse_limits, xlim = c(n / 2, n)) +
    labs(x = "Welfare", y = "MSE")
}

# Illustration ----------------------------------------------------------------

# batch_size = 1000
putTogetherSetupsWithETC(10000, 10)[(batch_size == 1000 & limit == 0) | (batch_size == 10000 & setup == "ETC")] %>%
    labelSetups() %>%
    .[is.na(label), label := setup] %>%
    .[, method := ifelse(label %in% c("RCT", "ETC"), "traditional", "bandit")] %>%
    ggplot(aes(welfare, mse, color = method)) +
    geom_point(aes(shape = (bias < 0.02), alpha = alpha), size = 2) +
    geom_label_repel(aes(label = label), segment.size = 0.2, point.padding = 0.5, show.legend = FALSE) +
    # annotate("rect", xmin = 5000, xmax = 7800, ymin = 2.6, ymax = 3.7, fill = "white") +
    annotate("label", x = 4900, y = 2.8, label = "    Biased estimates are hollow", hjust = 0, size = 8 / .pt, label.size = 0) +
    scale_shape_manual(values = c(21, 16), guide = FALSE) +
    scale_color_manual(values = TWO_COLOR_SCHEME) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(3.7, 0), xlim = c(5000, 10000)) +
    labs(x = "Welfare", y = "MSE") +
    theme(
        legend.title = element_blank(),
        legend.position = c(0.18, 0.14),
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill = "white", size = 0)
    )
saveChart("illustration-welfare-vs-te-limited")

# TS only
setups <- putTogetherSetupsWithETC(10000, 10)[limit == 0 & setup == "TE"]
ggplot(setups, aes(welfare, mse)) +
    geom_blank() +
    scale_y_reverse() +
    coord_cartesian(ylim = c(1.5, 0), xlim = c(5000, 10000)) +
    labs(x = "Welfare", y = "MSE")
saveChart("illustration-welfare-vs-te-empty")

ggplot(setups, aes(welfare, mse, color = setup, alpha = alpha)) +
    geom_path(aes(group = setup_detailed), size = 1) +
    geom_point(aes(shape = (bias < 0.02), size = batch_size)) +
    annotate(
        "label", x = 5100, y = 1.4,
        label = "Biased estimates are hollow \nPoint size is proportional to batch size",
        hjust = 0, size = 8 / .pt, label.size = 0
    ) +
    scale_size_continuous(breaks = c(10, 100, 1000), range = c(0.5, 5), guide = FALSE) +
    scale_shape_manual(values = c(21, 16), guide = FALSE) +
    scale_color_manual(values = SETUP_COLORS, guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(1.5, 0), xlim = c(5000, 10000)) +
    labs(x = "Welfare", y = "MSE")
saveChart("illustration-welfare-vs-te-batch-size-te-only")


# TS + ETC + FBTE
setups <- putTogetherSetupsWithETC(10000, 10)[(limit == 0 & setup != "IPWE") | (batch_size == 10000 & setup == "TE")]
ggplot(setups, aes(welfare, mse, color = setup, alpha = alpha)) +
    geom_path(aes(group = setup_detailed), size = 1) +
    geom_point(aes(shape = (bias < 0.02), size = batch_size)) +
    annotate(
        "label", x = 5100, y = 1.4,
        label = "Biased estimates are hollow \nPoint size is proportional to batch size",
        hjust = 0, size = 8 / .pt, label.size = 0
    ) +
    annotate(
        "label", x = 7500, y = 1, label = "ETC", color = SETUP_COLORS["ETC"]
    ) +
    annotate(
        "label", x = 9500, y = 1, label = "TS-FB", color = SETUP_COLORS["FBTE"]
    ) +
    annotate(
        "label", x = 9500, y = 0, label = "TS", color = SETUP_COLORS["TE"]
    ) +
    scale_size_continuous(breaks = c(10, 100, 1000), range = c(0.5, 5), guide = FALSE) +
    scale_shape_manual(values = c(21, 16), guide = FALSE) +
    scale_color_manual(values = SETUP_COLORS, guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(1.5, 0), xlim = c(5000, 10000)) +
    labs(x = "Welfare", y = "MSE (reversed)")
saveChart("illustration-welfare-vs-te-batch-size")

limited_setups <- putTogetherSetupsWithETC(10000, 10)[setup == "limited IPWE"] %>%
    labelSetups()
ggplot(limited_setups, aes(welfare, mse, color = setup, alpha = alpha))  +
    geom_point(aes(shape = (bias < 0.02), size = batch_size)) +
    geom_path(aes(group = setup_detailed), size = 1) +
    geom_polygon(
        data = createConvexHullData(limited_setups, "limited IPWE", n = 10000),
        fill = SETUP_COLORS["IPWE"], color = NA, alpha = 0.3
    ) +
    geom_path(
        data = createConvexHullData(limited_setups, "limited IPWE", n = 10000),
        linetype = "dashed", alpha = 1
    ) +
    geom_label_repel(
        aes(label = label),
        nudge_x = -50, segment.size = 0.2, box.padding = 0.1, point.padding = 0.5,
        show.legend = FALSE, na.rm = TRUE
    ) +
    annotate(
        "text", x = 5100, y = 1.3,
        label = "Biased estimates are hollow \nPoint size is proportional to batch size \nThe shaded area shows the set of possible choices",
        hjust = 0, size = 8 / .pt
    ) +
    scale_size_continuous(breaks = c(10, 100, 1000), range = c(0.5, 5), guide = FALSE) +
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = FALSE) +
    scale_color_manual(values = SETUP_COLORS, guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(1.5, 0), xlim = c(5000, 10000)) +
    labs(x = "Welfare", y = "MSE (reversed)")
saveChart("illustration-welfare-vs-te-batch-size-limited")

all_setups <- putTogetherSetupsWithETC(10000, 10)[!(setup %in% c("limited TE", "IPWE"))]
plotWelfareVsMSE(all_setups, mse_limits = c(1.5, 0)) +
    annotate(
        "label", x = 7500, y = 1, label = "ETC", color = SETUP_COLORS["ETC"]
    ) +
    annotate(
        "label", x = 9500, y = 1, label = "TS-FB", color = SETUP_COLORS["FBTE"]
    ) +
    annotate(
        "label", x = 9500, y = 0, label = "TS", color = SETUP_COLORS["TE"]
    ) +
    annotate(
        "text", x = 7000, y = 0.25, label = "LTS-IPW", color = SETUP_COLORS["IPWE"]
    ) +
    annotate(
        "segment", x = 9000, xend = 9000, y = 1.1, yend = 1.4, arrow = arrow(length = unit(0.1,"cm"))
    ) +
    annotate(
        "text", x = 8500, y = 1.25, label = "Smaller \nbatches", size = 8 / .pt, hjust = 0.5
    )
saveChart("illustration-welfare-vs-te-batch-size-full")

# link by batch size
# frontier: ~ mainly best batch size and play with limit
all_setups %>%
    .[setup == "limited IPWE", setup_detailed := paste("limited IPWE", batch_size)] %>%
    .[setup == "limited IPWE", max_mse := max(mse), by = batch_size] %>%
    .[setup == "limited IPWE" & mse == max_mse, label := batch_size] %>%
    ggplot(aes(welfare, mse, color = setup, alpha = alpha)) +
    geom_point(aes(shape = (bias < 0.02), size = batch_size)) +
    geom_label_repel(aes(label = label), segment.size = 0.2, box.padding = 0.1, point.padding = 0.5, show.legend = FALSE, na.rm = TRUE) +
    geom_path(aes(group = setup_detailed), size = 1) +
    scale_size_continuous(breaks = c(10, 100, 1000), range = c(0.5, 3), guide = FALSE) +
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = FALSE) +
    scale_color_manual(values = SETUP_COLORS, guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(3.7, 0), xlim = c(5000, 10000)) +
    labs(y = "MSE")

n <- 10000
putTogetherSetupsWithETC(n, 30)[setup == "limited IPWE"] %>%
    .[, setup_detailed := paste("limited IPWE", batch_size)] %>%
    .[, max_mse := max(mse), by = batch_size] %>%
    .[mse == max_mse & batch_size %in% c(10, 100, 1000, 5000), label := batch_size] %>%
    .[order(mse)] %>%
    ggplot(aes(welfare, mse, color = factor(batch_size), alpha = alpha)) +
    geom_path(aes(group = setup_detailed)) +
    # geom_label_repel(
    #     aes(label = label),
    #     segment.size = 0.2, box.padding = 0.2, show.legend = FALSE, na.rm = TRUE) +
    scale_color_manual(values = generateGradientColors(BATCH_SIZES), guide = FALSE) +
    scale_y_reverse() +
    coord_cartesian(ylim = c(1.5, 0), xlim = c(n / 2, n)) +
    labs(y = "MSE")


# other n ---------------------------------------------------------------------

NS <- c(2000, 10000, 20000, 40000)

walk(NS, ~{
    all_setups <- putTogetherSetupsWithETC(.x, 10)[!(setup %in% c("limited TE", "IPWE"))]
    plotWelfareVsMSE(all_setups, n = .x, mse_limits = c(1.5, 0)) +
        annotate(geom = "label", label=glue("n == {.x}"), x = .x * 0.9, y = 1.25, parse = TRUE)
    saveChart(glue("welfare-vs-mse-n{.x}"))
})


# other sd --------------------------------------------------------------------
walk(SDS, ~{
    all_setups <- putTogetherSetupsWithETC(10000, .x)[!(setup %in% c("limited TE", "IPWE"))]
    plotWelfareVsMSE(all_setups, mse_limits = c(1.5, 0)) +
    annotate(geom = "label", label=glue("sigma == {.x}"), x = 9000, y = 1.25, parse = TRUE)
    saveChart(glue("welfare-vs-mse-sd{.x}"))
})
