plotXYBy <- function(dt, x, y, by, colors = NULL, size = 1, label_nudge_x = 0, box_padding = 0.5) {
    if (is.null(colors)) {
        colors <- generateGradientColors(dt[, sort(unique(get(by)))])
    }
    ggplot(dt, aes(x = get(x), y = get(y), color = factor(get(by)))) +
        geom_line(size = size) +
        geom_label_repel(
            aes(label = label),
            direction = "y", nudge_x = label_nudge_x, hjust = as.numeric(label_nudge_x < 0),
            segment.size = 0.2, box.padding = box_padding, na.rm = TRUE
        ) +
        scale_color_manual(values = colors, guide = FALSE) +
        scale_x(x) +
        lab_x(x)
}

scale_x <- function(x) {
    switch(x,
        limit = scale_x_continuous(
            breaks = c(0, 0.02, 0.05, 0.1, 0.15, 0.2), minor_breaks = LIMITS,
            labels = scales::percent_format(accuracy = 1)
        ),
        batch_size = scale_x_continuous(
            breaks = c(10, 50, 500, 5000), minor_breaks = BATCH_SIZES, trans = "log"
        ),
        sd = scale_x_continuous(breaks = c(1, 5, 10, 20, 30), minor_breaks = SDS)
    )
}

lab_x <- function(x) {
    switch(x,
        limit = labs(x = "Limit"),
        batch_size = labs(x = "Batch size (log scale)"),
        sd = labs(x = "Standard deviation")
    )
}

saveChart <- function(fig_name, format = CHART_FORMAT, width = 4, height = 4, scale = 0.85) {
    standardSave <- function(...) {
        ggsave(
            file.path("figure", paste0(fig_name, ".", format)),
            width = width, height = height, scale = scale, ...
        )
    }
    switch(format,
        png = standardSave(),
        eps = standardSave(device = cairo_ps)
    )

}

generateGradientColors <- function(variations) {
    scales::seq_gradient_pal(
        GRADIENT_LIMITS[1], GRADIENT_LIMITS[2]
    )(
        seq(0, 1, 1 / (length(variations) - 1))
    ) %>% setNames(variations)
}
