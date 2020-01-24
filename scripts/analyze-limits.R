source("global.R")

CHART_FORMAT <- "eps"

tes_by_setups <- collectInterimResults("TEBySetups", n = 10000, sd = 10)
welfare_by_setups <- collectInterimResults("WelfareBySetups", n= 10000, sd = 10)

limit_summary <- merge(
    tes_by_setups[method == "IPWE", .(TE = mean, bias = mean - 1, mse, sign_pct, method, batch_size, limit)],
    welfare_by_setups[, .(welfare = mean, batch_size, limit)],
    by = c("batch_size", "limit")
)

limit_summary <- limit_summary[limit == 0, base_welfare := welfare] %>%
    .[, base_welfare := max(base_welfare, na.rm = TRUE), batch_size] %>%
    .[batch_size == 5000 & !(limit %in% c(0.005, 0.01, 0.02)), label := scales::percent(limit, accuracy = 1)] %>%
    merge(CJ(batch_size = BATCH_SIZES, limit = LIMITS), by = c("batch_size", "limit"), all.y = TRUE)


limit_summary[batch_size < 10000] %>%
    .[, relative_welfare := welfare / base_welfare] %>%
    plotXYBy("batch_size", "relative_welfare", by = "limit", box_padding = 0.01) +
    scale_y_continuous(breaks = 1 - LIMITS[LIMITS != 0.005], minor_breaks = 1 - LIMITS[LIMITS != 0.005]) +
    labs(y = "Relative expected welfare to (unlimited = 1)")
saveChart("welfare-by-limits")


limit_summary[batch_size < 10000] %>%
    .[, max_welfare := max(welfare, na.rm = TRUE), limit] %>%
    .[welfare == max_welfare]

limit_summary[batch_size < 10000] %>%
    plotXYBy("batch_size", "welfare", by = "limit", box_padding = 0.01) +
    scale_y_continuous(breaks = 1 - LIMITS[LIMITS != 0.005], minor_breaks = 1 - LIMITS[LIMITS != 0.005]) +
    labs(y = "Relative expected welfare to (unlimited = 1)")
saveChart("absolute-welfare-by-limits")

limit_summary[batch_size < 10000] %>%
    .[limit %in% c(0.15, 0.1, 0.05), label := NA] %>%
    .[limit == c(0.005) & batch_size == 5000, label := "0.5%"] %>%
    plotXYBy("batch_size", "mse", by = "limit", box_padding = 0.01) +
    labs(y = "MSE of IPWE")
saveChart("mse-by-limits")



welfare_accumulation <- collectInterimResults("WelfareAccumulationBySetups", n = 10000, sd = 10)

melt(welfare_accumulation[batch_size == 10], id.vars = c("limit", "batch1000"), measure.vars = c("min", "inferior_pct")) %>%
    ggplot(aes(batch1000 * 1000, value, color = factor(limit))) +
    geom_line() +
    facet_wrap(~ variable, scales = "free")
