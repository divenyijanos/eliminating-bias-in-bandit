simulateParticipants <- function(n = 10000, te = 1, sd = 20) {
    set.seed(sd * 10)
    noise <- rnorm(n)
    Y0 <- (noise - mean(noise)) / sd(noise) * sd
    Y1 <- Y0 + te
    data.table(Y0, Y1)
}

runSimulations <- function(run_from, run_to, participants, batch_size, sd, limit = 0, n_treatment = 1) {
    mclapply(seq(run_from, run_to), function(run) {
        set.seed(run)
        cat(glue("{run}."))
        simulateAssignment(
            n = nrow(participants),
            participants[sample(.N)],
            batch_size,
            sd,
            limit,
            n_treatment
        ) %>%
        .[,
            .(run = run, mean_Y = mean(Y), size = .N),
            by = .(batch, assignment)
        ]
    }) %>% rbindlist()
}

simulateAssignment <- function(n, participants, batch_size, sd = NULL, limit = 0, n_treatment = 1) {
    assignees_up_to_now <- createEmptyAssignees()
    number_of_batches <- ceiling(n / batch_size)
    for (batch in seq(number_of_batches)) {
        assignment <- getBatchAssignment(assignees_up_to_now, batch_size, sd, limit, n_treatment)
        assignees_up_to_now <- updateAssignees(assignees_up_to_now, assignment, participants, batch)
    }
    assignees_up_to_now
}

createEmptyAssignees <- function() {
    data.table(
        assignment = numeric(),
        batch = numeric(),
        Y = numeric()
    )
}

getBatchAssignment <- function(assignees_up_to_now, batch_size, sd, limit, n_treatment, ...) {
    unassigned_treatments <- setdiff(
        seq(0, n_treatment),
        (assignees_up_to_now[, unique(assignment)])
    )

    if (length(unassigned_treatments)) {
        assignRCT(batch_size, unassigned_treatments)
    }
    else {
        assignTS(assignees_up_to_now, batch_size, sd, limit)
    }
}

assignRCT <- function(batch_size, treatments) {
    n_treatments <- length(treatments)
    map_dbl(seq(n_treatments), ~{
        floor(batch_size / n_treatments) + ifelse(. <= batch_size %% n_treatments, 1, 0)
    }) %>%
    setNames(treatments)
}

assignTS <- function(assignees_up_to_now, batch_size, sd, limit = 0, n_sim = 10000) {
    draws <- assignees_up_to_now[,
        .(mu = mean(Y), sigma = sd / sqrt(.N)),
        keyby = assignment
    ] %>%
    pmap(~with(list(...), rnorm(n_sim, mu, sigma))) %>%
    as.data.table()

    calculateProbFromTable <- function(table) {
        as.numeric(table / sum(table))
    }

    treatments <- seq(0, length(draws) - 1)
    assignment_probs <- table(draws[, factor(max.col(.SD) - 1, levels = treatments)]) %>%
        calculateProbFromTable()

    assignment <- sample(
        treatments, size = batch_size, replace = TRUE,
        prob = assignment_probs
    ) %>%
        factor(levels = treatments) %>% table()

    setNames(limitAssignment(assignment, limit), treatments)
}

roundStatistically <- function(x) {
    fractional_parts <- x - floor(x)
    if (any(fractional_parts > 0)) {
        x - fractional_parts + rbinom(length(x), 1, fractional_parts)
    } else {
        x
    }
}

limitAssignment <- function(assignment, limit) {
    if (limit > 1 / length(assignment)) {
        stop("Limit is too large relative to number of treatments.")
    }
    missing <- roundStatistically(limit * sum(assignment) - assignment)
    to_reassign <- sum(missing * (missing > 0))
    original_share <- (missing < 0) * missing / sum((missing < 0) * missing)
    limited_assignment <- round(
        assignment + ifelse(missing < 0, -original_share * to_reassign, missing),
        0
    )

    diff_in_assigned <- sum(limited_assignment) - sum(assignment)
    if (diff_in_assigned != 0) {
        limited_assignment[which.max(limited_assignment)] %<>% `-`(., diff_in_assigned)
    }

    limited_assignment
}

updateAssignees <- function(assignees_up_to_now, assignment, participants, batch) {
    participants[(nrow(assignees_up_to_now) + 1) : (nrow(assignees_up_to_now) + sum(assignment))] %>%
        .[, assignment := unlist(map(names(assignment), ~rep(as.numeric(.), assignment[.])))] %>%
        .[, .(batch = batch, assignment, Y = realizeOutcome(assignment, Y0, Y1))] %>%
        rbind(assignees_up_to_now, .)
}

realizeOutcome <- function(assignment, Y0, Y1) {
    assignment * Y1 + (1 - assignment) * Y0
}
