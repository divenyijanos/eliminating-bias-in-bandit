aggregateSimulations <- function(param_list = NULL, pattern = NULL, distribution = "normal") {
    if (is.null(param_list) == is.null(pattern)) {
        stop("please specify either params or pattern for simulation files")
    }
    if (is.null(pattern)) {
        pattern <- generateNamePatternFromParams(param_list)
    }
    aggregated_data <- readInSimulationFiles(pattern = pattern, add_params = FALSE, distribution = distribution)

    fwrite(aggregated_data, glue("{SIMULATION_FOLDER}/{distribution}/{pattern}full.csv"))
    cat("aggregated file written. Zipping... ")
    system(glue("gzip {SIMULATION_FOLDER}/{distribution}/{pattern}full.csv"))
    cat("done.\n")
}


readInSimulationFiles <- function(params = NULL, pattern = NULL, add_params = TRUE, distribution = "normal") {
    if (is.null(params) == is.null(pattern)) {
        stop("please specify either params or pattern for simulation files")
    }
    if (is.null(pattern)) {
        pattern <- pmap_chr(params, ~generateNamePatternFromParams(list(...))) %>%
            glue_collapse("|")
    }
    files <- list.files(glue("{SIMULATION_FOLDER}/{distribution}"), pattern = pattern, full.names = TRUE)
    cat(glue("Reading in {length(files)} simulation file(s)... "))
    data <- map(files, ~{
        if (add_params) {
            params <- extractParamsFromFilename(.x)
            dt <- fread(.x)
            if (nrow(dt) > 0) {
                dt[, c(.SD, params)]
            } else {
                data.table()
            }
        } else {
            fread(.x)
        }
    }) %>% rbindlist()
    cat("done.\n")
    data
}

extractParamsFromFilename <- function(filename) {
    params_and_values <- str_match_all(filename, "([a-z]+[_]?[a-z]*)([0-9]+[.]?[0-9]*)")[[1]]
    setDT(as.list(as.numeric(params_and_values[, 3]))) %>%
        setnames(params_and_values[, 2])
}

generateInterimResults <- function(params, calculations, distribution = "normal", redo = FALSE) {

    cat("\n\nCalculating for ")
    cat(glue("{distribution} distribution, "))
    cat(map_chr(names(params), ~glue("{.x} = {params[.x]}")) %>% glue_collapse(", "))
    cat("... ")

    filename_pattern <- generateNamePatternFromParams(params)

    interim_files_exists <- file.exists(generateInterimResultFileName(calculations, filename_pattern, distribution))
    if (redo | !all(interim_files_exists)) {
        mean_estimates_by_batch <- readInSimulationFiles(
            pattern = glue("{filename_pattern}"), distribution = distribution
        )
        if (nrow(mean_estimates_by_batch) > 0) {
            to_calc <- calculations[redo | !interim_files_exists]
            cat("Running the followings: ", to_calc, "... ")
            walk(to_calc, ~{
                interim_result <- get(.)(mean_estimates_by_batch)
                fwrite(interim_result, generateInterimResultFileName(., filename_pattern, distribution))
            })
            cat("written to file(s).")
        } else {
            cat("no simulation found.")
        }
    } else {
        cat("skipped.")
    }
    params
}

generateInterimResultFileName <- function(calculation, pattern, distribution = "normal") {
    glue("{INTERIM_RESULT_FOLDER}/{distribution}/{pattern}{str_remove(calculation, 'calculate')}.csv")
}

generateNamePatternFromParams <- function(param_list, regex = TRUE) {
    pattern <- map_chr(names(param_list), ~glue("{.x}{param_list[.x]}-")) %>%
        glue_collapse("")
    if (regex) {
        str_replace_all(pattern, "\\.", "\\\\.")
    } else {
        pattern
    }
}

collectInterimResults <- function(result_name, te = 1, n = 10000, sd = "[0-9]+", batch_size = "[0-9]+", distribution = "normal") {
    param_list <- list(te = te, n = n, sd = sd, batch_size = batch_size)
    pattern <- generateNamePatternFromParams(param_list, regex = FALSE)
    map_df(
        list.files(
            glue("{INTERIM_RESULT_FOLDER}/{distribution}"),
            pattern = glue("{pattern}{result_name}"), full.names = TRUE
        ),
        fread
    )
}
