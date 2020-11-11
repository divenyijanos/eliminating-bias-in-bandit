# Construct the list of record ids for the original adult analysis sample of
# 11,204 observations (the variable defining this sample is not included in the
# public JTPA files) using jtpa.raw downloaded from
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=hdl:1902.1/11300
# DOI: 1902.1/11300
# Replication data for:
# "Instrumental Variables Estimates of the Effect of Subsidized Training
# on the Quantiles of Trainee Earnings"
# by Alberto Abadie and Joshua Angrist and Guido Imbens (2002)
# treated = actually got the treatment

abadie_et_al_sample <- fread('data/jtpa.raw') %>%
    .[, .(id = V1, treated = factor(V4, labels = c('non-trainee', 'trainee')))]


# Import variables for these observations from the JTPA public release file
# provided by the W.E. Upjohn Institute for Employment Research
# http://www.upjohn.org/sites/default/files/erdc/data/jtpa_national_evaluation.zip

jtpa_public_release <- foreign::read.dta('data/jtpa_national_evaluation/Data/expbif/expbif.dta') %>%
    data.table() %>%
    .[, .(
        id = as.integer(recid),
        assignment = factor(ra_stat, labels = c('treatment', 'control')),
        age = as.numeric(age),
        sex = factor(sex, labels = c('male', 'female')),
        education_level = as.numeric(bfeduca),
        pre_program_earnings = as.numeric(yearearn)
    )]

# newern01-newern30 : participants' earnings in 30 months follow-up

jtpa_followup_earnings <- foreign::read.dta('data/jtpa_national_evaluation/Data/replacement_files/earns2.dta') %>%
    data.table() %>%
    .[, earnings_30m := rowSums(.SD), .SDcols = grep('newern', names(.))] %>%
    .[, .(id = as.integer(recid), earning_1m = newern01, earnings_30m)]


# merge & drop missing values

jtpa_sample <- merge(abadie_et_al_sample, jtpa_public_release, by = 'id') %>%
    merge(jtpa_followup_earnings, by = 'id')

fillWithNA <- function(variable, na_value) {
    ifelse(variable == na_value, NA, variable)
}

jtpa_sample[, `:=`(
    education_level = fillWithNA(education_level, 99),
    pre_program_earnings = fillWithNA(pre_program_earnings, 99999)
)]

fwrite(jtpa_sample, 'data/jtpa_sample.csv')
