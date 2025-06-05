load("dev/testing/toyData.RData")

aPc_fit <- fit_MAPC(toyData, response="backpain", apc_format = "aPc",
                    family = "binomial", strata_col = "education", reference_strata = "1",
                    age = "age", period = "period")


plots <- aPc_fit$plots
plots$age

load("dev/testing/cr_cancer_incidence.RData")

aPc_fit <- fit_MAPC(kolorektal_kreft_insidens, response="count", apc_format = "aPc",
                    family = "poisson", strata_col = "sex", reference_strata = "female",
                    age = "age_group", period = "year", poisson.offset=personyears)

