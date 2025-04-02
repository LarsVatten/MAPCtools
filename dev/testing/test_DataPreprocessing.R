# Uncleaned NHIS data: NHIS_data

NHIS_data <- add_cohort_column(NHIS_data)

NHIS_data <- as.APC.df(NHIS_data, age="age", period="period")

group_data <- MAPCtools::group_df(data, group_by=c("age", "period"), stratify_by = c("education"))

group_APC_data <- group_APC_df(data, "actual_age", "year", stratify_by = "education")
group_vars(group_APC_data)
group_keys(group_APC_data)



aggregated_data <- aggregate_df(data, gaussian=c("bmi"))
aggregated_group_data <- aggregate_grouped_df(group_APC_data, gaussian=c("bmi"), multinomial = c("race", "Stratum"), binomial="female")


