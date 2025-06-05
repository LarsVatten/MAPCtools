p1 <- plot_missing_APC_strata(female_data, group_vars = c("actual_age", "year", "race", "education"),
                              x_var = "actual_age", y_var="year",
                              facet_rows = "race", facet_cols="education",
                              facet_labeller = list(race = list("1"="White", "2"="Black", "3"="Hispanic", "4"="Other"),
                                                    education = list("1" = "LHS/GED", "2" = "HS", "3" = "SC/AA", "4" = "BA+")),
                              x_breaks = seq(25, 85, by=10), y_breaks=seq(1997, 2023, by = 10))

p2 <- plot_binned_counts(female_data, "actual_age", "education", bin_size = 10)


p3 <- plot_counts_edu_race(
  df            = female_data,
  time_var      = "birth_year",
  education_var = "education",
  race_var      = "race",
  bin_size      = 12)


