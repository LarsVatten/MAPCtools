#' @importFrom stringr str_split str_extract
#' @importFrom viridis viridis
#' @import ggplot2
NULL

#' Plot Linear Combinations of Age-Period-Cohort Effects by Strata
#'
#' Generates ggplot2 line plots of estimated linear combinations
#' for age, period, and/or cohort effects from an INLA fit, stratified by a factor.
#' Returns a named list of ggplot objects for each requested effect.
#'
#' @param inla_fit An object returned by \code{inla::inla()} containing
#'   \code{summary.lincomb.derived}.
#' @param apc_model Character string of length 1 giving which effects to plot:
#'   a combination of \code{"a"} (age), \code{"p"} (period), and \code{"c"} (cohort),
#'   e.g. \code{"ap"}, \code{"apc"}.
#' @param data A data.frame used to fit \code{inla_fit}, containing columns
#'   for age, period, cohort, and the strata variable.
#' @param strata_col Character name of the factor column in \code{data} defining strata.
#' @param reference_level Character value of \code{strata_col} to use as the reference.
#' @param family Optional character; if \code{NULL}, \code{y_lab} defaults to
#'   \code{"Mean differences"}.  If \code{"gaussian"}, same; if \code{"poisson"},
#'   \code{"Log mean ratio"}; if \code{"binomial"}, \code{"Log odds ratio"}.
#' @param age_ind Character name of the age variable in \code{data} (default \code{"age"}).
#' @param period_ind Character name of the period variable in \code{data} (default \code{"period"}).
#' @param cohort_ind Character name of the cohort variable in \code{data} (default \code{"cohort"}).
#' @param age_title Optional plot title for the age effect.
#' @param period_title Optional plot title for the period effect.
#' @param cohort_title Optional plot title for the cohort effect.
#' @param y_lab Optional y-axis label; if \code{NULL}, set according to \code{family}.
#' @param age_vals Optional numeric vector of x-values for age; defaults to
#'   \code{min(data\$age):max(data\$age)}.
#' @param period_vals Optional numeric vector of x-values for period; defaults to
#'   \code{min(data\$period):max(data\$period)}.
#' @param cohort_vals Optional numeric vector of x-values for cohort; defaults to
#'   \code{min(data\$cohort):max(data\$cohort)}.
#' @param age_breaks Optional vector of breaks for the age plot x-axis.
#' @param age_limits Optional numeric vector of length 2 giving x-axis limits for age.
#' @param period_breaks Optional vector of breaks for the period plot x-axis.
#' @param period_limits Optional numeric vector of length 2 giving x-axis limits for period.
#' @param cohort_breaks Optional vector of breaks for the cohort plot x-axis.
#' @param cohort_limits Optional numeric vector of length 2 giving x-axis limits for cohort.
#' @param PDF_export Logical; if \code{TRUE}, use larger font sizes/layout for PDF output.
#'
#' @return A named list of \code{ggplot} objects.  Elements are
#'   \code{"age"}, \code{"period"}, and/or \code{"cohort"} depending on \code{apc_model}.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Extracts linear combination summaries from \code{inla_fit\$summary.lincomb.derived}.
#'   \item Splits them into age/period/cohort segments based on \code{apc_model}.
#'   \item Constructs a data.frame of means and 95% HPD intervals by strata.
#'   \item Calls an internal \code{plot_time_effect()} to produce the ggplot for each effect.
#' }
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("INLA", quietly = TRUE)) {
#'   # Suppose fit_apc is your INLA model with lincomb.derived
#'   my_plots <- plot_lincombs(
#'     inla_fit    = fit_apc,
#'     apc_model   = "apc",
#'     data        = my_data,
#'     strata_col  = "gender",
#'     reference_level = "male",
#'     family      = "poisson",
#'     age_vals    = seq(30, 80, by = 5)
#'   )
#'   # Display the age effect plot
#'   print(my_plots$age)
#'   }
#' }
#'
#' @export

plot_lincombs <- function(inla_fit, apc_model, data, strata_col, reference_level, family=NULL, age_ind="age", period_ind="period", cohort_ind="cohort",
                          age_title=NULL, period_title=NULL, cohort_title=NULL, y_lab=NULL,
                          age_vals = NULL, period_vals = NULL, cohort_vals = NULL,
                          age_breaks=NULL, age_limits=NULL, period_breaks=NULL, period_limits=NULL, cohort_breaks=NULL, cohort_limits=NULL,
                          PDF_export=F) {


  if(is.null(y_lab)) {
    if(is.null(family)) {y_lab = "Mean differences"}
    else if(family=="gaussian") {
      y_lab = "Mean differences"
    }
    else if(family=="poisson") {y_lab = "Mean ratio"}
    else if(family=="binomial") {y_lab = "Odds ratio"}
    else{y_lab = "Mean differences"}
  }

  if(PDF_export) {gg_theme <- theme(axis.text.x = element_text(size = 25),
                                    axis.text.y = element_text(size=25),
                                    axis.title = element_text(size = 30),
                                    plot.title = element_text(size = 34, hjust = 0.5),
                                    legend.title = element_text(size = 30, hjust=0.5),
                                    legend.text = element_text(size = 28),
                                    legend.position = "bottom",
                                    legend.box = "vertical")
  } else {gg_theme = theme(axis.text.x = element_text(size = 10),
                           axis.text.y = element_text(size=10),
                           axis.title = element_text(size = 12),
                           plot.title = element_text(size = 15, hjust = 0.5),
                           legend.title = element_text(size = 13, hjust=0.5),
                           legend.text = element_text(size = 11),
                           legend.position = "bottom",
                           legend.box = "vertical")}

  legend_title <- paste0(strata_col, " (", reference_level, " is reference): ")

  apc_format <- str_split(apc_model, "")[[1]]

  age_ids <- min(data[[age_ind]]):max(data[[age_ind]])
  period_ids <- min(data[[period_ind]]):max(data[[period_ind]])
  cohort_ids <- min(data[[cohort_ind]]):max(data[[cohort_ind]])


  # Strata
  all_strata <- levels(data[[strata_col]])
  diff_strata <- setdiff(all_strata, reference_level)
  # Number of different strata that is compared:
  n_strata <- length(all_strata)
  n_strata_diff <- length(diff_strata)

  color.palette <- viridis(n_strata_diff) # Colors for differences

  differences_summary <- inla_fit$summary.lincomb.derived

  # Separate each effect:
  age_start <- ("a" %in% apc_format) * 1
  age_end <- ("a" %in% apc_format) * n_strata_diff * max(age_ids)
  age_inds <- age_start:age_end

  period_start <- ("p" %in% apc_format) * (age_end + 1)
  period_end <- ("p" %in% apc_format) * (age_end + n_strata_diff * max(period_ids))
  period_inds <- period_start:period_end

  if (period_end != period_start) {
    cohort_start <- ("c" %in% apc_format) * (age_end + (period_end-period_start) + 2)
    cohort_end <- ("c" %in% apc_format) * (cohort_start -1 + n_strata_diff * max(cohort_ids))
    cohort_inds <- cohort_start:cohort_end
  } else {
    cohort_start <- ("c" %in% apc_format) * (age_end + (period_end-period_start) + 1)
    cohort_end <- ("c" %in% apc_format) * (cohort_start -1 + n_strata_diff * max(cohort_ids))
    cohort_inds <- cohort_start:cohort_end
  }

  plot_list <- list()


  if ("a" %in% apc_format) {
    age_differences <- differences_summary[grepl("^Age = ", rownames(differences_summary)), ]

    age_labels <- rownames(age_differences)

    # Extract the numeric part of the age (after "Age : ")
    age_order <- as.numeric(str_extract(age_labels, "(?<=Age = )\\d+(?=\\sStrata)"))

    # Reorder the dataframe based on the extracted numeric age
    ordered_age_differences <- age_differences[order(age_order), ]

    # Extract labels for differences
    age_labels <- str_extract(rownames(ordered_age_differences), "(?<=Strata = )\\w+")

    print(tail(ordered_age_differences))

    age_data <- data.frame(
      median_differences = ordered_age_differences$`0.5quant`,
      hpd_lower = ordered_age_differences$`0.025quant`,
      hpd_upper = ordered_age_differences$`0.975quant`,
      Strata = factor(age_labels, levels = diff_strata)
    )

    if(!is.null(age_vals)) {
      age_data$x <- rep(age_vals, each=n_strata_diff)
    } else {age_data$x = rep(age_ids, each=n_strata_diff)}

    p_age <- plot_time_effect(age_data, x_lab="Age", y_lab=y_lab, family=family, color_palette = color.palette, plot_theme = gg_theme, legend_title = legend_title)

    plot_list <- c(plot_list, list("age" = p_age))
  }

  if ("p" %in% apc_format) {
    period_differences <- differences_summary[grepl("^Period = ", rownames(differences_summary)), ]
    period_labels <- rownames(period_differences)

    # Extract the numeric part of the period (after "period : ")
    period_order <- as.numeric(str_extract(period_labels, "(?<=Period = )\\d+(?=\\sStrata)"))

    # Reorder the dataframe based on the extracted numeric period
    ordered_period_differences <- period_differences[order(period_order), ]

    # Extract labels for differences
    period_labels <- str_extract(rownames(ordered_period_differences), "(?<=Strata = )\\w+")

    period_data <- data.frame(
      median_differences = ordered_period_differences$`0.5quant`,
      hpd_lower = ordered_period_differences$`0.025quant`,
      hpd_upper = ordered_period_differences$`0.975quant`,
      Strata = factor(period_labels, levels = diff_strata)
    )

    if(!is.null(period_vals)) {
      period_data$x <- rep(period_vals, each=n_strata_diff)
    } else {period_data$x = rep(period_ids, each=n_strata_diff)}


    p_period <- plot_time_effect(period_data, x_lab="Period", y_lab=y_lab, family=family, color_palette = color.palette, plot_theme = gg_theme, legend_title = legend_title)

    plot_list <- c(plot_list, list("period" = p_period))
  }

  if ("c" %in% apc_format) {
    cohort_differences <- differences_summary[grepl("^Cohort = ", rownames(differences_summary)), ]
    cohort_labels <- rownames(cohort_differences)

    # Reorder the dataframe based on the extracted numeric cohort
    cohort_order <- as.numeric(str_extract(cohort_labels, "(?<=Cohort = )\\d+(?=\\sStrata)"))

    # Reorder the dataframe based on the extracted numeric period
    ordered_cohort_differences <- cohort_differences[order(cohort_order), ]

    # Extract labels for differences
    cohort_labels <- str_extract(rownames(ordered_cohort_differences), "(?<=Strata = )\\w+")

    cohort_data <- data.frame(
      median_differences = ordered_cohort_differences$`0.5quant`,
      hpd_lower = ordered_cohort_differences$`0.025quant`,
      hpd_upper = ordered_cohort_differences$`0.975quant`,
      Strata = factor(cohort_labels, levels = diff_strata)
    )


    if(!is.null(cohort_vals)) {
      cohort_data$x = rep(cohort_vals, each=n_strata_diff)
    } else {cohort_data$x = rep(cohort_ids, each=n_strata_diff)}

    p_cohort <- plot_time_effect(cohort_data, x_lab="Cohort", y_lab=y_lab, family=family, color_palette = color.palette, plot_theme = gg_theme, legend_title = legend_title)

    plot_list <- c(plot_list, list("cohort" = p_cohort))
  }
  plot_list
}
