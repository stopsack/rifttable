#' Results Tables for Epidemiology
#'
#' @description This function displays descriptive
#' and inferential results for binary, continuous, and survival data
#' in the format of a table stratified by exposure and, if requested, by
#' effect modifiers.
#'
#' This function is intended only for tabulations of final results.
#' Model diagnostics for regression models need to be conducted separately.
#'
#' @param design Design matrix (data frame) that sets up the table.
#'   See Details.
#' @param data Dataset to be used for all analyses.
#' @param layout Optional. \code{"rows"} uses the \code{design} as rows and
#'   exposure categories as columns. \code{"cols"} is the
#'   opposite: \code{design} as columns and exposure categories as rows.
#'   Defaults to \code{"rows"}.
#' @param factor Optional. Used for \code{type = "rates"}: Factor to multiply
#'   events per person-time by. Defaults to \code{1000}.
#' @param risk_percent Optional. Show risk and risk difference estimates in
#'   percentage points instead of proportions. Defaults to \code{TRUE}.
#' @param risk_digits Optional. Number of decimal digits to show for risks/
#'   cumulative incidence. Defaults to \code{2} for \code{risk_percent = FALSE}
#'   and to \code{0} for \code{risk_percent = TRUE}. Can override for each line
#'   in \code{type}.
#' @param diff_digits Optional. Number of decimal digits to show for
#'   rounding of means and mean difference estimates. Defaults to \code{2}.
#' @param ratio_digits Optional. Number of decimal digits to show for ratio
#'   estimates. Defaults to \code{2}. Can override for each line in \code{type}.
#' @param rate_digits Optional. Number of decimal digits to show for rates.
#'   Defaults to \code{1}. Can override for each line in \code{type}.
#' @param to Optional. Separator between the lower and the upper bound
#'   of the 95% confidence interval (and interquartile range for medians).
#'   Defaults to \code{" to "} for means, medians, and differences in means or
#'   quantiles; defaults to \code{"-"} otherwise.
#' @param type2_layout Optional. If a second estimate is requested via
#'   \code{type2} in the \code{design} matrix, display it as in rows below
#'   (\code{"rows"}) or columns (\code{"columns"}) to the right. Defaults to
#'   \code{"rows"}.
#' @param custom Optional. Defaults to \code{NULL}. A custom function (or a
#'   \code{\link{list}} of such functions), that can be called via
#'   \code{type = "custom"} (or \code{type = "custom2"}, for example).
#'   Must have at minimum the arguments \code{data} and
#'   \code{...}, i.e., be defined as \code{function(data, ...)}.
#'   Custom functions receive \code{rifttable}'s dataset \code{data}, where
#'   the exposure variable has been renamed to \code{.exposure}. All other
#'   variables are unchanged. All arguments of \code{rifttable} and the elements of
#'   the  \code{design} dataset are passed along as arguments to the custom
#'   function. Functions must return a data frame or tibble with as many rows as
#'   exposure levels and two columns: an exposure column, \code{.exposure}, and
#'   a result column per exposure level, \code{res}, of type \code{character}.
#'   All estimation steps need to be done by the custom function, such as
#'   subsetting to the `stratum` of the `effect_modifier` or rounding of
#'   numeric estimates, as applicable. See examples.
#' @param overall Optional. Defaults to \code{FALSE}. Add a first column with
#'   unstratified estimates to an exposure-stratified table? Elements will be
#'   shown only for absolute estimates (e.g., \code{type = "mean"}) and blank
#'   for comparatible estimates (e.g., mean difference via
#'   \code{type = "diff"}).
#'
#' @details The main input parameter is the dataset \code{design}.
#'   Always required are the column \code{type} (the type of requested
#'   statistic, see below), as well as \code{outcome} for binary outcomes or
#'   \code{time} and \code{event} for survival outcomes:
#'
#'   *  \code{label} A label for each row (or column). If missing, \code{type}
#'        will be used as the label.
#'   *  \code{exposure} Optional. The exposure variable. Must be categorical
#'        (factor or logical). If missing (\code{NA}), then an unstratified
#'        table with absolute estimates only will be returned.
#'   *  \code{outcome} The outcome variable for non-survival data
#'        (i.e., whenever \code{event} and \code{time} are not used).
#'        For risk/prevalence data, this variable must be \code{0}/\code{1}
#'        or \code{FALSE}/\code{TRUE}.
#'   *  \code{time} The time variable for survival data. Needed for,
#'        e.g., \code{type = "hr"} and \code{type = "rate"}
#'        (i.e., whenever \code{outcome} is not used).
#'   *  \code{time2} The second time variable for late entry models.
#'        Only used in conjunction with \code{time}. If provided,
#'        \code{time} will become the entry time and \code{time2}
#'        the exit time, following conventions of
#'        \code{\link[survival]{Surv}}.
#'   *  \code{event} The event variable for survival data.
#'        Events are typically \code{1}, censored observations \code{0}.
#'        Needed for, e.g., \code{type = "hr"} and \code{type = "rate"}
#'        (i.e., whenever \code{outcome} is not used).
#'   *  \code{trend} Optional. For regression models, a continuous
#'        representation of the exposure, for which a slope per one unit
#'        increase ("trend") will be estimated. Must be a numeric variable.
#'        If joint models for \code{exposure} and \code{effect_modifier} are
#'        requested, trends are still reported within each stratum of the
#'        \code{effect_modifier}. Use \code{NA} to leave blank.
#'   *  \code{effect_modifier} Optional. A categorical effect modifier variable.
#'        Use \code{NA} to leave blank.
#'   *  \code{stratum} Optional. A stratum of the effect modifier.
#'        Use \code{NULL} to leave blank. \code{NA} will evaluate
#'        observations with missing data for the \code{effect_modifier}.
#'   *  \code{confounders} Optional. A string in the format
#'        \code{"+ var1 + var2"} that will be substituted into
#'        into \code{formula = exposure + confounders}.
#'        Use \code{NA} or \code{""} (empty string) to leave blank; the default.
#'        For Cox models, can add \code{"+ strata(site)"}
#'        to obtain models with stratification by, e.g., \code{site}.
#'        For Poisson models, can add \code{"+ offset(log(persontime))"}
#'        to define, e.g., \code{persontime} as the offset.
#'   *  \code{type} The statistic requested (case-insensitive):
#'
#'      Comparative estimates with 95% confidence intervals:
#'
#'      * \code{"hr"} Hazard ratio from Cox proportional
#'        hazards regression.
#'      * \code{"irr"} Incidence rate ratio for count outcomes
#'        from Poisson regression model.
#'      * \code{"irrrob"} Ratio for other outcomes
#'        from Poisson regression model with robust (sandwich) standard errors.
#'      * \code{"rr"} Risk ratio (or prevalence ratio)
#'        from \code{\link[risks]{riskratio}}. Can request specific model
#'        fitting  approach and, for marginal
#'        standardization only, the number of bootstrap repeats.
#'        Examples: \code{"rrglm_start"} or \code{"rrmargstd 2000"}.
#'      * \code{"rd"} Risk difference (or prevalence difference)
#'        from \code{\link[risks]{riskdiff}}. Can request model fitting
#'        approach and bootstrap repeats as for \code{"rr"}.
#'      * \code{"diff"} Mean difference from linear model.
#'      * \code{"quantreg"} Quantile difference from quantile regression using
#'        \code{\link[quantreg]{rq}} with \code{method = "fn"}.
#'        By default, this is the difference in medians. For a different
#'        quantile, e.g., the 75th percentile, use \code{"quantreg 0.75"}.
#'      * \code{"fold"} Fold change from generalized linear
#'        model with log link (i.e., ratio of arithmetic means).
#'      * \code{"foldlog"} Fold change from linear
#'        model after log transformation of the outcome
#'        (i.e., ratio of geometric means).
#'      * \code{"or"} Odds ratio from logistic regression.
#'      * \code{"survdiff"} Difference in survival from Kaplan-Meier estimator.
#'        Provide time horizon, e.g., \code{"survdiff 2.5"} to evaluate
#'        differences in survival at 2.5 years. Cannot not handle confounders.
#'        Uses \code{\link[rifttable]{survdiff_ci}}.
#'      * \code{"cumincdiff"} Difference in cumulative incidence from
#'        Kaplan-Meier estimator. Provide time horizon, e.g.,
#'        \code{"cumincdiff 2.5"} to evaluate differences in cumulative
#'        incidence at 2.5 years. Cannot not handle confounders.
#'        Uses \code{\link[rifttable]{survdiff_ci}}.
#'
#'      Absolute estimates per exposure category:
#'
#'      * \code{"events"} Event count.
#'      * \code{"time"} Person-time.
#'      * \code{"outcomes"} Outcome count.
#'      * \code{"total"} Number of observations.
#'      * \code{"events/time"} Events slash person-time.
#'      * \code{"events/total"} Events slash number of observations.
#'      * \code{"cases/controls"} Cases and non-cases (events and non-events);
#'        useful for case-control studies.
#'      * \code{"risk"} Risk (or prevalence), calculated as a proportion,
#'        i.e., outcomes divided by number of observations. Change between
#'        display as proportion or percent using the parameter
#'        \code{risk_percent}.
#'      * \code{"risk (ci)"} Risk with 95% confidence interval
#'        (Wilson score interval for binomial proportions, see
#'        \code{\link[rifttable]{scoreci}}).
#'      * \code{"cuminc"} Cumulative incidence ("risk") from the Kaplan-Meier
#'        estimator. Provide time point (e.g., 1.5-year cumulative incidence)
#'        using \code{"cuminc 1.5"}. If no time point is provided, returns
#'        cumulative incidence at end of follow-up. Change between display as
#'        proportion or percent using the parameter \code{risk_percent}.
#'      * \code{"cuminc (ci)"} Cumulative incidence ("risk") from the
#'        Kaplan-Meier estimator with 95% confidence intervals (Greenwood
#'        standard errors with log transformation, the
#'        default of the survival package/\code{\link[survival]{survfit}}).
#'        Provide time point as in \code{"cuminc"}.
#'      * \code{"surv"} Survival from the Kaplan-Meier
#'        estimator. Provide time point (e.g., 1.5-year survival)
#'        using \code{"surv 1.5"}. If no time point is provided, returns
#'        survival at end of follow-up. Change between display as
#'        proportion or percent using the parameter \code{risk_percent}.
#'      * \code{"surv (ci)"} Survival from the
#'        Kaplan-Meier estimator with 95% confidence interval (Greenwood
#'        standard errors with log transformation, the
#'        default of the survival package/\code{\link[survival]{survfit}}).
#'        Provide time point as in \code{"surv"}.
#'      * \code{"rate"} Event rate: event count divided by person-time,
#'        multiplied by \code{factor}.
#'      * \code{"rate (ci)"} Event rate with 95% confidence interval
#'        (Poisson-type large-sample interval).
#'      * \code{"outcomes (risk)"} A combination: Outcomes
#'        followed by risk in parentheses.
#'      * \code{"outcomes/total (risk)"} A combination: Outcomes slash total
#'        followed by risk in parentheses.
#'      * \code{"events/time (rate)"} A combination: Events slash time
#'        followed by rate in parentheses.
#'      * \code{"medsurv"} Median survival.
#'      * \code{"medsurv (ci)"} Median survival with 95% confidence interval.
#'      * \code{"medfu"} Median follow-up (reverse Kaplan-Meier), equals median
#'        survival for censoring.
#'      * \code{"medfu (iqr)"} Median and interquartile range for follow-up.
#'      * \code{"maxfu"} Maximum follow-up time.
#'      * \code{"mean"} Mean.
#'      * \code{"mean (ci)"} Mean and 95% CI.
#'      * \code{"mean (sd)"} Mean and standard deviation.
#'      * \code{"median"} Median.
#'      * \code{"median (iqr)"} Median and interquartile range.
#'      * \code{"range"} Range: Minimum to maximum value.
#'      * \code{"custom"} A custom function, provided to \code{rifttable} through
#'        the argument \code{custom}. See there for details.
#'        If a list of multiple functions is provided, use \code{"custom1"}
#'        through \code{"custom99"} to select one function.
#'      * \code{"blank"} or \code{""} An empty line.
#'
#'      By default, regression models will be fit separately for each
#'      stratum of the \code{effect_modifier}. Append \code{"_joint"}
#'      to \code{"hr"}, \code{"rr"}, \code{"rd"}, \code{"irr"},
#'      \code{"irrrob"}, \code{"diff"}, \code{"fold"}, \code{"foldlog"},
#'      \code{"quantreg"}, or \code{"or"} to
#'      obtain "joint" models for exposure and effect modifier that have a
#'      single reference category.
#'      Example: \code{type = "hr_joint"}. The reference categories
#'      for exposure and effect modifier are their first factor levels, which
#'      can be changed using \code{\link[forcats]{fct_relevel}}. Note that
#'      the joint model will be fit across all non-missing (\code{NA}) strata
#'      of the effect modifier, even if the \code{design} table does not
#'      request all strata be shown.
#'
#'   * \code{type2} Optional. A second statistic that is added in an adjacent
#'     row or column (global option \code{type2_layout} defaults to \code{"row"}
#'     and can alternatively be set to \code{"column"}). For example, use
#'     \code{type = "events/times", type2 = "hr"} to get both event
#'     counts/person-time and hazard ratios for the same data, exposure,
#'     stratum, confounders, and outcome.
#'   * \code{digits} Optional. The number of digits for rounding an individual
#'     line. Defaults to \code{NA}, where the number of
#'     digits will be determined based on \code{rifttable}'s arguments
#'     \code{risk_percent}, \code{risk_digits}, \code{diff_digits},
#'     \code{ratio_digits}, or \code{rate_digits}, as applicable.
#'   * \code{digits2} Optional. As \code{digits}, for the second
#'     estimate (\code{type2}).
#'   * \code{nmin}. Optional. Suppress estimates with \code{"--"} if a cell
#'     defined by exposure, and possibly the effect modifier, contains fewer
#'     observations or, for survival analyses, fewer events than \code{nmin}.
#'     Defaults to \code{NA}, i.e., to print all estimates.
#'
#' Use \code{\link[tibble]{tibble}}, \code{\link[tibble]{tribble}}, and
#' \code{\link[dplyr]{mutate}} to construct the \code{design} dataset,
#' especially variables that are used repeatedly (e.g., \code{exposure, time,
#' event}, or \code{outcome}). See examples.
#'
#' If regression models cannot provide estimates in a stratum, e.g.,
#' because there are no events, then \code{"--"} will be printed. Accompanying
#' warnings need to be suppressed manually, if appropriate, using
#' \code{suppressWarnings(rifttable(...))}.
#'
#' @return Tibble. Get formatted output as a gt table by passing on to
#'   \code{\link[rifttable]{rt_gt}}.
#' @export
#'
#' @references
#' Greenland S, Rothman KJ (2008). Introduction to Categorical Statistics. In:
#' Rothman KJ, Greenland S, Lash TL. Modern Epidemiology, 3rd edition.
#' Philadelpha, PA: Lippincott Williams & Wilkins. Page 242.
#' (Poisson/large-sample approximation for variance of incidence rates)
#'
#' @examples
#' # Load 'cancer' dataset from survival package (Used in all examples)
#' data(cancer, package = "survival")
#'
#' # The exposure (here, 'sex') must be categorical
#' cancer <- cancer |>
#'   tibble::as_tibble() |>
#'   dplyr::mutate(sex = factor(sex, levels = 1:2,
#'                              labels = c("Men", "Women")),
#'                 time = time / 365.25,
#'                 status = status - 1)
#'
#'
#' # Example 1: Binary outcomes (use 'outcome' variable)
#' # Set table design
#' design1 <- tibble::tibble(
#'   label = c("Outcomes",
#'             "Total",
#'             "Outcomes/Total",
#'             "Risk",
#'             "Risk (CI)",
#'             "Outcomes (Risk)",
#'             "Outcomes/Total (Risk)",
#'             "RR",
#'             "RD")) |>
#'   dplyr::mutate(type = label,
#'                 exposure = "sex",
#'                 outcome = "status")
#'
#' # Generate rifttable
#' rifttable(design = design1, data = cancer)
#'
#' # Use 'design' as columns (selecting RR and RD only)
#' rifttable(design = design1 |>
#'                   dplyr::filter(label %in% c("RR", "RD")),
#'        data = cancer,
#'        layout = "cols")
#'
#'
#' # Example 2: Survival outcomes (use 'time' and 'event'),
#' #   with an effect modifier and a confounder
#' # Set table design
#' design2 <- tibble::tribble(
#'   # Elements that vary by row:
#'   ~label,                       ~stratum, ~confounders, ~type,
#'   "**Overall**",                NULL,     "",           "blank",
#'   "  Events",                   NULL,     "",           "events",
#'   "  Person-years",             NULL,     "",           "time",
#'   "  Rate/1000 py (95% CI)",    NULL,     "",           "rate (ci)",
#'   "  Unadjusted HR (95% CI)",   NULL,     "",           "hr",
#'   "  Age-adjusted HR (95% CI)", NULL,     "+ age",      "hr",
#'   "",                           NULL,     "",           "blank",
#'   "**Stratified models**",      NULL,     "",           "",
#'   "*ECOG PS1* (events/N)",      1,        "",           "events/total",
#'   "  Unadjusted",               1,        "",           "hr",
#'   "  Age-adjusted",             1,        "+ age",      "hr",
#'   "*ECOG PS2* (events/N)",      2,        "",           "events/total",
#'   "  Unadjusted",               2,        "",           "hr",
#'   "  Age-adjusted",             2,        "+ age",      "hr",
#'   "",                           NULL,     "",           "",
#'   "**Joint model**, age-adj.",  NULL,     "",           "",
#'   "  ECOG PS1",                 1,        "+ age",      "hr_joint",
#'   "  ECOG PS2",                 2,        "+ age",      "hr_joint") |>
#'   # Elements that are the same for all rows:
#'   dplyr::mutate(exposure = "sex",
#'                 event = "status",
#'                 time = "time",
#'                 effect_modifier = "ph.ecog")
#'
#' # Generate rifttable
#' rifttable(design = design2,
#'        data = cancer |> dplyr::filter(ph.ecog %in% 1:2))
#'
#'
#' # Example 3: Get two estimates using 'type' and 'type2'
#' design3 <- tibble::tribble(
#'   ~label,     ~stratum, ~type,          ~type2,
#'   "ECOG PS1", 1,        "events/total", "hr",
#'   "ECOG PS2", 2,        "events/total", "hr") |>
#'   dplyr::mutate(exposure = "sex",
#'                 event = "status",
#'                 time = "time",
#'                 confounders = "+ age",
#'                 effect_modifier = "ph.ecog")
#'
#' rifttable(design = design3,
#'        data = cancer |> dplyr::filter(ph.ecog %in% 1:2))
#'
#' rifttable(design = design3,
#'        data = cancer |> dplyr::filter(ph.ecog %in% 1:2),
#'        layout = "cols", type2_layout = "cols")
#'
#'
#' # Example 4: Continuous outcomes (use 'outcome' variable);
#' # request rounding to 1 decimal digit in some cases;
#' # add continuous trend (slope per one unit of the 'trend' variable)
#' tibble::tribble(
#'   ~label,                   ~stratum, ~type,        ~digits,
#'   "Marginal mean (95% CI)", NULL,     "mean (ci)",  1,
#'   "  Men",                  "Men",    "mean",       NA,
#'   "  Women",                "Women",  "mean",       NA,
#'   "",                       NULL,     "",           NA,
#'   "Stratified model",       NULL,     "",           NA,
#'   "  Men",                  "Men",    "diff",       1,
#'   "  Women",                "Women",  "diff",       1,
#'   "",                       NULL,     "",           NA,
#'   "Joint model",            NULL,     "",           NA,
#'   "  Men",                  "Men",    "diff_joint", NA,
#'   "  Women",                "Women",  "diff_joint", NA) |>
#'   dplyr::mutate(exposure = "ph.ecog_factor",
#'                 trend = "ph.ecog",
#'                 outcome = "age",
#'                 effect_modifier = "sex") |>
#'   rifttable(data = cancer |>
#'                   dplyr::filter(ph.ecog < 3) |>
#'                   dplyr::mutate(ph.ecog_factor = factor(ph.ecog)))
#'
#'
#' # Example 5: Get formatted output for Example 2 (see above)
#' \dontrun{
#' rifttable(design = design2,
#'        data = cancer |> dplyr::filter(ph.ecog %in% 1:2)) |>
#'   rt_gt(md = 1)  # get markdown formatting in first column ('label')
#' }
#'
#'
#' # Example 6: Using custom functions and adding an
#' # overall unstratified estimate
#'
#' library(dplyr)  # for easier data handling
#'
#' # Define first, very rudimentary function
#' my_custom_fun1 <- function(data, ...) {
#'   data |>
#'     group_by(.exposure) |>
#'     # Directly referencing the variable 'status'--not portable
#'     summarize(res = paste("mean =", mean(status)))
#' }
#'
#' # Define second, improved function
#' my_custom_fun2 <- function(data, outcome, ...) {
#'   # 'outcome', like all columns of the 'design' matrix, is one argument
#'   # that is passed along to custom functions
#'   data |>
#'     group_by(.exposure) |>
#'     # This improved function will work in other data sets
#'     select(.exposure, variable = {{ outcome }}) |>
#'     summarize(res = paste(round(mean(variable), digits = 3)))
#' }
#'
#' tibble::tribble(
#'   ~label,                         ~type,
#'   "Mean (SD), built-in function", "mean (sd)",
#'   "Mean: 1st custom function",    "custom1",
#'   "Mean: 2nd custom function",    "custom2") |>
#'   mutate(exposure = "sex",
#'          outcome = "status") |>
#'   rifttable(data = cancer,
#'          custom = list(my_custom_fun1, my_custom_fun2),
#'          overall = TRUE)
#'
#' @section Example Output (see Example 5):
#' \if{html}{\figure{rifttable.png}{options: width=70\%}}
rifttable <- function(
    design,
    data,
    layout = "rows",
    factor = 1000,
    risk_percent = TRUE,
    risk_digits = dplyr::if_else(
      risk_percent == TRUE,
      true = 0,
      false = 2),
    diff_digits = 2,
    ratio_digits = 2,
    rate_digits = 1,
    to = NULL,
    type2_layout = "rows",
    custom = NULL,
    overall = FALSE) {
  if(!is.data.frame(design))
    stop("No 'design' data frame/tibble was provided.")

  if(!("type"       %in% names(design))) {
    stop(paste("The 'design' data frame must contain a 'type' column",
               "specifying the requested statistics."))
  }
  if(!("label"       %in% names(design))) design$label       <- design$type
  if(!("exposure"    %in% names(design))) design$exposure    <- NA
  if(!("event"       %in% names(design))) design$event       <- NA
  if(!("time"        %in% names(design))) design$time        <- NA
  if(!("time2"       %in% names(design))) design$time2       <- NA
  if(!("outcome"     %in% names(design))) design$outcome     <- NA
  if(!("trend"       %in% names(design))) design$trend       <- NA
  if(!("confounders" %in% names(design))) design$confounders <- ""
  if(!("type2"       %in% names(design))) design$type2       <- ""
  if(!("digits"      %in% names(design))) design$digits      <- NA
  if(!("digits2"     %in% names(design))) design$digits2     <- NA
  if(!("nmin"        %in% names(design))) design$nmin        <- NA
  if(!("effect_modifier" %in% names(design) & "stratum" %in% names(design))) {
    design <- design %>%
      dplyr::mutate(effect_modifier = NA, stratum = NA)
  }
  design <- design %>%
    dplyr::mutate(
      type2 = dplyr::if_else(
        is.na(.data$type2),
        true = "",
        false = .data$type2))

  if(sum(!is.na(design$exposure)) > 0) {
    name <- labelled::var_label(dplyr::pull(data, design$exposure[1]))
    if(is.null(name))
      name <- design$exposure[1]

    if(overall == TRUE) {
      if(layout == "rows") {
        return(
          dplyr::bind_cols(
            rifttable(
              design = design %>%
                dplyr::select(-.data$exposure),
              data = data,
              layout = layout,
              factor = factor,
              risk_percent = risk_percent,
              risk_digits = risk_digits,
              diff_digits = diff_digits,
              ratio_digits = ratio_digits,
              rate_digits = rate_digits,
              to = to,
              type2_layout = type2_layout,
              custom = custom,
              overall = FALSE),
            rifttable(
              design = design,
              data = data,
              layout = layout,
              factor = factor,
              risk_percent = risk_percent,
              risk_digits = risk_digits,
              diff_digits = diff_digits,
              ratio_digits = ratio_digits,
              rate_digits = rate_digits,
              to = to,
              type2_layout = type2_layout,
              custom = custom,
              overall = FALSE) %>%
              dplyr::select(-1)))
      } else {
        res_strat <- rifttable(
          design = design,
          data = data,
          layout = layout,
          factor = factor,
          risk_percent = risk_percent,
          risk_digits = risk_digits,
          diff_digits = diff_digits,
          ratio_digits = ratio_digits,
          rate_digits = rate_digits,
          to = to,
          type2_layout = type2_layout,
          custom = custom,
          overall = FALSE)
        return(
          dplyr::bind_rows(
            rifttable(
              design = design %>%
                dplyr::select(-.data$exposure),
              data = data,
              layout = layout,
              factor = factor,
              risk_percent = risk_percent,
              risk_digits = risk_digits,
              diff_digits = diff_digits,
              ratio_digits = ratio_digits,
              rate_digits = rate_digits,
              to = to,
              type2_layout = type2_layout,
              custom = custom,
              overall = FALSE) %>%
              dplyr::rename(!!names(res_strat)[1] := 1),
            res_strat))
      }
    }
  } else {
    name <- "Summary"
  }

  res <- design %>%
    dplyr::mutate(
      type   = stringr::str_to_lower(string = .data$type),
      type2  = stringr::str_to_lower(string = .data$type2),
      index  = dplyr::row_number(),
      result = purrr::pmap(
        .l = list(
          .data$event,
          .data$time, .data$time2,
          .data$outcome,
          .data$exposure,
          .data$effect_modifier,
          .data$stratum,
          .data$confounders,
          .data$type,
          .data$trend,
          .data$digits,
          .data$nmin),
        .f = fill_cells,
        data = data,
        factor = factor,
        risk_percent = risk_percent,
        risk_digits = risk_digits,
        diff_digits = diff_digits,
        ratio_digits = ratio_digits,
        rate_digits = rate_digits,
        to = to,
        custom_fn = custom))

  # simple reshaping if only "type" alone
  if(all(design$type2 == "")) {
    res <- res %>%
      dplyr::select(
        .data$index,
        .data$label,
        .data$result) %>%
      tidyr::unnest(cols = .data$result)
    if(layout == "rows") {
      res <- res %>%
        tidyr::pivot_wider(
          names_from = .data$.exposure,
          values_from = .data$res,
          values_fill = "") %>%
        dplyr::rename(!!name := .data$label) %>%
        dplyr::select(-.data$index)
      # capture rows that are indented for rt_gt()
      attr(res, which = "rt_gt.indent2") <- stringr::str_which(
        string = res %>% dplyr::pull(1),
        pattern = "^[:blank:]{2,3}")
      attr(res, which = "rt_gt.indent4") <- stringr::str_which(
        string = res %>% dplyr::pull(1),
        pattern = "^[:blank:]{4,}")
      return(res)
    } else {
      if(sum(duplicated(design$label)) > 0 |
         "" %in% design$label) {
        res %>%
          tidyr::pivot_wider(
            names_from = c(.data$index, .data$label),
            values_from = .data$res,
            values_fill = "")
      } else {
        res %>%
          dplyr::select(-.data$index) %>%
          tidyr::pivot_wider(
            names_from = .data$label,
            values_from = .data$res,
            values_fill = "") %>%
          dplyr::rename(!!name := .data$.exposure)
      }
    }

    # handle "type" and "type2" together
  } else {
    res <- res %>%
      dplyr::mutate(
        result2 = purrr::pmap(
          .l = list(
            .data$event,
            .data$time, .data$time2,
            .data$outcome,
            .data$exposure,
            .data$effect_modifier,
            .data$stratum,
            .data$confounders,
            .data$type2,  # !
            .data$trend,
            .data$digits2,  # !
            .data$nmin),
          .f = fill_cells,
          data = data,
          factor = factor,
          risk_percent = risk_percent,
          risk_digits = risk_digits,
          diff_digits = diff_digits,
          ratio_digits = ratio_digits,
          rate_digits = rate_digits,
          to = to,
          custom_fn = custom)) %>%
      dplyr::mutate(result = purrr::map2(
        .x = .data$result,
        .y = .data$result2,
        .f = ~dplyr::full_join(
          .x,
          .y,
          by = ".exposure",
          suffix = c(".1", ".2")))) %>%
      dplyr::select(
        .data$index,
        .data$label,
        .data$result) %>%
      tidyr::unnest(cols = .data$result) %>%
      tidyr::pivot_longer(
        cols = c(.data$res.1, .data$res.2),
        names_to = "whichres",
        values_to = "value") %>%
      dplyr::mutate(
        value = dplyr::if_else(
          is.na(.data$value),
          true = "", false = .data$value))
    if(layout == "rows") {
      if(type2_layout == "rows") {
        res <- res %>%
          tidyr::pivot_wider(
            names_from = .data$.exposure,
            values_from = .data$value,
            values_fill = "") %>%
          dplyr::group_by(.data$index) %>%
          dplyr::mutate(
            label = dplyr::if_else(
              dplyr::row_number() == 1,
              true = .data$label,
              false = "")) %>%
          dplyr::ungroup() %>%
          dplyr::rename(!!name := .data$label) %>%
          dplyr::select(-.data$index, -.data$whichres)
      } else {
        res <- res %>%
          dplyr::mutate(
            .exposure = dplyr::if_else(
              .data$whichres == "res.1",
              true = paste0(.data$.exposure),
              false = paste0(.data$.exposure,
                             " "))) %>%
          dplyr::select(-.data$whichres) %>%
          tidyr::pivot_wider(
            names_from = .data$.exposure,
            values_from = .data$value,
            values_fill = "") %>%
          dplyr::rename(!!name := .data$label) %>%
          dplyr::select(-.data$index)
      }
      if(type2_layout == "rows") {
        attr(res, which = "rt_gt.indent2") <- union(
          stringr::str_which(
            string = res %>% dplyr::pull(1),
            pattern = "^[:blank:]{2,3}"),
          which(!(1:nrow(res) %% 2)))
      } else {
        attr(res, which = "rt_gt.indent2") <- stringr::str_which(
          string = res %>% dplyr::pull(1),
          pattern = "^[:blank:]{2,3}")
      }
      attr(res, which = "rt_gt.indent4") <- stringr::str_which(
        string = res %>% dplyr::pull(1),
        pattern = "^[:blank:]{4,}")
      return(res)

    } else {
      if(type2_layout == "rows") {
        if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
          res <- res %>%
            tidyr::pivot_wider(
              names_from = c(.data$index, .data$label),
              values_from = .data$value,
              values_fill = "")
        } else {
          res <- res %>%
            dplyr::select(-.data$index) %>%
            tidyr::pivot_wider(
              names_from = .data$label,
              values_from = .data$value,
              values_fill = "")
        }
        res %>%
          dplyr::group_by(.data$.exposure) %>%
          dplyr::mutate(
            .exposure = dplyr::if_else(
              .data$whichres == "res.1",
              true = paste0(.data$.exposure),
              false = "")) %>%
          dplyr::ungroup() %>%
          dplyr::select(-.data$whichres) %>%
          dplyr::rename(!!name := .data$.exposure)
      } else {
        if(sum(duplicated(design$label)) > 0 | "" %in% design$label) {
          res %>%
            dplyr::mutate(
              label = dplyr::if_else(
                .data$whichres == "res.1",
                true = .data$label,
                false = paste0(.data$label,
                               " "))) %>%
            dplyr::select(-.data$whichres) %>%
            tidyr::pivot_wider(
              names_from = c(.data$index, .data$label),
              values_from = .data$value,
              values_fill = "") %>%
            dplyr::rename(!!name := .data$.exposure)
        } else {
          res %>%
            dplyr::mutate(
              label = dplyr::if_else(
                .data$whichres == "res.1",
                true = .data$label,
                false = paste0(.data$label,
                               " "))) %>%
            dplyr::select(-.data$whichres, -.data$index) %>%
            tidyr::pivot_wider(
              names_from = .data$label,
              values_from = .data$value,
              values_fill = "") %>%
            dplyr::rename(!!name := .data$.exposure)
        }
      }
    }
  }
}
