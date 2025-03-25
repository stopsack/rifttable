# rifttable 0.7.0

* Breaking changes:
  + Require base R pipe `|>` and thus R 4.1.
  + Make the `id` variable identifying clustered observations within the same
    individual a global `rifttable()` option for the entire data set, not only 
    for specific estimators.
* New functionality:
  + Expand input checks to missing values in time/event variables, to missing 
    effect modifiers for joint models and their levels, to nonexistent custom 
    estimators, and to empty input data sets.
  + Add `type = "sum"` estimator.
* Internal and bug fixes:
  + Cover entire package by unit tests.
  + Return exposure consistently as a `character`.
  + Let the `design` accept `weight` in addition to `weights`.
  + Require [{risks}](https://stopsack.github.io/risks/) >= 0.4.3.
  + Use modern tidyselect, anonymous functions, and code style.


# rifttable 0.6.3

* New functionality:
  + Provide easier interface to code competing events by directly providing 
    `event = "event_variable@Event_Type_One"` in the `design`. 
    If multiple event types are present, estimate cumulative incidence and 
    differences/ratios of cumulative incidence in a competing-event setting.
  + Allow for clustered observations in survival data, e.g., multiple rows per 
    person.
  + Estimate ratios of survival and cumulative incidence, i.e., *x*-year risk 
    ratios. Use MOVER estimation for confidence intervals of both ratios and
    differences in survival and cumulative incidence by default.
  + Support weights, e.g., inverse-probability weights, directly via a 
    `weights` argument in the `design`. Weighted estimates are currently 
    available for many survival estimators: `type = "cuminc"`, `"surv"`, their 
    differences and ratios (e.g., `"cumincdiff"`), and `"hr"`. This is a 
    breaking change for Cox models (`type = "hr"`), where providing `weights` in 
    the `arguments` list now generates an error.
* Bug fixes:
  + Allow for `@` in factor levels for a `table1_design()`.
  + `rt_gt()`: Output knitr-formatted tables for GitHub-flavored markdown also
    in Quarto `.qmd`, similar to `.Rmd`.
  + Better handling of edge cases, e.g., ratios of 0, when rounding estimates.
* Expanded documentation
  + Restructure site.
  + Separate documentation of estimators by outcome type.
  + New FAQs on confidence levels, reference levels, custom functions, and 
    joint models.


# rifttable 0.6.2

* New functionality:
  + Add overall argument `exposure_levels` to let user control handling of
    missing exposure levels (`NA`) or factors with empty levels as the exposure.
  + `type = "geomean"` for geometric means.
* Documentation: Expand FAQs.
* Internal and bug fixes:
  + Consider `exposure` or `trend` of `""` as missing, and `stratum = ""` as no 
    subsetting by the `effect_modifier`, instead  of subsetting to effect 
    modifier being an empty string. Input check that a stratum must be provided 
    for joint models and strata are not empty.
  + Consider missing `type` as `"blank"`.
  + Do not add empty rows/columns if `type2` has empty results for some cells or
    if only a `trend` variable and no `exposure` is given.
  + Rounding works even if result vector contains strings (e.g., no estimate).
  + More safeguards for all-`NA` `outcome` variables. More input checks.
  + Do not warn about non-`0`/`1` outcomes in log-linear models for ratios of
    continuous variables.
  + Add initial set of unit tests.


# rifttable 0.6.1

* New functionality:
  + Cox models (`type = "hr"`) allow for `weights`, clustering, and `robust` 
    standard errors.
  + Argument `ratio_digits_decrease`: By default, decrease number of decimal
    digits shown for ratios by 1 digit for ratios > 3 and by 2 digits for ratios
    > 10. Leads to rounded ratios and confidence intervals of `1.23`, `3.4`, and 
    `11`.
  + `rt_gt()` now indents the first column and applies markdown formatting to it 
    by default.
* New FAQ vignette.
* Bug fixes:
  + Binary outcomes returned `NA` instead of `0` in unstratified tables
    with all-null outcome.
  + `type = "maxfu"` ignored `digits` and `diff_digits`.
  + Allow for different `exposure` (strata labels) and `arguments` in one table.
  + Show unstratified estimate if `exposure` is `""`, not just for `NA`.
* Internal:
  + `rt_gt()`: suppress random `id` of gt tables to keep git diff slim.
  + Keep variables `.event`, `.outcome`, etc. available under their original 
    names.
  + Require {risks} >= 0.4.2.
  + Examples load the `breastcancer` dataset from the risks package.


# rifttable 0.6.0

* Breaking changes: 
  + `to` is set to `", "` by default, instead of `"-"` for ratio variables and 
    `" to "` for difference variables
  + Custom functions are now directly called via the `type` variable, 
    following a restructuring of all estimation functions with greater 
    flexibility.
  + `design$type` no longer accepts additional arguments, such as time points.
    Supply list instead via `design$arguments`.
  + Suppression of strata with sparse or re-identifiable data with `design$nmin`
    now differentiates between counts of total observations or outcomes, 
    depending on estimator.
* New function `table1_design()`: Generate design of a descriptive "Table 1."
* New `outcome` option `"variable@level"` for categorical variables that
  displays `level` as a binary outcome. Used by `table1_design()`.
* Support unstratified tables displaying the trend/linear slope (`trend` 
  variable in the `design`) without an `exposure`.
* More customization:
  + `rifttable(reference = ...)`: Label for the reference category.
  + `design$ci`: Width of confidence intervals.
  + `design$na_rm`: Omitting observations with missing outcome data.
  + `design$arguments`: Flexibly passing along any argument to estimation
    functions.
* New vignette describing all estimators.
* Internal:
  + Drop dependency on R >= 4.1 and native pipe.
  + Require [{risks}](https://stopsack.github.io/risks) >= 0.4.0.
  + Remove dependency on {labelled} package. 
  + The {gt} and {quantreg} packages are now optional as soft dependencies.
  + Compatible with {dplyr} 1.1.0, {tidyselect} 1.2.0
  

# rifttable 0.5.0

* khsmisc::table2() "graduated" into its own package. See [{khsmisc} Changelog](https://stopsack.github.io/khsmisc/news/) for earlier versions.
* Add `breastcancer()` dataset
* Use R >= 4.1 native pipe, `|>`
* Remove RMTL estimators
