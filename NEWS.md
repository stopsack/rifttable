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
