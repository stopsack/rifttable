#' Prepare Data For Estimation Functions
#'
#' @param data Data set
#' @param is_trend Binary: Whether run on the \code{trend} variable instead
#'   of the \code{exposure}. Optional.
#' @param na_rm Binary: Whether to drop missing outcome data. Optional.
#' @param outcome String: Name of the outcome variable. Optional.
#' @param event String: Name of the event variable. Optional.
#' @param time String: Name of the first time (entry) variable. Optional.
#' @param time2 String: Name of the second time (exit) variable. Optional.
#' @param type String: Name of estimator. Optional.
#' @param effectmodifier String: Name of the effect modifier variable.
#'   Optional.
#' @param effectmodifier_level String: Effect modifier level. Optional.
#'
#' @return List
#' @noRd
prepare_data <- function(
    data,
    is_trend = FALSE,
    na_rm = NA,
    outcome = NA,
    event = NA,
    time = NA,
    time2 = NA,
    type = "",
    effectmodifier = NULL,
    effectmodifier_level = NULL) {
  xlevels <- data %>%
    dplyr::pull(.data$.exposure) %>%
    levels()
  # Restrict to effect modifier, or generate joint exposure/effect modifier
  if(stringr::str_detect(
    string = type,
    pattern = "_joint") &
    is_trend == FALSE) {  # trends must be stratum-specific
    if(missing(effectmodifier) |
       missing(effectmodifier_level))
      stop(
        paste0(
          "Effect modifier and stratum must be specified for joint ",
          "model ('",
          type, "')."))
    if(is.na(effectmodifier) |
       is.null(effectmodifier) |
       is.null(effectmodifier_level))
      stop(
        paste0(
          "Effect modifier and stratum must be specified for joint ",
          "model ('",
          type, "')."))
    if(effectmodifier_level == "")
      stop(
        paste0(
          'An effect modifier stratum cannot be an empty string "" ',
          'for a joint model ("',
          type,
          '").'))
    pattern <- paste0(
      ".exposure[:digit:]{1,2}__",
      effectmodifier_level,
      "__[:digit:]{1,2}__")
    data$.effectmod <- data[[effectmodifier]]
    data <- data %>%
      dplyr::filter(!is.na(.data$.effectmod)) %>%
      dplyr::mutate(.effectmod = factor(.data$.effectmod))
    xlevels_indices <- 1:length(xlevels)
    names(xlevels_indices) <- xlevels
    emlevels <- data %>%
      dplyr::pull(.data$.effectmod) %>%
      factor() %>%
      levels()
    emlevels_indices <- 1:length(emlevels)
    names(emlevels_indices) <- emlevels
    data <- data %>%
      dplyr::mutate(
        .exposure = paste(
          emlevels_indices[.data$.effectmod],
          .data$.effectmod,
          xlevels_indices[.data$.exposure],
          .data$.exposure,
          sep = "__"))
    # not a joint model:
  } else {
    pattern <- ".exposure"
    if(!missing(effectmodifier)
       & !missing(effectmodifier_level)) {
      if(!is.null(effectmodifier_level) &
         !(is.null(effectmodifier) |
           is.na(effectmodifier))) {
        if(effectmodifier_level != "") {
          data$.effectmod <- data[[effectmodifier]]
          data <- data %>%
            dplyr::filter(.data$.effectmod %in% effectmodifier_level)
        }
      }
    }
  }

  # Create copies of outcome/event variables under standardized names
  if(!is.na(outcome)) {
    if(outcome != "") {
      if(!(outcome %in% names(data)))
        stop(
          paste0(
            "type = '",
            type,
            "': Outcome variable '",
            outcome,
            "' is not valid for the dataset."))
      data$.outcome <- data[[outcome]]
    }
  }
  if(!is.na(time) & !is.na(event)) {
    if(time != "" & event != "") {
      if(event != outcome | is.na(outcome)) {
        if(!(event %in% names(data)))
          stop(
            paste0(
              "type = '",
              type,
              "': Event variable '",
              event,
              "' is not valid for the dataset."))
        data$.event <- data[[event]]
      } else {
        # event and outcome can be the same variable
        data <- data %>%
          dplyr::mutate(.event = .data$.outcome)
      }
      if(!(time %in% names(data)))
        stop(
          paste0(
            "type = '",
            type,
            "': Time variable '",
            time,
            "' is not valid for the dataset."))
      # with 'time' and 'time2', the first is enter and the second is exit:
      has_time2 <- FALSE
      if(!is.na(time2)) {
        if(time2 != "") {
          if(!(time2 %in% names(data)))
            stop(
              paste0(
                "type = '",
                type,
                "': time2 variable '",
                time2,
                "' is not valid for the dataset."))
          data$.time_orig <- data[[time]]
          data$.time2 <- data[[time2]]
          data <- data %>%
            # for estimators that just sum the follow-up times:
            dplyr::mutate(.time = .data$.time2 - .data$.time_orig)
          has_time2 <- TRUE
        }
      }
      if(!has_time2) {
        data$.time <- data[[time]]
      }
    }
  }

  # Remove missing outcome data if requested
  if(!is.na(na_rm)) {
    if(na_rm == TRUE &
       !(type %in% c("", "blank"))) {
      if(is.na(outcome) | outcome == "")
        data <- data %>%
          tidyr::drop_na(dplyr::any_of(c(".event", ".time", ".time2")))
      else
          data <- data %>%
            tidyr::drop_na(".outcome")
    }
  }

  list(
    data = data,
    pattern = pattern,
    xlevels = xlevels)
}
