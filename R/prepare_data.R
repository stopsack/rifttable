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
#' @param weights String: Name of the weighting variable. Optional.
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
    weights = NA,
    type = "",
    effectmodifier = NULL,
    effectmodifier_level = NULL) {
  xlevels <- data %>%
    dplyr::pull(.data$.exposure) %>%
    levels()
  # Restrict to effect modifier, or generate joint exposure/effect modifier
  if (stringr::str_detect(
    string = type,
    pattern = "_joint"
  ) &
    is_trend == FALSE) { # trends must be stratum-specific
    if (is.na(effectmodifier) |
        is.null(effectmodifier) |
        is.null(effectmodifier_level)
    ) {
      stop(
        paste0(
          "Effect modifier and stratum must be specified for joint ",
          "model ('",
          type, "')."
        )
      )
    }
    if (any(effectmodifier_level == "") |
        any(is.na(effectmodifier_level))
    ) {
      stop(
        paste0(
          'An effect modifier stratum cannot be an empty string "" ',
          'or missing (NA) for a joint model ("',
          type,
          '").'
        )
      )
    }
    pattern <- paste0(
      ".exposure[:digit:]{1,2}__",
      effectmodifier_level,
      "__[:digit:]{1,2}__"
    )
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
          sep = "__"
        )
      )
    # not a joint model:
  } else {
    pattern <- ".exposure"
    if (!missing(effectmodifier) &
      !missing(effectmodifier_level)
    ) {
      if (!is.null(effectmodifier_level) &
          !(is.null(effectmodifier) |
            is.na(effectmodifier))
      ) {
        if (
          !all(
            effectmodifier_level == "",
            na.rm = TRUE
          )
        ) {
          data$.effectmod <- data[[effectmodifier]]
          data <- data %>%
            dplyr::filter(.data$.effectmod %in% effectmodifier_level)
          if (nrow(data) == 0) {
            warning(
              paste0(
                "Effect modifier '",
                effectmodifier,
                "': Stratum '",
                effectmodifier_level,
                "' is empty (0 observations). "
              )
            )
          }
        }
      }
    }
  }

  # Create copies of outcome/event variables under standardized names
  if (!is.na(outcome)) {
    if (outcome != "") {
      if (!(outcome %in% names(data))) {
        stop(
          paste0(
            "type = '",
            type,
            "': Outcome variable '",
            outcome,
            "' is not valid for the dataset."
          )
        )
      }
      data$.outcome <- data[[outcome]]
    }
  }
  event_type <- NULL
  if (!is.na(time) & !is.na(event)) {
    if (time != "" & event != "") {
      if (stringr::str_detect(
        string = event,
        pattern = "@"
      )
      ) {
        event_type <- stringr::str_split_i(
          string = event,
          pattern = "@",
          i = 2
        )
        event <- stringr::str_split_i(
          string = event,
          pattern = "@",
          i = 1
        )
      }
      if (event != outcome | is.na(outcome)) {
        if (!(event %in% names(data))) {
          stop(
            paste0(
              "type = '",
              type,
              "': Event variable '",
              event,
              "' is not valid for the dataset."
            )
          )
        }
        data$.event <- data[[event]]
      } else {
        # event and outcome can be the same variable
        data <- data %>%
          dplyr::mutate(.event = .data$.outcome)
      }
      if (!is.null(event_type)) {
        if (!any(event_type %in% unique(data$.event))) {
          stop(paste0(
            "For event variable '",
            event,
            "', the specified event type '",
            event_type,
            "' is not available in the data. Available are: ",
            paste(
              unique(data$.event),
              collapse = " "
            )
          ))
        }
        if (!length(stats::na.omit(unique(data$.event))) > 2) {
          stop(paste0(
            "For event variable '",
            event,
            "', the event type '",
            event_type,
            "' has been specified. However, the event variable does not ",
            "appear to have more than two levels to encode competing events ",
            "and censoring. Available levels are only: ",
            paste(
              unique(data$.event),
              collapse = " "
            )
          ))
        }
        if (!is.factor(data$.event)) {
          stop(paste0(
            "The event variable '",
            event,
            "' with more than two levels to presumably encode competing ",
            "events must be a factor. However, the current type was '",
            class(data$.event),
            "'."
          ))
        }
      } else {
        if (length(stats::na.omit(unique(data$.event))) > 2) {
          stop(paste0(
            "The event variable '",
            event,
            "' has more than two non-missing levels, suggesting that ",
            "competing events may be encoded, but no specific event type ",
            "(variable level) has been requested via ",
            "event = 'event_variable@level'. Available levels are: ",
            paste(
              unique(data$.event),
              collapse = " "
            )
          ))
        }
      }
      # Recode event variable for estimators that only handle one event type
      data$.event_compete <- data$.event
      if (!is.null(event_type)) {
        data <- data %>%
          dplyr::mutate(
            .event = dplyr::if_else(
              condition = .data$.event == event_type,
              true = 1,
              false = 0
            )
          )
      }
      if (any(is.na(data$.event)) |
          any(is.na(data[[time]]))
      ) {
        warning(
          paste0(
            "The event variable '",
            event,
            "' and/or the time variable '",
            time,
            "' contain missing values, which may be unintended and lead to undesirable results."
          )
        )
      }

      if (!(time %in% names(data))) {
        stop(
          paste0(
            "type = '",
            type,
            "': Time variable '",
            time,
            "' is not valid for the dataset."
          )
        )
      }

      if (!is.numeric(data[[time]])) {
        stop(
          paste0(
            "type = '",
            type,
            "': Time variable '",
            time,
            "' must be continuous (numeric). Its current class is '",
            class(data[[time]]),
            "'."
          )
        )
      }

      # with 'time' and 'time2', the first is enter and the second is exit:
      has_time2 <- FALSE
      if (!is.na(time2)) {
        if (time2 != "") {
          if (!(time2 %in% names(data))) {
            stop(
              paste0(
                "type = '",
                type,
                "': time2 variable '",
                time2,
                "' is not valid for the dataset."
              )
            )
          }
          data$.time_orig <- data[[time]]
          data$.time2 <- data[[time2]]
          if (!is.numeric(data$.time2)) {
            stop(
              paste0(
                "type = '",
                type,
                "': time2 variable '",
                time2,
                "' must be continuous (numeric). Its current class is '",
                class(data$.time2),
                "'."
              )
            )
          }
          if (any(is.na(data$.time2))) {
            warning(
              paste0(
                "The second (exit) time variable '",
                time2,
                "' contains missing values, which is may be unintended and lead to undesirable results."
              )
            )
          }
          data <- data %>%
            # for estimators that just sum the follow-up times:
            dplyr::mutate(.time = .data$.time2 - .data$.time_orig)
          has_time2 <- TRUE
        }
      }
      if (!has_time2) {
        data$.time <- data[[time]]
      }
    }
  }

  # Remove missing outcome data if requested
  if (!is.na(na_rm)) {
    if (na_rm == TRUE &
      !(type %in% c("", "blank"))
    ) {
      if (is.na(outcome) | outcome == "") {
        data <- data %>%
          tidyr::drop_na(dplyr::any_of(c(".event", ".time", ".time2")))
      } else {
        data <- data %>%
          tidyr::drop_na(".outcome")
      }
    }
  }

  if (!is.na(weights)) {
    if (weights != "") {
      if (!(weights %in% names(data))) {
        stop(
          paste0(
            "weights = '",
            weights,
            "': Variable is not valid for the dataset."
          )
        )
      }
      data$.weights <- data[[weights]]
      if (!is.numeric(data$.weights)) {
        stop(
          paste0(
            "weights = '",
            weights,
            "': Variable is not numeric."
          )
        )
      }
    }
  }
  if (!".weights" %in% colnames(data)) {
    data$.weights <- 1
  }

  list(
    data = data,
    pattern = pattern,
    xlevels = xlevels,
    event_type = event_type
  )
}
