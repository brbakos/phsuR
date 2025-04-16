#' Compute Easter Sunday using the Meeus/Jones/Butcher Algorithm
#'
#' This function computes the date of Easter Sunday for a given calendar year using the
#' Meeus/Jones/Butcher algorithm.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Easter Sunday in the given year.
#'
#' @examples
#' compute_easter_sunday(2025)
compute_easter_sunday <- function(calendar_year) {

  a <- calendar_year %% 19
  b <- calendar_year %/% 100
  c <- calendar_year %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8) %/% 25
  g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  easter_month <- (h + l - 7 * m + 114) %/% 31
  easter_day <- ((h + l - 7 * m + 114) %% 31) + 1

  easter_sunday <-
    as.Date(
      paste0(calendar_year, "-", easter_month, "-", easter_day)
    )

  easter_sunday
}

#' Compute Easter Monday
#'
#' This function computes the date of Easter Monday for a given calendar year. It is defined as
#' the day following Easter Sunday.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Easter Monday.
#'
#' @examples
#' compute_easter_monday(2025)
compute_easter_monday <- function(calendar_year) {
  compute_easter_sunday(calendar_year) + lubridate::days(1)
}


#' Compute Good Friday
#'
#' This function calculates Good Friday for the specified calendar year.
#' Good Friday is defined as two days before Easter Sunday.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Good Friday.
#'
#' @examples
#' compute_good_friday(2025)
compute_good_friday <- function(calendar_year) {
  compute_easter_sunday(calendar_year) - lubridate::days(2)
}

#' Compute Victoria Day
#'
#' This function computes Victoria Day for a given calendar year.
#' Victoria Day is defined as the Monday immediately preceding May 25.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Victoria Day.
#'
#' @examples
#' compute_victoria_day(2025)
compute_victoria_day <- function(calendar_year) {

  may_25 <- as.Date(sprintf("%d-05-25", calendar_year))
  weekday_number <- lubridate::wday(may_25, week_start = 1)

  victoria_day   <-
    dplyr::if_else(
      weekday_number == 1,
      may_25 - lubridate::days(7),
      may_25 - lubridate::days(weekday_number - 1)
    )

  victoria_day
}

#' Compute Labour Day
#'
#' This function computes Labour Day for a given calendar year.
#' Labour Day is defined as the first Monday in September.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Labour Day.
#'
#' @examples
#' compute_labour_day(2025)
compute_labour_day <- function(calendar_year) {

  sept_1 <- as.Date(sprintf("%d-09-01", calendar_year))
  weekday_number <- lubridate::wday(sept_1, week_start = 1)

  labour_day <-
    dplyr::if_else(
      weekday_number == 1,
      sept_1,
      sept_1 + lubridate::days(8 - weekday_number)
    )
  labour_day
}

#' Compute Thanksgiving Day
#'
#' This function computes Thanksgiving Day for a given calendar year.
#' In this context, Thanksgiving is defined as the second Monday in October.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Thanksgiving Day.
#'
#' @examples
#' compute_thanksgiving(2025)
compute_thanksgiving <- function(calendar_year) {

  oct_1 <- as.Date(sprintf("%d-10-01", calendar_year))
  weekday_number <- lubridate::wday(oct_1, week_start = 1)
  first_monday_oct <-
    dplyr::if_else(
      weekday_number == 1,
      oct_1,
      oct_1 + lubridate::days(8 - weekday_number)
    )
  second_monday_oct <- first_monday_oct + lubridate::days(7)

  second_monday_oct
}

#' Compute Family Day
#'
#' This function computes Family Day for a given calendar year.
#' Family Day is defined as the third Monday in February.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing Family Day.
#'
#' @examples
#' compute_family_day(2025)
compute_family_day <- function(calendar_year) {
  feb_1 <- as.Date(sprintf("%d-02-01", calendar_year))
  weekday_number_feb <- lubridate::wday(feb_1, week_start = 1)
  first_monday_feb <-
    dplyr::if_else(
      weekday_number_feb == 1,
      feb_1,
      feb_1 + lubridate::days(8 - weekday_number_feb)
    )
  third_monday_feb <- first_monday_feb + lubridate::days(14)

  third_monday_feb
}

#' Compute BC Day
#'
#' This function computes BC Day for a given calendar year.
#' BC Day is defined as the first Monday in August.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A \code{Date} object representing BC Day.
#'
#' @examples
#' compute_bc_day(2025)
compute_bc_day <- function(calendar_year) {
  aug_1 <- as.Date(sprintf("%d-08-01", calendar_year))
  weekday_number_aug <- lubridate::wday(aug_1, week_start = 1)
  first_monday_aug <-
    dplyr::if_else(
      weekday_number_aug == 1,
      aug_1,
      aug_1 + lubridate::days(8 - weekday_number_aug)
    )

  first_monday_aug
}

#' Get Official Government Statutory Holidays
#'
#' This function returns a vector of official government statutory holidays
#' (without weekend adjustments) for the specified calendar year. It includes
#' both fixed-date holidays and variable-date holidays computed by corresponding functions.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A vector of \code{Date} objects representing the official holidays.
#'
#' @examples
#' get_holidays(2025)
#'
#' @export
get_holidays <- function(calendar_year) {

  calendar_years <- unique(as.integer(calendar_year))
  holidays_per_year <-
    lapply(
      calendar_years,
      function(y) {

        h <-
          c(
            "New Year's Day"               = as.Date(sprintf("%d-01-01", y)),
            "Family Day"                   = compute_family_day(y),
            "Good Friday"                  = compute_good_friday(y),
            "Easter Monday"                = compute_easter_monday(y),
            "Victoria Day"                 = compute_victoria_day(y),
            "Canada Day"                   = as.Date(sprintf("%d-07-01", y)),
            "BC Day"                       = compute_bc_day(y),
            "Labour Day"                   = compute_labour_day(y),
            "Truth and Reconciliation Day" = as.Date(sprintf("%d-09-30", y)),
            "Thanksgiving Day"             = compute_thanksgiving(y),
            "Remembrance Day"              = as.Date(sprintf("%d-11-11", y)),
            "Christmas Day"                = as.Date(sprintf("%d-12-25", y)),
            "Boxing Day"                   = as.Date(sprintf("%d-12-26", y))
          )

        if (y < 2021) h  <- h[!names(h) %in% "Truth and Reconciliation Day"]

        h
      }
    )
  all_holidays <- do.call(c, holidays_per_year)

  all_holidays[order(all_holidays)]

}

#' Compute Observed Holiday Date
#'
#' This helper function calculates the observed date for a given holiday.
#' If the holiday falls on a weekend (Saturday or Sunday), the observed date is shifted to the following Monday.
#'
#' @param holiday_date A \code{Date} object representing the original holiday date.
#'
#' @return A \code{Date} object representing the observed holiday date.
#'
#' @examples
#' observed_date(as.Date("2025-12-25"))
observed_date <- function(holiday_date) {

  holiday_date <- as.Date(holiday_date)
  day_of_week <- lubridate::wday(holiday_date, week_start = 1)
  shift <-
    ifelse(
      day_of_week == 6,
      2,
      ifelse(
        day_of_week == 7,
        1,
        0
      )
    )
  holiday_observed_date <- holiday_date + lubridate::days(shift)

  holiday_observed_date
}

#' Get Observed Government Statutory Holidays
#'
#' This function returns a vector of observed government statutory holiday dates for a given calendar year.
#' Official holiday dates that fall on a weekend are adjusted to the following Monday.
#'
#' @param calendar_year An integer representing the calendar year.
#'
#' @return A vector of \code{Date} objects representing the observed holidays.
#'
#' @examples
#' get_observed_holidays(2025)
#'
#' @export
get_observed_holidays <- function(calendar_year) {

  calendar_years <- unique(as.integer(calendar_year))
  observed_holidays_per_year <-
    lapply(
      calendar_years,
      function(y) {
        hols <- get_holidays(y)
        obs  <- observed_date(hols)
        names(obs) <- names(hols)
        obs
      }
    )
  observed_all <- do.call(c, observed_holidays_per_year)

  observed_all

}

#' Check if a Date is an Official Holiday
#'
#' This function checks whether the given query date matches any official government statutory holiday.
#'
#' @param query_date A \code{Date} object to be evaluated.
#'
#' @return \code{TRUE} if the date is an official holiday; \code{FALSE} otherwise.
#'
#' @examples
#' is_holiday(as.Date("2025-07-01"))
#'
#' @export
is_holiday <- function(query_date) {

  query_date   <- as.Date(query_date)
  holiday_year <- lubridate::year(query_date)

  holiday_years <- unique(holiday_year)
  holiday_list <-
    setNames(
      lapply(holiday_years, get_holidays),
      holiday_years
    )

  vapply(
    seq_along(query_date),
    function(i) {
      d <- query_date[i]
      d %in% holiday_list[[ as.character(holiday_year[i]) ]]
    },
    logical(1)
  )
}

#' Check if a Date is an Observed Holiday
#'
#' This function checks whether the given query date is an observed government statutory holiday,
#' taking weekend adjustments into account.
#'
#' @param query_date A \code{Date} object to be evaluated.
#'
#' @return \code{TRUE} if the date is an observed holiday; \code{FALSE} otherwise.
#'
#' @examples
#' is_observed_holiday(as.Date("2025-07-01"))
#'
#' @export
is_observed_holiday <- function(query_date) {
  query_date <- as.Date(query_date)
  holiday_year <- lubridate::year(query_date)

  holiday_years <- unique(holiday_year)
  observed_list <-
    setNames(
      lapply(holiday_years, get_observed_holidays),
      holiday_years
    )

  vapply(
    seq_along(query_date),
    function(i) {
      d <- query_date[i]
      d %in% observed_list[[ as.character(holiday_year[i]) ]]
    },
    logical(1)
  )
}
