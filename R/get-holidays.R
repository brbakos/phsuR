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

  if (weekday_number == 1) {
    victoria_day <- may_25 - lubridate::days(7)
  } else {
    victoria_day <- may_25 - lubridate::days(weekday_number - 1)
  }

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

  if (weekday_number == 1) {

    labour_day <- sept_1

  } else {

    labour_day <- sept_1 + lubridate::days(8 - weekday_number)

  }
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

  ## fixed date
  new_years_day            <- as.Date(paste0(calendar_year, "-01-01"))
  canada_day               <- as.Date(paste0(calendar_year, "-07-01"))
  remembrance_day          <- as.Date(paste0(calendar_year, "-11-11"))
  christmas_day            <- as.Date(paste0(calendar_year, "-12-25"))
  boxing_day               <- as.Date(paste0(calendar_year, "-12-26"))
  truth_and_reconciliation <- as.Date(paste0(calendar_year, "-09-30"))

  good_friday      <- compute_good_friday(calendar_year)
  easter_monday    <- compute_easter_monday(calendar_year)
  victoria_day     <- compute_victoria_day(calendar_year)
  labour_day       <- compute_labour_day(calendar_year)
  thanksgiving_day <- compute_thanksgiving(calendar_year)
  family_day       <- compute_family_day(calendar_year)
  bc_day           <- compute_bc_day(calendar_year)

  # Compile all holiday dates into a vector.
  holidays <- c(
    "New Year's Day"               = new_years_day,
    "Family Day"                   = family_day,
    "Good Friday"                  = good_friday,
    "Easter Monday"                = easter_monday,
    "Victoria Day"                 = victoria_day,
    "Canada Day"                   = canada_day,
    "BC Day"                       = bc_day,
    "Labour Day"                   = labour_day,
    "Truth and Reconciliation Day" = truth_and_reconciliation,
    "Thanksgiving Day"             = thanksgiving_day,
    "Remembrance Day"              = remembrance_day,
    "Christmas Day"                = christmas_day,
    "Boxing Day"                   = boxing_day
  )

  holidays
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
  weekday_number <- lubridate::wday(holiday_date, week_start = 1)
  if (weekday_number == 6) {
    # Saturday: shift by 2 days to Monday.
    holiday_observed_date <- holiday_date + lubridate::days(2)
  } else if (weekday_number == 7) {
    # Sunday: shift by 1 day to Monday.
    holiday_observed_date <- holiday_date + lubridate::days(1)
  } else {
    holiday_observed_date <- holiday_date
  }

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

  official_holidays <- get_holidays(calendar_year)
  observed_holidays <- sapply(official_holidays, observed_date)

  as.Date(observed_holidays, origin = "1970-01-01")
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
  holiday_year <- lubriquery_date::year(query_date)
  holiday_query_dates <- get_holidays(holiday_year)

  query_date %in% holiday_query_dates
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
  holiday_year <- lubridate::year(query_date)
  observed_holidays <- get_observed_holidays(holiday_year)

  query_date %in% observed_holidays
}
