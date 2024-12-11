#' Geocode Addresses and Attach Regional Information
#'
#' A convenience function that geocodes addresses using the BC Address Geocoder API
#' and attaches regional boundary data based on the resulting latitude and longitude.
#'
#' @inheritParams get_geocode_data
#' @inheritParams spatial_join_with_boundaries
#'
#' @details
#' This function combines \code{\link{get_geocode_data}} and
#' \code{\link{spatial_join_with_boundaries}} into a single workflow. It first geocodes
#' addresses to obtain latitude and longitude coordinates using the BC Address Geocoder API.
#' Then, it performs a spatial join to attach regional data
#' to the geocoded results.
#'
#' @return
#' If `faults = TRUE`, returns a list with two `data.frame`s:
#' \describe{
#'   \item{\code{data}}{The geocoded data with attached regional information.}
#'   \item{\code{faults}}{Any issues noted by the geocoder tool (e.g., addresses that could not be geocoded).}
#' }
#' If `faults = FALSE`, only the geocoded data with attached regional information is returned as a `data.frame`.
#'
#' @examples
#' \dontrun{
#' addresses <- c("601 W Broadway, Vancouver, BC", "1081 Burrard St, Vancouver, BC")
#'
#' ## Geocode addresses and attach regional data
#' addresses_geocoded <- get_geocode_with_region(addresses)
#'
#' ## Access the geocoded data
#' geo_data <- addresses_geocoded$data
#'
#' ## Access any faults (if faults = TRUE)
#' faults_data <- addresses_geocoded$faults
#' }
#'
#' @export
get_geocode_with_region <- function(address,
                                    api_key = NULL,
                                    faults = TRUE,
                                    rate_limit = 1000,
                                    coords = c("longitude", "latitude"),
                                    crs = 4326) {

  address_data <-
    get_geocode_data(
      address = address,
      api_key = api_key,
      faults = faults,
      rate_limit = rate_limit
    )

  if (faults) {
    address_data$data <-
      spatial_join_with_boundaries(
        address_data$data,
        coords = coords,
        crs = crs
      )
  } else {
    address_data <-
      spatial_join_with_boundaries(
        address_data,
        coords = coords,
        crs = crs
      )
  }

  return(address_data)

}

#' Request Geographic Data for an Address from the BC Address Geocoder API
#'
#' This function sends addresses to the BC Address Geocoder API and retrieves geocoded
#' data (latitude and longitude) along with any flagged issues ("faults") identified by
#' the API during the geocoding process.
#'
#' @param address A character vector of addresses to geocode.
#' @param api_key Optionally, an API key for accessing the BC Address Geocoder API.
#'   Defaults to `NULL`.
#' @param faults Logical. If `TRUE`, the function returns flagged issues for each address
#'   (if any). The result will include a second `data.frame` named `faults`. Defaults to `TRUE`.
#' @param rate_limit Maximum number of requests allowed per minute. Defaults to 1000.
#'   Use `NULL` for no rate limit.
#'
#' @returns A geocoded dataset:
#' \describe{
#'   \item{faults = FALSE}{A single `data.frame` with geocoded data for each address.}
#'   \item{faults = TRUE}{A `list` with:
#'   \describe{
#'     \item{\code{data}}{A `data.frame` containing geocoded data for each address.}
#'     \item{\code{faults}}{A `data.frame` with one row per fault for any address that has faults.}
#'   }}
#' }
#'
#' @details
#' You must ensure the quality of the addresses sent to the API for optimal results.
#' Including unnecessary information (e.g., "CANADA") may degrade the match quality.
#' The API is biased toward addresses in British Columbia; addresses outside the province
#' may produce mixed results or be incorrectly linked to BC locations.
#'
#' The best address format for reliable results is:
#' `civic number` `street name`, `city name`, `province`.
#'
#' @examples
#' \dontrun{
#' addresses <- c("601 W Broadway, Vancouver, BC", "1081 Burrard St, Vancouver, BC")
#'
#' ## Geocode addresses with faults returned
#' result <- get_geocode_data(addresses)
#'
#' ## Geocoded data
#' geo_data <- result$data
#'
#' ## Faults (if any)
#' faults_data <- result$faults
#'
#' ## Geocode addresses without faults
#' geo_data <- get_geocode_data(addresses, faults = FALSE)
#' }
#'
#' @rdname get-geocode-data
#' @export
get_geocode_data <- function(address,
                             api_key = NULL,
                             faults = TRUE,
                             rate_limit = 1000) {

  if (!is.logical(faults) || length(faults) != 1) {
    stop("'faults' must be a single logical value (TRUE or FALSE).")
  }

  if (!is.null(rate_limit) && (!is.numeric(rate_limit) || rate_limit <= 0 || rate_limit %% 1 != 0)) {
    stop("'rate_limit' must be a positive integer or NULL.")
  }

  delay_per_request <- if (!is.null(rate_limit)) 60 / rate_limit else 0


  process_address <- function(address) {
    api_url <- httr2::url_parse('https://geocoder.api.gov.bc.ca/addresses.geojson')

    api_response <-
      httr2::request(httr2::url_build(api_url)) |>
      httr2::req_url_query(
        addressString = address,
        apiKey = api_key,
        ## Mercator (WGS-84) is the default format (EPSG: 4326)
        ## but basemaps tend to be in Pseudo-mercator (EPSG: 3857)
        outputSRS = "4326"
      ) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform()

    if (api_response$status_code %/% 200 == 1) {

      api_content <- httr2::resp_body_json(api_response)
      features_list <- api_content$features

      geocode_data <-
        purrr::map_dfr(
          features_list, ~ {
            lon <- .x$geometry$coordinates[[1]]
            lat <- .x$geometry$coordinates[[2]]

            properties <- .x$properties

            ## not every address will have faults, but there can be an arbitrary number
            ## I think returning a single data.frame with a list of faults is less tidy
            ## than returning a list with a `data.frame` for main results and
            ## a `data.frame` with the faults
            address_faults <- properties$faults
            has_faults <- ifelse(length(address_faults) > 0, TRUE, FALSE)

            properties$faults <- NULL
            properties_data <- dplyr::tibble(!!!properties)

            address_data <-
              dplyr::bind_cols(
                dplyr::tibble(
                  address = address,
                  longitude = lon,
                  latitude = lat,
                  has_faults = has_faults
                ),
                properties_data
              ) |>
              ## I was having challenges with differing data types for some values
              dplyr::mutate(dplyr::across(dplyr::everything(), as.character))


            address_data
          }
      )

      ## there is a little duplication of effort here, but I was having troubles
      ## getting 1 row of geocode data per address if there was more than 1 fault
      faults_data <-
        purrr::map_dfr(
          features_list, ~ {

            address_faults <- .x$properties$faults
            has_faults <- ifelse(length(address_faults) > 0, TRUE, FALSE)

            faults_data <- if (faults && has_faults) {
              purrr::map_dfr(address_faults, ~ dplyr::tibble(
                address = address,
                faults_value = as.character(.x$value),
                faults_element = as.character(.x$element),
                faults_fault = as.character(.x$fault),
                faults_penalty = as.character(.x$penalty)
              ))
            } else {
              NULL
            }

            faults_data
          }
        )

      result_list <- list(data = geocode_data, faults = faults_data)

      ## this is the return when status 200
      return(result_list)

    } else {

      warning(paste("Bad response for address:", address, "\nResponse code:", api_response$status_code))

      return(
        list(
          data = dplyr::tibble(address = address),
          faults =
            dplyr::tibble(
              address = address,
              faults_fault = paste("Bad response from API:", api_response$status_code)
            )
        )
      )

    }
  }

  results <- purrr::map(address, function(addr) {
    res <- process_address(addr)
    if (!is.null(rate_limit)) Sys.sleep(delay_per_request)
    res
  })


  geocode_results <- purrr::map_dfr(results, "data")
  geocode_results <- geocode_results |> purrr::set_names(~ snakecase::to_snake_case(.x))
  faults_results <- purrr::map_dfr(results, "faults")

  if (faults) {

    faults_results <- faults_results |> purrr::set_names(~ snakecase::to_snake_case(.x))

    return(list(data = geocode_results, faults = faults_results))

  } else {

    return(geocode_results)

  }
}


#' Attach Regional Data by Latitude and Longitude
#'
#' This function performs a spatial join to attach regional boundary information from
#' `phsuR::chsa_boundaries` to a dataset based on geographic coordinates.
#'
#' @param .data A `data.frame` containing the data to which regional information will be attached.
#' @param coords A character vector specifying the names of the columns holding geographic coordinates.
#'   Defaults to `c("longitude", "latitude")`.
#' @param crs An integer specifying the Coordinate Reference System (CRS) of the input data.
#'   Defaults to `4326` (WGS 84).
#'
#' @details
#' The input coordinate data is assumed to be in WGS 84 (EPSG:4326) by default. The function first
#' converts the data to a simple features (`sf`) object, then reprojects it to match the CRS of the
#' `phsuR::chsa_boundaries` dataset. A spatial join is performed to attach the corresponding
#' regional data. The resulting dataset is returned as a `data.frame`, with the `geometry` column removed.
#'
#' @note
#' The `geometry` column, created during the spatial join process, is dropped before the result
#' is returned. If the geometry is needed, you can modify the function to retain it.
#'
#' @return A `data.frame` with the original data and additional columns from the `phsuR::chsa_boundaries` dataset.
#'
#' @seealso \code{\link[sf]{st_as_sf}} for converting data to a spatial object and \code{\link[sf]{st_transform}} for CRS transformations.
#'
#' @rdname spatial-join-with-boundaries
#'
#' @export
spatial_join_with_boundaries <- function(.data,
                                         coords = c("longitude", "latitude"),
                                         crs = 4326) {

  .data <-
    sf::st_as_sf(
      .data,
      coords = c(coords),
      crs = crs,
      remove = FALSE
    )
  .data <-
    sf::st_transform(
      .data,
      sf::st_crs(phsuR::chsa_boundaries)
    )
  .data <- sf::st_join(.data, phsuR::chsa_boundaries)

  .data$geometry <- NULL

  return(.data)

}
