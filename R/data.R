#' CHSA Boundaries in British Columbia
#'
#' A shapefile dataset containing the boundaries of Community Health Service Areas (CHSAs) in British Columbia.
#'
#' @format A `sf` object with 231 rows and the following columns:
#' \describe{
#'   \item{chsa_code}{The unique code for each CHSA (character).}
#'   \item{chsa_name}{The name of the CHSA (character).}
#'   \item{chsa_urban_rural_class}{Classification of the CHSA as urban, rural, or mixed (character).}
#'   \item{lha_code}{The code for the Local Health Area (LHA) associated with the CHSA (character).}
#'   \item{lha_name}{The name of the LHA (character).}
#'   \item{hsda_code}{The code for the Health Service Delivery Area (HSDA) associated with the CHSA (character).}
#'   \item{hsda_name}{The name of the HSDA (character).}
#'   \item{ha_code}{The code for the Health Authority (HA) associated with the CHSA (character).}
#'   \item{ha_name}{The name of the HA (character).}
#'   \item{clha_code}{The code for the Consolidated Local Health Area (CLHA) (character).}
#'   \item{clha_name}{The name of the CLHA (character).}
#'   \item{community_of_care}{Specific to Vancouver Coastal Health (VCH). Categorizes the Coastal HSDA into urban and rural areas.}
#'   \item{geometry}{The spatial geometry of the CHSA (multipolygon).}
#' }
#'
#' @details
#' The CHSA boundaries are defined by the Government of British Columbia. The dataset
#' is updated every four years following the Census. Users will need to update their package or
#' data.
#'
#' The dataset includes detailed classification for urban and rural designations,
#' as well as hierarchical relationships to LHAs, HSDAs, and Health Authorities.
#'
#' The spatial data is provided in the WGS-84 system.
#'
#' @source
#' Data obtained from [Government of British Columbia Open Data Catalogue](https://www.data.gov.bc.ca/).
"chsa_boundaries"
