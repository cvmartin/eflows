#' Dummy (but realistic) data for the month of september
#'
#' @format A data frame with 720 observations and 7 variables, left aligned at hourly basis:
#' \describe{
#'   \item{datetime}{time series}
#'   \item{d_household}{Demand of one household (kWh)}
#'   \item{d_house_smooth}{Demand of one household (kWh), smoothed following publicly available data from Liander}
#'   \item{d_beng}{Demand of ventilation and hot water for one household(kWh)}
#'   \item{d_beng_heat}{Demand of heat, ventilation and hot water for one household(kWh). This data is original from the month of November instead}
#'   \item{d_ev}{Average demand of one electric vehicle(kWh)}
#'   \item{solar}{Production of one kWp of PV installation (kWh)}
#'   \item{eprice}{APX lectricity price, in euros/MWh}
#' }
"sept"