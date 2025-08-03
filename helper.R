pacman::p_load(httr2, tidyverse, rvest, countrycode, shiny, bslib, highcharter)

# GET COUNTRY CODE -------------------------------------------------------

get_country_code <- function(country_name) {
  return(countrycode(country_name, origin = "country.name.en", destination = "iso2c"))
}

# GET LOCATION DATA -------------------------------------------------------

location_base_url <- "https://geocoding-api.open-meteo.com/v1/search"

location_req <- location_base_url |> 
  request()

get_data <- function(city_name, country_name) {
  location_req |> 
    req_url_query(
      name = city_name,
      count = 1,
      language = "en",
      country_code = get_country_code(country_name = country_name)
    ) |> 
    req_perform() |> 
    resp_body_json()
}

process_data <- function(city_name, country_name) {
  
  fetch_data <- get_data(city_name = city_name, country_name = country_name)
  
  fetch_res <-  fetch_data$result[[1]]
  
  location_data <- data.frame(
    city_name = fetch_res$name,
    latitude = fetch_res$latitude,
    longitude = fetch_res$longitude,
    state = fetch_res$admin1,
    timezone = fetch_res$timezone,
    country = fetch_res$country
  )

  return(location_data)
}

location_info <-  process_data("abeokuta", "nigeria")


# GET WEATHER FORECAST DATA -----------------------------------------------

forecast_weather_url <- "https://api.open-meteo.com/v1"

forecast_req <- forecast_weather_url |> 
  request()

get_weather_data <- function(location_tbl, forecast_days) {
  
  stopifnot(is.numeric(forecast_days))
  stopifnot(is.data.frame(location_tbl))
  
  weather_raw <- forecast_req |> 
    req_url_path_append("forecast") |> 
    req_url_query(
      forecast_days = forecast_days,
      latitude = location_tbl$latitude,
      longitude = location_tbl$longitude,
      daily = c("temperature_2m_max", "sunrise", "sunset", "wind_speed_10m_max", "showers_sum", "rain_sum"),
      hourly = c("temperature_2m", "showers", "rain", "wind_speed_10m"),
      timezone = location_tbl$timezone,
      .multi = "comma"
    ) |> 
    req_perform() |> 
    resp_body_json()
}

weather_data <- get_weather_data(location_info, 16)

# PROCESS WEATHER FORECAST DATA
process_weather_tbl <- function(variable) {
  weather_data[[variable]] |> 
    transpose() |> 
    map_df(~as_tibble(.x))
}

hourly_tbl <- process_weather_tbl("hourly") |> 
  mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M"))

daily_tbl <- process_weather_tbl("daily") |> 
  mutate(
    across(sunrise:sunset, \(x) as.POSIXct(x, format ="%Y-%m-%dT%H:%M")),
    time = as.Date(time)
  )

ggplot(hourly_tbl, aes(time, temperature_2m)) +
  geom_line()
  
