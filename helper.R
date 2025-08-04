pacman::p_load(httr2, tidyverse, rvest, countrycode, shiny, bslib, plotly)

# GET COUNTRY CODE -------------------------------------------------------

get_country_code <- function(country_name) {
  if(!is.character(country_name)) {
    stop("Country should be a character")
  } else {
    return(
      countrycode(
        country_name, 
        origin = "country.name.en", 
        destination = "iso2c"
      )
    )  
  }
}

# GET LOCATION INFO -------------------------------------------------------

get_data <- function(city_name, country_name) {
  
  base_url <- "https://geocoding-api.open-meteo.com/v1/search"
  req <- base_url |> 
    request()
  
  stopifnot(is.character(city_name), is.character(country_name))
  
  req |> 
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
  
  stopifnot(is.character(city_name), is.character(country_name))
  
  # GET DATA
  fetch_data <- get_data(
    city_name = city_name, 
    country_name = country_name
  )
  
  fetch_result <-  fetch_data$result[[1]]
  
  # EXTRACT VARIABLES =================================================
  
  lat <- fetch_result$latitude
  long <-  fetch_result$longitude
  timezone <-  fetch_result$timezone
  country <- fetch_result$country
  city_name <- fetch_result$name
  state <- fetch_result$admin1
  
  
  location_data <- data.frame(
    city_name,
    lat,
    long,
    state,
    timezone,
    country
  )

  return(location_data)
}

location_info <-  process_data("abeokuta", "nigeria")


# GET WEATHER FORECAST DATA -----------------------------------------------


get_forecast_tbl <- function(location_tbl, forecast_days = 7) {
  
  stopifnot(is.data.frame(location_tbl), is.numeric(forecast_days))
  if(forecast_days > 17) {
    stop("Reduce forecast to 16. You can get a forecast for a maximum of 16 days")
  } else if (forecast_days < 1) {
    stop("History data not available at the moment")
  }
  
  lat <- location_tbl$lat
  long <- location_tbl$long
  timezone <- location_tbl$timezone
  
  # GET FORECAST TBL ==================================================
  
  base_forecast_url <- "https://api.open-meteo.com/v1"
  forecast_req <- base_forecast_url |> 
    request()
  
  weather_raw <- forecast_req |> 
    req_url_path_append("forecast") |> 
    req_url_query(
      forecast_days = forecast_days,
      latitude = lat,
      longitude = long,
      daily = c("temperature_2m_max", "sunrise", "sunset", "wind_speed_10m_max", "showers_sum", "rain_sum"),
      hourly = c("temperature_2m", "showers", "rain", "wind_speed_10m"),
      timezone = timezone,
      .multi = "comma"
    ) |> 
    req_perform() |> 
    resp_body_json()
  
  return( weather_raw )
}

weather_data <- get_forecast_tbl(location_info, 16) # max is 16 days

# PROCESS WEATHER FORECAST DATA
process_weather_tbl <- function(weather_tbl, variable) {
  weather_tbl[[variable]] |> 
    transpose() |> 
    map_df(~as_tibble(.x))
}

hourly_tbl <- process_weather_tbl(weather_data, "hourly") |> 
  mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M"))

daily_tbl <- process_weather_tbl(weather_data, "daily") |> 
  mutate(
    across(sunrise:sunset, \(x) as.POSIXct(x, format ="%Y-%m-%dT%H:%M")),
    time = as.Date(time)
  )

hourly_tbl |> 
  ggplot(
    aes(time, temperature_2m)
  ) +
  geom_line(col = "thistle") +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    
  )


plot_hourly_data <- function(plot_table, y_var) {
  plot_table |> 
    ggplot(
      aes(time, .data[[y_var]])
    ) +
    geom_line(col = "thistle") +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.background = element_line(col = "gray"),
      plot.background = element_line(col = "gray"),
      panel.background = element_rect(fill = "black")
    )
}

ggplotly( plot_hourly_data(plot_table = hourly_tbl,y_var = "showers") )
