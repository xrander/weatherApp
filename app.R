page_fluid(
  theme = theme_bootswatch("cerulean"),
  h2("weather App"),
  textInput("city_name", label = "City Name", placeholder = "Enter City Name: Ontario",width = "80%"),
  layout_columns(
    value_box(
      title = "weather",
      value = "title"
    ),
    card(textOutput("weather_info"))
  ),
  h2("$ weather forecast")
)
