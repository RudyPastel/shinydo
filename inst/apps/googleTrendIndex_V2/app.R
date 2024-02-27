# Load packages ----
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

# Load data ----
trend_data <- read_csv(file = "data/trendData.csv",
                       col_types = cols(
                           type = col_character(),
                           date = col_date(format = ""),
                           close = col_double()
                       ))

# DEFINE UI ----
ui <- fluidPage(theme = shinytheme("lumen"),

                indexTrendModuleUI(id = 'Google', searchEngine = 'Google', choices = unique(trend_data$type)),
                indexTrendModuleUI(id = 'Bing', searchEngine = 'Bing', choices = unique(trend_data$type)),
                indexTrendModuleUI(id = 'Yahoo', searchEngine = 'Yahoo', choices = unique(trend_data$type)),
                indexTrendModuleUI(id = 'Baidu', searchEngine = 'Baidu', choices = unique(trend_data$type)),
                indexTrendModuleUI(id = 'Yandex', searchEngine = 'Yandex', choices = unique(trend_data$type))

)

# DEFINE SERVER FUNCTION ----
server <- function(input, output) {

    indexTrendModuleServer(id = 'Google', trend_data = trend_data)
    indexTrendModuleServer(id = 'Bing', trend_data = trend_data)
    indexTrendModuleServer(id = 'Yahoo', trend_data = trend_data)
    indexTrendModuleServer(id = 'Baidu', trend_data = trend_data)
    indexTrendModuleServer(id = 'Yandex', trend_data = trend_data)
}

# Create Shiny object
shinyApp(ui = ui, server = server)
