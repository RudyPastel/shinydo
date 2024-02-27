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

# List top 10 search engines ----

searchEngines = c('Google', 'Bing', 'Yahoo', 'Baidu', 'Yandex',
                  'Ask', 'DuckDuckGo', 'Naver', 'AOL', 'Seznam')

# DEFINE UI ----
ui = fluidPage(theme = shinytheme("lumen"),

               lapply(
                   X = searchEngines,
                   FUN = function(searchEngine, choices){
                       indexTrendModuleUI(id = searchEngine,
                                          searchEngine = searchEngine,
                                          choices = choices)
                   },
                   choices = unique(trend_data$type))

)

# DEFINE SERVER FUNCTION ----
server <- function(input, output) {

    for (searchEngine in searchEngines){
        indexTrendModuleServer(id = searchEngine, trend_data = trend_data)
    }
}

# Create Shiny object
shinyApp(ui = ui, server = server)
