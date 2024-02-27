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
                # Google -----
                titlePanel("Google Trend Index"),
                sidebarLayout(
                    sidebarPanel(

                        # Select type of trend to plot
                        selectInput(inputId = "type", label = strong("Trend index"),
                                    choices = unique(trend_data$type),
                                    selected = "Travel"),

                        # Select date range to be plotted
                        dateRangeInput("date", strong("Date range"), start = "2007-01-01", end = "2017-07-31",
                                       min = "2007-01-01", max = "2017-07-31"),

                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "smoother", label = strong("Overlay smooth trend line"), value = FALSE),

                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.smoother == true",
                                         sliderInput(inputId = "f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),

                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "lineplot", height = "300px"),
                        textOutput(outputId = "desc")
                    )
                ),
                # Bing -----
                titlePanel("Bing Trend Index"),
                sidebarLayout(
                    sidebarPanel(

                        # Select type of trend to plot
                        selectInput(inputId = "Bing_type", label = strong("Trend index"),
                                    choices = unique(trend_data$type),
                                    selected = "Travel"),

                        # Select date range to be plotted
                        dateRangeInput(inputId = "Bing_date",
                                       strong("Date range"),
                                       start = "2007-01-01", end = "2017-07-31",
                                       min = "2007-01-01", max = "2017-07-31"),

                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "Bing_smoother", label = strong("Overlay smooth trend line"), value = FALSE),

                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.Bing_smoother == true",
                                         sliderInput(inputId = "Bing_f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),

                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "Bing_lineplot", height = "300px"),
                        textOutput(outputId = "Bing_desc")
                    )
                ),
                # Yahoo -----
                titlePanel("Yahoo Trend Index"),
                sidebarLayout(
                    sidebarPanel(

                        # Select type of trend to plot
                        selectInput(inputId = "Yahoo_type", label = strong("Trend index"),
                                    choices = unique(trend_data$type),
                                    selected = "Travel"),

                        # Select date range to be plotted
                        dateRangeInput(inputId = "Yahoo_date",
                                       strong("Date range"),
                                       start = "2007-01-01", end = "2017-07-31",
                                       min = "2007-01-01", max = "2017-07-31"),

                        # Select whether to overlay smooth trend line
                        checkboxInput(inputId = "Yahoo_smoother", label = strong("Overlay smooth trend line"), value = FALSE),

                        # Display only if the smoother is checked
                        conditionalPanel(condition = "input.Yahoo_smoother == true",
                                         sliderInput(inputId = "Yahoo_f", label = "Smoother span:",
                                                     min = 0.01, max = 1, value = 0.67, step = 0.01,
                                                     animate = animationOptions(interval = 100)),
                                         HTML("Higher values give more smoothness.")
                        )
                    ),

                    # Output: Description, lineplot, and reference
                    mainPanel(
                        plotOutput(outputId = "Yahoo_lineplot", height = "300px"),
                        textOutput(outputId = "Yahoo_desc")
                    )
                )
)

# DEFINE SERVER FUNCTION ----
server <- function(input, output) {

    # Google ----
    # Subset data
    selected_trends <- reactive({
        req(input$date)
        validate(need(!is.na(input$date[1]) & !is.na(input$date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$date[1] < input$date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                type == input$type,
                date > input$date[1] & date < input$date[2]
            )
    })


    # Create scatterplot object the plotOutput function is expecting
    output$lineplot <- renderPlot({
        color = "#434343"
        par(mar = c(4, 4, 1, 1))
        plot(x = selected_trends()$date, y = selected_trends()$close, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$smoother){
            smooth_curve <- lowess(x = as.numeric(selected_trends()$date), y = selected_trends()$close, f = input$f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })

    # Pull in description of trend
    output$desc <- renderText({
        'The data is a randomly generated place holder.'
    })

    # Bing ----
    # Subset data
    selected_Bing_trends <- reactive({
        req(input$Bing_date)
        validate(need(!is.na(input$Bing_date[1]) & !is.na(input$Bing_date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$Bing_date[1] < input$Bing_date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                type == input$Bing_type,
                date > input$Bing_date[1] & date < input$Bing_date[2]
            )
    })


    # Create scatterplot object the plotOutput function is expecting
    output$Bing_lineplot <- renderPlot({
        color = "#434343"
        par(mar = c(4, 4, 1, 1))
        plot(x = selected_Bing_trends()$date, y = selected_Bing_trends()$close, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$Bing_smoother){
            smooth_curve <- lowess(x = as.numeric(selected_Bing_trends()$date), y = selected_Bing_trends()$close, f = input$Bing_f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })

    # Pull in description of trend
    output$Bing_desc <- renderText({
        'The data is a randomly generated place holder.'
    })

    # Yahoo ----
    # Subset data
    selected_Yahoo_trends <- reactive({
        req(input$Yahoo_date)
        validate(need(!is.na(input$Yahoo_date[1]) & !is.na(input$Yahoo_date[2]), "Error: Please provide both a start and an end date."))
        validate(need(input$Yahoo_date[1] < input$Yahoo_date[2], "Error: Start date should be earlier than end date."))
        trend_data %>%
            filter(
                type == input$Yahoo_type,
                date > input$Yahoo_date[1] & date < input$Yahoo_date[2]
            )
    })


    # Create scatterplot object the plotOutput function is expecting
    output$Yahoo_lineplot <- renderPlot({
        color = "#434343"
        par(mar = c(4, 4, 1, 1))
        plot(x = selected_Yahoo_trends()$date, y = selected_Yahoo_trends()$close, type = "l",
             xlab = "Date", ylab = "Trend index", col = color, fg = color, col.lab = color, col.axis = color)
        # Display only if smoother is checked
        if(input$Yahoo_smoother){
            smooth_curve <- lowess(x = as.numeric(selected_Yahoo_trends()$date), y = selected_Yahoo_trends()$close, f = input$Yahoo_f)
            lines(smooth_curve, col = "#E6553A", lwd = 3)
        }
    })

    # Pull in description of trend
    output$Yahoo_desc <- renderText({
        'The data is a randomly generated place holder.'
    })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
