indexTrendModuleUI <- function(id, searchEngine, choices) {
    # All uses of Shiny input/output IDs in the UI must be namespaced,
    # as in ns("x").
    ns <- NS(id)
    tagList(
        titlePanel(sprintf(fmt = "%s Trend Index", searchEngine)),
        sidebarLayout(
            sidebarPanel(

                # Select type of trend to plot
                selectInput(inputId = ns("type"),
                            label = strong("Trend index"),
                            choices = choices,
                            selected = "Travel"),

                # Select date range to be plotted
                dateRangeInput(inputId = ns("date"),label =  strong("Date range"),
                               start = "2007-01-01", end = "2017-07-31",
                               min = "2007-01-01", max = "2017-07-31"),

                # Select whether to overlay smooth trend line
                checkboxInput(inputId = ns("smoother"),
                              label = strong("Overlay smooth trend line"),
                              value = FALSE),

                # Display only if the smoother is checked
                conditionalPanel(
                    condition = sprintf(fmt = 'input["%s"] == true', ns("smoother")),
                    sliderInput(inputId = ns('smootherSpan'), label = "Smoother span:",
                                min = 0.01, max = 1, value = 0.67, step = 0.01,
                                animate = animationOptions(interval = 100)),
                    HTML("Higher values give more smoothness.")
                )
            ),

            # Output: Description, lineplot, and reference
            mainPanel(
                plotOutput(outputId = ns("lineplot"), height = "300px"),
                textOutput(outputId = ns("desc"))
            )
        )
    )
}


indexTrendModuleServer <- function(id, trend_data) {
    # moduleServer() wraps a function to create the server component of a
    # module.
    moduleServer(
        id,
        function(input, output, session) {
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
                plotTrendIndex(
                    date = selected_trends()$date,
                    close = selected_trends()$close,
                    smoother = input$smoother,
                    smootherSpan =  input$smootherSpan)
            })

            # Pull in description of trend
            output$desc <- renderText({
                'The data is a randomly generated place holder.'
            })
        }
    )
}
