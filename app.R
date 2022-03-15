library(shiny)
library(COVID19)
library(tidyverse)
library(scales)

ui <- fluidPage(

    # Application title
    titlePanel("COVID: How did you fare?"),
    
    # add some blah blah text
    h5("If life is a competition and not getting covid before the masses is something upon which you wnat to pride yourself, then this page helps you feel good about how long you may have fended off the virus -- or learn that you weren't ahead (er, behind) the curve as you hoped."),
    h5("This page does not store any date you input. Your information stays private."),
    h5("The code lives here: https://github.com/michaelboerman/covid_percentiles"),
    
    # build the UI
    sidebarLayout(
        sidebarPanel(
            dateInput(
                inputId = "user_covid_date", 
                label = "When did you get Covid?", 
                value = "2022-01-01",
                min = all_covid_data %>% pull(date) %>% head(1),
                max = all_covid_data %>% pull(date) %>% tail(1)
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cdf_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # pull in all the data from this R package that reads from covid19hub source
    # set last day ato be yesterday due to incomplete info about current day.
    all_covid_data        <- COVID19::covid19("USA", end = Sys.Date()-1, verbose = FALSE)
    
    # grab the final number for use in calculating the percent completed later
    total_confirmed_count <- all_covid_data %>% 
        pull(confirmed) %>% 
        tail(1) 
    
    # calculate a dataframe that adds a row for the % completed
    data_in_percentiles   <- all_covid_data %>% 
        select(date, confirmed) %>% 
        mutate(pct_of_total = confirmed / total_confirmed_count)
    
    # the dynamic output after filtering on user input.
    percentile <- reactive(
        data_in_percentiles %>% 
            filter(date == input$user_covid_date) %>% 
            pull(pct_of_total) %>% 
            round(4)
    )
    
    # create a dynamic plot. 
    output$cdf_plot <- renderPlot(
        
        # input data, all transforms already performed
        data_in_percentiles %>% 
            
            # build basic plot layers
            ggplot(aes(x = date, y = pct_of_total)) +
            geom_line() + 
            
            # modify axes
            scale_y_continuous(
                name     = NULL,
                labels   = scales::percent_format(scale = 100, accuracy = 1),
                position = "right",
                n.breaks = 10,
                expand   = expansion(0, 0)
            ) +
            scale_x_date(
                date_breaks = "3 months", 
                date_labels = "%b %Y",
                expand      = expansion(0, 0),
                name        = NULL
            ) +
            
            # add a vertical cross hair line
            geom_vline(
                xintercept = input$user_covid_date, 
                color      = "grey", 
                linetype   = "dashed"
            ) +
            annotate(
                geom  = "label",
                x     = input$user_covid_date,
                y     = 0.975,
                label = format(as.Date(input$user_covid_date), "%a %b %d"),
                color = "grey", fill = "white"
            ) +
            
            # add a horizontal cross-hair line
            geom_hline(
                yintercept = percentile(), 
                color      = "grey", 
                linetype   = "dashed"
            ) +
            annotate(
                geom  = "label",
                x     = data_in_percentiles$date[34],
                y     = percentile(),
                label = paste0(percentile()*100, "%"),
                color = "grey", fill = "white"
            ) +
            
            # adjustments to theme and stuffs
            theme_minimal() +
            theme(
                panel.grid = element_blank(),
                plot.title = element_text(size = 18, face = "bold")
            ) +
            labs(
                title    = paste0("Of all the people in the US who caught COVID so far, \nyou caught it after ", percentile()*100, "% of them."),
                subtitle = "Analysis and Site made by Michael Boerman (www.michaelboerman.com).\nData from Covid19DataHub.",
                alt      = "A graph showing the cumulative COVID case count over time, with lines pointing to the intersection of date entered and the corresponding percentage of cases to date."
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
