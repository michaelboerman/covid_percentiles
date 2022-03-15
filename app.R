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
            dateInput("user_covid_date", "When did you get Covid?", value = "2022-01-01")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("cdf_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    all_covid_data        <- COVID19::covid19("USA")
    total_confirmed_count <- all_covid_data %>% 
        pull(confirmed) %>% 
        tail(1) 
    data_in_percentiles   <- all_covid_data %>% 
        select(date, confirmed) %>% 
        mutate(pct_of_total = confirmed / total_confirmed_count)
    
    percentile <- reactive(
        data_in_percentiles %>% 
            filter(date == input$user_covid_date) %>% 
            pull(pct_of_total)
    )
    
    output$cdf_plot <- renderPlot(
        data_in_percentiles %>% 
            ggplot(aes(x = date, y = pct_of_total)) +
            geom_line() + 
            scale_y_continuous(
                name = NULL,
                labels = scales::percent_format(scale = 100, accuracy = 1),
                position = "right",
                n.breaks = 10,
                expand = expansion(0, 0)
            ) +
            scale_x_date(
                date_breaks = "3 months", 
                date_labels = "%b %Y",
                expand = expansion(0, 0),
                name = NULL
            ) +
            geom_vline(
                xintercept = input$user_covid_date, 
                color = "grey", 
                linetype = "dashed"
            ) +
            geom_hline(
                yintercept = percentile(), 
                color = "grey", 
                linetype = "dashed"
            ) +
            theme_minimal() +
            theme(
                panel.grid = element_blank()
            ) +
            ggtitle(
                label = paste0("Of all the people in the US who caught COVID so far, you caught it after ", round(percentile(), 4)*100, "% of them."),
                subtitle = "Data from Covid19DataHub. Analysis and Site made by Michael Boerman (www.michaelboerman.com)"
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
