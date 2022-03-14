library(shiny)
library(COVID19)
library(tidyverse)

ui <- fluidPage(

    # Application title
    titlePanel("COVID: How did you fare?"),
    
    h5("If life is a competition and not getting covid before the masses is something upon which you wnat to pride yourself, then this page helps you feel good about how long you may have fended off the virus -- or learn that you weren't ahead (er, behind) the curve as you hoped."),
    h5("This page does not store any date you input. Your information stays private."),
    h5("The code lives here: https://github.com/michaelboerman/covid_percentiles"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("user_covid_date", "When did you get Covid?", value = "2022-01-01")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("percent")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    all_covid_data <- COVID19::covid19("USA")
    total_confirmed_count <- all_covid_data %>% pull(confirmed) %>% tail(1) 
    
    percentile <- reactive(
        all_covid_data %>% 
            select(date, confirmed) %>% 
            mutate(pct_of_total = confirmed / total_confirmed_count) %>% 
            filter(date == input$user_covid_date) %>% 
            pull(pct_of_total)
    )
    
    output$percent <- renderText(
        paste0("You got covid after ", round(percentile(), 4)*100, "% of people got it in the United States to date.")
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
