library(shiny)
library(COVID19)
library(tidyverse)
library(scales)

# pull in all the data from this R package that reads from covid19hub source
# set last day to be yesterday due to incomplete info about current day.
# Need to define outside of 'server' because the UI will be using it on the front end, before server is called.
all_covid_data <<- COVID19::covid19("USA", end = Sys.Date() - 1, verbose = FALSE)

# define the front-end:
ui <- fluidPage(

    # Application title
    titlePanel("COVID: How Long Did You Last?"),
    
    # add some blah blah text
    h5("If life is a competition and not getting covid before the masses is something upon which you want to pride yourself, then this page helps you feel good about how long you may have fended off the virus -- or learn that you weren't as ahead (er, behind) the curve as you hoped."),
    h5("This page does not store any date you input. Your information stays private."),
    
    # build the UI
    sidebarLayout(
        sidebarPanel(
            dateInput(
                inputId = "user_covid_date", 
                label = "When did you get Covid?", 
                value = "2022-01-01",
                min = all_covid_data %>% pull(date) %>% head(1),
                max = all_covid_data %>% pull(date) %>% tail(1)
            ),
            checkboxInput(
                inputId = "no_covid",
                label = "I didn't get COVID",
                value = FALSE
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel(
                condition = "input.no_covid",
                h3(textOutput('no_covid_text'))
            ),
            conditionalPanel(
                condition = "!input.no_covid",
                plotOutput("cdf_plot")
            ),
            h6(HTML("<p>Find the source code at <a href='https://github.com/michaelboerman/covid_percentiles'>github.com/michaelboerman/covid_percentiles</a><p>")),
            h6(HTML("<p>See more of my work at <a href='https://www.michaelboerman.com'>michaelboerman.com</a> and get in touch with me via michaelboerman@hey.com :)<p>"))
        )
    )
)

# Define the back-end:
server <- function(input, output) {
    
    # grab the final number for use in calculating the percent completed later
    total_confirmed_count <- all_covid_data %>% 
        pull(confirmed) %>% 
        tail(1) 
    
    total_population <- all_covid_data %>% 
        pull(population) %>% 
        tail(1) 
    
    # If they don't have covid, print text output instead.
    percent_had_covid <- total_confirmed_count / total_population * 100 %>% round(2)
    output$no_covid_text <- renderText(paste0(
        "Congratulations! You are in the ", 100 - percent_had_covid, "% of Americans who did NOT catch COVID!"
    ))
    
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
            geom_line(size = 1.25) + 
            
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
                title    = paste0("Of all the people in the U.S. who caught COVID so far, \nyou caught it after ", percentile()*100, "% of them."),
                caption  = "Data from Covid19DataHub",
                alt      = "A graph showing the cumulative COVID case count over time, with lines pointing to the intersection of date entered and the corresponding percentage of cases to date."
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
