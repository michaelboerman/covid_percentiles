library(shiny)
library(COVID19)
library(ISOcodes)
library(tidyverse)
library(scales)

# Before getting underway, we first need to create a list of countries with data.
# This list will be available to the user on the front-end, before any back-end dynamic work is done.
available_country_list <- 
    
    # pull ALL covid data
    COVID19::covid19(verbose = FALSE) %>% 
    
    # remove any rows that don't have data about confirmed cases
    drop_na(confirmed) %>% 
    
    # grab the unique countries that exist in the dataset
    select(iso_alpha_3) %>% 
    unique() %>% 
    
    # join in a dataset that matches ISO codes w/ country names
    left_join(ISOcodes::ISO_3166_1, by = c("iso_alpha_3" = "Alpha_3")) %>% 
    
    # grab just the iso_code and corresponding country name.
    # If there exists a "common name", use this instead of the more formal name.
    # (ie: People's Republic of China -> China)
    transmute(
        iso_code = iso_alpha_3, 
        country_name = case_when(
           !is.na(Common_name) ~ Common_name,
           TRUE ~ Name
        )
    ) %>% 
    arrange(country_name) %>% 
    drop_na()

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
                label   = "When did you get Covid?", 
                value   = "2022-01-01",
                min     = "2020-01-01",
                max     = Sys.Date()-1
            ),
            selectInput(
                inputId  = "which_country",
                label    = "In which country do you reside?",
                selected = "United States",
                choices  = available_country_list$country_name
            ),
            checkboxInput(
                inputId = "no_covid",
                label   = "I didn't get COVID",
                value   = FALSE
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
            h6(HTML("<p>Find the source code at <a href='https://github.com/michaelboerman/covid_percentiles/blob/main/app.R'>github.com/michaelboerman/covid_percentiles</a><p>")),
            h6(HTML("<p>See more of my work at <a href='https://www.michaelboerman.com'>michaelboerman.com</a> and get in touch with me via michaelboerman@hey.com :)<p>"))
        )
    )
)

# Define the back-end:
server <- function(input, output) {
    
    this_iso_code <- reactive({
        available_country_list %>% 
            filter(country_name == input$which_country) %>% 
            pull(iso_code)
    })
    
    all_covid_data <- reactive({
        COVID19::covid19(this_iso_code(), end = Sys.Date() - 1, verbose = FALSE)
    })
    
    # grab the final number for use in calculating the percent completed later
    total_confirmed_count <- reactive({
        all_covid_data() %>% 
            pull(confirmed) %>% 
            tail(1)
    })
    
    total_population <- reactive({
        all_covid_data() %>% 
            pull(population) %>% 
            tail(1)
    })
    
    # If they don't have covid, print text output instead.
    percent_had_covid <- reactive({
        (total_confirmed_count() / total_population() * 100) %>% round(2)
    })
    
    output$no_covid_text <- renderText(paste0(
        "Congratulations! You are in the ", 100 - percent_had_covid(), "% of citizens who did NOT catch COVID in ", input$which_country, "!"
    ))
    
    # calculate a dataframe that adds a row for the % completed
    data_in_percentiles   <- reactive({
        all_covid_data() %>% 
            select(date, confirmed) %>% 
            mutate(pct_of_total = confirmed / total_confirmed_count())
    })
    
    # the dynamic output after filtering on user input.
    percentile <- reactive({
        data_in_percentiles() %>% 
            filter(date == input$user_covid_date) %>% 
            pull(pct_of_total) %>% 
            round(4)
    })
    
    # create a dynamic plot. 
    output$cdf_plot <- renderPlot(
        
        # input data, all transforms already performed
        data_in_percentiles() %>% 
            
            # remove na in data (ggplot will do this automatically, but making it explicit will remove the warnings)
            drop_na() %>% 
            
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
                expand      = expansion(0, c(1, 0)),
                name        = NULL,
                limits      = c(as.Date("2020-01-01"), Sys.Date() - 1)
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
                x     = as.Date("2020-02-15"),
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
                title    = paste0("Of those in ", input$which_country, " who have caught COVID so far*, \nyou caught it after ", percentile()*100, "% of them!"),
                subtitle = paste0('*"Cought COVID so far" = tested positive and reported to government.'),
                caption  = 'Data from Covid19DataHub.',
                alt      = "A graph showing the cumulative COVID case count over time, with lines pointing to the intersection of date entered and the corresponding percentage of cases to date."
            )
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
