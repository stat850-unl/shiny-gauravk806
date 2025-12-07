#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
library(shiny)
library(bslib)
library(tidyverse)


cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-26/cocktails.csv')


ui <- page_sidebar(
  
  title = "Cocktail Explorer",
  
  # -------- SIDEBAR (INPUT CONTROLS) --------
  sidebar = sidebar(
    
    h4("Visualization Control"),
    
    selectInput(
      "viz_type",
      "Select Visualization:",
      choices = c(
        "Alcoholic" = "alcoholic",
        "Category" = "category",
        "Glass" = "glass"
      ),
      selected = "category"   
    ),
    
    hr(),
    
    h4("Recipe Search"),
    
    textInput(
      "ingredient_input",
      "Search by Ingredient:",
      placeholder = "e.g., vodka, gin, lime"
    )
  ),
  
  # Outpuyt Area
  
  h3("Cocktail Data Visualization"),
  div(
    style = "height: 450px;",
    plotOutput("cocktail_plot", height = "450px") #450px used to accommodate properly on screen. 
                                                  #If not used, was getting error message taht margin is too large
  ),
  
  hr(),
  
  h3("Ingredient-Based Recipe Search"),
  tableOutput("search_results")
)

server <- function(input, output) {
  
  # -------- REACTIVE VISUALIZATION --------
  output$cocktail_plot <- renderPlot({
    
    # Code to count based on what user selects
    plot_data <- switch(
      input$viz_type,
      "alcoholic" = cocktails %>% distinct(drink, alcoholic) %>% count(alcoholic),
      "category" = cocktails %>% distinct(drink, category) %>% count(category),
      "glass" = cocktails %>% distinct(drink, glass) %>% count(glass)
    )
    
    # Code to plot the chart for what is selected
    ggplot(plot_data, aes(x = reorder(!!sym(names(plot_data)[1]), n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(
        x = input$viz_type,
        y = "Count",
        title = paste("Distribution by", tools::toTitleCase(input$viz_type))
      )
  })
  
  
  # Code to show table for recipe 
  output$search_results <- renderTable({
    
    req(input$ingredient_input)
    
    cocktails %>%
      filter(grepl(input$ingredient_input, ingredient, ignore.case = TRUE)) %>%
      select(drink, ingredient, measure) %>%
      distinct()
  })
  
}

shinyApp(ui = ui, server = server)
