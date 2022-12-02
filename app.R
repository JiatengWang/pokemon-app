

#load package
library(shiny)
library(tidyverse)
library(shinyWidgets)

#import dataset
pokemon <- read_csv("data/pokemon.csv")

#preprocess the dataset
#pokemon <- pokemon %>%
#  select(name, species, type_1, type_2, hp, attack, defense, sp_attack, sp_defense, speed, height_m, weight_kg, generation)
pokemon$type_1 <- as.factor(pokemon$type_1)
pokemon$type_2 <- as.factor(pokemon$type_2)
pokemon$generation <- as.factor(pokemon$generation)
pokemon[which(is.na(pokemon$weight_kg)),]$weight_kg <- 0
pokemon[which(is.na(pokemon$height_m)),]$height_m <- 0

backgroundImageCSS <- "/* background-color: #cccccc; */
                       height: 80vh;
                       background-position: center;
                       background-repeat: no-repeat;
                       /* background-size: cover; */
                       background-image: url('%s');
                       "

ui <- navbarPage(
  tags$head(tags$style(
    HTML(
      "
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: white;
        color: black;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }"
    )
  )),
  title = img(src = "logo.svg", height = 38),
  
  tabPanel(
    title = "Data Visualization",
    titlePanel(title = "Pokemon Type Distribution Until Gen 8"),
    style = sprintf(backgroundImageCSS,  "IMG_1168.JPG"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "gen",
          label = "Generation:",
          choices = sort(unique(pokemon$generation)),
          selected = 1
        ),
        sliderInput(
          inputId = "weight",
          label = "Weight(in kg) Range:",
          min = min(pokemon$weight_kg),
          max = max(pokemon$weight_kg),
          value = c(min(pokemon$weight_kg), max(pokemon$weight_kg))
        ),
        sliderInput(
          inputId = "height",
          label = "Height(in m) Range:",
          min = min(pokemon$height_m),
          max = max(pokemon$height_m),
          value = c(min(pokemon$height_m), max(pokemon$height_m))
        ),
        submitButton("Apply changes", icon("refresh")),
      ),
      mainPanel(plotOutput("plot"))
    )
    
  ),
  tabPanel(
    title = "Summary Table",
    titlePanel(title = "Pokemon Species Strength Summary Table Across 8 Generations"),
    style = sprintf(backgroundImageCSS,  "IMG_1168.JPG"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "gen2",
          label = "Generation:",
          choices = sort(unique(pokemon$generation)),
          selected = 1
        ),
        submitButton("Apply changes", icon("refresh")),
        textOutput(outputId = "explain"),
      ),
      mainPanel(
        verbatimTextOutput("summary"),
      )
    )
    
  ),
  
  tabPanel(title = "About",
           includeMarkdown("about.Rmd")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  pokemon_gen <- reactive({
    pokemon %>%
      filter(generation == input$gen)
  })
  
  observeEvent(eventExpr = input$gen,
               handlerExpr = {
                 updateSliderInput(
                   inputId = "weight",
                   min = min(pokemon_gen()$weight_kg),
                   max = max(pokemon_gen()$weight_kg),
                   value = c(
                     min(pokemon_gen()$weight_kg),
                     max(pokemon_gen()$weight_kg)
                   )
                 )
                 updateSliderInput(
                   inputId = "height",
                   min = min(pokemon_gen()$height_m),
                   max = max(pokemon_gen()$height_m),
                   value = c(min(pokemon_gen()$height_m), max(pokemon_gen()$height_m))
                 )
               })
  
  output$plot <- renderPlot({
    pokemon %>%
      filter(generation == input$gen) %>%
      filter(weight_kg >= input$weight[1] &
               weight_kg <= input$weight[2]) %>%
      filter(height_m >= input$height[1] &
               height_m <= input$height[2]) %>%
      pivot_longer(
        cols = type_1:type_2,
        names_to = "type_name",
        values_to = "type",
        values_drop_na = TRUE
      ) %>%
      group_by(type) %>%
      summarise(count = n()) %>%
      ggplot() +
      aes(x = type, y = count, fill = type) %>%
      geom_bar(stat = "identity") +
      theme_bw()
  })
  
  output$summary <- renderPrint({
    pokemon_summary <- pokemon %>%
      filter(generation == input$gen2)
    summary(pokemon_summary$total_points)
  })
  
  output$explain <-
    renderText(
      "This table shows each generation pokemons' strength summary. Users can use this smmary table to analyze pokemon's strength for each generation and select the best pokemon for their team."
    )
  
}

# Run the application
shinyApp(ui = ui, server = server)
