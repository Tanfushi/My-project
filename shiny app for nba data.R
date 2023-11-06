library(shiny)
library(tidyverse)
library(jsonlite)

data_url <- "https://uofi.box.com/shared/static/ocu15xq3khl3nq3muhs3md8wvygipd8h.json"
data <- fromJSON(data_url, flatten = TRUE) %>% as_tibble()

ui <- fluidPage(
  titlePanel("NBA 2022 Season Player Per Game Data"),
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data_type", "Choose data type:",
                   choices = list("Individual Player" = "player", "Team" = "team")),
      uiOutput("select_input")
    ),
    mainPanel(
      plotOutput("text_plot")
    )
  )
)

server <- function(input, output, session) {
  output$select_input <- renderUI({
    if(input$data_type == "player") {
      selectInput("select_data", "Select Player:", choices = unique(data$Player))
    } else {
      selectInput("select_data", "Select Team:", choices = unique(data$Tm))
    }
  })
  output$text_plot <- renderPlot({
    req(input$select_data) 
    plot_data <- if(input$data_type == "player") {
      data %>%
        filter(Player == input$select_data) %>%
        summarise(across(c(`FG%`, `3P%`, `FT%`, ORB, DRB, AST, STL, BLK, TOV), mean, na.rm = TRUE))
    } else {
      data %>%
        filter(Tm == input$select_data) %>%
        summarise(across(c(`FG%`, `3P%`, `FT%`, ORB, DRB, AST, STL, BLK, TOV), mean, na.rm = TRUE))
    }
    
    plot_data <- plot_data %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")
    ggplot(plot_data, aes(x = 1, y = 1)) +
      geom_text(aes(label = sprintf("%.3f", Value)), size = 6, vjust = -1) +
      geom_text(aes(label = Category), size = 4, vjust = 2) +
      facet_wrap(~Category, ncol = 3) +
      theme_void() +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(1.5, "lines"),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
      ) +
      labs(title = paste(input$data_type, "Statistics for", input$select_data))
  })
  
}

shinyApp(ui, server)