library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggmap)
library(shiny)


get_restaurants_by_food_type <- function(location, api_key, food_type) {
  base_url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
  all_restaurants <- NULL  # Start with NULL for flexibility
  next_page_token <- NULL  # Initialize next_page_token before the repeat loop
  
  repeat {
    query_list <- list(
      location = location,
      radius = 50000,
      type = "restaurant",
      keyword = food_type,
      key = api_key
    )
    
    if (!is.null(next_page_token)) {
      query_list$pagetoken <- next_page_token
    }
    
    res <- GET(url = base_url, query = query_list)
    if (status_code(res) != 200) {
      stop("API request failed with status code: ", status_code(res))
    }
    
    content <- content(res, "text", encoding = "UTF-8")
    parsed <- fromJSON(content, flatten = TRUE)
    
    if (!"results" %in% names(parsed) || length(parsed$results) == 0) {
      break  # No more results or bad response
    }
    
    restaurants <- as.data.frame(parsed$results)
    # Use bind_rows to handle different columns and missing data
    all_restaurants <- bind_rows(all_restaurants, restaurants)
    
    next_page_token <- parsed$next_page_token
    
    if (is.null(next_page_token)) {
      break
    } else {
      Sys.sleep(2)
    }
  }
  
  return(all_restaurants)
}

illinois_location <- "40.116421,-88.243385"
chinese_restaurants <- get_restaurants_by_food_type(illinois_location, api_key, "Chinese")

# Replace "YOUR_API_KEY" with your actual API key and call the function
champaign_location <- "40.116421, -88.243385" # Coordinates for Champaign, IL
api_key <- "AIzaSyDALi4PFJJIPse6S1ROYaO8av8B5pyAUUw"

# Example call for "Chinese" restaurants. You can replace "Chinese" with any food type.
Chinese_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "Chinese")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)
Japanese_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "Japanese")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)
Korean_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "Korean")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)
American_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "American")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)
French_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "French")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)
Italian_restaurants_data <- get_restaurants_by_food_type(champaign_location, api_key, "Italian")%>%
  select(price_level,rating,vicinity,`geometry.location.lat`,`geometry.location.lng`)

ui <- fluidPage(
  titlePanel("Restaurant Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("foodType", "Choose Food Type",
                  choices = c("Chinese", "Japanese", "Korean", "American", "French", "Italian")),
      selectInput("price_level", "Choose Price Level",
                  choices = c("1", "2", "3", "4")),
      selectInput("rating", "Choose Rating Level",
                  choices = c("1", "2", "3", "4", "5")),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      plotOutput("map",height = "600px", width = "100%"),
      tableOutput("table"),
      textOutput("noDataMessage")  # Element to display the message
    )
  )
)

server <- function(input, output, session) {
  
  
  ggmap::register_google(key = "AIzaSyDALi4PFJJIPse6S1ROYaO8av8B5pyAUUw")
  observeEvent(input$submit, {
    selectedData <- switch(input$foodType,
                           "Chinese" = Chinese_restaurants_data,
                           "Japanese" = Japanese_restaurants_data,
                           "Korean" = Korean_restaurants_data,
                           "American" = American_restaurants_data,
                           "French" = French_restaurants_data,
                           "Italian" = Italian_restaurants_data)

    filteredData <- reactive({
      selectedData %>%
        filter(price_level == input$price_level,
               rating >= input$rating)
    })

    output$map <- renderPlot({
      data <- filteredData()

      if (nrow(data) > 0) {
        base_map <- get_map(location = c(lon = mean(data$`geometry.location.lng`, na.rm = TRUE),
                                         lat = mean(data$`geometry.location.lat`, na.rm = TRUE)), zoom = 12)

        ggmap(base_map) +
          geom_point(data = data, aes(x = `geometry.location.lng`, y = `geometry.location.lat`), size = 3, alpha = 0.8,color="red") +
          labs(title = paste(input$foodType, "Restaurants Map"), x = "", y = "")
      } 
    })
    output$table <- renderTable({
      data <- filteredData()
      if (nrow(data) > 0) {
        data
      }
    })
    
    output$noDataMessage <- renderText({
      data <- filteredData()
      if (nrow(data) == 0) {
        "No data available for the selected criteria."
      }
    })
  })
}
shinyApp(ui = ui, server = server)


