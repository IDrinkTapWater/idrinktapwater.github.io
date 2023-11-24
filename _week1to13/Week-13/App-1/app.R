library(shiny)
library(DT)
library(tidyverse)

data <- read_csv("all_seasons.csv")
curated <- data %>%
  select(player_name, player_height, gp, pts, reb, ast, net_rating, season)
Bestof <- curated %>%
  group_by(player_name) %>%
  filter(gp == max(gp, na.rm = TRUE)) %>%
  filter(net_rating == max(net_rating, na.rm = TRUE)) %>%
  ungroup()
average_row <- Bestof %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE)) %>%
  mutate(player_name = "Average Player", season = NA_character_)

# Add the new row to the original dataset
Bestof_with_average <- bind_rows(Bestof, average_row)
Full <- Bestof_with_average%>%
  arrange(desc(net_rating))
Full <- Full %>%
  filter(gp >= 10)
Full2 <- Full %>%
  group_by(player_name) %>%
  filter(gp == max(gp, na.rm = TRUE)) %>%
  filter(net_rating == max(net_rating, na.rm = TRUE)) %>%
  filter(pts == max(pts, na.rm = TRUE)) %>%
  ungroup()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("NBA Player Stats Viewer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Player selection
      selectInput("selected_players", "Select Players", choices = unique(Full2$player_name), multiple = TRUE),
      # Add button
      actionButton("add_button", "Add Selected Players")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Display the table
      DTOutput("player_table")
      
    )
  )
)

# Define server
server <- function(input, output, session) {

  # Reactive values to store selected players
  selected_players <- reactiveVal(NULL)
  
  # Update selected_players when add_button is clicked
  observeEvent(input$add_button, {
    selected_players(c(selected_players(), input$selected_players))
  })
  
  # Render the table based on selected players
  output$player_table <- renderDT({
    req(selected_players())
    
    # Filter data for selected players
    selected_data <- Full2[Full2$player_name %in% selected_players(), ]
    
    datatable(selected_data, options = list(pageLength = 5))
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)