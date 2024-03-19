library(tidyverse)
library(shiny)
library(shinyjs)

# Data ####
movement <- read_csv("Movement.csv") %>% 
  mutate(min_x = min_x*12,
         max_x = max_x*12,
         min_z = min_z*12,
         max_z = max_z*12)


# Functions ####
geom_zone <- function(top = 3.4, bottom = 1.75, linecolor = "black"){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = 5)
}

geom_plate <- function(pov = "pitcher"){
  df <- case_when(
    pov == "pitcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, .25, .5, .25)),
    pov == "catcher" ~ 
      data.frame(x = c(-.7083, .7083, .7083 ,0, -.7083), y = c(0, 0, -.25, -.5, -.25)))
  g <- geom_polygon(data = df, aes(x = x, y = y), fill = "white", color = "black", linewidth = 1)
  g
}

# Barrel Function
is.barrel <- function(LA, EV){
  upper <- 1.11*EV - 78.89
  lower <- -EV + 124
  outcome <- (LA >= lower) & (LA <= upper) & (EV >= 98) & (LA >= 8) & (LA <= 50)
  outcome <- replace_na(outcome, FALSE)
  outcome
}


ui <- fluidPage(
  
  titlePanel("Pitch Effectiveness Predicter"),
  
  tabsetPanel(
    tabPanel("Tab 1",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 radioButtons(inputId = "p_hand", 
                              "Pitcher Hand",
                              choices = list("Right" = "R", 
                                             "Left" = "L"),
                              selected = "R"),
                 
                 radioButtons(inputId = "b_hand", 
                              "Batter Hand",
                              choices = list("Right" = "R", 
                                             "Left" = "L"),
                              selected = "R"),
                 
                 numericInput(inputId = "speed",
                              label = "Pitch Speed",
                              min = 60,
                              max = 105,
                              value = 94,
                              step = 1),
                 
                 selectInput(inputId = "pitch",
                              label = "Pitch Type",
                              choices = c(
                                "Fastball" = "FF",
                                "Sinker" = "SI",
                                "Cutter" = "FC",
                                "Slider" = "SL",
                                "Sweeper" = "ST",
                                "Curveball" = "CU",
                                "Splitter" = "FS",
                                "Change Up" = "CH"),
                             selected = "Fastball"),
                 
                 sliderInput(inputId = "movement_x",
                             label = "Horizontal Movement",
                             min = -10, 
                             max = 10,
                             value = 0),
                 
                 sliderInput(inputId = "movement_z",
                             label = "Vertical Movement (Induced)",
                             min = -10, 
                             max = 10, 
                             value = 0),
                 
                 textOutput("coord_x"),
                 textOutput("coord_y")
               ),
               
               mainPanel(
                 br(),
                 plotOutput("zone", click = "plot_click"),
                 br(),
                 tableOutput("limits"),
                 br(),
               )
             )
    ), 
  ) # tabPanel end
  
  
) # ui Fluid end


server <- function(input, output) {
  
  click_coords <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_click, {
    click_coords$x <- tail(c(click_coords$x, input$plot_click$x), 1)
    click_coords$y <- tail(c(click_coords$y, input$plot_click$y), 1)
  })
  
  output$zone <- renderPlot({
    ggplot() +
      geom_plate() +
      geom_rect(data = data.frame(xmin = click_coords$x - 0.1,
                                  ymin = click_coords$y - 0.1,
                                  xmax = click_coords$x + 0.1,
                                  ymax = click_coords$y + 0.1),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      # geom_point(data = data.frame(x = click_coords$x, y = click_coords$y),
      #           aes(x = x, y = y), color = "black") +
      xlim(-2, 2) + ylim(-0.5, 5) + coord_fixed() +
      theme_void()
  })
  
  output$coord_x <- renderText({
    paste("x = ", ifelse(click_coords$x > -1000, sprintf("%.2f", round(click_coords$x, 2)), ""))
  })
  
  output$coord_y <- renderText({
    paste("y = ", ifelse(click_coords$x > -1000, sprintf("%.2f", round(click_coords$y, 2)), ""))
  })
  
  output$limits <- renderTable({
    
    pitch_lims <- movement %>% 
      filter(pitch_type == input$pitch, 
             pitch_speed == input$speed,
             p_throws == input$p_hand, 
             hitter == input$b_hand)
    
    pitch_lims %>% 
      mutate("X Range" = paste(min_x, "to", max_x),
             "Y Range" = paste(min_z, "to", max_z)) %>% 
      select(`X Range`, `Y Range`)
    
  })
  
}


shinyApp(ui = ui, server = server)