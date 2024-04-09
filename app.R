library(tidyverse)
library(shiny)
library(shinyjs)
library(scales)
library(knitr)

# Data ####
movement <- read_csv("Movement.csv") %>% 
  mutate(min_x = min_x*12,
         max_x = max_x*12,
         min_z = min_z*12,
         max_z = max_z*12) %>% 
  filter(min_spin != "Inf", 
         !is.na(pitch_speed))

comp_data <- read_csv("Pitcher_Comps.csv") %>% 
  select(-...1)

comp_mean <- comp_data %>% 
  group_by(pitch_hand, pitch_type) %>% 
  summarize(mean_speed = weighted.mean(avg_speed, pitches_thrown, na.rm = TRUE),
            mean_spin = weighted.mean(avg_spin, pitches_thrown, na.rm = TRUE),
            mean_move_x = weighted.mean(pitcher_break_x, pitches_thrown, na.rm = TRUE),
            mean_move_z = weighted.mean(pitcher_break_z, pitches_thrown, na.rm = TRUE))

model <- read_csv("models1.csv") %>% 
  select(-...1)


# Functions ####
geom_zone <- function(top = 3.4, bottom = 1.6, linecolor = "black"){
  geom_rect(xmin = -.7083, xmax = .7083, ymin = bottom, ymax = top,
            alpha = 0, color = linecolor, linewidth = 1)
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
  
                     
  fluidRow(
    column(2,
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
    ),
    column(4,             
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
                 
                 uiOutput("speed_ui"),
           uiOutput("spin_ui")
    ),
     column(3,            
                 uiOutput("movex_ui"),
                 
                 uiOutput("movez_ui"),
     ),
    column(6,            
           
    )
               
  ),
  fluidRow(
        column(3,       
               plotOutput("zone", click = "plot_click")),
  
        column(3,
                 plotOutput("zone2", click = "plot_click"),
                 br(),
                 br(),
               ),
        column(3,
               plotOutput("zone3", click = "plot_click"),
               br(),
               br(),
        )
  ),
  
  fluidRow(
    column(8,
           tableOutput("comps")
           ),
    column(4,
           h5(textOutput("preds_title")),
           tableOutput("preds")
    )
    )
  
  
) # ui Fluid end


server <- function(input, output) {
  
  click_coords <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$plot_click, {
    click_coords$x <- tail(c(click_coords$x, input$plot_click$x), 1)
    click_coords$y <- tail(c(click_coords$y, input$plot_click$y), 1)
  })
  
  output$zone <- renderPlot({
    ggplot(data.frame(x = 1, y = 1)) +
      geom_plate() +
      geom_zone() +
      geom_rect(data = data.frame(xmin = click_coords$x - 0.075,
                                  ymin = click_coords$y - 0.075,
                                  xmax = click_coords$x + 0.075,
                                  ymax = click_coords$y + 0.075),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2, 2) + ylim(0, 4) + coord_fixed() +
      theme_void()
  })
  
  output$coord_x <- renderText({
    paste("x = ", ifelse(click_coords$x > -1000, sprintf("%.2f", round(click_coords$x, 2)), ""))
  })
  
  output$coord_y <- renderText({
    paste("y = ", ifelse(click_coords$x > -1000, sprintf("%.2f", round(click_coords$y, 2)), ""))
  })
  
  speed_filter <- reactive({
    speed_data <- movement %>%
      filter(pitch_type == input$pitch)
    speed_data
  })
  
  output$speed_ui <- renderUI({
    sliderInput(inputId = "speed",
                label = "Pitch Speed",
                min = min(speed_filter()$pitch_speed, na.rm = TRUE),
                max = max(speed_filter()$pitch_speed, na.rm = TRUE),
                value = round(mean(speed_filter()$pitch_speed, na.rm = TRUE)),
                step = 1)
  })
  
  output$spin_ui <- renderUI({
    sliderInput(inputId = "spin",
                label = "Spin Rate",
                min = floor(min(speed_filter()$min_spin, na.rm = TRUE)/100)*100,
                max = ceiling(max(speed_filter()$max_spin, na.rm = TRUE)/100)*100,
                value = 2000,
                step = 100)
  })
  
  output$movex_ui <- renderUI({
    sliderInput(inputId = "movement_x",
                label = "Horizontal Movement",
                min = floor(min(speed_filter()$min_x, na.rm = TRUE)),
                max = ceiling(max(speed_filter()$max_x, na.rm = TRUE)),
                value = round(max(speed_filter()$min_x, na.rm = TRUE)),
                step = 0.5)
  })
  
  output$movez_ui <- renderUI({
    sliderInput(inputId = "movement_z",
                label = "Vertical Movement (Induced)",
                min = floor(min(speed_filter()$min_z, na.rm = TRUE)),
                max = ceiling(max(speed_filter()$max_z, na.rm = TRUE)),
                value = round(max(speed_filter()$min_z, na.rm = TRUE)),
                step = 0.5)
  })
  
  output$zone2 <- renderPlot({
    ggplot(data.frame(x = 1, y = 1)) +
      geom_plate() +
      geom_zone() +
      geom_rect(data = data.frame(xmin = click_coords$x - 0.075,
                                  ymin = click_coords$y - 0.075,
                                  xmax = click_coords$x + 0.075,
                                  ymax = click_coords$y + 0.075),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2, 2) + ylim(0, 4) + coord_fixed() +
      theme_void()
  })
  
  
  output$zone3 <- renderPlot({
    ggplot(data.frame(x = 1, y = 1)) +
      geom_plate() +
      geom_zone() +
      geom_rect(data = data.frame(xmin = click_coords$x - 0.075,
                                  ymin = click_coords$y - 0.075,
                                  xmax = click_coords$x + 0.075,
                                  ymax = click_coords$y + 0.075),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2, 2) + ylim(0, 4) + coord_fixed() +
      theme_void()
  })
  
  
  output$comps <- renderTable({
    hand <- input$p_hand
    pitch <- input$pitch
    speed <- input$speed
    spin <- input$spin
    move_x <- abs(input$movement_x)
    move_z <- input$movement_z
    
    comps <- comp_data %>% 
      filter(pitch_hand == hand,
             pitch_type == pitch) %>% 
      mutate(speed_sim = abs((avg_speed - speed) / comp_mean$mean_speed)) %>% 
      mutate(spin_sim = abs((avg_spin - spin) / comp_mean$mean_spin)) %>%
      mutate(bx_sim = abs((pitcher_break_x - move_x) / comp_mean$mean_move_x)) %>% 
      mutate(bz_sim = abs((pitcher_break_z - move_z) / comp_mean$mean_move_z)) %>% 
      mutate(similarity = round((4 - rowSums(cbind(speed_sim, spin_sim, bx_sim, bz_sim)))/4, 3)*100)
    
    comps %>% 
      arrange(desc(similarity)) %>% 
      select(pitcher_name, year, avg_speed, avg_spin, 
             pitcher_break_x, pitcher_break_z, similarity) %>% 
      mutate(year = as.character(year),
             avg_speed = format(avg_speed, nsmall = 1),
             avg_spin = format(avg_spin, big.mark = ",", nsmall = 0),
             pitcher_break_x = format(pitcher_break_x, nsmall = 1),
             pitcher_break_z = format(pitcher_break_z, nsmall = 1),
             similarity = format(similarity, nsmall = 1)) %>% 
      head(10)
    
  })
  
  
  
  output$preds <- renderTable({
    
    m <- model %>% 
      filter(pitch == "slider",
             type == "complex")
    
    whiff <- m %>% 
      filter(Response == "whiff")
  
    barrel <- m %>% 
      filter(Response == "barrel")
    
    strike <- m %>% 
      filter(Response == "strike")
    
    
    pred <- data.frame(
      "Whiff" = 0.2,
      "Barrel" = 0.1,
      "Strike" = 0.6) %>% 
      mutate(Whiff = scales::percent(Whiff),
             Barrel = scales::percent(Barrel),
             Strike = scales::percent(Strike))
    
    pred
    
    })
  
  output$preds_title <- renderText({
    
    "Predicted Outcome Likelihood"
    
  })
  
}


shinyApp(ui = ui, server = server)

