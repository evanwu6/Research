library(tidyverse)
library(shiny)
library(shinyjs)
library(scales)
library(knitr)
library(broom)

# Data ####
movement <- read_csv("Movement.csv") %>% 
  mutate(min_x = min_x*12,
         max_x = max_x*12,
         min_z = min_z*12,
         max_z = max_z*12) %>% 
  filter(min_spin != "Inf", 
         !is.na(pitch_speed))

comp_data <- read_csv("Pitcher_Comps.csv") %>% 
  select(-...1) %>% 
  mutate(`X Movement` = round(`X Movement`*12, 2),
         `Z Movement` = round(`Z Movement`*12, 2),
         `Spin Rate` = round(`Spin Rate`, 0),
         Speed = round(Speed, 1))

comp_mean <- comp_data %>% 
  group_by(p_throws, pitch_type) %>% 
  summarize(mean_speed = weighted.mean(Speed, Pitches, na.rm = TRUE),
            mean_spin = weighted.mean(`Spin Rate`, Pitches, na.rm = TRUE),
            mean_move_x = weighted.mean(`X Movement`, Pitches, na.rm = TRUE),
            mean_move_z = weighted.mean(`Z Movement`, Pitches, na.rm = TRUE),
            std_speed = sd(Speed),
            std_spin = sd(`Spin Rate`),
            std_move_x = sd(`X Movement`),
            std_move_z = sd(`Z Movement`))

model <- read_csv("models3.csv") %>% 
  select(-...1)

pred_means <- read_csv("pred means.csv") %>% 
  select(-...1)


zone_data <- read_csv("zone_data.csv") %>% 
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
  
  output$zone <- renderPlot({
    
    # Whiff
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(input$pitch == "FF" ~ "fastball", 
                       input$pitch == "SL" ~ "slider",
                       input$pitch == "CU" ~ "curveball",
                       input$pitch == "CH" ~ "changeup",
                       TRUE ~ "fastball")
    
    pitch_speed <- input$speed
    pfx_x <- input$movement_x
    pfx_z <- input$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    # dist_x <- click_coords$x
    # dist_z <- click_coords$y
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- input$spin
    
    plate_x <- dist_x*0.708333
    plate_z <- 2.5 + dist_z*0.9
    
    zone <- case_when(
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 3,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.8) & plate_z <= 3.4 ~ 2,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 1,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 6,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.2) & plate_z < (2.8) ~ 5,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 4,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 9,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z <= (2.2) & plate_z >= 1.6 ~ 8,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 7,
      
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 2.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 2.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 2.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 2.5 ~ 14)
    
    whiff <- model %>% 
      filter(Response == "whiff",
             Hand == "right",
             Pitch == pitch)
    
    test <- whiff %>% 
      select(term:estimate) %>% 
      pivot_wider(names_from = "term",
                  values_from = "estimate")
    
    
    possible_cols <- c("(Intercept)", "pitch_speed", "pfx_x", "pfx_z", "pfx_total", 
                       "zone1", "zone2", "zone3", "zone4", "zone6", "zone7", 
                       "zone8", "zone9", "zone11", "zone12", "zone13", "zone14", 
                       "dist_x", "dist_z", "dist_prop", "I(dist_x^2)", "I(dist_z^2)", 
                       "release_spin_rate", "pred_bwhiff")
    
    for (col_name in possible_cols) {
      if (col_name %in% colnames(test)) {
      } else {
        test[[col_name]] <- 0
      }
    }
    
    zone_value <- case_when(zone == 1 ~  test$zone1,
                            zone == 2 ~  test$zone2,
                            zone == 3 ~  test$zone3,
                            zone == 4 ~  test$zone4,
                            zone == 6 ~  test$zone6,
                            zone == 7 ~  test$zone7,
                            zone == 8 ~  test$zone8,
                            zone == 9 ~  test$zone9,
                            zone == 11 ~  test$zone11,
                            zone == 12 ~  test$zone12,
                            zone == 13 ~  test$zone13,
                            zone == 14 ~  test$zone14,
                            TRUE ~ 0)
    
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$release_spin_rate*release_spin_rate + zone_value
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      filter(response == "whiff",
             hand == "right",
             pitch == pitch_type) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
  
        
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 3.25, alpha = 1) +
      scale_color_gradient2(low = "antiquewhite", high = "red") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_rect(data = data.frame(xmin = dist_x - 0.075,
                                  ymin = dist_z - 0.075/1.27,
                                  xmax = dist_x + 0.075,
                                  ymax = dist_z + 0.075/1.27),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2.5, 2.5) + ylim(-2.5, 2.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Whiff Chance",
           caption = paste("Location Whiff Chance:", prediction))
    
    
    
  })
  
  
  
  output$zone2 <- renderPlot({
    
    # Barrel
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(input$pitch == "FF" ~ "fastball", 
                       input$pitch == "SL" ~ "slider",
                       input$pitch == "CU" ~ "curveball",
                       input$pitch == "CH" ~ "changeup",
                       TRUE ~ "fastball")
    
    pitch_speed <- input$speed
    pfx_x <- input$movement_x
    pfx_z <- input$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    # dist_x <- click_coords$x
    # dist_z <- click_coords$y
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- input$spin
    
    plate_x <- dist_x*0.708333
    plate_z <- 2.5 + dist_z*0.9
    
    
    zone <- case_when(
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 3,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.8) & plate_z <= 3.4 ~ 2,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 1,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 6,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.2) & plate_z < (2.8) ~ 5,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 4,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 9,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z <= (2.2) & plate_z >= 1.6 ~ 8,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 7,
      
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 2.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 2.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 2.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 2.5 ~ 14)
    
    barrel <- model %>% 
      filter(Response == "barrel",
             Hand == "right",
             Pitch == pitch)
    
    test <- barrel %>% 
      select(term:estimate) %>% 
      pivot_wider(names_from = "term",
                  values_from = "estimate")
    
    
    possible_cols <- c("(Intercept)", "pitch_speed", "pfx_x", "pfx_z", "pfx_total", 
                       "zone1", "zone2", "zone3", "zone4", "zone6", "zone7", 
                       "zone8", "zone9", "zone11", "zone12", "zone13", "zone14", 
                       "dist_x", "dist_z", "dist_prop", "I(dist_x^2)", "I(dist_z^2)", 
                       "release_spin_rate", "pred_bbarrel")
    
    for (col_name in possible_cols) {
      if (col_name %in% colnames(test)) {
      } else {
        test[[col_name]] <- 0
      }
    }
    
    zone_value <- case_when(zone == 1 ~  test$zone1,
                            zone == 2 ~  test$zone2,
                            zone == 3 ~  test$zone3,
                            zone == 4 ~  test$zone4,
                            zone == 6 ~  test$zone6,
                            zone == 7 ~  test$zone7,
                            zone == 8 ~  test$zone8,
                            zone == 9 ~  test$zone9,
                            zone == 11 ~  test$zone11,
                            zone == 12 ~  test$zone12,
                            zone == 13 ~  test$zone13,
                            zone == 14 ~  test$zone14,
                            TRUE ~ 0)
    
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$release_spin_rate*release_spin_rate + zone_value
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      filter(response == "barrel",
             hand == "right",
             pitch == pitch_type) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 3.25, alpha = 1) +
      scale_color_gradient2(low = "antiquewhite", high = "red") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_rect(data = data.frame(xmin = dist_x - 0.075,
                                  ymin = dist_z - 0.075/1.27,
                                  xmax = dist_x + 0.075,
                                  ymax = dist_z + 0.075/1.27),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2.5, 2.5) + ylim(-2.5, 2.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Barrel Chance",
           caption = paste("Location Barrel Chance:", prediction))
    
    
    
  })
  
  
  
  output$zone3 <- renderPlot({
    
    # Strike
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(input$pitch == "FF" ~ "fastball", 
                       input$pitch == "SL" ~ "slider",
                       input$pitch == "CU" ~ "curveball",
                       input$pitch == "CH" ~ "changeup",
                       TRUE ~ "fastball")
    
    pitch_speed <- input$speed
    pfx_x <- input$movement_x
    pfx_z <- input$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    # dist_x <- click_coords$x
    # dist_z <- click_coords$y
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- input$spin
    
    plate_x <- dist_x*0.708333
    plate_z <- 2.5 + dist_z*0.9
    
    
    zone <- case_when(
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 3,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.8) & plate_z <= 3.4 ~ 2,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 1,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 6,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.2) & plate_z < (2.8) ~ 5,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 4,
      
      plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 9,
      plate_x >= (-17/72) & plate_x < (17/72) & plate_z <= (2.2) & plate_z >= 1.6 ~ 8,
      plate_x >= (17/72) & plate_x <= (39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 7,
      
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 2.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 2.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 2.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 2.5 ~ 14)
    
    strike <- model %>% 
      filter(Response == "strike",
             Hand == "right",
             Pitch == pitch)
    
    test <- strike %>% 
      select(term:estimate) %>% 
      pivot_wider(names_from = "term",
                  values_from = "estimate")
    
    
    possible_cols <- c("(Intercept)", "pitch_speed", "pfx_x", "pfx_z", "pfx_total", 
                       "zone1", "zone2", "zone3", "zone4", "zone6", "zone7", 
                       "zone8", "zone9", "zone11", "zone12", "zone13", "zone14", 
                       "dist_x", "dist_z", "dist_prop", "I(dist_x^2)", "I(dist_z^2)", 
                       "release_spin_rate", "pred_bstrike")
    
    for (col_name in possible_cols) {
      if (col_name %in% colnames(test)) {
      } else {
        test[[col_name]] <- 0
      }
    }
    
    zone_value <- case_when(zone == 1 ~  test$zone1,
                            zone == 2 ~  test$zone2,
                            zone == 3 ~  test$zone3,
                            zone == 4 ~  test$zone4,
                            zone == 6 ~  test$zone6,
                            zone == 7 ~  test$zone7,
                            zone == 8 ~  test$zone8,
                            zone == 9 ~  test$zone9,
                            zone == 11 ~  test$zone11,
                            zone == 12 ~  test$zone12,
                            zone == 13 ~  test$zone13,
                            zone == 14 ~  test$zone14,
                            TRUE ~ 0)
    
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$release_spin_rate*release_spin_rate + zone_value
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      filter(response == "strike",
             hand == "right",
             pitch == pitch_type) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 3.25, alpha = 1) +
      scale_color_gradient2(low = "antiquewhite", high = "red") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_rect(data = data.frame(xmin = dist_x - 0.075,
                                  ymin = dist_z - 0.075/1.27,
                                  xmax = dist_x + 0.075,
                                  ymax = dist_z + 0.075/1.27),
                aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
                fill = "blue", color = "black") +
      xlim(-2.5, 2.5) + ylim(-2.5, 2.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Strike Chance",
           caption = paste("Location Strike Chance:", prediction))
    
    
    
  })
  
  
  
  output$preds <- renderTable({
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(input$pitch == "FF" ~ "fastball", 
                       input$pitch == "SL" ~ "slider",
                       input$pitch == "CU" ~ "curveball",
                       input$pitch == "CH" ~ "changeup",
                       TRUE ~ "fastball")
    
    pitch_speed <- input$speed
    pfx_x <- input$movement_x
    pfx_z <- input$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    # dist_x <- click_coords$x
    # dist_z <- click_coords$y
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- input$spin
    
    plate_x <- dist_x*0.708333
    plate_z <- 2.5 + dist_z*0.9
    
    zone <- case_when(
        plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 3,
        plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.8) & plate_z <= 3.4 ~ 2,
        plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.8) & plate_z <= 3.4 ~ 1,
        
        plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 6,
        plate_x >= (-17/72) & plate_x < (17/72) & plate_z >= (2.2) & plate_z < (2.8) ~ 5,
        plate_x >= (17/72) & plate_x <= (39/48) & plate_z >= (2.2) & plate_z < (2.8) ~ 4,
        
        plate_x <= (-17/72) & plate_x >= (-39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 9,
        plate_x >= (-17/72) & plate_x < (17/72) & plate_z <= (2.2) & plate_z >= 1.6 ~ 8,
        plate_x >= (17/72) & plate_x <= (39/48) & plate_z <= (2.2) & plate_z >= 1.6 ~ 7,
        
        plate_x >= 0 & plate_x <= (121/48) & plate_z >= 2.5 ~ 11,
        plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 2.5 ~ 12,
        plate_x >= 0 & plate_x <= (121/48) & plate_z < 2.5 ~ 13,
        plate_x <= 0 & plate_x >= (-121/48) & plate_z < 2.5 ~ 14)
    
    m <- model %>% 
      filter(Hand == input$b_hand,
             Pitch == pitch)
    
    whiff <- m %>% 
      filter(Response == "whiff")
  
    barrel <- m %>% 
      filter(Response == "barrel")
    
    strike <- m %>% 
      filter(Response == "strike")
    
    preds <- pred_means %>% 
      filter(hitter == input$b_hand,
             pitch_type == input$pitch)
    
    
    pred <- data.frame(
      "Whiff" = 0.2,
      "Barrel" = 0.1,
      "Strike" = 0.6) %>% 
      mutate(Whiff = scales::percent(Whiff),
             Barrel = scales::percent(Barrel),
             Strike = scales::percent(Strike))
    
    
    t <- data.frame(pitch, pitch_speed, pfx_x, pfx_z, pfx_total, plate_x, plate_z, dist_x, dist_z, dist_prop, release_spin_rate, zone)
    
    t
    
    })
  
  output$preds_title <- renderText({
    
    "Predicted Outcome Likelihood"
    
  })
  
}


shinyApp(ui = ui, server = server)

