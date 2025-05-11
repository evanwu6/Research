library(tidyverse)
library(shiny)
library(shinyjs)
library(scales)
library(knitr)
library(broom)
library(ggforce)
library(gt)
library(ggimage)


# Data ####
movement <- read_csv("Movement.csv") %>% 
  mutate(min_x2 = -min_x*12,
         max_x2 = -max_x*12,
         min_z = min_z*12,
         max_z = max_z*12) %>%
  filter(min_spin != "Inf", 
         !is.na(pitch_speed)) %>% 
  mutate(min_x = max_x2,
         max_x = min_x2) %>% 
  select(-min_x2, -max_x2)

comp_data <- read_csv("Pitcher_App_Comps.csv") %>% 
  mutate(`X Movement` = -round(`X Movement`*12, 2),
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

model <- read_csv("models_final.csv") %>% 
  mutate(Pitch = str_replace(Pitch, "FF", "fastball"),
         Pitch = str_replace(Pitch, "SL", "slider"),
         Pitch = str_replace(Pitch, "CU", "curveball"),
         Pitch = str_replace(Pitch, "CH", "changeup"),
         Pitch = str_replace(Pitch, "FC", "cutter"),
         Pitch = str_replace(Pitch, "SI", "sinker"),
         Pitch = str_replace(Pitch, "FS", "splitter")) %>% 
  mutate(PHand = ifelse(PHand == "R", "right", "left")) %>%
  mutate(BHand = ifelse(BHand == "R", "right", "left")) %>%
  mutate(term = case_when(term == "pred" & Response == "whiff" ~ "pred_bwhiff",
                          term == "pred" & Response == "barrel" ~ "pred_bbarrel",
                          term == "pred" & Response == "benefit" ~ "pred_bbenefit",
                          TRUE ~ term))
  

pred_means <- read_csv("pred means.csv") %>% 
  select(-...1)


zone_data <- read_csv("zone_data_pc.csv") %>% 
  mutate(pred = ifelse(N >= 50, pred, NA))

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

# Zone Function (by Proportion)
get_zone <- function(x, z){
  
  case_when(x > 0.33 & x <= 1 & z <= 1 & z > 0.33 ~ "1",
                 x <= 0.33 & x >= -0.33 & z <= 1 & z > 0.33 ~ "2",
                 x < -0.33 & x >= -1 & z <= 1 & z > 0.33 ~ "3",
                 
                 x > 0.33 & x <= 1 & z <= 0.33 & z >= -0.33 ~ "4",
                 x <= 0.33 & x >= -0.33 & z <= 0.33 & z >= -0.33 ~ "5",
                 x < -0.33 & x >= -1 & z <= 0.33 & z >= -0.33 ~ "6",
                 
                 x > 0.33 & x <= 1 & z < -0.33 & z >= -1 ~ "7",
                 x <= 0.33 & x >= -0.33 & z < -0.33 & z >= -1 ~ "8",
                 x < -0.33 & x >= -1 & z < -0.33 & z >= -1 ~ "9",
                 
                 x > 0 & z >= 0 ~ "11",
                 x <= 0 & z > 0 ~ "12",
                 x >= 0 & z < 0 ~ "13",
                 x < 0 & z <= 0 ~ "14",
                  
                TRUE ~ "15")

  
}



ui <- fluidPage(
 
  titlePanel("Pitch Effectiveness Predicter"),
  
  tags$head(
    tags$style(HTML("
      body, label, .control-label, .btn, h1, h2, h3, h4, h5, h6 {
        font-family: 'Times New Roman', Times, serif; }"))),
                     
  fluidRow(
    column(2,
             radioButtons(inputId = "p_hand", 
                              "Pitcher Hand",
                              choices = list("Right" = "R", 
                                             "Left" = "L"),
                              selected = "R",
                              inline = TRUE),
                 
                 radioButtons(inputId = "b_hand", 
                              "Batter Hand",
                              choices = list("Right" = "R", 
                                             "Left" = "L"),
                              selected = "R",
                              inline = TRUE),
           
           selectInput(inputId = "pitch",
                       label = "Pitch Type",
                       choices = c(
                         "Fastball" = "FF",
                         "Sinker" = "SI",
                         "Cutter" = "FC",
                         "Slider" = "SL",
                         "Curveball" = "CU",
                         "Change Up" = "CH",
                         "Splitter" = "FS"),
                       selected = "Fastball")
    ),
    column(4,             
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
    column(2,            
           actionButton("refresh", "Update Graphs"),
    ),
    
    column(6, 
           h3(textOutput("graphs_title"))),
    
  ),
  
  fluidRow(
    column(2,
    ),
    column(6, 
           h4(textOutput("update_indicator")),
           br())
    
  ),
  
  
  
  fluidRow(
    tags$style(type = "text/css",
               ".shiny-output-error {visibility:hidden;}",
               ".shiny-output-error:before {visibility:hidden}"
               ), # suppressing error messages
    
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


server <- function(input, output, session) {

  
  
  # stored_inputs <- list(pitch = "FF",
  #                                 speed = 93,
  #                                 movement_x = 11,
  #                                 movement_z = 16,
  #                                 spin = 2000,
  #                                 p_hand = "R",
  #                                 b_hand = "R")
  
  
  # open <- observe({
  #   
  #   shinyjs::click("refresh")
  #   open$destroy()
  #   
  # })
  
  
  click_coords <- reactiveValues(x = 0, y = 0)
  
  
  observeEvent(input$plot_click, {
    click_coords$x <- tail(c(click_coords$x, input$plot_click$x), 1)
    click_coords$y <- tail(c(click_coords$y, input$plot_click$y), 1)
  })
  
  
  
  speed_filter <- reactive({
    speed_data <- movement %>%
      filter(pitch_type == input$pitch, p_throws == input$p_hand)
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
  
  showModal(
    modalDialog(
      p(style = "font-family: 'Times New Roman';",
      "This app takes pitch metric inputs and matchup information and predicts
      the likelihood of various outcomes based on these inputted metrics and
      the specified pitch location, which can be changed by clicking any heat map.",
      br(),
      br(),
      "Set the input sliders and use the 'update graphs' button to create and update the heat maps with your pitch metric inputs.",
      br(),
      br(),
      "The heat maps and likelihood predictions are created using logistic regression models
      using pitch-by-pitch data from the 2022 MLB season for every pitcher with 1800+ pitches on the season.
      A purple and light pink color scale is used to denote the likelihood of an outcome, with purple being more desireable for the pitcher. Gray is shown in areas where the combination of pitch type, matchup, and location has insufficient data."),
      br(),
      "The pitch comparisons table displays the most similar pitches from the 2024 MLB season based on the inputted metrics. The table displays the metrics and performance of each pitch, as well as a similarity score. Similarity is based on a z-score metric where pitches that have lower scores are closer to the input values.",
      title = "App Explanation",
      footer = tagList(
        modalButton("x")
      )
    )
  )
  
  observeEvent(input$p_hand, {
    pitch_choices <- if (input$p_hand == "L") {
      c("Fastball" = "FF",
        "Sinker" = "SI",
        "Cutter" = "FC",
        "Slider" = "SL",
        "Curveball" = "CU",
        "Change Up" = "CH")
    } 
    
    else {
      c("Fastball" = "FF",
        "Sinker" = "SI",
        "Cutter" = "FC",
        "Slider" = "SL",
        "Curveball" = "CU",
        "Change Up" = "CH",
        "Splitter" = "FS")
    }
    
    new_selection <- if (input$pitch %in% pitch_choices) input$pitch else "CH"
    
    updateSelectInput(session, "pitch", choices = pitch_choices, selected = new_selection)
  })
  
  
  stored_inputs <- reactiveValues()


  observeEvent(input$refresh, ignoreNULL = FALSE, {
    stored_inputs$pitch <- input$pitch
    stored_inputs$speed <- input$speed
    stored_inputs$movement_x <- input$movement_x
    stored_inputs$movement_z <- input$movement_z
    stored_inputs$spin <- input$spin
    stored_inputs$p_hand <- input$p_hand
    stored_inputs$b_hand <- input$b_hand
  })

  
  
  
  zone_plot1 <- reactive({
    
    # Whiff
    
    req(click_coords$x, click_coords$y)
    
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(stored_inputs$pitch == "FF" ~ "fastball", 
                       stored_inputs$pitch == "SL" ~ "slider",
                       stored_inputs$pitch == "CU" ~ "curveball",
                       stored_inputs$pitch == "CH" ~ "changeup",
                       stored_inputs$pitch == "FC" ~ "cutter", 
                       stored_inputs$pitch == "SI" ~ "sinker",
                       stored_inputs$pitch == "FS" ~ "splitter",
                       TRUE ~ "fastball")
    
    pitch_speed <- stored_inputs$speed
    pfx_x <- -stored_inputs$movement_x
    pfx_z <- stored_inputs$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    dist_x <- click_coords$x
    dist_z <- click_coords$y
    dist_x2 <- ifelse(is.null(dist_x), 0, dist_x^2)
    dist_z2 <- ifelse(is.null(dist_z), 0, dist_z^2)
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- stored_inputs$spin
    b_hand <- stored_inputs$b_hand
    p_hand <- stored_inputs$p_hand
    
    plate_x <- dist_x*0.708333
    plate_z <- 1.5 + dist_z*0.9
    
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
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 1.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 1.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 1.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 1.5 ~ 14)
    
    whiff <- model %>% 
      mutate(PHand = ifelse(PHand == "right", "R", "L")) %>% 
      mutate(BHand = ifelse(BHand == "right", "R", "L")) %>% 
      filter(Response == "whiff",
             PHand == p_hand,
             BHand == b_hand,
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
      if (!(col_name %in% colnames(test))) {
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
    
    pred_bwhiff <- pred_means %>% 
      filter(p_throws == p_hand,
             hitter == b_hand,
             pitch_type == stored_inputs$pitch) %>% 
      select(whiff_mean)
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$`I(dist_x^2)`*dist_x2 + test$`I(dist_z^2)`*dist_z2 +
      test$release_spin_rate*release_spin_rate + zone_value + test$pred_bwhiff*pred_bwhiff$whiff_mean
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate + test$pred_bwhiff*pred_bwhiff$whiff_mean
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      mutate(bhand = ifelse(bhand == "right", "R", "L"),
             phand = ifelse(phand == "right", "R", "L")) %>% 
      filter(response == "whiff",
             phand == p_hand,
             bhand == b_hand,
             pitch == pitch_type) %>% 
      mutate(zone = get_zone(x, z)) %>% 
      mutate(z_pred = case_when(zone == "1" ~  test$zone1,
                                zone == "2" ~  test$zone2,
                                zone == "3" ~  test$zone3,
                                zone == "4" ~  test$zone4,
                                zone == "6" ~  test$zone6,
                                zone == "7" ~  test$zone7,
                                zone == "8" ~  test$zone8,
                                zone == "9" ~  test$zone9,
                                zone == "11" ~  test$zone11,
                                zone == "12" ~  test$zone12,
                                zone == "13" ~  test$zone13,
                                zone == "14" ~  test$zone14,
                                TRUE ~ 0)) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred + z_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
    
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 5, alpha = 1) +
      scale_color_gradient(low = "#FFE6EE", high = "purple",
                           labels = scales::percent, na.value = "gray85") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_point(data = NULL, aes(x = click_coords$x, y = click_coords$y),
                 color = "gray85", fill = "white", shape = 21, size = 10) +
      geom_curve(data = NULL, aes(x = click_coords$x - 0.08, xend = click_coords$x - 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = 0.4,
                 color = "red") +
      geom_curve(data = NULL, aes(x = click_coords$x + 0.08, xend = click_coords$x + 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = -0.4,
                 color = "red") +
      xlim(-1.5, 1.5) + ylim(-1.5, 1.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Swing & Miss Chance",
           caption = paste("Swing & Miss Chance:", prediction),
           color = "Prediction") +
      theme(text = element_text(family = "Times New Roman"),
            plot.caption = element_text(hjust = 0.5, size = 16),
            plot.title = element_text(hjust = 0.5, size = 20))
  })
  
  output$zone <- renderPlot({
    zone_plot1()
  })
  
  
  
  zone_plot2 <- reactive({
    
    # Barrel
    
    req(click_coords$x, click_coords$y)
    
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(stored_inputs$pitch == "FF" ~ "fastball", 
                       stored_inputs$pitch == "SL" ~ "slider",
                       stored_inputs$pitch == "CU" ~ "curveball",
                       stored_inputs$pitch == "CH" ~ "changeup",
                       stored_inputs$pitch == "FC" ~ "cutter", 
                       stored_inputs$pitch == "SI" ~ "sinker",
                       stored_inputs$pitch == "FS" ~ "splitter",
                       TRUE ~ "fastball")
    
    pitch_speed <- stored_inputs$speed
    pfx_x <- -stored_inputs$movement_x
    pfx_z <- stored_inputs$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    dist_x <- click_coords$x
    dist_z <- click_coords$y
    dist_x2 <- ifelse(is.null(dist_x), 0, dist_x^2)
    dist_z2 <- ifelse(is.null(dist_z), 0, dist_z^2)
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- stored_inputs$spin
    b_hand <- stored_inputs$b_hand
    p_hand <- stored_inputs$p_hand
    
    plate_x <- dist_x*0.708333
    plate_z <- 1.5 + dist_z*0.9
    
    
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
      
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 1.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 1.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 1.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 1.5 ~ 14)
    
    barrel <- model %>% 
      mutate(PHand = ifelse(PHand == "right", "R", "L"),
             BHand = ifelse(BHand == "right", "R", "L")) %>% 
      filter(Response == "barrel",
             PHand == p_hand,
             BHand == b_hand,
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
    
    pred_bbarrel <- pred_means %>% 
      filter(p_throws == p_hand,
             hitter == b_hand,
             pitch_type == stored_inputs$pitch) %>% 
      select(barrel_mean)
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$`I(dist_x^2)`*dist_x2 + test$`I(dist_z^2)`*dist_z2 +
      test$release_spin_rate*release_spin_rate + zone_value + test$pred_bbarrel*pred_bbarrel$barrel_mean
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate + test$pred_bbarrel*pred_bbarrel$barrel_mean
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      mutate(phand = ifelse(phand == "right", "R", "L"),
             bhand = ifelse(bhand == "right", "R", "L")) %>% 
      filter(response == "barrel",
             phand == p_hand,
             bhand == b_hand,
             pitch == pitch_type) %>% 
      mutate(zone = get_zone(x, z)) %>% 
      mutate(z_pred = case_when(zone == "1" ~  test$zone1,
                                zone == "2" ~  test$zone2,
                                zone == "3" ~  test$zone3,
                                zone == "4" ~  test$zone4,
                                zone == "6" ~  test$zone6,
                                zone == "7" ~  test$zone7,
                                zone == "8" ~  test$zone8,
                                zone == "9" ~  test$zone9,
                                zone == "11" ~  test$zone11,
                                zone == "12" ~  test$zone12,
                                zone == "13" ~  test$zone13,
                                zone == "14" ~  test$zone14,
                                TRUE ~ 0)) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred + z_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
    
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 5, alpha = 1) +
      scale_color_gradient(high = "#FFE6EE", low = "purple",
                            labels = scales::percent, na.value = "gray85") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_point(data = NULL, aes(x = click_coords$x, y = click_coords$y),
                 color = "gray85", fill = "white", shape = 21, size = 10) +
      geom_curve(data = NULL, aes(x = click_coords$x - 0.08, xend = click_coords$x - 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = 0.4,
                 color = "red") +
      geom_curve(data = NULL, aes(x = click_coords$x + 0.08, xend = click_coords$x + 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = -0.4,
                 color = "red") +
      xlim(-1.5, 1.5) + ylim(-1.5, 1.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Barrel Chance",
           caption = paste("Barrel Chance:", prediction),
           color = "Prediction") +
      theme(text = element_text(family = "Times New Roman"),
            plot.caption = element_text(hjust = 0.5, size = 16),
            plot.title = element_text(hjust = 0.5, size = 20))
    
    
    
  })
  
  output$zone2 <- renderPlot({
    zone_plot2()
  })
  
  
  
  zone_plot3 <- reactive({
    
    # Benefit
    
    req(click_coords$x, click_coords$y)
    
    
    dist_x <- 0
    dist_z <- 0
    
    pitch <- case_when(stored_inputs$pitch == "FF" ~ "fastball", 
                       stored_inputs$pitch == "SL" ~ "slider",
                       stored_inputs$pitch == "CU" ~ "curveball",
                       stored_inputs$pitch == "CH" ~ "changeup",
                       stored_inputs$pitch == "FC" ~ "cutter", 
                       stored_inputs$pitch == "SI" ~ "sinker",
                       stored_inputs$pitch == "FS" ~ "splitter",
                       TRUE ~ "fastball")
    
    pitch_speed <- stored_inputs$speed
    pfx_x <- -stored_inputs$movement_x
    pfx_z <- stored_inputs$movement_z
    pfx_total <- sqrt(pfx_x^2 + pfx_z^2)
    dist_x <- click_coords$x
    dist_z <- click_coords$y
    dist_x2 <- ifelse(is.null(dist_x), 0, dist_x^2)
    dist_z2 <- ifelse(is.null(dist_z), 0, dist_z^2)
    dist_prop <- sqrt(dist_z^2 + dist_x^2)
    release_spin_rate <- stored_inputs$spin
    b_hand <- stored_inputs$b_hand
    p_hand <- stored_inputs$p_hand
    
    plate_x <- dist_x*0.708333
    plate_z <- 1.5 + dist_z*0.9
    
    
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
      
      plate_x >= 0 & plate_x <= (121/48) & plate_z >= 1.5 ~ 11,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z >= 1.5 ~ 12,
      plate_x >= 0 & plate_x <= (121/48) & plate_z < 1.5 ~ 13,
      plate_x <= 0 & plate_x >= (-121/48) & plate_z < 1.5 ~ 14)
    
    benefit <- model %>% 
      mutate(PHand = ifelse(PHand == "right", "R", "L"),
             BHand = ifelse(BHand == "right", "R", "L")) %>% 
      filter(Response == "benefit",
             PHand == p_hand,
             BHand == b_hand,
             Pitch == pitch)
    
    test <- benefit %>% 
      select(term:estimate) %>% 
      pivot_wider(names_from = "term",
                  values_from = "estimate")
    
    
    possible_cols <- c("(Intercept)", "pitch_speed", "pfx_x", "pfx_z", "pfx_total", 
                       "zone1", "zone2", "zone3", "zone4", "zone6", "zone7", 
                       "zone8", "zone9", "zone11", "zone12", "zone13", "zone14", 
                       "dist_x", "dist_z", "dist_prop", "I(dist_x^2)", "I(dist_z^2)", 
                       "release_spin_rate", "pred_bbenefit")
    
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
    
    pred_bbenefit <- pred_means %>% 
      filter(p_throws == p_hand,
             hitter == b_hand,
             pitch_type == stored_inputs$pitch) %>% 
      select(benefit_mean)
    
    prediction <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$dist_x*dist_x + test$dist_z*dist_z + test$dist_prop*dist_prop +
      test$`I(dist_x^2)`*dist_x2 + test$`I(dist_z^2)`*dist_z2 +
      test$release_spin_rate*release_spin_rate + zone_value + test$pred_bbenefit*pred_bbenefit$benefit_mean
    
    prediction <- 1 / (1 + exp(-prediction))
    prediction <- scales::percent(prediction, accuracy = 0.1)
    
    gen_pred <- test$`(Intercept)` + test$pitch_speed*pitch_speed +
      test$pfx_x*pfx_x + test$pfx_z*pfx_z + test$pfx_total*pfx_total + 
      test$release_spin_rate*release_spin_rate + test$pred_bbenefit*pred_bbenefit$benefit_mean
    
    pitch_type <- pitch
    
    heat_map <- zone_data %>% 
      mutate(phand = ifelse(phand == "right", "R", "L"),
             bhand = ifelse(bhand == "right", "R", "L")) %>% 
      filter(response == "benefit",
             phand == p_hand,
             bhand == b_hand,
             pitch == pitch_type) %>% 
      mutate(zone = get_zone(x, z)) %>% 
      mutate(z_pred = case_when(zone == "1" ~  test$zone1,
                                zone == "2" ~  test$zone2,
                                zone == "3" ~  test$zone3,
                                zone == "4" ~  test$zone4,
                                zone == "6" ~  test$zone6,
                                zone == "7" ~  test$zone7,
                                zone == "8" ~  test$zone8,
                                zone == "9" ~  test$zone9,
                                zone == "11" ~  test$zone11,
                                zone == "12" ~  test$zone12,
                                zone == "13" ~  test$zone13,
                                zone == "14" ~  test$zone14,
                                TRUE ~ 0)) %>% 
      select(-response:-pitch) %>% 
      mutate(pred = pred + gen_pred + z_pred) %>% 
      mutate(pred = 1 / (1 + exp(-pred)))
    
    ggplot(heat_map) +
      geom_point(aes(x = x, y = z, 
                     color = pred), shape = 15, size = 5, alpha = 1) +
      scale_color_gradient(low = "#FFE6EE", high = "purple",
                            labels = scales::percent, na.value = "gray85") +
      geom_rect(xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                alpha = 0, color = "black", linewidth = 1) +
      geom_point(data = NULL, aes(x = click_coords$x, y = click_coords$y),
                 color = "gray85", fill = "white", shape = 21, size = 10) +
      geom_curve(data = NULL, aes(x = click_coords$x - 0.08, xend = click_coords$x - 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = 0.4,
                 color = "red") +
      geom_curve(data = NULL, aes(x = click_coords$x + 0.08, xend = click_coords$x + 0.08,
                                  y = click_coords$y - 0.065, yend = click_coords$y + 0.065),
                 curvature = -0.4,
                 color = "red") +
      xlim(-1.5, 1.5) + ylim(-1.5, 1.5) + coord_fixed(ratio = 1.27) +
      theme_void() +
      labs(title = "Positive Outcome Chance",
           caption = paste("Positive Outcome Chance:", prediction),
           color = "Prediction") +
      theme(text = element_text(family = "Times New Roman"),
            plot.caption = element_text(hjust = 0.5, size = 16),
            plot.title = element_text(hjust = 0.5, size = 20))
    
    
    
  })
  
  output$zone3 <- renderPlot({
    zone_plot3()
  })
  
  
  graphs_title <- reactive({
    
    pitch <- case_when(stored_inputs$pitch == "FF" ~ "Fastball", 
                       stored_inputs$pitch == "SL" ~ "Slider",
                       stored_inputs$pitch == "CU" ~ "Curveball",
                       stored_inputs$pitch == "CH" ~ "Change Up",
                       stored_inputs$pitch == "FC" ~ "Cutter", 
                       stored_inputs$pitch == "SI" ~ "Sinker",
                       stored_inputs$pitch == "FS" ~ "Splitter")
   
    params <- list(
    
    pitch = pitch,
    pitch_speed = stored_inputs$speed,
    pfx_x = -stored_inputs$movement_x,
    pfx_z = stored_inputs$movement_z,
    release_spin_rate = stored_inputs$spin,
    b_hand = stored_inputs$b_hand,
    p_hand = stored_inputs$p_hand
    
    )
    
    plot_title <- paste0("Graphs Display A ", params$pitch_speed, " MPH ", 
                        params$pitch, " From A ", params$p_hand, "HP To A ", 
                        params$b_hand, "HH")
    
    plot_title
    
    
    
  })
  
  output$graphs_title <- renderText({
    graphs_title()
  })
  
  
  
  info <- reactive({
    params <- list(
      pitch = stored_inputs$pitch,
      pitch_speed = stored_inputs$speed,
      pfx_x = -stored_inputs$movement_x,
      pfx_z = stored_inputs$movement_z,
      release_spin_rate = stored_inputs$spin,
      b_hand = stored_inputs$b_hand,
      p_hand = stored_inputs$p_hand
    )
    return(params)
  })
  
  
  output$update_indicator <- 
    renderText({
    current <- list(
      pitch = input$pitch,
      pitch_speed = input$speed,
      pfx_x = -input$movement_x,
      pfx_z = input$movement_z,
      release_spin_rate = input$spin,
      b_hand = input$b_hand,
      p_hand = input$p_hand
    )
    
    params <- info()
    
    
    if(current$pitch == params$pitch & current$pitch_speed == params$pitch_speed &
       current$pfx_x == params$pfx_x & current$pfx_z == params$pfx_z &
       current$release_spin_rate == params$release_spin_rate & 
       current$b_hand == params$b_hand & current$p_hand == params$p_hand) {
      
      indicator <- 0  # Same
    } 
    
    else {
      indicator <- 1  # Different
    }
    
    if (indicator == 0) {
      return("")
    } else {
      return("Graph values have been changed. Click 'update' for updated graphs")
    }
  })

  
  

  
  
  
  
  
  output$comps <- render_gt({
    p_hand <- stored_inputs$p_hand
    b_hand <- stored_inputs$b_hand
    pitch <- stored_inputs$pitch
    speed <- stored_inputs$speed
    spin <- stored_inputs$spin
    move_x <- stored_inputs$movement_x
    move_z <- stored_inputs$movement_z
    
    comps <- comp_data %>% 
      filter(p_throws == p_hand,
             hitter == b_hand) %>% 
      left_join(comp_mean, by = join_by(p_throws, pitch_type)) %>% 
      mutate(speed_sim = abs((Speed - speed) / comp_mean$std_speed)) %>%
      mutate(spin_sim = abs((`Spin Rate` - spin) / comp_mean$std_spin)) %>%
      mutate(bx_sim = abs((`X Movement` - move_x) / comp_mean$std_move_x)) %>%
      mutate(bz_sim = abs((`Z Movement` - move_z) / comp_mean$std_move_z)) %>%
      mutate(similarity = round(rowSums(cbind(speed_sim, spin_sim, bx_sim, bz_sim))/4, 2))
    
    comps <- comps %>% 
      arrange(similarity) %>% 
      select(pitch_type, Name, Speed, `Spin Rate`, 
             `X Movement`, `Z Movement`, 
             `Whiff Prop`, `Barrel Prop`, `Benefit Prop`, similarity) %>% 
      rename(Pitch = pitch_type,
             `+RE Prop` = `Benefit Prop`) %>% 
      mutate(Speed = format(Speed, nsmall = 1),
             `Spin Rate` = format(`Spin Rate`, big.mark = ",", nsmall = 0),
             `Hor Break` = format(`X Movement`, nsmall = 1),
             `IVB` = format(`Z Movement`, nsmall = 1),
             `Swing&Miss%` = percent(`Whiff Prop`, accuracy = 0.1),
             `Barrel%` = percent(`Barrel Prop`, accuracy = 0.1),
             `+RE%` = percent(`+RE Prop`, accuracy = 0.1),
             similarity = format(similarity, nsmall = 1)) %>% 
      mutate(Pitch = case_when(Pitch == "FF" ~ "Fastball",
                               Pitch == "SL" ~ "Slider",
                               Pitch == "CU" ~ "Curveball",
                               Pitch == "CH" ~ "Changeup",
                               Pitch == "FC" ~ "Cutter",
                               Pitch == "SI" ~ "Sinker",
                               Pitch == "FS" ~ "Splitter",
                               Pitch == "ST" ~ "Sweeper",
                               Pitch == "KC" ~ "Knuckle Curve")) %>% 
      select(Pitch, Name, Speed, `Spin Rate`, `Hor Break`, IVB, `Swing&Miss%`, `Barrel%`, `+RE%`, similarity) %>% 
      head(10)
    
    comps %>% 
      gt() %>% 
      tab_options(table.font.names = "Times New Roman")
    
    
  })
  

  
  
  
  
  
}


shinyApp(ui = ui, server = server)

