library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(gt)
library(scales)
library(shinyWidgets)
library(tidyverse)
library(readr)
library(pitchRx)
library(cowplot)
library(gtsummary)
library(ggrepel)

Game_File <- read.csv("Bullpens.csv") %>% 
  filter(PitcherTeam == "WM_PRA" | PitcherTeam == "WM_TRI") %>% 
  mutate(RelHeight = substr(RelHeight, 1, nchar(RelHeight)), 
         RelHeight = as.double(RelHeight),
         Balls = substr(Balls, 1, nchar(Balls)), 
         Balls = as.double(Balls),
         Strikes = substr(Strikes, 1, nchar(Strikes)), 
         Strikes = as.double(Strikes))
BACON <- read.csv("xwOBAcon.csv")
umpire_data <- read_csv("called_strike.csv")

Game_File$Count <- paste(Game_File$Balls, Game_File$Strikes, sep = "-")

# Checks and CSV adjustments
ChecksCSV <- Game_File %>%
  mutate(HCheck = case_when(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun') ~ TRUE, TRUE ~ FALSE),
         GBCheck = case_when(TaggedHitType %in% c('GroundBall') ~ TRUE, TRUE ~ FALSE),
         BBECheck = case_when(TaggedHitType %in% c('GroundBall', 'LineDrive', 'FlyBall', 'Popup') ~ TRUE, TRUE ~ FALSE), 
         SwingCheck = case_when(PitchCall %in% c('FoulBall', 'StrikeSwinging','InPlay') ~ TRUE, TRUE ~ FALSE),
         WhiffCheck = case_when(PitchCall %in% c('StrikeSwinging') ~ TRUE, TRUE ~ FALSE),
         CSWCheck = case_when(PitchCall %in% c('StrikeSwinging','StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         StrikeCheck = case_when(PitchCall %in% c('StrikeSwinging', 'FoulBall', 'InPlay','StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         ZoneCheck = case_when(between(PlateLocHeight, 1.59, 3.41) & between(PlateLocSide, -1, 1) ~ TRUE, TRUE ~ FALSE),
         SweetSpotCheck = case_when(between(Angle, 10, 30) ~ TRUE, TRUE ~ FALSE),
         HardHitCheck = case_when(between(ExitSpeed, 95, 120) ~ TRUE, TRUE ~ FALSE),
         BarrelCheck = case_when(between(Angle, 10, 30)  & between(ExitSpeed, 95, 120) ~ TRUE, TRUE ~ FALSE),
         WhiffCheck = case_when(PitchCall %in% c('StrikeSwinging') ~ TRUE, TRUE ~ FALSE),
         SwingCheck = case_when(PitchCall %in% c('StrikeSwinging', 'InPlay','FoulBall') ~ TRUE, TRUE ~ FALSE),
         CalledStrikeCheck = case_when(PitchCall %in% c('StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         BallCheck = case_when(PitchCall %in% c('BallCalled', 'HitByPitch') ~ TRUE, TRUE ~ FALSE),
         FoulCheck = case_when(PitchCall %in% c('FoulBall') ~ TRUE, TRUE ~ FALSE),
         SingleCheck = case_when(PlayResult %in% c('Single') ~ TRUE, TRUE ~ FALSE),
         DoubleCheck = case_when(PlayResult %in% c('Double') ~ TRUE, TRUE ~ FALSE),
         TripleCheck = case_when(PlayResult %in% c('Triple') ~ TRUE, TRUE ~ FALSE),
         HRCheck = case_when(PlayResult %in% c('HomeRun') ~ TRUE, TRUE ~ FALSE),
         SacCheck = case_when(PlayResult %in% c('Sacrifice') ~ TRUE, TRUE ~ FALSE),
         HBPCheck = case_when(PitchCall %in% c('HitByPitch') ~ TRUE, TRUE ~ FALSE),
         StrikeoutCheck = case_when(KorBB %in% c('Strikeout') ~ TRUE, TRUE ~ FALSE),
         WalkCheck = case_when(KorBB %in% c('Walk') ~ TRUE, TRUE ~ FALSE),
         BIPCheck = case_when(PlayResult %in% c('Undefined') ~ FALSE, TRUE ~ TRUE), 
         ErrorCheck = if_else(PlayResult %in% c('Error'), TRUE, FALSE),
         ABCheck = StrikeoutCheck + BIPCheck - SacCheck,
         PACheck = StrikeoutCheck + WalkCheck + HBPCheck + BIPCheck)

BACONCSV <- ChecksCSV %>% 
  mutate(floorExitSpeed = floor(ExitSpeed),
         floorLaunchAngle = trunc(Angle),) %>% 
  left_join(BACON, by = c("floorExitSpeed" = "launch_speed", 
                          "floorLaunchAngle" = "launch_angle"))

strike_exp_data <- BACONCSV %>%  
  mutate(RoundedPLH = round(PlateLocHeight, digits = 1),
         RoundedPLS = round(PlateLocSide, digits = 1)) %>% 
  left_join(umpire_data, by = c("RoundedPLH" = "PlateLocHeight",
                                "RoundedPLS" = "PlateLocSide"))

strike_exp_diff <- strike_exp_data %>% 
  mutate(strike_exp_diff = abs(StrikeCheck - called_strike_prob))

FinalCSV <- strike_exp_diff %>%
  mutate(across("av_xwOBAcon", ~replace_na(., 0)),
         across("strike_exp_diff", ~replace_na(., 0)),
         xwOBAvalues = av_xwOBAcon + WalkCheck * 0.83 + HBPCheck * 0.86 + StrikeoutCheck * 0)

FinalCSV$Date <- as.Date(FinalCSV$Date, "%Y-%m-%d")

FinalCSV$GameType <- ifelse(FinalCSV$Batter == "", "Bullpen", "Intersquad")

FinalCSV$CallType <- ifelse(strike_exp_diff$StrikeCheck & !strike_exp_diff$ZoneCheck 
                                   | !strike_exp_diff$StrikeCheck & strike_exp_diff$ZoneCheck, "Incorrect", "Correct")


FinalCSV$PitchCount <- 0
for (i in 1:nrow(FinalCSV)) {FinalCSV$PitchCount[i] <- sum(FinalCSV$Pitcher[1:i] == FinalCSV$Pitcher[i])}

# ui
{ui <- fluidPage(
    
    titlePanel("William & Mary Pitching Data"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "PitcherInput", label = "Select Pitcher", 
                    choices = c(All = "All", sort(unique(FinalCSV$Pitcher)))),
        dateRangeInput(inputId = "DateRangeInput", label = "Select Date Range", 
                       start = min(FinalCSV$Date), end = max(FinalCSV$Date)),
        img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/William_and_Mary_Tribe_wordmark.svg/1200px-William_and_Mary_Tribe_wordmark.svg.png", 
            style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 150)),
      mainPanel(
        tabsetPanel(
          tabPanel("Metrics/Results", br(), dataTableOutput("summary_table"), 
                   dataTableOutput("pitcher_summary_table"), 
                   dataTableOutput("pitcher_results_table"),
                   dataTableOutput("pitcher_splits_table")),
          tabPanel("Results By Split", br(), dataTableOutput("pitcher_count_results"),
                   dataTableOutput("pitch_type_table")),
          tabPanel("Heat Maps", br(), plotOutput("plot1")),
          tabPanel("Movement/Release", br(), plotOutput("pitch_movement_plot"),
                   plotOutput("pitch_release_plot")),
          tabPanel("Locations", br(), plotOutput("pitch_location_plot1"),
                   plotOutput("pitch_location_plot2"),
                   plotOutput("pitch_location_plot3")),
          tabPanel("Velocity/Stuff", br(), plotOutput("lineplot1"),
                   plotOutput("lineplot2"))
        )
      )
    )
  )
}  

server <- function(input, output, session) {
  
  observeEvent(input$PitcherInput, 
               updateSelectInput(session, inputId = "GameInput", label = "Select Game/Date:", 
                                 choices = sort(unique(FinalCSV$Date[FinalCSV$Pitcher == input$PitcherInput]))))
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)})
  
  output$selected_game <- renderText({paste(input$DateRangeInput)})
  
  output$summary_table <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
      summarize('Pitches' = n(),
                PA = sum(PACheck, na.rm = TRUE), 
                BBE = sum(BBECheck, na.rm = TRUE),
                H = sum(HCheck, na.rm = TRUE), 
                `1B` = sum(SingleCheck, na.rm = TRUE), 
                `2B` = sum(DoubleCheck, na.rm = TRUE), 
                `3B` = sum(TripleCheck, na.rm = TRUE), 
                HR = sum(HRCheck, na.rm = TRUE), 
                SO = sum(StrikeoutCheck, na.rm = TRUE), 
                BB = sum(WalkCheck, na.rm = TRUE), 
                HBP = sum(HBPCheck, na.rm = TRUE),
                Strikes = sum(StrikeCheck, na.rm = TRUE), 
                Chases = sum(SwingCheck[ZoneCheck == FALSE], na.rm = TRUE), 
                Whiffs = sum(WhiffCheck[SwingCheck == TRUE], na.rm = TRUE))
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  output$pitcher_summary_table <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
      group_by('Pitch' = TaggedPitchType) %>%
      summarize('Pitches' = n(),
                'Stuff+' = round(mean(Stuff, na.rm = TRUE), 0), 
                'AvgVelo' = round(mean(RelSpeed, na.rm = TRUE), 1), 
                'MaxVelo' = round(max(RelSpeed, na.rm = TRUE), 1), 
                'SpinRate' = round(mean(SpinRate, na.rm = TRUE), 0),
                'IVB' = round(mean(InducedVertBreak, na.rm = TRUE), 1),
                'HB' = round(mean(HorzBreak, na.rm = TRUE), 1),
                'RelZ' = round(mean(RelHeight, na.rm = TRUE), 1),
                'RelX' = round(mean(RelSide, na.rm = TRUE), 1)) %>% 
      mutate(Usage = Pitches / sum(Pitches)) %>% 
      mutate(Usage = scales::percent(Usage, accuracy = 0.1)) %>%
      select(Pitch, Pitches, Usage, `Stuff+`, AvgVelo, MaxVelo, SpinRate, IVB, HB, RelZ, RelX)
    
    table[is.na(table)] = 0  
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  output$pitcher_results_table <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%   
      group_by('Pitch' = TaggedPitchType) %>%
      summarize('Pitches' = n(),
                PA = sum(PACheck, na.rm = TRUE), 
                AB = sum(ABCheck, na.rm = TRUE), 
                H = sum(HCheck, na.rm = TRUE), 
                `1B` = sum(SingleCheck, na.rm = TRUE), 
                `2B` = sum(DoubleCheck, na.rm = TRUE), 
                `3B` = sum(TripleCheck, na.rm = TRUE), 
                HR = sum(HRCheck, na.rm = TRUE), 
                TB = (`1B`) + (`2B` * 2) + (`3B` * 3) + (HR * 4),
                SO = sum(StrikeoutCheck, na.rm = TRUE), 
                BB = sum(WalkCheck, na.rm = TRUE), 
                HBP = sum(HBPCheck, na.rm = TRUE),
                xwOBA = round(mean(xwOBAvalues[PACheck == TRUE], na.rm = TRUE), 3),
                `CSW%` = mean(CSWCheck, na.rm = TRUE), 
                `Strike%` = mean(StrikeCheck, na.rm = TRUE), 
                `StrikeProb%` = mean(called_strike_prob, na.rm = TRUE),
                `Zone%` = mean(ZoneCheck, na.rm = TRUE), 
                `Swing%` = mean(SwingCheck, na.rm = TRUE), 
                `Chase%` = mean(SwingCheck[ZoneCheck == FALSE], na.rm = TRUE), 
                `Z-Whiff%` = mean(WhiffCheck[ZoneCheck == TRUE & SwingCheck == TRUE]),
                `Whiff%` = mean(WhiffCheck[SwingCheck == TRUE], na.rm = TRUE), 
                `Z-Swing%` = mean(SwingCheck[ZoneCheck == TRUE], na.rm = TRUE),
                BBE = sum(BBECheck, na.rm = TRUE),
                xDamage = mean(av_xwOBAcon[BBECheck == TRUE],  na.rm = TRUE),
                AvgEV = mean(ExitSpeed[BBECheck == TRUE], na.rm = TRUE), 
                AVG = H / AB,
                OBP = (H + BB + HBP) / PA,
                SLG = TB / AB) %>% 
      mutate(`CSW%` = scales::percent(`CSW%`, accuracy = 0.1),
             `Strike%` = scales::percent(`Strike%`, accuracy = 0.1),
             `StrikeProb%` = scales::percent(`StrikeProb%`, accuracy = 0.1),
             `Zone%` = scales::percent(`Zone%`, accuracy = 0.1),
             `Chase%` = scales::percent(`Chase%`, accuracy = 0.1),
             `Whiff%` = scales::percent(`Whiff%`, accuracy = 0.1),
             `Z-Whiff%` = scales::percent(`Z-Whiff%`, accuracy = 0.1),
             `Z-Swing%` = scales::percent(`Z-Swing%`, accuracy = 0.1),
             `Swing%` = scales::percent(`Swing%`, accuracy = 0.1),
             xDamage = sprintf("%.3f", xDamage),
             xwOBA = sprintf("%.3f", xwOBA),
             AvgEV = sprintf("%.1f", AvgEV),
             AVG = sprintf("%.3f", AVG),
             OBP = sprintf("%.3f", OBP),
             SLG = sprintf("%.3f", SLG)) %>% 
      select(Pitch, Pitches, `CSW%`, `Zone%`, `Chase%`, `Whiff%`, 
             `Z-Whiff%`, `Z-Swing%`, `Swing%`, xDamage, xwOBA, AvgEV, AVG, SLG)
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  output$pitch_type_table <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%   
      group_by('Pitch' = TaggedPitchType,
               'Side' = BatterSide) %>%
      summarize('Pitches' = n(),
                PA = sum(PACheck, na.rm = TRUE), 
                AB = sum(ABCheck, na.rm = TRUE), 
                H = sum(HCheck, na.rm = TRUE), 
                `1B` = sum(SingleCheck, na.rm = TRUE), 
                `2B` = sum(DoubleCheck, na.rm = TRUE), 
                `3B` = sum(TripleCheck, na.rm = TRUE), 
                HR = sum(HRCheck, na.rm = TRUE), 
                TB = (`1B`) + (`2B` * 2) + (`3B` * 3) + (HR * 4),
                SO = sum(StrikeoutCheck, na.rm = TRUE), 
                BB = sum(WalkCheck, na.rm = TRUE), 
                HBP = sum(HBPCheck, na.rm = TRUE),
                xwOBA = round(mean(xwOBAvalues[PACheck == TRUE], na.rm = TRUE), 3),
                `CSW%` = mean(CSWCheck, na.rm = TRUE), 
                `Strike%` = mean(StrikeCheck, na.rm = TRUE), 
                `StrikeProb%` = mean(called_strike_prob, na.rm = TRUE),
                `Zone%` = mean(ZoneCheck, na.rm = TRUE), 
                `Swing%` = mean(SwingCheck, na.rm = TRUE), 
                `Chase%` = mean(SwingCheck[ZoneCheck == FALSE], na.rm = TRUE), 
                `Z-Whiff%` = mean(WhiffCheck[ZoneCheck == TRUE & SwingCheck == TRUE]),
                `Whiff%` = mean(WhiffCheck[SwingCheck == TRUE], na.rm = TRUE), 
                `Z-Swing%` = mean(SwingCheck[ZoneCheck == TRUE], na.rm = TRUE),
                BBE = sum(BBECheck, na.rm = TRUE),
                xDamage = mean(av_xwOBAcon[BBECheck == TRUE],  na.rm = TRUE),
                AvgEV = mean(ExitSpeed[BBECheck == TRUE], na.rm = TRUE), 
                AVG = H / AB,
                OBP = (H + BB + HBP) / PA,
                SLG = TB / AB) %>% 
      mutate(`CSW%` = scales::percent(`CSW%`, accuracy = 0.1),
             `Strike%` = scales::percent(`Strike%`, accuracy = 0.1),
             `StrikeProb%` = scales::percent(`StrikeProb%`, accuracy = 0.1),
             `Zone%` = scales::percent(`Zone%`, accuracy = 0.1),
             `Chase%` = scales::percent(`Chase%`, accuracy = 0.1),
             `Whiff%` = scales::percent(`Whiff%`, accuracy = 0.1),
             `Z-Whiff%` = scales::percent(`Z-Whiff%`, accuracy = 0.1),
             `Z-Swing%` = scales::percent(`Z-Swing%`, accuracy = 0.1),
             `Swing%` = scales::percent(`Swing%`, accuracy = 0.1),
             xDamage = sprintf("%.3f", xDamage),
             xwOBA = sprintf("%.3f", xwOBA),
             AvgEV = sprintf("%.1f", AvgEV),
             AVG = sprintf("%.3f", AVG),
             OBP = sprintf("%.3f", OBP),
             SLG = sprintf("%.3f", SLG)) %>% 
      select(Pitch, Side, Pitches, `CSW%`, `Zone%`, `Chase%`, `Whiff%`, 
             `Z-Whiff%`, `Z-Swing%`, `Swing%`, xDamage, xwOBA, AvgEV, AVG, SLG)
  
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  output$pitcher_splits_table <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
      group_by('Side' = BatterSide) %>%
      summarize('Pitches' = n(),
                PA = sum(PACheck, na.rm = TRUE), 
                AB = sum(ABCheck, na.rm = TRUE), 
                H = sum(HCheck, na.rm = TRUE), 
                `1B` = sum(SingleCheck, na.rm = TRUE), 
                `2B` = sum(DoubleCheck, na.rm = TRUE), 
                `3B` = sum(TripleCheck, na.rm = TRUE), 
                HR = sum(HRCheck, na.rm = TRUE), 
                TB = (`1B`) + (`2B` * 2) + (`3B` * 3) + (HR * 4),
                SO = sum(StrikeoutCheck, na.rm = TRUE), 
                BB = sum(WalkCheck, na.rm = TRUE), 
                HBP = sum(HBPCheck, na.rm = TRUE),
                xwOBA = round(mean(xwOBAvalues[PACheck == TRUE], na.rm = TRUE), 3),
                `CSW%` = mean(CSWCheck, na.rm = TRUE), 
                `Strike%` = mean(StrikeCheck, na.rm = TRUE), 
                `StrikeProb%` = mean(called_strike_prob, na.rm = TRUE),
                `Zone%` = mean(ZoneCheck, na.rm = TRUE), 
                `Swing%` = mean(SwingCheck, na.rm = TRUE), 
                `Chase%` = mean(SwingCheck[ZoneCheck == FALSE], na.rm = TRUE), 
                `Z-Whiff%` = mean(WhiffCheck[ZoneCheck == TRUE & SwingCheck == TRUE]),
                `Whiff%` = mean(WhiffCheck[SwingCheck == TRUE], na.rm = TRUE), 
                `Z-Swing%` = mean(SwingCheck[ZoneCheck == TRUE], na.rm = TRUE),
                BBE = sum(BBECheck, na.rm = TRUE),
                xDamage = mean(av_xwOBAcon[BBECheck == TRUE],  na.rm = TRUE),
                AvgEV = mean(ExitSpeed[BBECheck == TRUE], na.rm = TRUE), 
                AVG = H / AB,
                OBP = (H + BB + HBP) / PA,
                SLG = TB / AB) %>% 
      mutate(`CSW%` = scales::percent(`CSW%`, accuracy = 0.1),
             `Strike%` = scales::percent(`Strike%`, accuracy = 0.1),
             `StrikeProb%` = scales::percent(`StrikeProb%`, accuracy = 0.1),
             `Zone%` = scales::percent(`Zone%`, accuracy = 0.1),
             `Chase%` = scales::percent(`Chase%`, accuracy = 0.1),
             `Whiff%` = scales::percent(`Whiff%`, accuracy = 0.1),
             `Z-Whiff%` = scales::percent(`Z-Whiff%`, accuracy = 0.1),
             `Z-Swing%` = scales::percent(`Z-Swing%`, accuracy = 0.1),
             `Swing%` = scales::percent(`Swing%`, accuracy = 0.1),
             xDamage = sprintf("%.3f", xDamage),
             xwOBA = sprintf("%.3f", xwOBA),
             AvgEV = sprintf("%.1f", AvgEV),
             AVG = sprintf("%.3f", AVG),
             OBP = sprintf("%.3f", OBP),
             SLG = sprintf("%.3f", SLG)) %>% 
      select(Side, Pitches, `CSW%`, `Zone%`, `Chase%`, `Whiff%`, 
             `Z-Whiff%`, `Z-Swing%`, `Swing%`, xDamage, xwOBA, AvgEV, AVG, SLG)
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE))))
  })
  
  output$pitcher_count_results <- renderDataTable({
    table <- FinalCSV
    
    if(input$PitcherInput != "All") {
      table <- table %>% filter(Pitcher %in% input$PitcherInput)
    }
    
    table <- table %>%
      filter(between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
      group_by(Count) %>%
      summarize('Pitches' = n(),
                PA = sum(PACheck, na.rm = TRUE), 
                AB = sum(ABCheck, na.rm = TRUE), 
                H = sum(HCheck, na.rm = TRUE), 
                `1B` = sum(SingleCheck, na.rm = TRUE), 
                `2B` = sum(DoubleCheck, na.rm = TRUE), 
                `3B` = sum(TripleCheck, na.rm = TRUE), 
                HR = sum(HRCheck, na.rm = TRUE), 
                TB = (`1B`) + (`2B` * 2) + (`3B` * 3) + (HR * 4),
                SO = sum(StrikeoutCheck, na.rm = TRUE), 
                BB = sum(WalkCheck, na.rm = TRUE), 
                HBP = sum(HBPCheck, na.rm = TRUE),
                xwOBA = round(mean(xwOBAvalues[PACheck == TRUE], na.rm = TRUE), 3),
                `CSW%` = mean(CSWCheck, na.rm = TRUE), 
                `Strike%` = mean(StrikeCheck, na.rm = TRUE), 
                `StrikeProb%` = mean(called_strike_prob, na.rm = TRUE),
                `Zone%` = mean(ZoneCheck, na.rm = TRUE), 
                `Swing%` = mean(SwingCheck, na.rm = TRUE), 
                `Chase%` = mean(SwingCheck[ZoneCheck == FALSE], na.rm = TRUE), 
                `Z-Whiff%` = mean(WhiffCheck[ZoneCheck == TRUE & SwingCheck == TRUE]),
                `Whiff%` = mean(WhiffCheck[SwingCheck == TRUE], na.rm = TRUE), 
                `Z-Swing%` = mean(SwingCheck[ZoneCheck == TRUE], na.rm = TRUE),
                BBE = sum(BBECheck, na.rm = TRUE),
                xDamage = mean(av_xwOBAcon[BBECheck == TRUE],  na.rm = TRUE),
                AvgEV = mean(ExitSpeed[BBECheck == TRUE], na.rm = TRUE), 
                AVG = H / AB,
                OBP = (H + BB + HBP) / PA,
                SLG = TB / AB) %>% 
      mutate(`CSW%` = scales::percent(`CSW%`, accuracy = 0.1),
             `Strike%` = scales::percent(`Strike%`, accuracy = 0.1),
             `StrikeProb%` = scales::percent(`StrikeProb%`, accuracy = 0.1),
             `Zone%` = scales::percent(`Zone%`, accuracy = 0.1),
             `Chase%` = scales::percent(`Chase%`, accuracy = 0.1),
             `Whiff%` = scales::percent(`Whiff%`, accuracy = 0.1),
             `Z-Whiff%` = scales::percent(`Z-Whiff%`, accuracy = 0.1),
             `Z-Swing%` = scales::percent(`Z-Swing%`, accuracy = 0.1),
             `Swing%` = scales::percent(`Swing%`, accuracy = 0.1),
             xDamage = sprintf("%.3f", xDamage),
             xwOBA = sprintf("%.3f", xwOBA),
             AvgEV = sprintf("%.1f", AvgEV),
             AVG = sprintf("%.3f", AVG),
             OBP = sprintf("%.3f", OBP),
             SLG = sprintf("%.3f", SLG)) %>% 
      select(Count, Pitches, `CSW%`, `Zone%`, `Chase%`, `Whiff%`, 
             `Z-Whiff%`, `Z-Swing%`, `Swing%`, xDamage, xwOBA, AvgEV, AVG, SLG)
    
    table[is.na(table)] <- "-"
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', pageLength = nrow(tableFilter()), columnDefs = list(list(targets = 0, visible = FALSE))))
    
  })
  
  output$pitch_movement_plot <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput, 
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
    })
    ggplot(data = dataFilter(), aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) + 
      labs(x = "Horizontal Movement (HB)", y = "Vertical Movement (IVB)", color = "Pitch Type", title = "Pitch Movement") + 
      xlim(-30, 30) + ylim(-30, 30) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") + 
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 450, height = 400)
  
  output$umpire_analysis <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput, 
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]),
               PitchCall == "BallCalled" | PitchCall == "StrikeCalled" | PitchCall == "BallinDirt")
    })
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
      geom_point(size = 3) +
      theme_bw() +
      labs(x = "PlateLocSide", y = "PlateLocHeight",
           fill = "Called Strike Probability") +
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6, ymax = 3.4,
               fill= NA, color= "black", 
               alpha = .1) +
      ylim(1.2, 3.8) +
      xlim(-1.6, 1.6)
  }, width = 450, height = 450)
  
  output$pitch_release_plot <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput, 
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
    })
    ggplot(data = dataFilter(), aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point", color = " ", title = "Release") + 
      xlim(-4, 4) + ylim(2, 7) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") + 
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 450, height = 400)
  
  output$pitch_location_plot1 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
    }) 
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight,color = TaggedPitchType)) +
      labs(x = "Horizontal Location", y = "Vertical Location", color = " ", title = "Location (All)", subtitle = "From the Pitcher's Perspective") +  
      geom_point(size = 3)+
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6,ymax = 3.4,
               fill= NA,color= "black", 
               alpha = .1) +
      ylim(1, 4) + xlim(-1.8, 1.8) + theme_bw() + 
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 400, height = 350)
  
  
  output$pitch_location_plot2 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]),
               BatterSide == "Left")
    }) 
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight,color = TaggedPitchType)) +
      labs(x = "Horizontal Location", y = "Vertical Location", color = " ", title = "Location (LHB)", subtitle = "From the Pitcher's Perspective") + 
      geom_point(size = 3)+
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6,ymax = 3.4,
               fill= NA,color= "black", 
               alpha = .1) +
      ylim(1, 4) + xlim(-1.8, 1.8) + theme_bw() + 
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 400, height = 350)
  
  output$pitch_location_plot3 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]),
               BatterSide == "Right")
    }) 
    ggplot(data = dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight,color = TaggedPitchType)) +
      labs(x = "Horizontal Location", y = "Vertical Location", color = " ", title = "Location (RHB)", subtitle = "From the Pitcher's Perspective") + 
      geom_point(size = 3)+
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6,ymax = 3.4,
               fill= NA,color= "black", 
               alpha = .1) +
      ylim(1, 4) + xlim(-1.8, 1.8) + theme_bw() + 
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 400, height = 350)
  
  output$lineplot1 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>% 
        mutate(PitchNo = row_number())
    })
    ggplot(dataFilter(), aes(x = PitchNo, y = Stuff, color = TaggedPitchType)) + 
      geom_line(size = 2) +
      labs(x = "Pitch Count", y = "Stuff+", title = "Stuff+ Over Time", color = "") +
      ylim(0, 250) + 
      theme_bw() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
      theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12))
  }, width = 900, height = 400)
  
  output$lineplot2 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2])) %>%
        mutate(PitchNo = row_number())
    })
    ggplot(data = dataFilter()) + 
      geom_line(aes(y = RelSpeed, x = PitchNo, color = TaggedPitchType), size = 2) + 
      labs(x = "Pitch Count", y = "Pitch Velocity (MPH)", color = " ", title = "Pitch Velocity") + 
      ylim(65, 95) + 
      theme_bw() + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 12)) +
      theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.title = element_text(size = 14))
  }, width = 900, height = 400)
  
  output$plot1 <- renderPlot({
    dataFilter <- reactive({
      FinalCSV %>%
        filter(Pitcher == input$PitcherInput,
               between(Date, input$DateRangeInput[1], input$DateRangeInput[2]))
    })
    ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = F) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6,ymax = 3.4,
               fill= NA,color= "black", 
               alpha = .1) +
      ylim(1, 4) + xlim(-1.8, 1.8) + theme_bw() + 
      theme_classic() +
      xlab("Horizontal Pitch Location") +
      ylab("Vertical Pitch Location") +
      ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
      facet_wrap(~TaggedPitchType, ncol = 3) +
      guides(fill = FALSE)
  }, width = 700, height = 400)
  
  
}

shinyApp(ui = ui, server = server)
