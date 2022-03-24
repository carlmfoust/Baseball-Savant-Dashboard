library(tidyverse)
library(ggplot2)
library(ggiraph)
library(GeomMLBStadiums)
library(RSQLite)
library(DBI)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(anytime)
library(reactable)
library(Lahman)

db = dbConnect(SQLite(), "statcastDaily2021.sqlite")

#   View tables in 
#   as.data.frame(dbListTables(db))
   statcast_hitting = dbReadTable(db, 'statcast_hitting') %>% 
                      arrange(game_pk,at_bat_number) %>% 
                      mutate(game_date = anydate(game_date)) %>% 
                      arrange(game_date)
#   statcast_hitting = statcast_hitting %>% filter(statcast_hitting$player_name == 'Pujols, Albert') %>% 
#       arrange(game_pk,at_bat_number)
   statcast_pitching = dbReadTable(db, 'statcast_pitching') %>% 
                       arrange(game_pk,at_bat_number) %>% 
                       mutate(game_date = anydate(game_date)) %>% 
                       arrange(desc(game_date))
   
chad = dbReadTable(db,'chadwick')

sc = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_pitching") %>% 
     mutate(pit_team = ifelse(inning_topbot == 'Top',home_team,away_team), 
            game_date = anydate(game_date)) %>% 
     arrange(pit_team, player_name)

rad2deg <- function(rad) {(rad * 180) / (pi)}

sc$bbref = chad$key_bbref[match(sc$pitcher,chad$key_mlbam)]
sc$height = (Lahman::Master$height[match(sc$bbref,Lahman::Master$bbrefID)])/12

armSlot = sc %>% 
    mutate(adj = (release_pos_z - height*0.7),
           opp =  abs(release_pos_x), # distance from middle of chest to arm
           hyp = sqrt(opp^2 + adj^2), 
           arm_angle = rad2deg(acos((adj^2 + hyp^2 - opp^2)/ (2*(adj*hyp))))) %>% 
    group_by(player_name, pitch_name) %>% 
    summarise(arm_angle = mean(arm_angle, na.rm = T),
              velocity = mean(release_speed, na.rm = T),
              spin_rate = mean(release_spin_rate, na.rm = T),
              pitch_num = n()) %>% 
    mutate(arm_slot = case_when(arm_angle >= 90 ~ "Submarine",
                                arm_angle >= 70 & arm_angle < 90 ~ "Side Arm", 
                                arm_angle < 70 & arm_angle >= 30 ~ "Three-Quarters", 
                                arm_angle < 30 ~ "Overhand")) %>% 
    drop_na(arm_angle) %>% 
    mutate_if(is.numeric, round, digits = 2)

sc$tooltip <- c(paste0("Pitch Type:  ", sc$pitch_name, 
                       " \n Velocity: ", sc$release_speed, 
                       " MPH \n Spin Rate: ", sc$release_spin_rate, 
                       " RPM \n Date: ", sc$game_date))

sch = dbGetQuery(conn = db, statement = "SELECT * FROM statcast_hitting") %>% 
    mutate(pos_team = ifelse(inning_topbot == 'Top',away_team,home_team), 
           game_date = anydate(game_date),
           events = ifelse(events == 'grounded_into_double_play','gidp', events)) %>% 
    arrange(pos_team, player_name)

sch$tooltip <- c(paste0("Pitch Type:  ", sch$pitch_name, 
                        "\n Distance: ", sch$hit_distance_sc, 
                        "\n Launch Speed: ", sch$launch_speed, 
                        "\n Launch Angle: ", sch$launch_angle, 
                        "\n Event: ", sch$events, 
                        "\n Date: ", sch$game_date))

# Strike Zone for plots
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data_frame(x,z) 

ui <- dashboardPage(skin = 'red',
    dashboardHeader(title = "2021 Statcast Data",
                    titleWidth = 200),
    dashboardSidebar(width = 200, collapsed = TRUE,
        sidebarMenu(
            menuItem("Dashboard", icon = icon("dashboard")),
            menuSubItem("Home"),
            menuItem("Pitching", tabName = 'pitcher', icon = icon('baseball-ball', lib = "font-awesome")),
            menuItem("Hitting", tabName = 'hitter', icon = icon('quidditch', lib = "font-awesome"))
        )
    ),
    dashboardBody(
        tags$script('
          // Bind function to the toggle sidebar button
          $(".sidebar-toggle").on("click",function(){
            $(window).trigger("resize"); // Trigger resize event
          })'
        ),
        tabItems(
            tabItem(tabName = 'pitcher',
                    fluidRow(
                        box(title = 'User Input', width = 4,
                            pickerInput('team', 'Select Team', choices = unique(sc$pit_team)),
                            pickerInput('name', 'Select Pitcher', choices = unique(sc$player_name)),
                            pickerInput(
                                inputId = "type",
                                label = "Pitch Type", 
                                choices = unique(sc$pitch_name),
                                options = list(
                                    `actions-box` = TRUE), 
                                multiple = TRUE
                            )
                        ),
                        box(title = 'Plot', width = 4, height = 450, girafeOutput('zone',width = '100%',height = 450)),
                        box(title = 'Plot', width = 4, height = 450, girafeOutput('release',width = '100%',height = 450)),
                        #box(title = "Table",width = 12,
                        #    div(style = 'overflow-y:scroll;height:500px;',
                        #        DT::dataTableOutput("table"))),
                        #box(title = "Table",width = 12,
                        #    div(style = 'overflow-y:scroll;height:500px;',
                        #        DT::dataTableOutput("table2"))),
                        tabBox(
                            side = "left", width = 12,
                            selected = "Pitch Types",
                            tabPanel("Pitch Types", reactableOutput('table1')),
                            tabPanel("Starts", reactableOutput('table2')),
                            tabPanel("Raw Data", reactableOutput('table3')),
                            tabPanel("Graph", girafeOutput('table4')),
                            tabPanel('Slot', reactableOutput('table5'))
                        )
                    )),
            tabItem(tabName = 'hitter',
                    fluidRow(
                        box(title = 'User Input', width = 4,
                            pickerInput('teamh', 'Select Team', choices = unique(sch$pos_team)),
                            pickerInput('hitter', 'Select Hitter', choices = unique(sch$player_name)),
                            pickerInput(
                                inputId = "hitType",
                                label = "Hit Type", 
                                choices = unique(sch$event),
                                options = list(
                                    `actions-box` = TRUE), 
                                multiple = TRUE
                            )
                        ),
                        box(title = 'Spray Chart', width = 8, girafeOutput('spray')),
                        tabBox(
                            side = "left", width = 12,
                            selected = "Raw Data",
                            tabPanel("Raw Data", reactableOutput('table'))
                        )
                    )
                    )
        )
        

    )
)

server <- function(input, output, session) {
    
    sc_reactive = reactive({
        sc %>% filter(player_name == input$name) %>% 
            arrange(desc(release_speed))
    })
    
    slot_reactive = reactive({
        armSlot %>% filter(player_name == input$name) %>% 
            arrange(desc(velocity))
    })
    
    output$table1 <- renderReactable({
        pitchAvg = sc_reactive() %>% group_by(pitch_name) %>% summarise(AverageVelo = mean(release_speed),
                                                             AverageSpin = mean(release_spin_rate, na.rm = T),
                                                             AvgHorMvt = mean(pfx_x, na.rm = T)*12,
                                                             AvgVertMvt = mean(pfx_z, na.rm = T)*12,
                                                             PitchNum = n()) %>% 
            mutate_if(is.numeric, round, digits = 2) %>% 
            rename(Type = pitch_name) %>% 
            arrange(desc(PitchNum))
        reactable(pitchAvg, striped = T)
    })
    
    output$table2 = renderReactable({
        reactable(sc_reactive(), striped = T, groupBy = 'game_date', columns = list(
            release_speed = colDef('Velocity', aggregate = 'mean')
        ))
    })
    
    output$table3 = renderReactable({
        reactable(sc_reactive(), striped = T, searchable = T, filterable = T)
    })
    
    output$table4 = renderGirafe({
        Spinrate = sc_reactive() %>% group_by(pitch_name, game_date) %>% 
                                     summarise(spin = mean(release_spin_rate,  na. = TRUE), num = n())
        Spinrate$tooltip = c(paste0("Pitch Type:  ", Spinrate$pitch_name, 
                                    " MPH \n Spin Rate: ", round(Spinrate$spin,0), 
                                    " RPM \n Date: ", Spinrate$game_date, 
                                    "\n Count: ", Spinrate$num))
        
        h = ggplot()+
            geom_point_interactive(data = Spinrate, aes(x = game_date,
                                                        y = spin,
                                                        color = pitch_name,
                                                        tooltip = tooltip)) +
            scale_colour_manual(values = c("4-Seam Fastball" = "#f0027f", "Slider" = "#beaed4", "Sinker" = "#fdc08c", 
                                           "Changeup" = "#ffff99", "Curveball" = "#386cb0", "Cutter" = "#FF66B6", 
                                           "Knuckle Curve" = "#bf5b17", "Split-Finger" = "#BF0064", "Knuckleball" = "#F167FF",
                                           "Eephus" = "#00C2FF", "Fastball" = "#BE3E81", "2-Seam Fastball" = "#7FC97F",
                                           "NA" = "#666666", "Screwball" = "#00E9DA")) +
            geom_vline(xintercept = as.numeric(as.Date("2021-06-21")), colour="grey", linetype="dashed") +#Policy Enforcement Started
            geom_vline(xintercept = as.numeric(as.Date("2021-06-15")), colour="grey", linetype="dashed") +#Press Release Anounced
            geom_vline(xintercept = as.numeric(as.Date("2021-06-02")), colour="grey", linetype="dashed")  #4 Minor Leaguers Suspended
        girafe(ggobj = h)
        })
    
    output$table5 = renderReactable({
        reactable(slot_reactive(), striped = T)
    })
    
    teams = reactive({
        team = subset(sc, pit_team %in% input$team)
        team = select(team, 'player_name')
    })

    observe({
        updatePickerInput(session, 'name',
                             choices = unique(teams()),
                             selected = NULL)
    })
    
    observe({
        pitches = unique(sc_reactive()$pitch_name)
        updatePickerInput(session, 'type',
                          choices = pitches,
                          selected = pitches)
    })
    
    #pitchType = reactive({
    #    sc %>% filter(player_name == input$name, pitch_type %in% input$type)
    #})
    
    output$zone = renderGirafe({
        pitchType = sc_reactive() %>% filter(pitch_name %in% input$type)
        
        cdata = session$clientData
        
        g = ggplot()+
            geom_path(data = sz, aes(x=x, y=z))+
            coord_equal()+
            labs(title = str_wrap(paste(input$name,"Pitch Chart on Balls Greater Than or Equal to",
                                        input$type,"MPH"), 40)) +
            theme(text = element_text(family = "Source Sans Pro")) +
            xlab("Feet From Home Plate")+
            ylab("Feet Above the Ground")+
            geom_point_interactive(data = pitchType,aes(x=plate_x,
                                                        y=plate_z,
                                                        size=release_speed,
                                                        color=pitch_name,
                                                        tooltip = pitchType$tooltip))+
            scale_size(range = c(0.1,3)) +
            coord_cartesian(xlim = c(-4,4), ylim = c(-1,6.5)) +
            scale_colour_manual(values = c("4-Seam Fastball" = "#f0027f", "Slider" = "#beaed4", "Sinker" = "#fdc08c", 
                                           "Changeup" = "#ffff99", "Curveball" = "#386cb0", "Cutter" = "#FF66B6", 
                                           "Knuckle Curve" = "#bf5b17", "Split-Finger" = "#BF0064", "Knuckleball" = "#F167FF",
                                           "Eephus" = "#00C2FF", "Fastball" = "#BE3E81", "2-Seam Fastball" = "#7FC97F",
                                           "NA" = "#666666", "Screwball" = "#00E9DA"))
        
        girafe(ggobj = g, options = list(opts_sizing(rescale = T, width = .5)))
    })
    
    output$release = renderGirafe({
        pitchType = sc_reactive() %>% filter(pitch_name %in% input$type)
        
        m = ggplot()+
            coord_equal()+
            labs(title = str_wrap(paste(input$name,"Pitch Release Point on Balls Greater Than or Equal to",
                                        input$ev,"MPH"), 40)) +
            theme(text = element_text(family = "Source Sans Pro")) +
            xlab("Feet From Home Plate")+
            ylab("Feet Above the Ground")+
            geom_point_interactive(data = pitchType %>% filter(pitch_name %in% input$type),
                                   aes(x=release_pos_x,
                                       y=release_pos_z,
                                       color=pitch_name,
                                       tooltip = pitchType$tooltip))+
            scale_size(range = c(0.1,3)) +
            coord_cartesian(xlim = c(-5,5), ylim = c(0,7.5)) +
            scale_colour_manual(values = c("4-Seam Fastball" = "#f0027f", "Slider" = "#beaed4", "Sinker" = "#fdc08c", 
                                           "Changeup" = "#ffff99", "Curveball" = "#386cb0", "Cutter" = "#FF66B6", 
                                           "Knuckle Curve" = "#bf5b17", "Split-Finger" = "#BF0064", "Knuckleball" = "#F167FF",
                                           "Eephus" = "#00C2FF", "Fastball" = "#BE3E81", "2-Seam Fastball" = "#7FC97F",
                                           "NA" = "#666666", "Screwball" = "#00E9DA"))
        

        girafe(ggobj = m, options = list(opts_sizing(rescale = T, width = .5)))
    })
    
    
    sch_reactive = reactive({
        sch %>% filter(player_name == input$hitter) %>% mlbam_xy_transformation() %>% 
            arrange(desc(release_speed))
    })
    
    output$table <- renderDataTable({
        sch %>% select(-c(tooltip)) %>% filter(player_name == input$hitter)
    })
    
    output$table = renderReactable({
        reactable(sch_reactive())
    })
    
    teamsh = reactive({
        teamh = subset(sch, pos_team %in% input$teamh)
        teamh = select(teamh, 'player_name')
    })
    
    observe({
        updatePickerInput(session, 'hitter',
                          choices = unique(teamsh()),
                          selected = NULL)
    })
    
    output$spray <- renderGirafe({
        g <- ggplot(sch_reactive(),aes(x=hc_x_, y=hc_y_, color=events)) + 
            geom_curve(x = -93, xend = 93, y = 85, yend = 80,
                       curvature = -.80, linetype = "dotted", color = "black") + 
            geom_curve(x = -260, xend = 260, y = 255, yend = 255,
                       curvature = -.60,color = "black") +
            geom_segment(x = 0, xend = 260, y = -10, yend = 255, size = 0.7, color = "black", lineend = "round") +
            geom_segment(x = 0, xend = -260, y = -10, yend = 255, size = 0.7, color = "black", lineend = "round") +
            geom_point_interactive(data = sch_reactive(),
                                   aes(x=hc_x_,
                                       y=hc_y_,
                                       color=events,
                                       tooltip = sch_reactive()$tooltip)) +
            theme(text = element_text(family = "Source Sans Pro")) +
            coord_cartesian(xlim = c(-300,300), ylim = c(-50,500)) +
            ggtitle(paste(input$hitter,"Spray Chart on Balls Greater Than or Equal to",
                          input$ev,"MPH"))
        girafe(ggobj = g, width_svg = 7)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
