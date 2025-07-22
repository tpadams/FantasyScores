library(shiny)
library(jsonlite)
library(curl)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(bs4Dash)
library(plotly)
library(future.apply)
library(future)
library(shinycssloaders)

#setwd("~/GitHub/FantasyScores/")

# Set up parallel processing
plan(multisession, workers = 4) # Adjust workers based on your system

#load current scores from RDS file
server<-shinyServer(function(input, output,session) {
scorelist <- list()
ffdata <- jsonlite::fromJSON('http://fantasy.premierleague.com/api/bootstrap-static/')

players_noteam <- ffdata$elements[,c("id","web_name","element_type","team_code","event_points","total_points","news","points_per_game")]
teams <- ffdata$teams[,c("name","code")]
colnames(teams) <- c("name","team_code")
players <- (merge(players_noteam, teams, by = 'team_code'))
players <- players[ , !(names(players) == 'team_code')]
players$element_type <- recode(players$element_type, "1" = "GK", "2" = "Def", "3" = "Mid", "4" = "Atk")
ppg<- players[,c("web_name","element_type","name","points_per_game")]
players <- players[,c(1,2,3,8,4,5,6)] #reorder columns
picks <- read.csv("www/picks.csv") 
playerscopy <- players
players <- merge(players,picks,by='id')

notpicked <- subset(playerscopy, !(id %in% players$id))

names(notpicked) <- c("id","Name","Position","Team","GW points","Total points","News")
names(playerscopy) <- c("id","Name","Position","Team","GW points","Total points","News")
colnames(players) <- c("id","Name","Position","Team","GW points","Total points","News","Picked by")
names(ppg) <- c("Name","Position","Team","PPG")
ppg$PPG <- as.numeric(ppg$PPG)

currentGW <- match(TRUE,ffdata$events$is_current)

# Simple progress with parallel processing
withProgress(message = 'Loading player data...', value = 0, {
  
  # Create parallel function (simplified - no per-worker progress)
  fetch_player_data <- function(i) {
    player_id <- players[i,]$id
    
    # API call with error handling
    individual <- tryCatch({
      jsonlite::fromJSON(paste0("http://fantasy.premierleague.com/api/element-summary/", player_id, "/"))
    }, error = function(e) {
      return(list(history = data.frame(), fixtures = data.frame()))
    })
    
    # Process round scores
    tryCatch({
      roundscores <- transpose(individual$history[,c("round","total_points")])
    }, error = function(e) {
      roundscores <<- as.data.frame(matrix(c(1,2,3,0,0,0), nrow=2, ncol=3, byrow=TRUE))
    })
    
    # Process minutes played
    tryCatch({
      minsplayed <- individual$history[individual$history$round==max(individual$history$round),][,c("minutes")]
    }, error = function(e) {
      minsplayed <<- 0
    }, warning = function(w) {
      minsplayed <<- 0
    })
    
    # Format data
    names(roundscores) <- c(roundscores[1,])
    temp <- names(roundscores)
    roundscores <- data.frame(roundscores[2,])
    names(roundscores) <- temp
    player_with_scores <- cbind(players[i,], roundscores)
    player_with_scores$`GW minutes` <- sum(minsplayed)
    player_with_scores <- player_with_scores[,c(1:5,ncol(player_with_scores),6:(ncol(player_with_scores)-1))]
    
    # Check if started
    started <- ifelse(currentGW==min(individual$fixtures$event,na.rm=T),0,1)
    if(is.null(started)){started <- FALSE}
    player_with_scores <- cbind(player_with_scores, started)
    
    # Handle double gameweeks
    if("24.1" %in% names(player_with_scores)){
      player_with_scores$`24` <- player_with_scores$`24`+player_with_scores$`24.1`
    }
    if("25.1" %in% names(player_with_scores)){
      player_with_scores$`25` <- player_with_scores$`25`+player_with_scores$`25.1`
    }
    
    return(player_with_scores)
  }
  
  # Execute in parallel
  scorelist <- future_lapply(1:nrow(players), fetch_player_data, future.seed = TRUE)
  
  # Update progress at the end
  incProgress(1, detail = paste("Loaded", nrow(players), "players"))
})

allplayers <- dplyr::bind_rows(scorelist)
gws <- as.character(seq(1,max(as.numeric(names(allplayers)),na.rm=T)))
#rem <- as.character(seq(30,38))
#gws <- gws[! gws %in% c(7)]
allplayers <- allplayers[,c("id","Name","Position","Team","GW points","GW minutes","Total points","News","Picked by","started",gws)]

##########################IN########################################OUT################################
#allplayers[allplayers$id=='617',11:13] <- allplayers[allplayers$id=='387',11:13] #Evanilson 617 in for Shaw 387 pre GW4 Hodge


out<-c('999')


notpicked <- subset(playerscopy, !(id %in% players$id & !id %in% out))

allplayers<-allplayers[!allplayers$id %in%out,]

players<-players[!players$id %in%out,]

tOut<-out

allplayers<-allplayers[!allplayers$id %in% tOut,] #now remove transferred out player from DF. 
players<-players[!players$id %in% tOut,] 
 

allplayers[11:ncol(allplayers)] <- sapply(allplayers[11:ncol(allplayers)],as.numeric) #convert columns to numeric

#e<- names(allplayers)
#e[10:47] <- c(1:38)
#allplayers <- allplayers[,e] #put in right order after bind_rows puts NA values at end
finaldf <- split(allplayers,allplayers$`Picked by`)


gameweekpoints <- data.frame(Player=character(),Week=character(),Points=integer())
assign("Tom",finaldf$Tom)
assign("Warnes",finaldf$Warnes)
assign("Hodge",finaldf$Hodge)
assign("Luke",finaldf$Luke)
for(q in c("Tom","Warnes","Hodge","Luke")){
assign(paste0(q), get(q)[,c(1:9,11:ncol(get(q)))])
gwscores<-sapply(get(q)[10:ncol(get(q))],function(x) sum(head(sort(x, decreasing=TRUE), 11))) #11 is scored players.
assign(paste0("gwpoints",q),data.frame(q,names(gwscores),gwscores))
assign(paste0("gwpoints",q),get(paste0("gwpoints",q)) %>%
  group_by(q) %>%
  mutate(cumsum = cumsum(gwscores)))
}

gameweekpoints<- dplyr::bind_rows(gwpointsTom,gwpointsWarnes,gwpointsHodge,gwpointsLuke)
names(gameweekpoints) <- c("Player","Week","Points","Cumulative") #create frame of gameweeks and how many points scored
gws
gameweekpoints$Week <- rep(as.numeric(gws),4)
gameweekpoints <- gameweekpoints[!is.na(gameweekpoints$Week),]
gameweekpoints <- arrange(gameweekpoints,Player,Week)


#Define server logic required to summarize and view the selected data
options(DT.options = list(paging=FALSE))
  #############PLAYER SCORES##############
  datasetInput <- reactive({a<-switch(input$p,
           "Tom" = Tom,
           "Warnes" = Warnes,
           "Hodge" = Hodge,
           "Luke" = Luke
           )
  a[,!names(a)%in%c("started")]
  }) #get player
  
  DF <- reactive({ #this section adds the GW points only including top X as specified
  gwpoints<- data.frame(transpose(subset(gameweekpoints,gameweekpoints$Player == input$p)[,2:3])[2,])
  names(gwpoints) <- c(transpose(subset(gameweekpoints,gameweekpoints$Player == input$p)[,2:3])[1,])
  gwpoints<- cbind(a="",b="",c="",d="",e="",f="",f1="",g="TOTAL",h="TOTAL",gwpoints)
  names(gwpoints) <- names(datasetInput())
  q<- rbind(datasetInput(),gwpoints)
  q$`GW points` <- as.numeric(q$`GW points`)
  q$`Total points` <- as.numeric(q$`Total points`)
  q
  })
  
  output$playerscores <- DT::renderDataTable({
    datatable(DF()[-c(1,9)], 
              rownames=FALSE,
              class = 'table-striped table-hover',
              options=list(
                order=list(3,'desc'),
                scrollX=TRUE,
                pageLength = 15,
                dom = 'Bfrtip',
                responsive = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>% 
    formatStyle(colnames(DF())[c(5,10:ncol(DF()))], 
                color = styleInterval(c(0.56), c('#dc3545','#28a745')),
                fontWeight = 'bold') %>%
    formatStyle(0, target = 'row', lineHeight='80%')
  })
  
  
  ##################LEAGUE TABLE##############
  
  overallTable <- aggregate(. ~ Player, data=gameweekpoints[,c(1,3)], sum)
  thisWeek <- subset(gameweekpoints,gameweekpoints$Week==max(gameweekpoints$Week))
  
  startedTeams <- allplayers %>% #count number of players whose game this GW has started
    group_by(`Picked by`) %>%
    summarise(n=sum(started,na.rm = T))

  playedMins <- allplayers %>% #count number of players whose game this GW has started
    group_by(`Picked by`) %>%
    summarise(n=sum(`GW minutes`>0))

  leagueTable<-merge(overallTable,thisWeek,by="Player")
  leagueTable <- merge(leagueTable,playedMins,by.y="Picked by",by.x="Player")
  leagueTable <- merge(leagueTable,startedTeams,by.y="Picked by",by.x="Player")
  leagueTable <- leagueTable[,c(1,5,4,7,6)]
  names(leagueTable) <- c("Player","Total points","This GW","Teams started","Players played any mins")

  output$league <- DT::renderDataTable({
    datatable(leagueTable[with(leagueTable,order(-`Total points`)),],
              rownames=FALSE,
              class = 'table-striped table-hover',
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                dom = 'Bfrtip',
                responsive = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>%
    formatStyle('Total points', backgroundColor = '#37003c', color = 'white', fontWeight = 'bold') %>%
    formatStyle('This GW', backgroundColor = '#00ff87', color = 'black', fontWeight = 'bold')
  })
  
  #need to make this work and add to UI
  output$topScores <- DT::renderDataTable({
    datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(-`Points`)),],10),
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(dom='t', autoWidth = TRUE, pageLength = 10)) %>%
    formatStyle('Points', backgroundColor = '#28a745', color = 'white', fontWeight = 'bold')
  })
  
  output$bottomScores <- DT::renderDataTable({
    datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(`Points`)),],10),
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(dom='t', autoWidth = TRUE, pageLength = 10)) %>%
    formatStyle('Points', backgroundColor = '#dc3545', color = 'white', fontWeight = 'bold')
  })
  
  #gameweekpoints[gameweekpoints$Week>29,]$Week <- gameweekpoints[gameweekpoints$Week>29,]$Week -9
  output$graph <-renderPlotly(ggplot(data=gameweekpoints,aes(x=Week,y=Cumulative)) +geom_line(aes(colour=Player),size=2) +geom_point(aes(text=paste("Points:",Points))) +scale_x_continuous(breaks= c(1:38)))
  
  ########Other Stats#########
  output$notpicked <- DT::renderDataTable({
    datatable(head(notpicked[c(2,3,4,6)][with(notpicked,order(-`Total points`)),],10),
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(dom='t', autoWidth = TRUE, pageLength = 10)) %>%
    formatStyle('Total points', backgroundColor = '#ffc107', color = 'black', fontWeight = 'bold')
  })
  
  output$topoverall <- DT::renderDataTable({
    datatable(head(playerscopy[c(2,3,4,6)][with(playerscopy,order(-`Total points`)),],10),
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(dom='t', autoWidth = TRUE, pageLength = 10)) %>%
    formatStyle('Total points', backgroundColor = '#17a2b8', color = 'white', fontWeight = 'bold')
  })
  
  output$ppg <- DT::renderDataTable({
    datatable(head(ppg[with(ppg,order(-`PPG`)),],10),
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(dom='t', autoWidth = TRUE, pageLength = 10)) %>%
    formatStyle('PPG', backgroundColor = '#28a745', color = 'white', fontWeight = 'bold')
  })
  
  notpicked$Team <- as.factor(notpicked$Team)
  notpicked$Position <- as.factor(notpicked$Position)
  finderData <- merge(ffdata$elements,notpicked, by='id') 
  output$finder <- DT::renderDataTable({
    datatable(finderData[,c('Name','Position','Team','Total points',input$Columns)],
              rownames=FALSE,
              class = 'table-striped table-hover',
              filter='top',
              options=list(
                dom='Bfrtip',
                order=list(3,'desc'),
                scrollX=TRUE,
                pageLength = 15,
                responsive = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>%
    formatStyle('Total points', backgroundColor = '#17a2b8', color = 'white', fontWeight = 'bold')
  })
  
  #####DOWNLOAD PLAYER DATA#####
  
  output$downloadData <- downloadHandler(
    filename = paste("All-Player-Data-", format(Sys.time(), "%d%b%Y") ,".csv", sep = ""),
    content = function(file) {
      write.csv(ffdata$elements, file, row.names = FALSE)
    }
  )
  
  #####TRANSFERS######
  
  transfersDF <- data.frame(Player=c('Placeholder'),
                            In=c('Example'),
                            Out=c('Example'),
                            BeforeGameweek=c('1'))

  output$transfers <- DT::renderDataTable({
    datatable(transfersDF,
              rownames=FALSE,
              class = 'table-striped table-hover',
              options=list(
                pageLength = 15,
                dom = 'Bfrtip',
                responsive = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>%
    formatStyle('Player', backgroundColor = '#37003c', color = 'white', fontWeight = 'bold')
  })
  
  orderDF <- data.frame(Pick=c('1','2','3','4'),
                            Player=c('Tom','Luke','Warnes','Hodge'))
  
  output$order <- DT::renderDataTable({
    datatable(orderDF,
              rownames=FALSE,
              class = 'table-striped table-hover compact',
              options=list(
                dom='t',
                pageLength = 4,
                columnDefs = list(list(className = 'dt-center', targets = '_all'))
              )) %>%
    formatStyle('Pick', backgroundColor = '#28a745', color = 'white', fontWeight = 'bold')
  })
  
  ###Keep Alive###
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
  #session$onSessionEnded(stopApp)
}) #end of shiny section


# Premier League Custom CSS Theme (using custom CSS instead of fresh)
premier_league_css <- "
  :root {
    --bs-primary: #37003c;
    --bs-success: #00ff87;
    --bs-info: #17a2b8;
    --bs-warning: #ffc107;
    --bs-danger: #dc3545;
  }
  
  .navbar-light {
    background-color: #37003c !important;
  }
  
  .navbar-light .navbar-brand {
    color: white !important;
  }
  
  .navbar-light .navbar-toggler {
    border-color: white !important;
  }
  
  .navbar-light .navbar-toggler-icon {
    background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%28255, 255, 255, 1%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e\") !important;
  }
  
  .main-header .navbar-nav .nav-link {
    color: white !important;
  }
  
  .main-header .sidebar-toggle {
    color: white !important;
  }
  
  /* Hide any remaining toggles in top right corner */
  .main-header .navbar-nav.ml-auto,
  .main-header .navbar-custom-menu,
  .navbar-nav.order-1.order-md-3.navbar-no-expand,
  .main-header .navbar-nav:last-child,
  [data-widget='fullscreen'],
  [data-widget='control-sidebar-slide'],
  .nav-link[data-toggle='control-sidebar'] {
    display: none !important;
  }
  
  .sidebar-dark-primary .nav-sidebar > .nav-item > .nav-link.active {
    background-color: #37003c !important;
    color: white !important;
  }
  
  .btn-primary {
    background-color: #37003c !important;
    border-color: #37003c !important;
  }
  
  .btn-primary:hover {
    background-color: #2d0030 !important;
    border-color: #2d0030 !important;
  }
  
  .card-primary .card-header {
    background-color: #37003c !important;
    color: white !important;
  }
  
  .card-success .card-header {
    background-color: #00ff87 !important;
    color: black !important;
  }
  
  .card-info .card-header {
    background-color: #17a2b8 !important;
    color: white !important;
  }
"

#######UI######

ui <- dashboardPage(
  title = "⚽ Fantscores.uk",
  dark = NULL,  # Disable dark mode toggle
  fullscreen = FALSE,  # Disable fullscreen toggle
  controlbar = NULL,  # Disable controlbar toggle
  
  # Modern Header
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Fantscores.uk",
      color = "primary",
      href = "#",
      image = NULL
    )
  ),
  
  # Modern Sidebar
  sidebar = dashboardSidebar(
    skin = "dark",
    status = "primary",
    title = "Navigation",
    brandColor = "primary",
    minified = FALSE,  # Keep sidebar expanded by default
    sidebarMenu(
      id = "FantMenu",
      menuItem(
        text = "League Table",
        tabName = "LeagueTable",
        icon = icon("trophy", class = "fas")
      ),
      menuItem(
        text = "Player Scores", 
        tabName = "PlayerScores",
        icon = icon("users", class = "fas")
      ),
      menuItem(
        text = "Transfers",
        tabName = "Transfers", 
        icon = icon("exchange-alt", class = "fas")
      ),
      menuItem(
        text = "Statistics",
        tabName = "Stats",
        icon = icon("chart-bar", class = "fas")
      ),
      menuItem(
        text = "Player Finder",
        tabName = "PlayerFinder",
        icon = icon("search", class = "fas")
      )
    )
  ),
  
  # Modern Body with enhanced styling
  body = dashboardBody(
            # Premier League Theme CSS + Mobile Responsiveness
        tags$head(
          tags$style(HTML(paste0(premier_league_css, "
            .content-wrapper, .right-side {
              background-color: #f4f4f9;
            }
            .card {
              box-shadow: 0 2px 4px rgba(0,0,0,0.1);
              border-radius: 8px;
            }
            .card-header {
              font-weight: 600;
              font-size: 1.1em;
            }
            .dataTables_wrapper {
              font-size: 0.9em;
            }
            .nav-pills .nav-link.active {
              background-color: #37003c !important;
            }
            .value-box .value {
              font-size: 2.5rem;
            }
            .plotly {
              border-radius: 8px;
            }
            
            /* Mobile Responsive Improvements */
            @media (max-width: 768px) {
              .content-wrapper {
                padding: 10px !important;
              }
              
              .card {
                margin-bottom: 15px;
              }
              
              .card-header {
                font-size: 1em;
                padding: 10px;
              }
              
              h2 {
                font-size: 1.5em !important;
                margin-bottom: 15px !important;
              }
              
              .dataTables_wrapper {
                font-size: 0.75em;
              }
              
              .dataTables_wrapper .dataTables_length,
              .dataTables_wrapper .dataTables_filter {
                margin-bottom: 10px;
              }
              
              .btn-lg {
                font-size: 1em !important;
                padding: 8px 16px !important;
              }
              
              /* Sidebar stays expanded on mobile when needed */
              .main-sidebar {
                transition: width 0.3s ease;
              }
              
              /* Stack cards vertically on mobile */
              .col-sm-4, .col-md-4, .col-lg-4 {
                margin-bottom: 20px;
              }
              
              /* Better checkbox group spacing */
              .form-check {
                margin-bottom: 8px;
              }
              
              /* Adjust plotly charts */
              .plotly {
                height: 300px !important;
              }
            }
            
            @media (max-width: 480px) {
              .card-header {
                font-size: 0.9em;
              }
              
              .dataTables_wrapper {
                font-size: 0.7em;
              }
              
              h2 {
                font-size: 1.3em !important;
              }
              
              /* Even smaller plotly on phones */
              .plotly {
                height: 250px !important;
              }
            }
          ")))
        ),
    
    tabItems(
      # Modern League Table Tab
      tabItem(
        tabName = "LeagueTable",
        fluidRow(
          column(
            width = 12,
            h2("🏆 League Standings", style = "color: #37003c; margin-bottom: 20px;")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Current League Table", 
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            height = "400px",
            withSpinner(DT::dataTableOutput('league'), type = 4, color = "#37003c")
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Points Progression", 
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            height = "500px",
            withSpinner(plotlyOutput("graph"), type = 4, color = "#37003c")
          )
        ),
        textOutput("keep_alive")
      ),
      
      # Modern Player Scores Tab  
      tabItem(
        tabName = "PlayerScores",
        fluidRow(
          column(
            width = 12,
            h2("👤 Player Performance", style = "color: #37003c; margin-bottom: 20px;")
          )
        ),
        fluidRow(
          column(
            width = 12, md = 4, lg = 3,
            bs4Card(
              title = "Select Player",
              status = "primary", 
              solidHeader = TRUE,
              selectInput(
                "p", 
                "Choose Player:",
                choices = c("Tom", "Warnes", "Hodge", "Luke"),
                selected = "Tom"
              )
            )
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Detailed Player Scores",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            withSpinner(DT::dataTableOutput("playerscores"), type = 4, color = "#37003c")
          )
        )
      ),
      
      # Modern Statistics Tab
      tabItem(
        tabName = "Stats",
        fluidRow(
          column(
            width = 12,
            h2("📊 Statistical Analysis", style = "color: #37003c; margin-bottom: 20px;")
          )
        ),
        fluidRow(
          column(
            width = 12, md = 4,
            bs4Card(
              title = "Top Unpicked Players", 
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              withSpinner(DT::dataTableOutput('notpicked'), type = 4, color = "#37003c")
            )
          ),
          column(
            width = 12, md = 4,
            bs4Card(
              title = "Overall Top Scorers", 
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              withSpinner(DT::dataTableOutput('topoverall'), type = 4, color = "#37003c")
            )
          ),
          column(
            width = 12, md = 4,
            bs4Card(
              title = "Points Per Game Leaders", 
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              withSpinner(DT::dataTableOutput('ppg'), type = 4, color = "#37003c")
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12, md = 6,
            bs4Card(
              title = "🏆 Best Gameweek Performances", 
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              withSpinner(DT::dataTableOutput('topScores'), type = 4, color = "#37003c")
            )
          ),
          column(
            width = 12, md = 6,
            bs4Card(
              title = "😞 Worst Gameweek Performances", 
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              width = NULL,
              withSpinner(DT::dataTableOutput('bottomScores'), type = 4, color = "#37003c")
            )
          )
        )
      ),
      
      # Modern Player Finder Tab
      tabItem(
        tabName = "PlayerFinder",
        fluidRow(
          column(
            width = 12,
            h2("🔍 Player Discovery", style = "color: #37003c; margin-bottom: 20px;")
          )
        ),
        fluidRow(
          bs4Card(
            title = "Filter Options",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            checkboxGroupInput(
              "Columns",
              label = "Select statistics to display:",
              choiceValues = c("GW points","News","points_per_game","minutes","goals_scored","assists",
                              "clean_sheets","goals_conceded","penalties_saved","penalties_missed","own_goals","yellow_cards",
                              "red_cards","saves","bonus","ict_index"),
              choiceNames = c("Current GW","News","PPG","Minutes","Goals","Assists","Clean Sheets",
                             "Goals Conceded","Pen Saves","Pen Misses","Own Goals","Yellow Cards","Red Cards","Saves",
                             "Bonus","ICT Index"),
              selected = c("points_per_game", "goals_scored", "assists"),
              width = '100%', 
              inline = TRUE
            )
          )
        ),
        br(),
        fluidRow(
          bs4Card(
            title = "Available Players",
            status = "info",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            withSpinner(DT::dataTableOutput("finder"), type = 4, color = "#37003c")
          )
        )
      ),
      
      # Modern Transfers Tab
      tabItem(
        tabName = "Transfers",
        fluidRow(
          column(
            width = 12,
            h2("🔄 Transfer Activity", style = "color: #37003c; margin-bottom: 20px;")
          )
        ),
        fluidRow(
          column(
            width = 12, md = 8,
            bs4Card(
              title = "Transfer History",
              status = "primary", 
              solidHeader = TRUE,
              collapsible = FALSE,
              width = NULL,
              withSpinner(DT::dataTableOutput("transfers"), type = 4, color = "#37003c")
            )
          ),
          column(
            width = 12, md = 4,
            bs4Card(
              title = "Draft Order",
              status = "success",
              solidHeader = TRUE, 
              collapsible = FALSE,
              width = NULL,
              withSpinner(DT::dataTableOutput("order"), type = 4, color = "#37003c")
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            downloadButton(
              "downloadData",
              "📥 Download All Player Data",
              class = "btn-primary btn-lg",
              style = "margin: 20px; border-radius: 25px; padding: 10px 30px;"
            )
          )
        )
      )
    ),
    
    # Keep existing functionality
    tags$head(includeScript("www/google.js")),
    tags$head(tags$script("
      var socket_timeout_interval;
      var n = 0;
      $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function() {
          Shiny.onInputChange('alive_count', n++)
        }, 100);
      });
      $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
      });
    ")),
    tags$head(tags$style(HTML("#keep_alive { visibility: hidden; }")))
  )
)

shinyApp(ui = ui, server = server)
