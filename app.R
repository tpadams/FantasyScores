library(shiny)
library(jsonlite)
library(curl)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(bs4Dash)
library(plotly)
library(shinycssloaders)

#setwd("~/GitHub/FantasyScores/")

#load current scores from RDS file
server<-shinyServer(function(input, output,session) {
# Fresh data load every session - no caching
scorelist <- list()
# Add cache-busting to main API call
ffdata <- jsonlite::fromJSON(paste0('http://fantasy.premierleague.com/api/bootstrap-static/?t=', as.numeric(Sys.time())))
##Add something to check if currentGW is finsihed or not?

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

# Smart caching system - load from RDS if less than 2 minutes old
cache_file <- "player_data_cache.rds"
use_cache <- TRUE
print(file.exists(cache_file))

# Check if cache file exists and is fresh (less than 2 minutes old)
if (file.exists(cache_file)) {
  file_age_minutes <- as.numeric(difftime(Sys.time(), file.mtime(cache_file), units = "mins"))
  if (file_age_minutes > 2 & sum(ffdata$events$is_current) == 1) {
    use_cache <- FALSE
  } else if(file_age_minutes>1339){
    use_cache <- FALSE
  }
}

if (use_cache) {
  # Load from cache
  withProgress(message = 'Loading cached player data...', value = 0, {
    scorelist <- readRDS(cache_file)
    incProgress(1, detail = paste("Loaded", length(scorelist), "players from cache"))
  })
} else {
  # Fresh API calls with progress bar
  withProgress(message = 'Loading fresh player data...', value = 0, {
  
for(i in 1:nrow(players)){
    player_id <- players[i,]$id
    
    # API call with cache-busting using fromJSON directly
    individual <- tryCatch({
      # Add cache-busting timestamp to prevent caching
      api_url <- paste0("http://fantasy.premierleague.com/api/element-summary/", player_id, "/?t=", as.numeric(Sys.time()))
      
      # Use jsonlite directly
      jsonlite::fromJSON(api_url)
    }, error = function(e) {
      # Fallback - minimal data structure
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
  temp <- names(roundscores) #store names for reassignment
  roundscores <- data.frame(roundscores[2,])
  names(roundscores) <- temp
    player_with_scores <- cbind(players[i,], roundscores)
  player_with_scores$`GW minutes` <- sum(minsplayed)
  player_with_scores <- player_with_scores[,c(1:5,ncol(player_with_scores),6:(ncol(player_with_scores)-1))]
    
    # Check if started
  started <- ifelse(currentGW==min(individual$fixtures$event,na.rm=T),0,1)
  if(is.null(started)){started <- FALSE} #controls for blank GWs
    player_with_scores <- cbind(player_with_scores, started)
    
    # Handle double gameweeks
    # if("24.1" %in% names(player_with_scores)){
    #   player_with_scores$`24` <- player_with_scores$`24`+player_with_scores$`24.1`
    # }
    # if("25.1" %in% names(player_with_scores)){
    #   player_with_scores$`25` <- player_with_scores$`25`+player_with_scores$`25.1`
    # }
    
    scorelist[[i]] <- player_with_scores
    
    # Update progress for each player
    incProgress(amount = 1/nrow(players), detail = paste("Loading player", i, "of", nrow(players)))
  }
  
  # Save fresh data to cache
  saveRDS(scorelist, cache_file)
  })
}

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
  
  # Create beautiful Premier League themed graph
  output$graph <- renderPlotly({
    # Validate data is available and not empty
    req(exists("gameweekpoints"))
    req(nrow(gameweekpoints) > 0)
    req(all(c("Week", "Cumulative", "Player", "Points") %in% names(gameweekpoints)))
    
    # Define Premier League colors for each player
    pl_colors <- c("Tom" = "#37003c", "Warnes" = "#00ff87", "Hodge" = "#17a2b8", "Luke" = "#ffc107")
    
    # Create the plot with error handling
    tryCatch({
      pg <- ggplot(data = gameweekpoints, aes(x = Week, y = Cumulative, color = Player)) +
        # Add lines with custom colors
        geom_line(aes(group = Player), size = 3, alpha = 0.8) +
        
        # Add points with hover information
        geom_point(aes(text = paste0("<b>", Player, "</b><br>",
                                     "Gameweek: ", Week, "<br>",
                                     "GW Points: ", Points, "<br>",
                                     "Total: ", Cumulative)),
                   size = 4, alpha = 0.9) +
        
        # Custom color scheme
        scale_color_manual(values = pl_colors) +
        
        # Professional styling
        scale_x_continuous(breaks = seq(1, 38, 2), 
                          minor_breaks = 1:38,
                          expand = c(0.02, 0)) +
        scale_y_continuous(expand = c(0.02, 0)) +
        
        # Labels and title
        labs(title = "Points by week",
             subtitle = "Cumulative points by gameweek",
             x = "Gameweek",
             y = "Cumulative Points",
             color = "Manager") +
        
        # Premium theme
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "#37003c", hjust = 0.5),
          plot.subtitle = element_text(size = 14, color = "#666666", hjust = 0.5, margin = margin(b = 20)),
          axis.title = element_text(size = 12, face = "bold", color = "#37003c"),
          axis.text = element_text(size = 11, color = "#555555"),
          legend.title = element_text(size = 12, face = "bold", color = "#37003c"),
          legend.text = element_text(size = 11),
          legend.position = "bottom",
          panel.grid.major = element_line(color = "#f0f0f0", size = 0.5),
          panel.grid.minor = element_line(color = "#f8f8f8", size = 0.3),
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          legend.box.background = element_rect(fill = "white", color = NA)
        )
      
      # Convert to plotly with custom configuration
      ggplotly(pg, tooltip = "text") %>%
        layout(
          font = list(family = "Arial", size = 12),
          hovermode = "closest",
          showlegend = TRUE
        ) %>%
        config(
          displayModeBar = TRUE,
          modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"),
          displaylogo = FALSE
        )
    }, error = function(e) {
      # Fallback: return a simple plot if main plot fails
      plot_ly(x = 1, y = 1, type = "scatter", mode = "markers") %>%
        layout(title = "Loading chart data...", 
               xaxis = list(title = "Gameweek"),
               yaxis = list(title = "Points"))
    })
  })
  
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
  
  /* Custom brand styling - targeting our specific element */
  .custom-brand {
    color: white !important;
    background-color: #00ff87 !important;
    border: none !important;
    border-radius: 4px !important;
    padding: 8px 12px !important;
    font-weight: bold !important;
    font-size: 1.3em !important;
    text-transform: none !important;
    letter-spacing: 1px !important;
    text-decoration: none !important;
    box-shadow: none !important;
    outline: none !important;
    width: 250px !important;
    display: block !important;
    text-align: center !important;
  }
  
  /* Aggressive hover/focus/active state control */
  .custom-brand:hover,
  .custom-brand:focus,
  .custom-brand:active,
  .custom-brand:visited,
  .custom-brand:link {
    color: white !important;
    background-color: #00ff87 !important;
    border: none !important;
    text-decoration: none !important;
    box-shadow: none !important;
    outline: none !important;
    transform: none !important;
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
    title = tags$a(
      class = "navbar-brand custom-brand",
      href = "#",
      "Fantscores.uk"
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
                transition: margin-left 0.3s ease !important;
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
              
              /* Restore proper sidebar responsive behavior */
              .main-sidebar {
                position: fixed !important;
                top: 0 !important;
                left: -250px !important;
                width: 250px !important;
                height: 100vh !important;
                transition: left 0.3s ease !important;
                z-index: 1040 !important;
              }
              
              /* When sidebar is open on mobile */
              .sidebar-open .main-sidebar {
                left: 0 !important;
              }
              
              /* Add overlay when sidebar is open on mobile */
              .sidebar-open::before {
                content: '';
                position: fixed;
                top: 0;
                left: 0;
                width: 100%;
                height: 100%;
                background: rgba(0,0,0,0.5);
                z-index: 1030;
              }
              
              /* Ensure main content stays in place */
              .content-wrapper {
                margin-left: 0 !important;
                width: 100% !important;
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
      
      // Simplified brand protection + Mobile sidebar behavior
      $(document).ready(function() {
        $('.custom-brand').on('focus blur', function(e) {
          e.preventDefault();
          $(this).blur();
        });
        
        // Ensure proper mobile sidebar behavior
        if (window.innerWidth <= 768) {
          $('body').addClass('sidebar-mini sidebar-collapse');
          
          // Handle sidebar toggle on mobile
          $(document).on('click', '.sidebar-toggle', function() {
            $('body').toggleClass('sidebar-open');
          });
          
          // Close sidebar when clicking overlay
          $(document).on('click', function(e) {
            if ($(e.target).closest('.main-sidebar, .sidebar-toggle').length === 0) {
              $('body').removeClass('sidebar-open');
            }
          });
        }
      });
    ")),
    tags$head(tags$style(HTML("#keep_alive { visibility: hidden; }")))
  )
)
                        
shinyApp(ui = ui, server = server)
