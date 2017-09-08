library(shiny)
library(jsonlite)
library(curl)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
#setwd("H:/Personal/Fantasy/FantasyScores")

#load current scores from RDS file
server<-shinyServer(function(input, output,session) {
scorelist <- list()
ffdata <- fromJSON('https://fantasy.premierleague.com/drf/bootstrap-static')

homeTeamsStarted <- ffdata$next_event_fixtures[,c('team_h','started')]
awayTeamsStarted <- ffdata$next_event_fixtures[,c('team_a','started')]
names(homeTeamsStarted) <- c("id","started")
names(awayTeamsStarted) <- c("id","started")
teamsStarted <- rbind(homeTeamsStarted,awayTeamsStarted)
teamsStarted<- merge(teamsStarted,ffdata$teams,by="id")[,c('code','started')]
names(teamsStarted) <- c("team_code","started")

players_noteam <- ffdata$elements[,c("id","web_name","element_type","team_code","event_points","total_points","news","points_per_game")]
teams <- ffdata$teams[,c("name","code")]
colnames(teams) <- c("name","team_code")
players <- (merge(players_noteam, teams, by = 'team_code'))
players <- (merge(players,teamsStarted, by = 'team_code'))
players <- players[ , !(names(players) == 'team_code')]
players$element_type <- recode(players$element_type, "1" = "GK", "2" = "Def", "3" = "Mid", "4" = "Atk")
ppg<- players[,c("web_name","element_type","name","points_per_game")]
players <- players[,c(1,2,3,8,4,5,6,9)] #reorder columns
picks <- read.csv("www/picks.csv") 
playerscopy <- players
players <- merge(players,picks,by='id')

notpicked <- subset(playerscopy, !(id %in% players$id))

names(notpicked) <- c("id","Name","Position","Team","GW points","Total points","News")
names(playerscopy) <- c("id","Name","Position","Team","GW points","Total points","News")
colnames(players) <- c("id","Name","Position","Team","GW points","Total points","News","Team started","Picked by")
names(ppg) <- c("Name","Position","Team","PPG")
ppg$PPG <- as.numeric(ppg$PPG)


for(i in 1:nrow(players)){
  individual <- fromJSON(paste0("https://fantasy.premierleague.com/drf/element-summary/",players[i,]$id))
  tryCatch({roundscores <- transpose(individual$history[,c("round","total_points")])},error=function(e){roundscores <<- as.data.frame(matrix(c(1,2,3,0,0,0), nrow=2, ncol=3,byrow=TRUE))})
  tryCatch({minsplayed <- individual$history[individual$history$round==max(individual$history$round),][,c("minutes")]},error=function(e){minsplayed<<-0},warning=function(w){minsplayed<<-0})
  names(roundscores) <- c(roundscores[1,])
  temp <- names(roundscores) #store names for reassignment
  roundscores <- data.frame(roundscores[2,])
  names(roundscores) <- temp
  player_with_scores <- cbind(players[i,],roundscores)
  player_with_scores$`GW minutes` <- minsplayed
  player_with_scores <- player_with_scores[,c(1:5,ncol(player_with_scores),6:(ncol(player_with_scores)-1))]
  scorelist[[i]] <- player_with_scores
}

allplayers <- dplyr::bind_rows(scorelist)
#SEPTEMBER PICKS - IGNORE FIRST THREE GAMEWEEKS
allplayers <- within(allplayers, `1`[id %in% c('513','512','518','543','536')] <- '0')
allplayers <- within(allplayers, `2`[id %in% c('513','512','518','543','536')] <- '0')
allplayers <- within(allplayers, `3`[id %in% c('513','512','518','543','536')] <- '0')

#TRANSFERS - replace gameweeks with those of player transferred out
allplayers[allplayers$Name=='Sanches',11:13] <- allplayers[allplayers$Name=='Llorente',11:13] #Sanches in for Llorente

allplayers<-allplayers[!allplayers$Name %in%c('Llorente'),] #now remove transferred out player from DF. 371 = Llorente
players<-players[!players$Name %in%c('Llorente'),] 

allplayers[11:ncol(allplayers)] <- sapply(allplayers[11:ncol(allplayers)],as.numeric) #convert columns to numeric

#e<- names(allplayers)
#e[10:47] <- c(1:38)
#allplayers <- allplayers[,e] #put in right order after bind_rows puts NA values at end
finaldf <- split(allplayers,allplayers$`Picked by`)

gameweekpoints <- data.frame(Player=character(),Week=character(),Points=integer())
assign("Tom",finaldf$Tom)
assign("Warnes",finaldf$Warnes)
assign("David",finaldf$David)
assign("Hodge",finaldf$Hodge)
assign("Luke",finaldf$Luke)
for(q in c("Tom","Warnes","David","Hodge","Luke")){
gwscores<-apply(get(q)[11:ncol(get(q))],2,function(x) sum(head(sort(x, decreasing=TRUE), 20))) #change 2 to number of scored players
assign(paste0("gwpoints",q),data.frame(q,names(gwscores),gwscores))
assign(paste0("gwpoints",q),get(paste0("gwpoints",q)) %>%
  group_by(q) %>%
  mutate(cumsum = cumsum(gwscores)))
}


gameweekpoints<- dplyr::bind_rows(gwpointsTom,gwpointsWarnes,gwpointsDavid,gwpointsHodge,gwpointsLuke)
names(gameweekpoints) <- c("Player","Week","Points","Cumulative") #create frame of gameweeks and how many points scored
gameweekpoints$Week <- as.double(levels(gameweekpoints$Week))[gameweekpoints$Week] #convert weeknumber from Factor to Numeric



#Define server logic required to summarize and view the selected data
options(DT.options = list(paging=FALSE))
  #############PLAYER SCORES##############
  datasetInput <- reactive({
    switch(input$p,
           "Tom" = Tom,
           "Warnes" = Warnes,
           "Hodge" = Hodge,
           "David" = David,
           "Luke" = Luke
           )}) #get player
  
  DF <- reactive({ #this section adds the GW points only including top X as specified 
  gwpoints<- data.frame(transpose(subset(gameweekpoints,gameweekpoints$Player == input$p)[,2:3])[2,])
  names(gwpoints) <- c(transpose(subset(gameweekpoints,gameweekpoints$Player == input$p)[,2:3])[1,])
  gwpoints<- cbind(a="",b="",c="",d="",e="",f="",f1="",f2="",g="TOTAL",h="TOTAL",gwpoints)
  names(gwpoints) <- names(datasetInput())
  q<- rbind(datasetInput(),gwpoints)
  q$`GW points` <- as.numeric(q$`GW points`)
  q$`Total points` <- as.numeric(q$`Total points`)
  q
  })
  
  output$playerscores <- DT::renderDataTable(datatable(DF()[-c(1,9,10)],rownames=FALSE,options=list(order=list(3,'desc'),scrollX=TRUE)) %>% #-1 excludes ID columns, -9 picked by col
    formatStyle(colnames(DF())[c(5,11:ncol(DF()))],Color = styleInterval(c(0.769),c('black','green')))) #10 is the column number of first round column
  #change 0.769 to 0.740 after 27th player added
  
  ##################LEAGUE TABLE##############
  
  overallTable <- aggregate(. ~ Player, data=gameweekpoints[,c(1,3)], sum)
  thisWeek <- subset(gameweekpoints,gameweekpoints$Week==max(gameweekpoints$Week))
  playersTeamPlayed <- players %>%
    group_by(`Picked by`,`Team started`) %>%
    count()
  playersTeamPlayed <- as.data.frame(playersTeamPlayed)
  playersTeamPlayed <- playersTeamPlayed[playersTeamPlayed$`Team started`=='FALSE',]
  playersTeamPlayed$n <- playersTeamPlayed$n - 27
  leagueTable<-merge(overallTable,thisWeek,by="Player")
  leagueTable <- merge(leagueTable,playersTeamPlayed,by.y="Picked by",by.x="Player")
  leagueTable <- leagueTable[,c(1,5,4,7)]
  names(leagueTable) <- c("Player","Total points",paste0("GW",ffdata$`current-event`," Points"),paste0("GW",unique(ffdata$next_event_fixtures$event)," Teams played"))

  output$league <- DT::renderDataTable(datatable(leagueTable[with(leagueTable,order(-`Total points`)),],rownames=FALSE))
  
  output$graph <-renderPlot(ggplot(data=gameweekpoints,aes(x=Week,y=Cumulative)) +geom_line(aes(colour=Player),size=2) +scale_x_continuous(breaks= c(1:38)))
  
  ########Other Stats#########
  output$notpicked <- DT::renderDataTable(datatable(head(notpicked[c(2,3,4,6)][with(notpicked,order(-`Total points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$topoverall <- DT::renderDataTable(datatable(head(playerscopy[c(2,3,4,6)][with(playerscopy,order(-`Total points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$ppg <- DT::renderDataTable(datatable(head(ppg[with(ppg,order(-`PPG`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  #####DOWNLOAD PLAYER DATA#####
  
  output$downloadData <- downloadHandler(
    filename = paste("All-Player-Data-", format(Sys.time(), "%d%b%Y") ,".csv", sep = ""),
    content = function(file) {
      write.csv(ffdata$elements, file, row.names = FALSE)
    }
  )
  
  #####TRANSFERS######
  
  transfersDF <- data.frame(Player="Hodge",In="Sanches (SWA)",Out="Llorente (TOT)",Date="07/09/2017")

  output$transfers <- DT::renderDataTable(datatable(transfersDF))
  
  session$onSessionEnded(stopApp)
}) #end of shiny section


#######UI######

ui<- dashboardPage(skin='green',
              dashboardHeader(title='Fantasy Scores'),
              dashboardSidebar(sidebarMenu(id="FantMenu",
                menuItem("League Table",tabName="LeagueTable",icon=icon("list-ol")),                                           
                menuItem("Player Scores",tabName="PlayerScores",icon=icon("futbol-o")),
                menuItem("Transfers", tabName="Transfers",icon=icon("exchange")),
                menuItem("Statistics", tabName="Stats",icon=icon("table")),
                downloadButton("downloadData","Download All Player Data",class="butt"),
                tags$head(tags$style(".butt{background-color:#00a65a;} .butt{color: white !important} .butt{margin: 15px}"))
              )),
              dashboardBody(
                tabItems(
                tabItem(tabName="LeagueTable",
                fluidRow(DT::dataTableOutput('league'),plotOutput("graph"))),
                tabItem(tabName="PlayerScores",
                fluidRow(selectInput("p","Player:",c("Tom","Warnes","David","Hodge","Luke"))),
                fluidRow(DT::dataTableOutput("playerscores"))),
                tabItem(tabName="Stats",
                fluidRow(box(DT::dataTableOutput('notpicked'),width=4,title="Top scoring non-picked players"),
                box(DT::dataTableOutput('topoverall'),width=4,title="Top scoring players overall"),
                box(DT::dataTableOutput('ppg'),width=4,title="Top scoring points per game"))),
                tabItem(tabName="Transfers",
                fluidRow(DT::dataTableOutput("transfers")))
              )))

                        
shinyApp(ui = ui, server = server)