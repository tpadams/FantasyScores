library(shiny)
library(jsonlite)
library(curl)
library(data.table)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(plotly)
#setwd("~/GitHub/FantasyScores/")

#load current scores from RDS file
server<-shinyServer(function(input, output,session) {
scorelist <- list()
ffdata <- fromJSON('http://fantasy.premierleague.com/api/bootstrap-static/')

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
withProgress(message = 'Loading...', value = 0,{
for(i in 1:nrow(players)){
  individual <- fromJSON(paste0("http://fantasy.premierleague.com/api/element-summary/",players[i,]$id,"/"))
  tryCatch({roundscores <- transpose(individual$history[,c("round","total_points")])},error=function(e){roundscores <<- as.data.frame(matrix(c(1,2,3,0,0,0), nrow=2, ncol=3,byrow=TRUE))})
  tryCatch({minsplayed <- individual$history[individual$history$round==max(individual$history$round),][,c("minutes")]},error=function(e){minsplayed<<-0},warning=function(w){minsplayed<<-0})
  names(roundscores) <- c(roundscores[1,])
  temp <- names(roundscores) #store names for reassignment
  roundscores <- data.frame(roundscores[2,])
  names(roundscores) <- temp
  player_with_scores <- cbind(players[i,],roundscores)
  player_with_scores$`GW minutes` <- sum(minsplayed)
  player_with_scores <- player_with_scores[,c(1:5,ncol(player_with_scores),6:(ncol(player_with_scores)-1))]
  started <- ifelse(currentGW==min(individual$fixtures$event,na.rm=T),0,1)
  if(is.null(started)){started <- FALSE} #controls for blank GWs
  player_with_scores <- cbind(player_with_scores,started)
  #you need to combine the two games in the double GWs and then remove the extra GW entry (e.g. 22.1)
  if("21.1" %in% names(player_with_scores)){player_with_scores$`21` <- player_with_scores$`21`+player_with_scores$`21.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("21.1"))]}
  if("22.1" %in% names(player_with_scores)){player_with_scores$`22` <- player_with_scores$`22`+player_with_scores$`22.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("22.1"))]}
  if("23.1" %in% names(player_with_scores)){player_with_scores$`23` <- player_with_scores$`23`+player_with_scores$`23.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("23.1"))]}
  if("25.1" %in% names(player_with_scores)){player_with_scores$`25` <- player_with_scores$`25`+player_with_scores$`25.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("25.1"))]}
  if("26.1" %in% names(player_with_scores)){player_with_scores$`26` <- player_with_scores$`26`+player_with_scores$`26.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("26.1"))]}
  if("28.1" %in% names(player_with_scores)){player_with_scores$`28` <- player_with_scores$`28`+player_with_scores$`28.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("28.1"))]}
  if("29.1" %in% names(player_with_scores)){player_with_scores$`29` <- player_with_scores$`29`+player_with_scores$`29.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("29.1"))]}
  if("31.1" %in% names(player_with_scores)){player_with_scores$`31` <- player_with_scores$`31`+player_with_scores$`31.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("31.1"))]}
  if("33.1" %in% names(player_with_scores)){player_with_scores$`33` <- player_with_scores$`33`+player_with_scores$`33.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("33.1"))]}
  if("34.1" %in% names(player_with_scores)){player_with_scores$`34` <- player_with_scores$`34`+player_with_scores$`34.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("34.1"))]}
  if("36.1" %in% names(player_with_scores)){player_with_scores$`36` <- player_with_scores$`36`+player_with_scores$`36.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("36.1"))]}
  scorelist[[i]] <- player_with_scores #}
  incProgress(amount=1/nrow(players),detail=paste("Player: ",i,"/",nrow(players)))
}})

allplayers <- dplyr::bind_rows(scorelist)
gws <- as.character(seq(1,max(as.numeric(names(allplayers)),na.rm=T)))
#rem <- as.character(seq(30,38))
gws <- gws[! gws %in% c(7)]
allplayers <- allplayers[,c("id","Name","Position","Team","GW points","GW minutes","Total points","News","Picked by","started",gws)]

##########################IN########################################OUT################################
allplayers[allplayers$id=='113',11:13] <- allplayers[allplayers$id=='50',11:13] #Carlos 50 out for Sanchez 113 Before GW4
allplayers[allplayers$id=='609',11:15] <- allplayers[allplayers$id=='290',11:15] #Konate 290 out for Antony 609 before GW6
allplayers[allplayers$id=='594',11:15] <- allplayers[allplayers$id=='8',11:15] #Tierney 8 out for Isak 594 before GW6
allplayers[allplayers$id=='617',11:15] <- allplayers[allplayers$id=='191',11:15] #Calvert-Lewin 191 out for Auba 617
allplayers[allplayers$id=='603',11:15] <- allplayers[allplayers$id=='12',11:15] #Smith-Rowe 12 out for Paqueta 603
allplayers[allplayers$id=='342',11:15] <- allplayers[allplayers$id=='114',11:15] #Lamptey 114 out for Dalot 342
allplayers[allplayers$id=='169',11:15] <- allplayers[allplayers$id=='37',11:15] #McGinn 37 out for Eze 169 before GW6
allplayers[allplayers$id=='169',11:15] <- allplayers[allplayers$id=='37',11:15] #McGinn 37 out for Eze 169 before GW6
allplayers[allplayers$id=='346',11:19] <- allplayers[allplayers$id=='486',11:19] #Neto 486 out for Pereira 346 before GW11 H
allplayers[allplayers$id=='26',11:19] <- allplayers[allplayers$id=='464',11:19] #Benrahma 464 out for Saliba 26 before GW11 W
allplayers[allplayers$id=='104',11:19] <- allplayers[allplayers$id=='527',11:19] #Lingard 527 out for Gross 104 before GW11 T
allplayers[allplayers$id=='589',11:19] <- allplayers[allplayers$id=='272',11:19] #Fofana 272 out for Nunes 589 before GW11 W
allplayers[allplayers$id=='433',11:19] <- allplayers[allplayers$id=='135',11:19] #Ziyech 135 out for Hojberg 433 before GW11 L




out<-c('50','290','8','191','12','114','37','486','464','527','272','135')

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
  
  output$playerscores <- DT::renderDataTable(datatable(DF()[-c(1,9)],rownames=FALSE,options=list(order=list(3,'desc'),scrollX=TRUE)) %>% #-1 excludes ID columns, -9 picked by col
    formatStyle(colnames(DF())[c(5,10:ncol(DF()))],Color = styleInterval(c(0.56),c('black','green')))) #10 is the column number of first round column
  
  
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

  output$league <- DT::renderDataTable(datatable(leagueTable[with(leagueTable,order(-`Total points`)),],rownames=FALSE,options = list(scrollX = TRUE)))
  
  #need to make this work and add to UI
  output$topScores <- DT::renderDataTable(datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(-`Points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$bottomScores <- DT::renderDataTable(datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(`Points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  #gameweekpoints[gameweekpoints$Week>29,]$Week <- gameweekpoints[gameweekpoints$Week>29,]$Week -9
  output$graph <-renderPlotly(ggplot(data=gameweekpoints,aes(x=Week,y=Cumulative)) +geom_line(aes(colour=Player),size=2) +geom_point(aes(text=paste("Points:",Points))) +scale_x_continuous(breaks= c(1:38)))
  
  ########Other Stats#########
  output$notpicked <- DT::renderDataTable(datatable(head(notpicked[c(2,3,4,6)][with(notpicked,order(-`Total points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$topoverall <- DT::renderDataTable(datatable(head(playerscopy[c(2,3,4,6)][with(playerscopy,order(-`Total points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$ppg <- DT::renderDataTable(datatable(head(ppg[with(ppg,order(-`PPG`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  notpicked$Team <- as.factor(notpicked$Team)
  notpicked$Position <- as.factor(notpicked$Position)
  finderData <- merge(ffdata$elements,notpicked, by='id') 
  output$finder <- DT::renderDataTable(datatable(finderData[,c('Name','Position','Team','Total points',input$Columns)],rownames=FALSE,filter='top',options=list(dom='t',order=list(3,'desc'),scrollX=TRUE)))
  
  #####DOWNLOAD PLAYER DATA#####
  
  output$downloadData <- downloadHandler(
    filename = paste("All-Player-Data-", format(Sys.time(), "%d%b%Y") ,".csv", sep = ""),
    content = function(file) {
      write.csv(ffdata$elements, file, row.names = FALSE)
    }
  )
  
  #####TRANSFERS######
  
  transfersDF <- data.frame(Player=c("Hodge","Hodge","Luke","Warnes","Tom","Tom","Warnes","Warnes","Tom","Warnes","Luke","Hodge"),In=c("Sanchez","Antony","Isak","Aubameyang","Paqueta","Dalot","Eze","Saliba","Gross","Nunes","Hojberg","Pereira"),Out=c("Diego Carlos","Konate","Tierney","Calvert-Lewin","Smith-Rowe","Lamptey","McGinn","Benrahma","Lingard","Fofana","Ziyech","Neto"),BeforeGameweek=c("4","6","6","6","6","6","6","11","11","11","11","11"))

  output$transfers <- DT::renderDataTable(datatable(transfersDF))
  
  ###Keep Alive###
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
  #session$onSessionEnded(stopApp)
}) #end of shiny section


#######UI######

ui<- dashboardPage(skin='green',
              dashboardHeader(title='Fantasy Scores'),
              dashboardSidebar(sidebarMenu(id="FantMenu",
                menuItem("League Table",tabName="LeagueTable",icon=icon("list-ol")),                                           
                menuItem("Player Scores",tabName="PlayerScores",icon=icon("futbol-o")),
                menuItem("Transfers", tabName="Transfers",icon=icon("exchange")),
                menuItem("Statistics", tabName="Stats",icon=icon("table")),
                menuItem("Available players", tabName="PlayerFinder",icon=icon("search")),
                downloadButton("downloadData","Download All Player Data",class="butt"),
                tags$head(tags$style(".butt{background-color:#00a65a;} .butt{color: white !important} .butt{margin: 15px}"))
              )),
              dashboardBody(
                tabItems(
                tabItem(tabName="LeagueTable",
                fluidRow(DT::dataTableOutput('league'),plotlyOutput("graph")),fluidRow(textOutput("keep_alive"))),
                tabItem(tabName="PlayerScores",
                fluidRow(selectInput("p","Player:",c("Tom","Warnes","Hodge","Luke"))),
                fluidRow(DT::dataTableOutput("playerscores"))),
                tabItem(tabName="Stats",
                fluidRow(box(DT::dataTableOutput('notpicked'),width=4,title="Top scoring non-picked players"),
                box(DT::dataTableOutput('topoverall'),width=4,title="Top scoring players overall"),
                box(DT::dataTableOutput('ppg'),width=4,title="Top scoring points per game")),
                fluidRow(box(DT::dataTableOutput('topScores'),width=6,title="Best GW Scores"),
                         box(DT::dataTableOutput('bottomScores'),width=6,title="Worst GW Scores"))),
                tabItem(tabName="PlayerFinder",
                fluidRow(checkboxGroupInput("Columns",label = "Select columns:",
                                            choiceValues = c("GW points","News","points_per_game","minutes","goals_scored","assists",
                                                        "clean_sheets","goals_conceded","penalties_saved","penalties_missed","own_goals","yellow_cards",
                                                        "red_cards","saves","bonus","ict_index"),
                                            choiceNames = c("Current gameweek","News","Points per game","Total minutes","Goals","Assists","Clean sheets",
                                                            "Goals conceded","Penalties saved","Penalties missed","Own goals","Yellows","Red","Saves",
                                                            "Bonus points","ICT index"),
                                            selected = c(),
                                            width = '100%', inline = TRUE)),
                fluidRow(DT::dataTableOutput("finder"))),
                tabItem(tabName="Transfers",
                fluidRow(DT::dataTableOutput("transfers")))
              ),tags$head(includeScript("www/google.js")),tags$head(tags$script("var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 100);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});")),tags$head(tags$style(HTML("#keep_alive {
  visibility: hidden;
}")))))

                        
shinyApp(ui = ui, server = server)
