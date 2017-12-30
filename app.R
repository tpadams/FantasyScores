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
ffdata <- fromJSON('https://fantasy.premierleague.com/drf/bootstrap-static')

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

withProgress(message = 'Loading...', value = 0,{
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
  started <- individual$explain$fixture$started
  if(is.null(started)){started <- FALSE}
  player_with_scores <- cbind(player_with_scores,started)
  scorelist[[i]] <- player_with_scores #}
  incProgress(amount=1/nrow(players),detail=paste("Player: ",i,"/",nrow(players)))
}})

allplayers <- dplyr::bind_rows(scorelist)
allplayers <- allplayers[,c(1:9,ncol(allplayers),(10:(ncol(allplayers)-1)))]
#SEPTEMBER PICKS - IGNORE FIRST THREE GAMEWEEKS
allplayers <- within(allplayers, `1`[id %in% c('513','512','518','543','536','520')] <- '0')
allplayers <- within(allplayers, `2`[id %in% c('513','512','518','543','536','520')] <- '0')
allplayers <- within(allplayers, `3`[id %in% c('513','512','518','543','536')] <- '0')

#TRANSFERS - replace gameweeks with those of player transferred out
allplayers[allplayers$Name=='Sanches',11:13] <- allplayers[allplayers$Name=='Llorente',11:13] #Sanches in for Llorente 3GW
allplayers[allplayers$id=='520',11:16] <- allplayers[allplayers$id=='484',11:16] #Davinson Sanchez (520) in for Mendy (484) 6GW
allplayers[allplayers$id=='523',11:21] <- allplayers[allplayers$id=='273',11:21] #Ibrahimovic (523) in for Lindelof (273)
allplayers[allplayers$id=='23',11:30] <- allplayers[allplayers$id=='271',11:30] #Xhaka (23) in for Bailly (271) GW20
allplayers[allplayers$id=='274',11:30] <- allplayers[allplayers$id=='181',11:30] #Mata (274) in for Kachunga (181) GW20
allplayers[allplayers$id=='245',11:30] <- allplayers[allplayers$id=='244',11:30] #Otamendi (245) in for Kompany (244) GW20
allplayers[allplayers$id=='264',11:30] <- allplayers[allplayers$id=='425',11:30] #Jones (264) in for McAuley (425) GW20
allplayers[allplayers$id=='342',11:30] <- allplayers[allplayers$id=='317',11:30] #Shaqiri (342) in for Ward-Prowse (317) GW20
allplayers[allplayers$id=='78',11:30] <- allplayers[allplayers$id=='332',11:30] #Stephen Ward (78) in for Pieters (332) GW20
allplayers[allplayers$id=='161',11:30] <- allplayers[allplayers$id=='25',11:30] #Rooney (161) in for Giroud (25) GW20
allplayers[allplayers$id=='250',11:30] <- allplayers[allplayers$id=='149',11:30] #Fernandinho (250) in for Barkley (149) GW20
allplayers[allplayers$id=='131',11:30] <- allplayers[allplayers$id=='512',11:30] #Townsend (131) in for Jese (512) GW20
allplayers[allplayers$id=='468',11:30] <- allplayers[allplayers$id=='217',11:30] #Hegazi (468) in for Clyne (217) GW20
allplayers[allplayers$id=='501',11:30] <- allplayers[allplayers$id=='427',11:30] #Richarlison (501) in for Chadli (427) GW20
allplayers[allplayers$id=='279',11:30] <- allplayers[allplayers$id=='124',11:30] #Lingard (279) in for Van Aanholt (124) GW20
allplayers[allplayers$id=='77',11:30] <- allplayers[allplayers$id=='270',11:30] #Mee (77) in for Blind (270) GW20
allplayers[allplayers$id=='147',11:30] <- allplayers[allplayers$id=='87',11:30] #Bolasie (147) in for Brady (87) GW20
allplayers[allplayers$id=='300',11:30] <- allplayers[allplayers$id=='15',11:30] #Atsu (300) in for Walcott (15) GW20
allplayers[allplayers$id=='29',11:30] <- allplayers[allplayers$id=='536',11:30] #Begovic (29) in for Karnezis (536) GW20

allplayers<-allplayers[!allplayers$id %in%c('371','484','273','271','181','244','425','317','332','25','149','512','217','427','124','270','87','15','536'),] #now remove transferred out player from DF. 371 = Llorente
players<-players[!players$id %in%c('371','484','273','271','181','244','425','317','332','25','149','512','217','427','124','270','87','15','536'),] 

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
gwscores<-apply(get(q)[11:ncol(get(q))],2,function(x) sum(head(sort(x, decreasing=TRUE), 20))) #20 is scored players. 
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
  
  playersStarted <- allplayers %>% #count number of players whose game this GW has started
    group_by(`Picked by`,`started`) %>%
    count()
  
  finalStarted <- c()
  for(y in levels(playersStarted$`Picked by`)){
    if('TRUE' %in% subset(playersStarted,playersStarted$`Picked by`==y)['started']){
      finalStarted <- rbind(finalStarted,playersStarted[playersStarted$started==TRUE & playersStarted$`Picked by`==y,c(1,3)])
    } else{
      interim <- playersStarted[playersStarted$started==FALSE & playersStarted$`Picked by`==y ,c(1,3)]
      interim$n <- 27 - interim$n
      finalStarted <- rbind(finalStarted,interim)
      }}
    

  leagueTable<-merge(overallTable,thisWeek,by="Player")
  leagueTable <- merge(leagueTable,finalStarted,by.y="Picked by",by.x="Player")
  leagueTable <- leagueTable[,c(1,5,4,6)]
  names(leagueTable) <- c("Player","Total points","This GW","Teams played")

  output$league <- DT::renderDataTable(datatable(leagueTable[with(leagueTable,order(-`Total points`)),],rownames=FALSE))
  
  #need to make this work and add to UI
  output$topScores <- DT::renderDataTable(datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(-`Points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
  output$bottomScores <- DT::renderDataTable(datatable(head(gameweekpoints[c(1,2,3)][with(gameweekpoints,order(`Points`)),],10),rownames=FALSE,options=list(dom='t',autoWidth = TRUE)))
  
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
  
  transfersDF <- data.frame(Player=c("Hodge","Warnes","Luke","Luke","Luke","David","David","David","David","Warnes","Warnes","Warnes","Warnes","Tom","Tom","Tom","Hodge","Hodge","Hodge"),In=c("Sanches (SWA)","Sanchez (TOT)","Ibrahimovic (MNU)","Xhaka (ARS)","Mata (MNU)","Otamendi (MNC)","Jones (MNU)","Shaqiri (STO)","Ward (BUR)","Rooney (EVE)","Fernandinho (MNC)","Townsend (CRY)","Hegazi (WBA)","Richarlison (WAT)","Lingard (MNU)","Mee (BUR)","Bolasie (EVE)","Atsu (NEW)","Begovic (BOU)"),Out=c("Llorente (TOT)","Mendy (MNC)","Lindelof (MNU)","Bailly (MNU)","Kachunga (HUD)","Kompany (MNC)","McAuley (WBA)","Ward-Prowse (SOT)","Pieters (STO)","Giroud (ARS)","Barkley (EVE)","Jese (STO)","Clyne (LIV)","Chadli (WBA)","Van Aanholt (CRY)","Blind (MNU)","Brady (BUR)","Walcott (ARS)","Karnezis (WAT)"),Date=c("07/09/2017","28/09/2017","18/11/2017",replicate(16,"30/12/2017")))

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
                menuItem("Available players", tabName="PlayerFinder",icon=icon("search")),
                downloadButton("downloadData","Download All Player Data",class="butt"),
                tags$head(tags$style(".butt{background-color:#00a65a;} .butt{color: white !important} .butt{margin: 15px}"))
              )),
              dashboardBody(
                tabItems(
                tabItem(tabName="LeagueTable",
                fluidRow(DT::dataTableOutput('league'),plotlyOutput("graph"))),
                tabItem(tabName="PlayerScores",
                fluidRow(selectInput("p","Player:",c("Tom","Warnes","David","Hodge","Luke"))),
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
              )))

                        
shinyApp(ui = ui, server = server)