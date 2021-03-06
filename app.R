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
ffdata <- fromJSON('https://fantasy.premierleague.com/api/bootstrap-static')

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
  individual <- fromJSON(paste0("https://fantasy.premierleague.com/api/element-summary/",players[i,]$id))
  tryCatch({roundscores <- transpose(individual$history[,c("round","total_points")])},error=function(e){roundscores <<- as.data.frame(matrix(c(1,2,3,0,0,0), nrow=2, ncol=3,byrow=TRUE))})
  tryCatch({minsplayed <- individual$history[individual$history$round==max(individual$history$round),][,c("minutes")]},error=function(e){minsplayed<<-0},warning=function(w){minsplayed<<-0})
  names(roundscores) <- c(roundscores[1,])
  temp <- names(roundscores) #store names for reassignment
  roundscores <- data.frame(roundscores[2,])
  names(roundscores) <- temp
  player_with_scores <- cbind(players[i,],roundscores)
  player_with_scores$`GW minutes` <- sum(minsplayed)
  player_with_scores <- player_with_scores[,c(1:5,ncol(player_with_scores),6:(ncol(player_with_scores)-1))]
  started <- sum(individual$explain$fixture$started)
  if(is.null(started)){started <- FALSE} #controls for blank GWs
  player_with_scores <- cbind(player_with_scores,started)
  #you need to combine the two games in the double GWs and then remove the extra GW entry (e.g. 22.1)
  if("25.1" %in% names(player_with_scores)){player_with_scores$`25` <- player_with_scores$`25`+player_with_scores$`25.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("25.1"))]}
  if("32.1" %in% names(player_with_scores)){player_with_scores$`32` <- player_with_scores$`32`+player_with_scores$`32.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("32.1"))]}
  if("35.1" %in% names(player_with_scores)){player_with_scores$`35` <- player_with_scores$`35`+player_with_scores$`35.1` 
  player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("35.1"))]}
  scorelist[[i]] <- player_with_scores #}
  incProgress(amount=1/nrow(players),detail=paste("Player: ",i,"/",nrow(players)))
}})

allplayers <- dplyr::bind_rows(scorelist)
allplayers <- allplayers[,c("id","Name","Position","Team","GW points","GW minutes","Total points","News","Picked by","started",as.character(seq(1,max(as.numeric(names(allplayers)),na.rm=T))))]

#allplayers[allplayers$id=='36',11:16] <- allplayers[allplayers$id=='206',11:16] #Stanislas (36) in for Pritchard (206)

out<-c()

notpicked <- subset(playerscopy, !(id %in% players$id & !id %in% out))

allplayers<-allplayers[!allplayers$id %in%out,]

players<-players[!players$id %in%out,]

# 
# allplayers<-allplayers[!allplayers$id %in%c('206'),] #now remove transferred out player from DF. 371 = Llorente
# players<-players[!players$id %in%c('206'),] 
# allplayers<-allplayers[!allplayers$id %in%c('359'),] #now remove transferred out player from DF. 371 = Llorente
# players<-players[!players$id %in%c('359'),] 
# allplayers<-allplayers[!allplayers$id %in%c('18'),] #now remove transferred out player from DF. 371 = Llorente
# players<-players[!players$id %in%c('18'),] 
# allplayers<-allplayers[!allplayers$id %in%c('492'),] #now remove transferred out player from DF. 371 = Llorente
# players<-players[!players$id %in%c('492'),]
# allplayers<-allplayers[!allplayers$id %in%c('402'),] #now remove transferred out player from DF. 371 = Llorente
# players<-players[!players$id %in%c('402'),] 


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
assign(paste0(q), get(q)[,c(1:9,11:ncol(get(q)))])
gwscores<-apply(get(q)[10:ncol(get(q))],2,function(x) sum(head(sort(x, decreasing=TRUE), 14))) #20 is scored players. 
assign(paste0("gwpoints",q),data.frame(q,names(gwscores),gwscores))
assign(paste0("gwpoints",q),get(paste0("gwpoints",q)) %>%
  group_by(q) %>%
  mutate(cumsum = cumsum(gwscores)))
}


gameweekpoints<- dplyr::bind_rows(gwpointsTom,gwpointsWarnes,gwpointsDavid,gwpointsHodge,gwpointsLuke)
names(gameweekpoints) <- c("Player","Week","Points","Cumulative") #create frame of gameweeks and how many points scored
gameweekpoints$Week <- as.double(levels(gameweekpoints$Week))[gameweekpoints$Week] #convert weeknumber from Factor to Numeric
gameweekpoints <- gameweekpoints[!is.na(gameweekpoints$Week),]


#Define server logic required to summarize and view the selected data
options(DT.options = list(paging=FALSE))
  #############PLAYER SCORES##############
  datasetInput <- reactive({a<-switch(input$p,
           "Tom" = Tom,
           "Warnes" = Warnes,
           "Hodge" = Hodge,
           "David" = David,
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
  
  playersStarted <- allplayers %>% #count number of players whose game this GW has started
    group_by(`Picked by`) %>%
    summarise(sum(started))
  
  playedMins <- allplayers %>% #count number of players whose game this GW has started
    group_by(`Picked by`) %>%
    summarise(n=sum(`GW minutes`>0))

  leagueTable<-merge(overallTable,thisWeek,by="Player")
  leagueTable <- merge(leagueTable,playersStarted,by.y="Picked by",by.x="Player")
  leagueTable <- merge(leagueTable,playedMins,by.y="Picked by",by.x="Player")
  leagueTable <- leagueTable[,c(1,5,4,6,7)]
  names(leagueTable) <- c("Player","Total points","This GW","Teams played","Players played any mins")

  output$league <- DT::renderDataTable(datatable(leagueTable[with(leagueTable,order(-`Total points`)),],rownames=FALSE,options = list(scrollX = TRUE)))
  
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
  
  transfersDF <- data.frame(Player=c(),In=c(),Out=c(),Date=c())

  output$transfers <- DT::renderDataTable(datatable(transfersDF))
  
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
              ),tags$head(includeScript("www/google.js"))))

                        
shinyApp(ui = ui, server = server)
