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
withProgress(message = 'Loading...', value = 0,{
for(i in 1:nrow(players)){
  individual <- jsonlite::fromJSON(paste0("http://fantasy.premierleague.com/api/element-summary/",players[i,]$id,"/"))
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
  if("24.1" %in% names(player_with_scores)){player_with_scores$`24` <- player_with_scores$`24`+player_with_scores$`24.1`}
  if("25.1" %in% names(player_with_scores)){player_with_scores$`25` <- player_with_scores$`25`+player_with_scores$`25.1`}
  #player_with_scores<-player_with_scores[,!(names(player_with_scores) %in% c("7.1"))]}
  scorelist[[i]] <- player_with_scores #}
  incProgress(amount=1/nrow(players),detail=paste("Player: ",i,"/",nrow(players)))
}})

allplayers <- dplyr::bind_rows(scorelist)
gws <- as.character(seq(1,max(as.numeric(names(allplayers)),na.rm=T)))
#rem <- as.character(seq(30,38))
#gws <- gws[! gws %in% c(7)]
allplayers <- allplayers[,c("id","Name","Position","Team","GW points","GW minutes","Total points","News","Picked by","started",gws)]

##########################IN########################################OUT################################
allplayers[allplayers$id=='617',11:13] <- allplayers[allplayers$id=='387',11:13] #Evanilson 617 in for Shaw 387 pre GW4 Hodge
allplayers[allplayers$id=='177',11:13] <- allplayers[allplayers$id=='378',11:13] #Madueke 177 in for Mainoo 378 pre GW4 Warnes
allplayers[allplayers$id=='293',11:13] <- allplayers[allplayers$id=='341',11:13] #Hermansen 293 in for Ake 341 pre GW4 LP
allplayers[allplayers$id=='233',11:13] <- allplayers[allplayers$id=='565',11:13] #Onana 233 in for Sarabia 565 pre GW4 Tom
allplayers[allplayers$id=='248',11:13] <- allplayers[allplayers$id=='349',11:13] #Leno 248 in for Grealish 349 pre GW4 Hodge
allplayers[allplayers$id=='326',11:13] <- allplayers[allplayers$id=='108',11:13] #Konate 326 in for Toney 108 pre GW4 Warnes
allplayers[allplayers$id=='38',11:15] <- allplayers[allplayers$id=='360',11:15] #Duran 38 in for Rodri 360 pre GW6 Warnes
allplayers[allplayers$id=='38',11:21] <- allplayers[allplayers$id=='360',11:21] #Duran 38 in for Rodri 360 pre GW6 Warnes
allplayers[allplayers$id=='54',11:21] <- allplayers[allplayers$id=='45',11:21] #Rogers 54 in for Maatsen 45 pre gw 12 Tom
allplayers[allplayers$id=='148',11:21] <- allplayers[allplayers$id=='251',11:21] #Muniz 251 out for Welbeck 148 pre gw12 Warnes
allplayers[allplayers$id=='618',11:21] <- allplayers[allplayers$id=='550',11:21] #Rutter 618 in for Hwang 550 pre gw12 Hodge
allplayers[allplayers$id=='185',11:21] <- allplayers[allplayers$id=='513',11:21] #Sanchez 185 in for Areola 513 pre gw12 Hodge
allplayers[allplayers$id=='443',11:23] <- allplayers[allplayers$id=='508',11:23] #Vicario 508 out for Sels 443 pre gw14
allplayers[allplayers$id=='573',11:29] <- allplayers[allplayers$id=='79',11:29] #Senesi 79 out for Milenkovic 573 pre gw21
allplayers[allplayers$id=='399',11:29] <- allplayers[allplayers$id=='418',11:29] #Trippier 418 out for Hall 399 pre gw21
allplayers[allplayers$id=='252',11:29] <- allplayers[allplayers$id=='589',11:29] #Fullkrug 589 out for Jiminez 252 pre gw21
allplayers[allplayers$id=='268',11:29] <- allplayers[allplayers$id=='2',11:29] #Jesus 2 out for Delap 268 pre gw21
allplayers[allplayers$id=='232',11:32] <- allplayers[allplayers$id=='38',11:32] #Duran out for Ndiaye

out<-c('999','387','378','341','565','349','108','360','45','251','550','513','508','79','418','589','2','38')


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
  
  transfersDF <- data.frame(Player=c('Hodge','Luke','Warnes','Tom','Hodge','Warnes','Warnes','Tom','Warnes','Hodge','Hodge','Tom','Warnes','Tom','Tom','Luke'),
                            In=c('Evanilson','Hermansen','Madueke','Onana','Leno','Konate','Duran','Rogers','Welbeck','Rutter','Sanchez','Sels','Senesi','Trippier','Fullkrug','Jesus'),
                            Out=c('Shaw','Ake','Mainoo','Sarabia','Grealish','Toney','Rodri','Maatsen','Muniz','Hwang','Areola','Vicario','Milenkovic','Hall','Jiminez','Delap'),
                            BeforeGameweek=c('4','4','4','4','4','4','6','12','12','12','12','14','21','21','21','21'))

  output$transfers <- DT::renderDataTable(datatable(transfersDF,rownames= FALSE))
  
  
  orderDF <- data.frame(Pick=c('1','2','3','4'),
                            Player=c('Tom','Luke','Warnes','Hodge'))
  
  output$order <- DT::renderDataTable(datatable(orderDF,rownames= FALSE))
  
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
                menuItem("Player Scores",tabName="PlayerScores",icon=icon("futbol")),
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
                fluidRow(DT::dataTableOutput("transfers"),DT::dataTableOutput("order")))
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
