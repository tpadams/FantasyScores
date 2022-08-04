library(shiny)
library(shinydashboard)
library(XML)
library(xml2)
library(jsonlite)
library(DT)
library(rvest)

server<-shinyServer(function(input, output,session) {
  
  latLong <- reactive({
    req(input$url)
    url <- input$url
    webpage <- read_html(url)
    extracted <- webpage %>%
      html_nodes("script") %>% as.character()
    for(x in 1:length(extracted)){
      if(substr(extracted[x],1,35)=='<script type=\"application/ld+json\">'){
        json <- gsub("</script>","",substr(extracted[x],36,nchar(extracted[x])),fixed=T)
        break
      }
    }
    details <- fromJSON(json)
    geo<-details$`@graph`$geo
    geo <- geo[!is.na(geo$`@type`),]
    lat <- geo$latitude
    long <-geo$longitude
    data.frame(latitude=lat,longitude=long)
  })
  
  mapitCall <- eventReactive(input$submit,{
    req(latLong())
    result <- fromJSON(paste0("https://mapit.mysociety.org/point/4326/",latLong()$longitude,",",latLong()$latitude))
    for(x in 1:length(result)){
      if(result[[x]]$type=="WMC"){wpc <- result[[x]]$name}
      if(result[[x]]$type %in% c("DIW","LBW","MTW","UTW")){ward <- result[[x]]$name}
      if(result[[x]]$type %in% c("DIS","LBO","MTD","UTA")){council <- result[[x]]$name}
    }
    
    data.frame(Constituency=wpc,Council=council,Ward=ward)
    })
    
  output$geo <- DT::renderDataTable({
    req(mapitCall())
    datatable(mapitCall(),options=list(paging=FALSE,searching=FALSE),rownames = F)})
  
  })
  


ui<- dashboardPage(skin='purple',
                   dashboardHeader(title='Zoopla Ward'),
                   dashboardSidebar(collapsed=T,sidebarMenu(id="menu",
                   menuItem("Home",tabName="main",icon=icon("list-ol")))),
                   dashboardBody(
                     tabItems(
                       tabItem(tabName="main",
                               fluidRow(
                                 column(width=9,textInput("url","Enter Zoopla URL:",width="90%")),column(width=3,actionButton("submit", "Submit"))),
                               fluidRow(dataTableOutput("geo"))
                               
                               ))))

shinyApp(ui = ui, server = server)