library(shiny)
library(shinydashboard)
library(dplyr)
library(geosphere)
library(leaflet)
library(leaflet.extras)
library(readr)

data=read_csv(file="ships.csv")


mydrawPolylineOptions <- function (allowIntersection = TRUE, 
                                   drawError = list(color = "#b00b00", timeout = 2500), 
                                   guidelineDistance = 20, metric = TRUE, feet = FALSE, zIndexOffset = 2000, 
                                   shapeOptions = drawShapeOptions(fill = FALSE), repeatMode = FALSE) {
   leaflet::filterNULL(list(allowIntersection = allowIntersection, 
                            drawError = drawError, guidelineDistance = guidelineDistance, 
                            metric = metric, feet = feet, zIndexOffset = zIndexOffset,
                            shapeOptions = shapeOptions,  repeatMode = repeatMode)) }

ui=shinyUI(
   dashboardPage(
   dashboardHeader(title = "SHIPS VISUALISATION"),
   
   #dashboardSidebar(width = 0),
   
  dashboardSidebar(
      
      sidebarMenu(
         #unique(data$ship_type)
         selectInput("Ship_type", "Ship_type:",choices = unique(data$ship_type))
         
      )),
      dashboardBody(
         
         tabsetPanel(
            tabPanel("Map",
                    # column(12,box(height =200,width = 800, solidHeader = FALSE, status = "success",
                          #         uiOutput("menu"))),  
                     
                     column(12,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                                   leafletOutput("mymap"))),
                    column(12,box(height =400,width = 800, solidHeader = FALSE, status = "success",
                                  tableOutput("table")))
            
           
            )))
         ))
   
   
   
   
   server=shinyServer(function(input, output) {
      
      res <- reactive({
         data=data%>%filter(ship_type==input$Ship_type)
         d=vector(length=nrow(data))
         for (i in 1:nrow(data)){
            x1=data$LON[i]
            x2=data$LON[i+1]
            y1=data$LAT[i]
            y2=data$LAT[i+1]
            
            d[i]=sqrt((x2-x1)**2+(y2-y1)**2)
            #d[i]=distm (c(x1, y1), c(x2, y2), fun = distHaversine)
         }
         w=which(d==max(d,na.rm=T))
         max_dist=d[w[length(w)]]
         
         resu <- list(data = data, w = w,max_dist=max_dist)
         resu
      })
      
      output$mymap <- renderLeaflet({
        data=res()$data
        w=res()$w
        dist_max=res()$dist_max
        
         p1=paste(paste("Lon=",data$LON[w]),",",paste("Lat=",data$LAT[w]),sep="\n")
         p2=paste(paste("Lon=",data$LON[w+1]),",",paste("Lat=",data$LAT[w+1]),sep="\n")
         m <- leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=c(data$LON[w],data$LON[w+1]), lat=c(data$LAT[w],data$LAT[w+1]), popup=c(p1,p2),label=c("START of movement","END of movement"))%>%
            
            addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="kilometers")%>%
            addDrawToolbar(
               polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
               editOptions=editToolbarOptions(selectedPathOptions=selectedPathOptions())
            ) 
         
      })
      output$menu=renderUI({
         fluidPage(
            selectInput("Ship_type", "Ship_type:",unique(data$ship_type),"Cargo")
            
         )
      })
      output$table=renderTable({
         data=res()$data
         w=res()$w
         dist_max=res()$dist_max
         
         df=data[w:(w+1),]
         df$DATETIME=as.character(df$DATETIME)
         df
      })
   
   })
   
   shinyApp(ui = ui, server = server)
   