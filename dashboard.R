list.of.packages <- c("shiny", "shinydashboard", "raster", "rgdal", "sp", "leaflet", "leaflet.extras", "geojsonio", "markdown", "ggplot2", "DT", "shinyWidgets")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(shinydashboard)
library(raster)
library(rgdal)
library(sp)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(markdown)
library(ggplot2)
library(dplyr)
library(DT)
library(shinyWidgets)

#fm.old <- geojsonio::geojson_read("data/fm_guides_r.geojson", what = "sp")
fm <- geojsonio::geojson_read("data/global_guides_r.geojson", what = "sp")
fg.data <- read.csv("data/FG_Category_Counry_cleaned_3.12.2019.csv")

ui <- fluidPage(
  setBackgroundColor("Gainsboro"),
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:#green;}"),#to change the side bar color
      h4(align = "center", "Click a desired country on the map for more information, or choose a country from the dropdown menu below."),
      selectInput(inputId = "variableselected", label = "Select country", 
                  choices = fm$admin),
      plotOutput("PieChart"),
      h2(align = "center",
         img(src = "logo.png", height = 70, width = 70),
         img(src = "shiny.png", height = 70, width = 120)),
      style = "background-color:white;"
    ),
    
    mainPanel(
      #tags$style(".well {background-color:red;}"),#to change the side bar color
        leafletOutput("map"),
        DT::dataTableOutput("link_list")
      )
    )
  )
  
server <- function(input, output, session) {
  
  #  output$selected_var <- renderText({ 
  #    #paste("You have selected", input$variableselected)
  #    paste(fg.data$guide_no)
  #  })
  
  output$map <- renderLeaflet({
    
    bins <- c(1, 5, 100, 300)
    pal <- colorBin("YlGn", domain = fm$guides_no, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong>", fm$admin) %>% lapply(htmltools::HTML)
    #"<strong>%s</strong><br/>No. of Guides: <strong>%g</strong><sup></sup><br>Link:<strong><a href = %f></a>GO</strong></br>",
    #     fm$admin, fm$guides_no, fm$guide_link) %>% lapply(htmltools::HTML)
    
    m <- leaflet(fm, height = "100%", options = leafletOptions(minZoom = 1, maxZoom = 5)) %>%
      setView(7, 17, 1.5) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Black & White")
    
    m %>% addPolygons(
      fillColor = ~pal(guides_no),
      weight = 1,
      opacity = 1,
      color = "black",
      fillOpacity = 1,
      group = "Field Guides",
      
      popup = paste("<strong>", fm$admin, "</strong> <br> No. of Guides: <strong>", fm$guides_no,
                    "</strong> <br> Link: <a href = ", fm$guide_link, "> Go to guides </a>"),
      
      highlight = highlightOptions(
        weight = 2,
        color = "Yellow",
        fillOpacity = 1,
        bringToFront = TRUE),
      
      label = labels,
      
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 6px"),
        textsize = "12px",
        direction = "auto")) %>% 
      
      addLayersControl(
        baseGroups = c("OSM", "Black & White"),
        overlayGroups = c("Field Guides"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      
      addLegend(pal = pal, values = ~guides_no, title = "Number of Field Guides",
                position = "bottomleft") %>%
      
      addResetMapButton() %>%
      
      addSearchFeatures(
        targetGroups = c("Field Guides"),
        options = searchFeaturesOptions(
          zoom=18, openPopup = FALSE, firstTipSubmit = FALSE,
          autoCollapse = TRUE, hideMarkerOnCollapse = FALSE ))
    
    ## Working on click event    
    #      observeEvent(input$mymap_marker_click, { 
    #        p <- input$mymap_marker_click  # typo was on this line
    #        print(p)
    #      })
  })
  output$PieChart<-renderPlot({
    fg.data %>%subset(Countries == input$variableselected) %>%
      select(Countries, Category) %>%
      group_by(Category) %>%
      summarise(count=n()) %>%
      mutate(Proportion = count/sum(count))%>%
      ggplot(aes(x="", y=Proportion, fill=Category))+ geom_bar(width = 1, stat = "identity")+ coord_polar("y", start=0)+
      scale_fill_manual(values = c("Plants" ="seagreen2", "Birds" = "mediumpurple3", "Fishes"="deepskyblue3", 
                                   "Herp" = "cyan3", "Insects" = "goldenrod2","Mammals" = "darkorange2",
                                   "Fungi"= "lightgoldenrod4", "Other" = "palevioletred2")) +
      theme_bw() + theme(legend.text=element_text( colour = "black", size=rel(1.2)))# +
      # theme(legend.title=element_text(size=15),
      #       panel.background = element_rect(fill = 'black', colour = 'black'),
      #       plot.background = element_rect(fill = "black"),
      #       legend.background = element_rect(fill="black", size=0.5, linetype="solid", colour ="grey"),
      #       panel.border = element_rect(colour = "black"))
          
  })
  
  output$link_list <- DT::renderDataTable({
    fg.data %>%subset(Countries == input$variableselected) %>%
      dplyr::select(-category,-country,-state, -language, -date_created) %>%
      dplyr::mutate(URL = paste0("https://fieldguides.fieldmuseum.org/guides/guide/", guide_no)) %>%
      dplyr::mutate(title = paste0("<a href='", URL, "'>",guide_title,"</a>")) %>% 
      dplyr::select(title, guide_no, page_no,Category) %>% 
      dplyr :: rename(Title = title, Guide_No = guide_no,Page_Number = page_no)
  }, escape = FALSE)
  
}

shinyApp(ui = ui, server = server)