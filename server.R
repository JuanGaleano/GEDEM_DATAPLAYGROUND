library(shiny)
library(leaflet)
library(sp)
library(dplyr)
library(ggplot2)
library (googleVis)
library(plotly)

load("./data/CATALONIA14UTF.Rdata")
load("./data/CATFINAL2014.Rdata")
load("./data/CATSECC2014.Rdata")
load("./data/CAT_NAC15.Rdata")

x1$EXT<-NULL
x1 <-  x1[order(x1$YEAR),] 
x1$POP_TOTAL_EXT <- x1$POP_TOTAL-x1$POP_SPANISH
colnames (x1) <- c("IDMUN", "PROVINCE", "MUNICIPALITY","Census tracts","YEAR","[Pop]Spain",                
                          "[Pop]Latin America","[Pop]Western Europe","[Pop]Eastern Europe",          
                          "[Pop]Africa","[Pop]Asia","[Pop]Others","[Pop]Total Population","[Dissimilarity Index]Latin America",
                          "[Dissimilarity Index]Western Europe","[Dissimilarity Index]Eastern Europe","[Dissimilarity Index]Africa",
                          "[Dissimilarity Index]Asia","[Dissimilarity Index]Others","[Isolation Index]Latin America",
                          "[Isolation Index]Western Europe","[Isolation Index]Eastern Europe","[Isolation Index]Africa","[Isolation Index]Asia",
                          "[Isolation Index]Others","Population Size","COM","LON","LAT",
                          "Percentage_Foreign","Diversity", "[Pop]Total Foreign-born Population")       
x1$Percentage_Spanish <-100 -x1$Percentage_Foreign
x1$IDMUN <- as.character(x1$IDMUN)
x1$MUNICIPALITY2 <-x1$MUNICIPALITY 
x1$MUNICIPALITY3 <-x1$MUNICIPALITY 
x1$MUNICIPALITY4 <-x1$MUNICIPALITY 
x1$MUNICIPALITY5 <-x1$MUNICIPALITY 
x1$MUNICIPALITY6 <-x1$MUNICIPALITY 
x1<- x1[c(1:5,6:13,32,29,33,14:25,31,26:28,34:38,30)]

CATSECC2014@data$Prop<-as.factor(CATSECC2014@data$Prop)
CATSECC2014@data$Nom_Mun<-paste(as.character(CATSECC2014@data$Nom_Mun), sep="")
choices <- c("Select All", unique(levels(CATSECC2014@data$Prop)))
CATSECC2014@data$Nom_Mun1<-CATSECC2014@data$Nom_Mun




shinyServer(function(input, output,session) {  
  
  #### MOTION CHART ###
  
  output$ComposicionPlot <-renderGvis({ 
    Mun <- x1[x1$PROVINCE %in% input$PROVINCE, c(1:14,15:16,32,38)]
    gvisMotionChart(Mun, 
                    idvar = "MUNICIPALITY", 
                    time = "YEAR", 
                    xvar = "LON", 
                    yvar = "LAT",
                    colorvar = "IDMUN", 
                    sizevar = "Percentage_Foreign", 
                    #options= list (height= 500,
                     #              width= 850), 
                    chartid="GEDEM_2014")
  })
  
  #### DOWNLOAD DATA ###
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$PROVINCE, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[x1$PROVINCE %in% input$PROVINCE, c(1:14,15:16,32,38)], file, sep = sep,
                  row.names = FALSE)
    })
  
  
  ####maps
  
  output$mymap <- renderLeaflet({
    Munz <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun %in% input$Nom_Mun &
                                  CATSECC2014@data$Prop %in% input$Prop , ]  })
    
    observe({
      if ("Select All" %in% input$Prop) {
        # choose all the choices _except_ "Select All"
        selected_choices <- setdiff(choices, "Select All")
        updateSelectInput(session, "Prop", selected = selected_choices)
      }
    })
    
    df<- Munz()
    palette<-c("#D8D8D8","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A" ,"#E31A1C", "#B10026")
    pal <- colorFactor(palette, NULL, n = length(unique(df@data$MUN)))
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df$ID, 
                          "<br>",
                          "<br><strong>Total population: </strong>",
                          df$Totalpop, 
                          "<br><strong>Total foreign-born population: </strong>",
                          df$TotalExt, 
                          "<br><strong>Foreign-born (%): </strong>", 
                          round(df$PROPEXT,2) , 
                          "<br>",
                          "<br><strong>Population born in: </strong>",
                          "<br><strong>Spain: </strong>",
                          df$Spanish, 
                          "<br><strong>Latin-America: </strong>",
                          df$LatinAmerica, 
                          "<br><strong>Western Europe: </strong>",
                          df$WesternEurope, 
                          "<br><strong>Eastern Europe: </strong>",
                          df$EasternEurope, 
                          "<br><strong>Africa: </strong>",
                          df$Africa, 
                          "<br><strong>Asia: </strong>",
                          df$Asia,
                          "<br><strong>Other: </strong>",
                          df$Other)
    leaflet(df) %>%
      addProviderTiles("Esri.WorldImagery")%>%
      addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df$PropCOL), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Esri.WorldImagery"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal, values = ~Prop,
                title = "Foreign-born population (%)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  })
  observe({
    
    
    leafletProxy("mymap", data = df) %>%
      addLayersControl(
        baseGroups = c("OSM (default)",  "Esri.WorldImagery"))
    
    
    })
  
  #### DOWNLOAD DATA ###
  
  output$downloadDataz <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$Nom_Mun, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(CATSECC2014@data[CATSECC2014@data$Nom_Mun %in% input$Nom_Mun, c(1:18)], file, sep = sep,
                  row.names = FALSE)
    })
  
  
  ####maps2: LATIN AMERICA
  output$mymap1 <- renderLeaflet({
    Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
    
    
    #observe({
     # if ("Select All" %in% input$Prop) {
        # choose all the choices _except_ "Select All"
    #    selected_choices <- setdiff(choices, "Select All")
     #   updateSelectInput(session, "Prop", selected = selected_choices)
   #   }
  #  }) 
    
    df1<- Munz1() 
    df1$CL_LA<-with(df1@data,(LatinAmerica/Totalpop)/(sum(LatinAmerica)/sum(Totalpop)))
    
    df1@data$CL_LAI<-with(df1@data,ifelse(CL_LA<=0.5, "High Underreprentation",  
                                          ifelse((CL_LA>0.5&CL_LA<=0.8), "Underreprentation",  
                                                 ifelse((CL_LA>0.8&CL_LA<=1.2), "Average representation",
                                                        ifelse((CL_LA>1.2&CL_LA<=1.5), "Overrepresentation",
                                                               ifelse(CL_LA>1.5, "High overrepresentation",0)))))) 
    
    df1@data$CL_LAIC<-with(df1@data, ifelse(CL_LAI=="High Underreprentation", "#2b83ba", 
                                            ifelse(CL_LAI=="Underreprentation", "#abdda4", 
                                                   ifelse(CL_LAI=="Average representation", "#ffffbf",
                                                          ifelse(CL_LAI=="Overrepresentation", "#fdae61",
                                                                 ifelse(CL_LAI=="High overrepresentation","#d7191c",0)))))) 
    
    
    df1@data$CL_LAI<-factor(df1@data$CL_LAI, levels=c("High Underreprentation", "Underreprentation", "Average representation", 
                                                      "Overrepresentation", "High overrepresentation"))
    
    paleta1 <- c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c")
    pal1 <- colorFactor(paleta1, NULL, n = length(unique(df1@data$MUN)))
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df1$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df1$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df1$ID, 
                          "<br>",
                          "<br><strong>Category: </strong>", 
                          df1$CL_LAI,
                          "<br><strong>Location quotient: </strong>", 
                          round(df1$CL_LA,2),
                          "<br><strong>Latin-americans in census tract: </strong>",
                          df1$LatinAmerica,
                          "<br><strong>Total population in census tract: </strong>",
                          df1$Totalpop,
                          "<br><strong>Latin-americans in selected area: </strong>",
                          sum(df1$LatinAmerica),
                          "<br><strong>Total population in selected area: </strong>",
                          sum(df1$Totalpop))
    

    
    leaflet(df1) %>%
      addProviderTiles("CartoDB.Positron")%>%
      #addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df1$CL_LAIC), 
                  fillOpacity = 0.6, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal1, values = ~CL_LAI,
                title = "Location quotient (Categories)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  }) 
  observe({
    
    
    leafletProxy("mymap1", data = df) %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"))
    
    
  })
  
  ####maps2: WESTERN EUROPE
  output$mymap2 <- renderLeaflet({
    Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
    
    
    #observe({
    # if ("Select All" %in% input$Prop) {
    # choose all the choices _except_ "Select All"
    #    selected_choices <- setdiff(choices, "Select All")
    #   updateSelectInput(session, "Prop", selected = selected_choices)
    #   }
    #  }) 
    
    df1<- Munz1() 
    df1$CL_WE<-with(df1@data,(WesternEurope/Totalpop)/(sum(WesternEurope)/sum(Totalpop)))
    
    df1@data$CL_WEI<-with(df1@data,ifelse(CL_WE<=0.5, "High Underreprentation",  
                                   ifelse((CL_WE>0.5&CL_WE<=0.8), "Underreprentation",  
                                   ifelse((CL_WE>0.8&CL_WE<=1.2), "Average representation",
                                   ifelse((CL_WE>1.2&CL_WE<=1.5), "Overrepresentation",
                                            ifelse(CL_WE>1.5, "High overrepresentation",0)))))) 
    
    df1@data$CL_WEIC<-with(df1@data, ifelse(CL_WEI=="High Underreprentation", "#2b83ba", 
                                     ifelse(CL_WEI=="Underreprentation", "#abdda4", 
                                     ifelse(CL_WEI=="Average representation", "#ffffbf",
                                     ifelse(CL_WEI=="Overrepresentation", "#fdae61",
                                     ifelse(CL_WEI=="High overrepresentation","#d7191c",0)))))) 
    
    
    df1@data$CL_WEI<-factor(df1@data$CL_WEI, levels=c("High Underreprentation", "Underreprentation", "Average representation", 
                                                    "Overrepresentation", "High overrepresentation"))
    
    paleta1 <- c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c")
    pal1 <- colorFactor(paleta1, NULL, n = length(unique(df1@data$MUN)))
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df1$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df1$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df1$ID, 
                          "<br>",
                          "<br><strong>Category: </strong>", 
                          df1$CL_WEI,
                          "<br><strong>Location quotient: </strong>", 
                          round(df1$CL_WE,2),
                          "<br><strong>Western Europeans in census tract: </strong>",
                          df1$WesternEurope,
                          "<br><strong>Total population in census tract: </strong>",
                          df1$Totalpop,
                          "<br><strong>Western Europeans in selected area: </strong>",
                          sum(df1$WesternEurope),
                          "<br><strong>Total population in selected area: </strong>",
                          sum(df1$Totalpop))
    
    
    
    leaflet(df1) %>%
      addProviderTiles("CartoDB.Positron")%>%
     # addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df1$CL_WEIC), 
                  fillOpacity = 0.6, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal1, values = ~CL_WEI,
                title = "Location quotient (Categories)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  }) 
  observe({
    
    
    leafletProxy("mymap2", data = df) %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"))
    
    
  })
  
  ####maps2: EASTERN EUROPE
  output$mymap3 <- renderLeaflet({
    Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
    
    
    #observe({
    # if ("Select All" %in% input$Prop) {
    # choose all the choices _except_ "Select All"
    #    selected_choices <- setdiff(choices, "Select All")
    #   updateSelectInput(session, "Prop", selected = selected_choices)
    #   }
    #  }) 
    
    df1<- Munz1() 
    df1$CL_EE<-with(df1@data,(EasternEurope/Totalpop)/(sum(EasternEurope)/sum(Totalpop)))
    
    df1@data$CL_EEI<-with(df1@data,ifelse(CL_EE<=0.5, "High Underreprentation",  
                                          ifelse((CL_EE>0.5&CL_EE<=0.8), "Underreprentation",  
                                                 ifelse((CL_EE>0.8&CL_EE<=1.2), "Average representation",
                                                        ifelse((CL_EE>1.2&CL_EE<=1.5), "Overrepresentation",
                                                               ifelse(CL_EE>1.5, "High overrepresentation",0)))))) 
    
    df1@data$CL_EEIC<-with(df1@data, ifelse(CL_EEI=="High Underreprentation", "#2b83ba", 
                                            ifelse(CL_EEI=="Underreprentation", "#abdda4", 
                                                   ifelse(CL_EEI=="Average representation", "#ffffbf",
                                                          ifelse(CL_EEI=="Overrepresentation", "#fdae61",
                                                                 ifelse(CL_EEI=="High overrepresentation","#d7191c",0)))))) 
    
    
    df1@data$CL_EEI<-factor(df1@data$CL_EEI, levels=c("High Underreprentation", "Underreprentation", "Average representation", 
                                                      "Overrepresentation", "High overrepresentation"))
    
    paleta1 <- c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c")
    pal1 <- colorFactor(paleta1, NULL, n = length(unique(df1@data$MUN)))
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df1$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df1$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df1$ID, 
                          "<br>",
                          "<br><strong>Category: </strong>", 
                          df1$CL_EEI,
                          "<br><strong>Location quotient: </strong>", 
                          round(df1$CL_EE,2),
                          "<br><strong>Eastern Europeans in census tract: </strong>",
                          df1$EasternEurope,
                          "<br><strong>Total population in census tract: </strong>",
                          df1$Totalpop,
                          "<br><strong>Eastern Europeans in selected area: </strong>",
                          sum(df1$EasternEurope),
                          "<br><strong>Total population in selected area: </strong>",
                          sum(df1$Totalpop))
    
    
    
    leaflet(df1) %>%
      addProviderTiles("CartoDB.Positron")%>%
      # addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df1$CL_EEIC), 
                  fillOpacity = 0.6, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal1, values = ~CL_EEI,
                title = "Location quotient (Categories)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  }) 
  observe({
    
    
    leafletProxy("mymap3", data = df) %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"))
    
    
  })
  
  ####maps2: AFRICA
  output$mymap4 <- renderLeaflet({
    Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
    
    
    #observe({
    # if ("Select All" %in% input$Prop) {
    # choose all the choices _except_ "Select All"
    #    selected_choices <- setdiff(choices, "Select All")
    #   updateSelectInput(session, "Prop", selected = selected_choices)
    #   }
    #  }) 
    
    df1<- Munz1() 
    df1$CL_AF<-with(df1@data,(Africa/Totalpop)/(sum(Africa)/sum(Totalpop)))
    
    df1@data$CL_AFI<-with(df1@data,ifelse(CL_AF<=0.5, "High Underreprentation",  
                                          ifelse((CL_AF>0.5&CL_AF<=0.8), "Underreprentation",  
                                                 ifelse((CL_AF>0.8&CL_AF<=1.2), "Average representation",
                                                        ifelse((CL_AF>1.2&CL_AF<=1.5), "Overrepresentation",
                                                               ifelse(CL_AF>1.5, "High overrepresentation",0)))))) 
    
    df1@data$CL_AFIC<-with(df1@data, ifelse(CL_AFI=="High Underreprentation", "#2b83ba", 
                                            ifelse(CL_AFI=="Underreprentation", "#abdda4", 
                                                   ifelse(CL_AFI=="Average representation", "#ffffbf",
                                                          ifelse(CL_AFI=="Overrepresentation", "#fdae61",
                                                                 ifelse(CL_AFI=="High overrepresentation","#d7191c",0)))))) 
    
    
    df1@data$CL_AFI<-factor(df1@data$CL_AFI, levels=c("High Underreprentation", "Underreprentation", "Average representation", 
                                                      "Overrepresentation", "High overrepresentation"))
    
    paleta1 <- c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c")
    pal1 <- colorFactor(paleta1, NULL, n = length(unique(df1@data$MUN)))
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df1$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df1$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df1$ID, 
                          "<br>",
                          "<br><strong>Category: </strong>", 
                          df1$CL_AFI,
                          "<br><strong>Location quotient: </strong>", 
                          round(df1$CL_AF,2),
                          "<br><strong>Africans in census tract: </strong>",
                          df1$Africa,
                          "<br><strong>Total population in census tract: </strong>",
                          df1$Totalpop,
                          "<br><strong>Africans in selected area: </strong>",
                          sum(df1$Africa),
                          "<br><strong>Total population in selected area: </strong>",
                          sum(df1$Totalpop))
    
    
    
    leaflet(df1) %>%
      addProviderTiles("CartoDB.Positron")%>%
      # addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df1$CL_AFIC), 
                  fillOpacity = 0.6, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal1, values = ~CL_AFI,
                title = "Location quotient (Categories)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  }) 
  observe({
    
    
    leafletProxy("mymap4", data = df) %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"))
    
    
  })
  
  ####maps2: ASIA
  output$mymap5 <- renderLeaflet({
    Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
    
    
    #observe({
    # if ("Select All" %in% input$Prop) {
    # choose all the choices _except_ "Select All"
    #    selected_choices <- setdiff(choices, "Select All")
    #   updateSelectInput(session, "Prop", selected = selected_choices)
    #   }
    #  }) 
    
    df1<- Munz1() 
    df1$CL_AS<-with(df1@data,(Asia/Totalpop)/(sum(Asia)/sum(Totalpop)))
    
    df1@data$CL_ASI<-with(df1@data,ifelse(CL_AS<=0.5, "High Underreprentation",  
                                          ifelse((CL_AS>0.5&CL_AS<=0.8), "Underreprentation",  
                                                 ifelse((CL_AS>0.8&CL_AS<=1.2), "Average representation",
                                                        ifelse((CL_AS>1.2&CL_AS<=1.5), "Overrepresentation",
                                                               ifelse(CL_AS>1.5, "High overrepresentation",0)))))) 
    
    df1@data$CL_ASIC<-with(df1@data, ifelse(CL_ASI=="High Underreprentation", "#2b83ba", 
                                            ifelse(CL_ASI=="Underreprentation", "#abdda4", 
                                                   ifelse(CL_ASI=="Average representation", "#ffffbf",
                                                          ifelse(CL_ASI=="Overrepresentation", "#fdae61",
                                                                 ifelse(CL_ASI=="High overrepresentation","#d7191c",0)))))) 
    
    
    df1@data$CL_ASI<-factor(df1@data$CL_ASI, levels=c("High Underreprentation", "Underreprentation", "Average representation", 
                                                      "Overrepresentation", "High overrepresentation"))
    
    paleta1 <- c("#2b83ba", "#abdda4","#ffffbf","#fdae61","#d7191c")
    pal1 <- colorFactor(paleta1, NULL, n = length(unique(df1@data$MUN)))
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df1$Nom_Mun, 
                          "<br><strong>Municipal ID: </strong>", 
                          df1$MUN, 
                          "<br><strong>Census tract ID: </strong>", 
                          df1$ID, 
                          "<br>",
                          "<br><strong>Category: </strong>", 
                          df1$CL_ASI,
                          "<br><strong>Location quotient: </strong>", 
                          round(df1$CL_AS,2),
                          "<br><strong>Asians in census tract: </strong>",
                          df1$Asia,
                          "<br><strong>Total population in census tract: </strong>",
                          df1$Totalpop,
                          "<br><strong>Asians in selected area: </strong>",
                          sum(df1$Asia),
                          "<br><strong>Total population in selected area: </strong>",
                          sum(df1$Totalpop))
    
    
    
    leaflet(df1) %>%
      addProviderTiles("CartoDB.Positron")%>%
      # addTiles(group = "OSM (default)") %>%
      addPolygons(fillColor = (df1$CL_ASIC), 
                  fillOpacity = 0.6, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = state_popup)%>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"),
        #overlayGroups = c("geo_label"),
        options = layersControlOptions(collapsed = FALSE))%>%
      addLegend("bottomright", pal = pal1, values = ~CL_ASI,
                title = "Location quotient (Categories)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1)
  }) 
  observe({
    
    
    leafletProxy("mymap5", data = df) %>%
      addLayersControl(
        baseGroups = c("CartoDB.Positron"))
    
    
  })
  
  
  #### DOWNLOAD DATA ###
  
  output$downloadDataz1 <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Your data by GEDEM", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      Munz1 <- reactive({CATSECC2014[CATSECC2014@data$Nom_Mun1 %in% input$Nom_Mun1,  ]})
      df1<- Munz1() 
      df1$CL_LA<-with(df1@data,(LatinAmerica/Totalpop)/(sum(LatinAmerica)/sum(Totalpop)))
      df1$CL_WE<-with(df1@data,(WesternEurope/Totalpop)/(sum(WesternEurope)/sum(Totalpop)))
      df1$CL_EE<-with(df1@data,(EasternEurope/Totalpop)/(sum(EasternEurope)/sum(Totalpop)))
      df1$CL_AF<-with(df1@data,(Africa/Totalpop)/(sum(Africa)/sum(Totalpop)))
      df1$CL_AS<-with(df1@data,(Asia/Totalpop)/(sum(Asia)/sum(Totalpop)))
      
      df1@data$CL_LAI<-with(df1@data,ifelse(CL_LA<=0.5, "High Underreprentation",  
                                            ifelse((CL_LA>0.5&CL_LA<=0.8), "Underreprentation",  
                                                   ifelse((CL_LA>0.8&CL_LA<=1.2), "Average representation",
                                                          ifelse((CL_LA>1.2&CL_LA<=1.5), "Overrepresentation",
                                                                 ifelse(CL_LA>1.5, "High overrepresentation",0)))))) 
      df1@data$CL_WEI<-with(df1@data,ifelse(CL_WE<=0.5, "High Underreprentation",  
                                            ifelse((CL_WE>0.5&CL_WE<=0.8), "Underreprentation",  
                                                   ifelse((CL_WE>0.8&CL_WE<=1.2), "Average representation",
                                                          ifelse((CL_WE>1.2&CL_WE<=1.5), "Overrepresentation",
                                                                 ifelse(CL_WE>1.5, "High overrepresentation",0)))))) 
      df1@data$CL_EEI<-with(df1@data,ifelse(CL_EE<=0.5, "High Underreprentation",  
                                            ifelse((CL_EE>0.5&CL_EE<=0.8), "Underreprentation",  
                                                   ifelse((CL_EE>0.8&CL_EE<=1.2), "Average representation",
                                                          ifelse((CL_EE>1.2&CL_EE<=1.5), "Overrepresentation",
                                                                 ifelse(CL_EE>1.5, "High overrepresentation",0))))))
      df1@data$CL_AFI<-with(df1@data,ifelse(CL_AF<=0.5, "High Underreprentation",  
                                            ifelse((CL_AF>0.5&CL_AF<=0.8), "Underreprentation",  
                                                   ifelse((CL_AF>0.8&CL_AF<=1.2), "Average representation",
                                                          ifelse((CL_AF>1.2&CL_AF<=1.5), "Overrepresentation",
                                                                 ifelse(CL_AF>1.5, "High overrepresentation",0)))))) 
      df1@data$CL_ASI<-with(df1@data,ifelse(CL_AS<=0.5, "High Underreprentation",  
                                            ifelse((CL_AS>0.5&CL_AS<=0.8), "Underreprentation",  
                                                   ifelse((CL_AS>0.8&CL_AS<=1.2), "Average representation",
                                                          ifelse((CL_AS>1.2&CL_AS<=1.5), "Overrepresentation",
                                                                 ifelse(CL_AS>1.5, "High overrepresentation",0)))))) 
      # Write to a file specified by the 'file' argument
      write.table(df1@data[df1@data$Nom_Mun1 %in% input$Nom_Mun1, c(1:17,21:30)], file, sep = sep,
                  row.names = FALSE)
    })
  #### POPULATION COMPOSITION PLOTS ###
  
  output$ComposicionPlot2 <-renderGvis({ 
    Mun2 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun2 <-  Mun2[order(Mun2$YEAR),] 
    gvisColumnChart(Mun2, xvar="YEAR", yvar=c("[Pop]Spain", "[Pop]Total Foreign-born Population" ),
                    options=list(height= 650,
                                 width= 1200,
                                 backgroundColor='white',
                                 chartArea.backgroundColor="red",
                                 legend="{position: 'right', 
                                 textStyle: {color: 'blue', fontSize: 12}}",
                                 legend.alignment="center",
                                 title= paste(paste(input$MUNICIPALITY," ", "(",Mun2$PROVINCE,")",sep=""), 
                                              "Population composition in absolute terms", sep=": "),
                                 hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",
                                 vAxis= "{title: 'Population',
                                 format:'#', 
                                 titleTextStyle: {color: 'black',fontName:'Courier'}}",
                                 isStacked=TRUE))
    })
  
  output$ComposicionPlot3 <-renderGvis({ 
    Mun3 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,]
    Mun3 <-  Mun3[order(Mun3$YEAR),] 
    Mun3$Percentage_Spanish <-  round(Mun3$Percentage_Spanish, digits=2) 
    Mun3$Percentage_Foreign <-  round(Mun3$Percentage_Foreign, digits=2)
    gvisColumnChart(Mun3, xvar="YEAR", yvar=c("Percentage_Spanish", "Percentage_Foreign"),
                    options=list(height= 650,
                                 width= 1200,
                                 backgroundColor='white',
                                 chartArea.backgroundColor="red",
                                 legend="{position: 'right', 
                                 textStyle: {color: 'blue', fontSize: 12}}",
                                 legend.alignment="center",
                                 title= paste(paste(input$MUNICIPALITY," ", "(",Mun3$PROVINCE,")",sep=""),"Population composition in relative terms", sep=": "),
                                 hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",
                                 vAxis= "{title: 'Percentage',
                                 format:'#', 
                                 titleTextStyle: {color: 'black',fontName:'Courier'}}",
                                 isStacked=TRUE))
    })
  
  #### DOWNLOAD DATA ###
  
  output$downloadData3 <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$MUNICIPALITY, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,
                            c("YEAR", "[Pop]Spain", "[Pop]Total Foreign-born Population", "Percentage_Spanish", "Percentage_Foreign") ], 
                  file, sep = sep,row.names = FALSE)
    })
  
  #### DOWNLOAD IMAGE STACKED BARS 
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {paste("Stacked bar plot1", paste(input$MUNICIPALITY, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun3 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,]
      Mun3 <-  Mun3[order(Mun3$YEAR),] 
      Munplot <-data.frame(YEAR=rep(2000:2015,2),
                           POP=c(Mun3[1:16,6],Mun3[1:16,14]),
                           ORIGEN=c(rep("[Pop]Spain", 16),rep("[Pop]Total Foreig-born Population", 16)))
      print(ggplot(data=Munplot, aes(x=YEAR, y=POP, fill=ORIGEN)) + geom_bar(stat="identity", colour="black")+
              scale_x_continuous(breaks=seq(2000,2015, 1))+
              ggtitle(paste(paste(input$MUNICIPALITY," ", "(",Mun3$PROVINCE,")",sep=""),"Population composition", sep=": "))+
              scale_fill_manual(values=c("#3366cc", "#dc3912"),
                                breaks=c("[Pop]Spain", "[Pop]Total Foreig-born Population"),
                                labels=c("Spanish-born", "Foreign-born"))+
              theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 20),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FFFFFF"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 90,vjust=0.5, size=20,colour="black",face="bold"),
                    axis.title.y = element_text(face="bold", colour="black", size=20),
                    axis.text.y  = element_text( vjust=0.5, size=20,colour="black",face="bold"),
                    plot.background = element_rect(fill = "#FFFFFF"))+
              ylab("Population"))
      dev.off()
    })
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() {paste("Stacked bar plot2", paste(input$MUNICIPALITY, '.png', sep=''), sep=" ") },
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun3 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,]
      Mun3 <-  Mun3[order(Mun3$YEAR),] 
      Munplot2 <-data.frame(YEAR=rep(2000:2015,2),
                            POP=c(Mun3[1:16,16],Mun3[1:16,38]),
                            ORIGEN=c(rep("[Pop]Spain", 16),rep("[Pop]Total Foreig-born Population", 16)))
      print(ggplot(data=Munplot2, aes(x=YEAR, y=POP, fill=ORIGEN)) + geom_bar(stat="identity", colour="black")+
              scale_x_continuous(breaks=seq(2000,2015, 1))+
              ggtitle(paste(paste(input$MUNICIPALITY," ", "(",Mun3$PROVINCE,")",sep=""),"Population composition", sep=": "))+
              scale_fill_manual(values=c("#3366cc", "#dc3912"),
                                breaks=c("[Pop]Spain", "[Pop]Total Foreig-born Population"),
                                labels=c("Spanish-born", "Foreign-born"))+
              theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 20),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FFFFFF"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 90,vjust=0.5, size=20,colour="black",face="bold"),
                    axis.title.y = element_text(face="bold", colour="black", size=20),
                    axis.text.y  = element_text( vjust=0.5, size=20,colour="black",face="bold"),
                    plot.background = element_rect(fill = "#FFFFFF"))+
              ylab("Percentage (%)"))
      dev.off()
    })
  
  #### PIE CHARTS FOREIGN BORN POP
  
  output$ComposicionPlot6 <-renderGvis({ 
    Mun6 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun6<- data.frame(t(Mun6[(Mun6$YEAR==2000),7:12]))
    Mun6<- cbind(ORIGIN = rownames(Mun6), Mun6) 
    gvisPieChart(Mun6,
                 options=list(height= 400,
                              width= 770,
                              backgroundColor='white',
                              chartArea.backgroundColor="red",
                              legend="{position: 'bottom', 
                              textStyle: {color: 'blue', fontSize: 12}}",
                              legend.alignment="center",
                              title= paste(paste(input$MUNICIPALITY,"Foreign-born population composition year 2000", sep=": "),
                                           paste("\nTotal Foreign-born population", sum(Mun6[,2]), sep=": "),sep=". "),
                              vAxis= "{title: 'Population',
                              format:'#', 
                              titleTextStyle: {color: 'black',fontName:'Courier'}}"))
    })
  
  output$ComposicionPlot7 <-renderGvis({ 
    Mun7 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun7<- data.frame(t(Mun7[(Mun7$YEAR==2015),7:12]))
    Mun7<- cbind(ORIGIN = rownames(Mun7), Mun7) 
    gvisPieChart(Mun7,
                 options=list(height= 400,
                              width= 770,
                              backgroundColor='white',
                              chartArea.backgroundColor="red",
                              legend="{position: 'bottom', 
                              textStyle: {color: 'blue', fontSize: 12}}",
                              legend.alignment="center",
                              title= paste(paste(input$MUNICIPALITY,"Foreign-born population composition year 2015", sep=": "),
                                           paste("\nTotal Foreign-born population", sum(Mun7[,2]), sep=": "),sep=". "),
                              vAxis= "{title: 'Population',
                              format:'#', 
                              titleTextStyle: {color: 'black',fontName:'Courier'}}"))
  })
  
  
  ########################################
  #ranking pasises de nacimiento
  ######################################
  
  output$ComposicionPlotly2 <-renderPlotly({ 
    Munplotly2<-CAT_NAC15[CAT_NAC15$MUNICIPALITY %in% input$MUNICIPALITY, ]
    
    y2 <- list(
      title = "Population"
    )
    x2 <- list(
      title = ""
    )
    
    p <- plot_ly(
      x =Munplotly2$Nom_pais,
      y = Munplotly2$n,
      name = "SF Zoo",
      type = "bar")%>%layout( yaxis=y2,xaxis=x2, showlegend=F)
    p
  }) 
  
  
  #### RESIDENTIAL SEGREGATION PLOTS ###
  
  output$ComposicionPlot4 <-renderGvis({ 
    Mun4 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun4 <-  Mun4[order(Mun4$YEAR),] 
    Mun4 <- cbind(Mun4[,1:16,],round(Mun4[,17:22], digits=2))
    gvisLineChart(Mun4, "YEAR", c("[Dissimilarity Index]Latin America",
                                  "[Dissimilarity Index]Western Europe",
                                  "[Dissimilarity Index]Eastern Europe",
                                  "[Dissimilarity Index]Africa",
                                  "[Dissimilarity Index]Asia"),
                  options=list(height= 650,
                               width= 1200,
                               backgroundColor='white',
                               chartArea.backgroundColor="red",
                               legend="{position: 'right', 
                               textStyle: {color: 'blue', fontSize: 12}}",
                               legend.alignment="center",
                               title= paste(paste(input$MUNICIPALITY2," ", "(",Mun4$PROVINCE,")",sep=""),"Dissimilarity Index  for different groups. \nReference group: population born in Spain", sep=": "),
                               hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",
                               vAxis= "{title: 'Dissimilarity Index',
                               format:'#', 
                               titleTextStyle: {color: 'black',fontName:'Courier'}}"))
                                 })
  output$ComposicionPlot5 <-renderGvis({ 
    Mun5 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun5 <-  Mun5[order(Mun5$YEAR),] 
    Mun5 <- cbind(Mun5[,1:22,],round(Mun5[,23:28], digits=2))
    gvisLineChart(Mun5, "YEAR", c("[Isolation Index]Latin America",
                                  "[Isolation Index]Western Europe",
                                  "[Isolation Index]Eastern Europe",
                                  "[Isolation Index]Africa",
                                  "[Isolation Index]Asia"),
                  options=list(height= 650,
                               width= 1200,
                               backgroundColor='white',
                               chartArea.backgroundColor="red",
                               legend="{position: 'right', 
                               textStyle: {color: 'blue', fontSize: 12}}",
                               legend.alignment="center",
                               title= paste(paste(input$MUNICIPALITY2," ", "(",Mun5$PROVINCE,")",sep=""),"Isolation Index for different groups", sep=": "),
                               hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",                               
                               vAxis= "{title: 'Isolation Index',
                               format:'#', 
                               titleTextStyle: {color: 'black',fontName:'Courier'}}"))
    
})
  
  output$ComposicionPlot8 <-renderGvis({ 
    Mun8 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun8 <-  Mun8[order(Mun8$YEAR),] 
    gvisLineChart(Mun8, "YEAR", c("Census tracts"),
                  options=list(height= 650,
                               width= 1200,
                               backgroundColor='white',
                               chartArea.backgroundColor="red",
                               legend="{position: 'right', 
                               textStyle: {color: 'blue', fontSize: 12}}",
                               legend.alignment="center",
                               title= paste(paste(input$MUNICIPALITY2," ", "(",Mun8$PROVINCE,")",sep=""),"Number of Census Tract", sep=": "),
                               hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",
                               vAxis= "{title: 'Census tracts',
                               format:'#', 
                               titleTextStyle: {color: 'black',fontName:'Courier'}}"))
    
    })
  
  
  #### DOWNLOAD DATA RESIDENTIAL SEGREGATION ###
  
  output$downloadData4 <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$MUNICIPALITY2, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2,
                            c("YEAR", "Census tracts", "[Dissimilarity Index]Latin America",
                              "[Dissimilarity Index]Western Europe","[Dissimilarity Index]Eastern Europe","[Dissimilarity Index]Africa",
                              "[Dissimilarity Index]Asia","[Isolation Index]Latin America",
                              "[Isolation Index]Western Europe","[Isolation Index]Eastern Europe","[Isolation Index]Africa","[Isolation Index]Asia") ], 
                  file, sep = sep,row.names = FALSE)
    })
  #### DOWNLOAD IMAGE STACKED BARS 
  
  output$downloadPlotDIS <- downloadHandler(
    filename = function() {paste("Dissimilarity plot", paste(input$MUNICIPALITY2, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun4 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
      Mun4 <-  Mun4[order(Mun4$YEAR),] 
      MunDIS <-data.frame(YEAR=rep(2000:2015,5),
                          Dissimilarity=c(Mun4[1:16,17],Mun4[1:16,18],
                                          Mun4[1:16,19],Mun4[1:16,20],
                                          Mun4[1:16,21]),
                          Origin=c(rep("[Dissimilarity Index]Latin America", 16),
                                   rep("[Dissimilarity Index]Western Europe", 16),
                                   rep("[Dissimilarity Index]Eastern Europe", 16),
                                   rep("[Dissimilarity Index]Africa", 16),
                                   rep("[Dissimilarity Index]Asia", 16)))
      MunDIS$Origin <- factor(MunDIS$Origin, 
                              levels = c("[Dissimilarity Index]Latin America",
                                         "[Dissimilarity Index]Western Europe", 
                                         "[Dissimilarity Index]Eastern Europe",
                                         "[Dissimilarity Index]Africa",
                                         "[Dissimilarity Index]Asia"))
      
      print(ggplot(MunDIS, aes(YEAR, Dissimilarity, group = Origin, colour = Origin)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(0, 80))+
              scale_x_continuous(breaks=seq(2000,2015, 1))+
              ggtitle((paste(paste(input$MUNICIPALITY2," ", "(",Mun4$PROVINCE,")",sep=""),"Residential Segregation", sep=": ")))+
              scale_colour_manual(values = c("#3366cc","#dc3912","#ff9900","#109618","#990099"),
                                  breaks =levels(MunDIS$Origin), 
                                  labels= c("Latin-America", "Western Europe", "Eastern Europe", "Africa", "Asia"))+
              theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 20),
                    legend.position=c("bottom"),
                    legend.background = element_rect(fill="#FFFFFF"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 90,vjust=0.5, size=20,colour="black",face="bold"),
                    axis.title.y = element_text(face="bold", colour="black", size=20),
                    axis.text.y  = element_text( vjust=0.5, size=20,colour="black",face="bold"),
                    plot.background = element_rect(fill = "#FFFFFF"))+
              ylab("Dissimilarity Index"))
      dev.off()
    })
  output$downloadPlotIS <- downloadHandler(
    filename = function() {paste("Isolation plot", paste(input$MUNICIPALITY2, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun5 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
      Mun5 <-  Mun5[order(Mun5$YEAR),] 
      MunIS <-data.frame(YEAR=rep(2000:2015,5),
                         Isolation=c(Mun5[1:16,23],Mun5[1:16,24],
                                     Mun5[1:16,25],Mun5[1:16,26],
                                     Mun5[1:16,27]),
                         Origin=c(rep("[Isolation Index]Latin America", 16),
                                  rep("[Isolation Index]Western Europe", 16),
                                  rep("[Isolation Index]Eastern Europe", 16),
                                  rep("[Isolation Index]Africa", 16),
                                  rep("[Isolation Index]Asia", 16)))
      MunIS$Origin <- factor(MunIS$Origin, 
                             levels = c("[Isolation Index]Latin America",
                                        "[Isolation Index]Western Europe", 
                                        "[Isolation Index]Eastern Europe",
                                        "[Isolation Index]Africa",
                                        "[Isolation Index]Asia"))
      
      print(ggplot(MunIS, aes(YEAR, Isolation, group = Origin, colour = Origin)) +
              geom_path(alpha = 1, size=1.2)+
              #scale_y_continuous(limits=c(0, 50))+
              scale_x_continuous(breaks=seq(2000,2015, 1))+
              ggtitle((paste(paste(input$MUNICIPALITY2," ", "(",Mun5$PROVINCE,")",sep=""),"Residential Segregation", sep=": ")))+
              scale_colour_manual(values = c("#3366cc","#dc3912","#ff9900","#109618","#990099"),
                                  breaks =levels(MunIS$Origin), 
                                  labels= c("Latin-America", "Western Europe", "Eastern Europe", "Africa", "Asia"))+
              theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 20),
                    legend.position=c("bottom"),
                    legend.background = element_rect(fill="#FFFFFF"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 90,vjust=0.5, size=20,colour="black",face="bold"),
                    axis.title.y = element_text(face="bold", colour="black", size=20),
                    axis.text.y  = element_text( vjust=0.5, size=20,colour="black",face="bold"),
                    plot.background = element_rect(fill = "#FFFFFF"))+
              ylab("Isolation Index"))
      dev.off()
    })
  #### POPULATION DIVERSITY PLOTS ###
  
  output$ComposicionPlot9 <-renderGvis({ 
    
    Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(5,29)]
    Mun9 <-  Mun9[order(Mun9$YEAR),] 
    colnames(Mun9) <- c("YEAR",input$MUNICIPALITY3 )
    
    Mun10 <- x1[x1$MUNICIPALITY4 %in% input$MUNICIPALITY4,c(5,29) ]
    Mun10 <-  Mun10[order(Mun10$YEAR),]
    colnames(Mun10) <- c("YEAR",input$MUNICIPALITY4)
    
    Mun11 <- x1[x1$MUNICIPALITY5 %in% input$MUNICIPALITY5,c(5,29) ]
    Mun11 <-  Mun11[order(Mun11$YEAR),]
    colnames(Mun11) <- c("YEAR",input$MUNICIPALITY5)
    
    Mun14 <- x1 %>% group_by(YEAR) %>% summarise(diversity = mean(Diversity))
    colnames(Mun14) <- c("YEAR","Mean Diversity Catalonia")
    
    Mun12 <- merge(Mun9, Mun10, by="YEAR")
    Mun13 <- merge(Mun12, Mun11, by="YEAR")
    Mun15 <- merge(Mun14, Mun13, by="YEAR")
    gvisLineChart(Mun15, "YEAR", c(colnames(Mun15[2:5])),
                  options=list(height= 650,
                               width= 1200,
                               backgroundColor='white',
                               chartArea.backgroundColor="red",
                               legend="{position: 'bottom', 
                               textStyle: {color: 'blue', fontSize: 12}}",
                               legend.alignment="center",
                               title="Simpson Diversity Index",
                               hAxis= "{ticks: [2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015] }",
                               vAxis= "{title: 'Simpson Diversity Index (over 6 population groups)',
                               format:'#.###', 
                               titleTextStyle: {color: 'black',fontName:'Courier'}}"))
    
    })
  
  
  
  #### POPULATION PYRAMID PLOTS ###
  
  
  output$downloadDataPy <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(paste("Catalonia",input$year,sep=" "), input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(CATFINAL2014[CATFINAL2014$year %in% input$year, ], file, sep = sep, row.names = FALSE)
    })
  
  output$pyramidrel <- renderPlot({
    
    CATFINAL2014<-CATFINAL2014[CATFINAL2014$year %in% input$year, ]
    
    EXT<-CATFINAL2014[,c(1,3:5)]
    EXT$Sex<- paste("Foreign-born", EXT$Sex, sep=" ")
    colnames(EXT)<- c("n", "year", "Sex", "Age")
    EXT$n <-with(EXT, ifelse(Sex=="Foreign-born Males", n*-1,n))
    
    ESP<-CATFINAL2014[,c(2,3:5)]
    ESP$Sex<- paste("Spanish-born", ESP$Sex, sep=" ")
    colnames(ESP)<- c("n", "year", "Sex", "Age")
    ESP$n <-with(ESP, ifelse(Sex=="Spanish-born Males", n*-1,n))
    PIR<- rbind(ESP,EXT)
    PIR$Sex <- factor(PIR$Sex,
                      levels = c("Foreign-born Males",
                                 "Spanish-born Males",
                                 "Spanish-born Females",
                                 "Foreign-born Females"))
    PIR$nrel<-(abs(PIR$n)/sum(abs(PIR$n)))*100
    PIR$nrel <-with(PIR, ifelse((Sex=="Spanish-born Males"|Sex=="Foreign-born Males"), nrel*-1,nrel))
    PIR<-PIR[,c(1:5)]
    
    print(ggplot(PIR, aes(x=Age, y=nrel, fill=Sex))+
            annotate("segment", x = 15, xend = 15, y = -1.2, yend = 1.2,colour = "red")+
            annotate("segment", x = 65, xend = 65, y = -1.2, yend = 1.2,colour = "red")+
            geom_bar(data = PIR[(PIR$Sex=="Foreign-born Males"|
                                   PIR$Sex=="Spanish-born Males"),], 
                     colour = I("Black"),stat="identity", size=.3, colour="black",
                     aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
            geom_bar(data = PIR[(PIR$Sex=="Spanish-born Females"|
                                   PIR$Sex=="Foreign-born Females"),], 
                     colour = I("Black"),stat="identity", size=.3, colour="black",
                     aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
            coord_flip()+
            scale_y_continuous(limits=c(-1.2,1.2),breaks = c(-1.2,-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,1.2), 
                               labels =paste0(as.character(c(seq(1.2, 0, -0.2), seq(0.2, 1.2, 0.2))), "%")) +
            scale_x_continuous(breaks=seq(0,110,5))+
            #scale_fill_manual(values = c())+
            scale_fill_manual(values = c("#C77CFF", "#7CAE00","#F8766D","#00BFC4" ),
                              breaks=levels(PIR$Sex))+
            annotate("text", x = 107.5, y = -0.6, label = paste("Males",((sum(PIR$n[PIR$Sex == "Foreign-born Males"])+
                                                                          sum(PIR$n[PIR$Sex == "Spanish-born Males"]))*-1),sep=": "), size=5)+
            annotate("text", x = 107.5, y = 0.6, label = paste("Females",   sum(PIR$n[PIR$Sex == "Foreign-born Females"])+
                                                                          sum(PIR$n[PIR$Sex == "Spanish-born Females"]),sep=": "), size=5)+
            annotate("text", x = 67.5, y = -1, label = paste("65 and more",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males"& PIR$Age >=65])+
                                                                                    sum(PIR$n[PIR$Sex == "Spanish-born Males"& PIR$Age >=65])) ,sep=": "),size=4)+
            annotate("text", x = 67.5, y = 1, label = paste("65 and more",sum(PIR$n[PIR$Sex == "Foreign-born Females"& PIR$edad >=65])+
                                                                               sum(PIR$n[PIR$Sex == "Spanish-born Females"& PIR$Age >=65]) ,sep=": "),size=4)+
            annotate("text", x = 62.5, y = -1, label = paste("Between 16 and 64",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males" &(PIR$Age >15& PIR$Age <65)])+
                                                                                sum(PIR$n[PIR$Sex == "Spanish-born Males" &(PIR$Age >15& PIR$Age <65)])),sep=": "),size=4)+
            annotate("text", x = 62.5, y = 1, label = paste("Between 16 and 64",  1*(sum(PIR$n[PIR$Sex == "Foreign-born Females" &(PIR$Age >15& PIR$Age <65)])+
                                                                                sum(PIR$n[PIR$Sex == "Spanish-born Females" &(PIR$Age >15& PIR$Age <65)])),sep=": "),size=4)+
            annotate("text", x = 12.5, y = -1, label = paste("15 or less",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males"& PIR$Age <=15])+
                                                                                      sum(PIR$n[PIR$Sex == "Spanish-born Males"& PIR$Age <=15])),sep=": "),size=4)+
            annotate("text", x = 12.5, y = 1, label =  paste("15 or less", 1*(sum(PIR$n[PIR$Sex == "Foreign-born Females"& PIR$Age <=15])+
                                                                                      sum(PIR$n[PIR$Sex == "Spanish-born Females"& PIR$Age <=15])),sep=": "),size=4)+
            
            ggtitle(paste("Catalonia",unique(CATFINAL2014$year), 
                          "\nTotal population:", sum(CATFINAL2014[,1:2]),"|",
                          "Spanish-born population:", sum(CATFINAL2014[,2]),"|",
                          "Foreign-born population:", sum(CATFINAL2014[,1]),
                          "\nForeign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)))+
            theme(plot.title = element_text(lineheight=1.6, size=12, face="bold"),
                  legend.title = element_blank(),
                  legend.text = element_text(colour="black", size = 15),
                  legend.position="bottom",
                  legend.background = element_rect(fill="#FFFFFF"),
                  axis.title.x = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
                  axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
                  axis.title.y = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
                  axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
                  plot.background = element_rect(fill = "#FFFFFF"))+ylab("")+xlab("Age"))
  })
  
  output$downloadPYR <- downloadHandler(
    filename = function() { paste("Catalonia",input$year, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      CATFINAL2014<-CATFINAL2014[CATFINAL2014$year %in% input$year, ]
      
      EXT<-CATFINAL2014[,c(1,3:5)]
      EXT$Sex<- paste("Foreign-born", EXT$Sex, sep=" ")
      colnames(EXT)<- c("n", "year", "Sex", "Age")
      EXT$n <-with(EXT, ifelse(Sex=="Foreign-born Males", n*-1,n))
      
      ESP<-CATFINAL2014[,c(2,3:5)]
      ESP$Sex<- paste("Spanish-born", ESP$Sex, sep=" ")
      colnames(ESP)<- c("n", "year", "Sex", "Age")
      ESP$n <-with(ESP, ifelse(Sex=="Spanish-born Males", n*-1,n))
      PIR<- rbind(ESP,EXT)
      PIR$Sex <- factor(PIR$Sex,
                        levels = c("Foreign-born Males",
                                   "Spanish-born Males",
                                   "Spanish-born Females",
                                   "Foreign-born Females"))
      PIR$nrel<-(abs(PIR$n)/sum(abs(PIR$n)))*100
      PIR$nrel <-with(PIR, ifelse((Sex=="Spanish-born Males"|Sex=="Foreign-born Males"), nrel*-1,nrel))
      PIR<-PIR[,c(1:5)]
      print(ggplot(PIR, aes(x=Age, y=nrel, fill=Sex))+
              annotate("segment", x = 15, xend = 15, y = -1.2, yend = 1.2,colour = "red")+
              annotate("segment", x = 65, xend = 65, y = -1.2, yend = 1.2,colour = "red")+
              geom_bar(data = PIR[(PIR$Sex=="Foreign-born Males"|
                                     PIR$Sex=="Spanish-born Males"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
              geom_bar(data = PIR[(PIR$Sex=="Spanish-born Females"|
                                     PIR$Sex=="Foreign-born Females"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
              coord_flip()+
              scale_y_continuous(limits=c(-1.2,1.2),breaks = c(-1.2,-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,1.2), 
                                 labels =paste0(as.character(c(seq(1.2, 0, -0.2), seq(0.2, 1.2, 0.2))), "%")) +
              scale_x_continuous(breaks=seq(0,110,5))+
              #scale_fill_manual(values = c())+
              scale_fill_manual(values = c("#C77CFF", "#7CAE00","#F8766D","#00BFC4" ),
                                breaks=levels(PIR$Sex))+
              annotate("text", x = 107.5, y = -0.6, label = paste("Males",((sum(PIR$n[PIR$Sex == "Foreign-born Males"])+
                                                                              sum(PIR$n[PIR$Sex == "Spanish-born Males"]))*-1),sep=": "), size=5)+
              annotate("text", x = 107.5, y = 0.6, label = paste("Females",   sum(PIR$n[PIR$Sex == "Foreign-born Females"])+
                                                                   sum(PIR$n[PIR$Sex == "Spanish-born Females"]),sep=": "), size=5)+
              annotate("text", x = 67.5, y = -1, label = paste("65 and more",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males"& PIR$Age >=65])+
                                                                                   sum(PIR$n[PIR$Sex == "Spanish-born Males"& PIR$Age >=65])) ,sep=": "),size=4)+
              annotate("text", x = 67.5, y = 1, label = paste("65 and more",sum(PIR$n[PIR$Sex == "Foreign-born Females"& PIR$edad >=65])+
                                                                sum(PIR$n[PIR$Sex == "Spanish-born Females"& PIR$Age >=65]) ,sep=": "),size=4)+
              annotate("text", x = 62.5, y = -1, label = paste("Between 16 and 64",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males" &(PIR$Age >15& PIR$Age <65)])+
                                                                                         sum(PIR$n[PIR$Sex == "Spanish-born Males" &(PIR$Age >15& PIR$Age <65)])),sep=": "),size=4)+
              annotate("text", x = 62.5, y = 1, label = paste("Between 16 and 64",  1*(sum(PIR$n[PIR$Sex == "Foreign-born Females" &(PIR$Age >15& PIR$Age <65)])+
                                                                                         sum(PIR$n[PIR$Sex == "Spanish-born Females" &(PIR$Age >15& PIR$Age <65)])),sep=": "),size=4)+
              annotate("text", x = 12.5, y = -1, label = paste("15 or less",-1*(sum(PIR$n[PIR$Sex == "Foreign-born Males"& PIR$Age <=15])+
                                                                                  sum(PIR$n[PIR$Sex == "Spanish-born Males"& PIR$Age <=15])),sep=": "),size=4)+
              annotate("text", x = 12.5, y = 1, label =  paste("15 or less", 1*(sum(PIR$n[PIR$Sex == "Foreign-born Females"& PIR$Age <=15])+
                                                                                  sum(PIR$n[PIR$Sex == "Spanish-born Females"& PIR$Age <=15])),sep=": "),size=4)+
              
              ggtitle(paste("Catalonia",unique(CATFINAL2014$year), 
                            "\nTotal population:", sum(CATFINAL2014[,1:2]),"|",
                            "Spanish-born population:", sum(CATFINAL2014[,2]),"|",
                            "Foreign-born population:", sum(CATFINAL2014[,1]),
                            "\nForeign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)))+
              theme(plot.title = element_text(lineheight=1.6, size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 15),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FFFFFF"),
                    axis.title.x = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
                    axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
                    axis.title.y = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
                    axis.text.y  = element_text( vjust=0.5, size=15,colour="black"),
                    plot.background = element_rect(fill = "#FFFFFF"))+ylab("")+xlab("Age"))
      
      dev.off()
    })
  #### COMPOSICION POR EDADES
 # output$ComposicionEDADES <-renderGvis({ 
#    CATFINAL2014<-CATFINAL2014[CATFINAL2014$year %in% input$year, ]
#    POP <- CATFINAL2014 %>% group_by(Age_Groups) %>%  summarise( Spanish = sum(Spain), 
 #                                                            Foreign = sum(Foreign))
#    colnames(POP)<- c("Age_Groups", "Spanish-born", "Foreign-born")
#    gvisBarChart(POP, xvar="Age_Groups", yvar=c("Spanish-born","Foreign-born"),
 #                options=list(height= 400,
  #                            width= 850,
  #                            backgroundColor='white',
  #                            chartArea.backgroundColor="red",
  #                            legend="{position: 'bottom', 
  #                            textStyle: {color: 'blue', fontSize: 12}}",
  #                            legend.alignment="center",
  #                            title= "Population Composition by age groups",
  #                            vAxis= "{format:'#', 
  #                            titleTextStyle: {color: 'black',fontName:'Courier'}}",
  #                            hAxis= "{title: 'Population',
  #                            format:'#', 
  #                            titleTextStyle: {color: 'black',fontName:'Courier'}}",
  #                            isStacked=FALSE))
    #})
  
  #### BUILT YOUR POWN DATA TABLE
  
  updateSelectizeInput(session, "MUNICIPALITY6",
                       choices=unique(x1$MUNICIPALITY6),
                       selected="Barcelona")
  updateSelectizeInput(session, "YEAR",
                       choices=unique(x1$YEAR),
                       selected=c("2000", "2005", "2010","2015"))
  output$usertable <-renderDataTable({ 
    
    MunTA <- x1[(x1$MUNICIPALITY%in%input$MUNICIPALITY6 &
                          x1$YEAR %in%input$YEAR),input$VARIABLES, drop = FALSE ]
    MunTA <-  MunTA[order(MunTA$YEAR),] 
    
  })
  output$downloadDatausers <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Your table by GEDEM", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[(x1$MUNICIPALITY%in%input$MUNICIPALITY6 &
                               x1$YEAR %in%input$YEAR),
                            input$VARIABLES, drop = FALSE ], 
                  file, sep = sep,row.names = FALSE)
    })
  
  })