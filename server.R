
##### LIBRARIES ####

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(ggmap)
library(rgeos)
library(rgdal)
library(maptools)


############## PREPARATION ##################################
load("./data/CATALONIA14UTF2.Rdata")
load("./data/PIR.Rdata")
load("./data/COUNTRYBIRTH.Rdata")

#load("C:\\Users\\jgaleano\\Dropbox\\R\\SHINY\\NEW_DP\\SPAIN_XXI\\data\\CATALONIA14UTF2.Rdata")
#load("C:\\Users\\jgaleano\\Dropbox\\R\\SHINY\\NEW_DP\\population_change\\data\\CATALONIA14UTF2.Rdata")

lookup <- structure(c("SHPDATOSCAT2004","SHPDATOSCAT2005","SHPDATOSCAT2006","SHPDATOSCAT2007","SHPDATOSCAT2008",
                      "SHPDATOSCAT2009","SHPDATOSCAT2010","SHPDATOSCAT2011","SHPDATOSCAT2012","SHPDATOSCAT2013",
                      "SHPDATOSCAT2014","SHPDATOSCAT2015"), 
                    .Names = c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015" ))

lookup1 <- c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015" )
x1<-DATOS
x1$EXT<-NULL
x1 <-  x1[order(x1$YEAR),] 
x1$POP_TOTAL_EXT <- x1$POP_TOTAL-x1$POP_SPANISH
colnames (x1) <- c("IDMUN", "PROVINCE", "MUNICIPALITY","Census tracts","YEAR","[Pop]Spain",                
                   "[Pop]Latin America","[Pop]Western Europe","[Pop]Eastern Europe",          
                   "[Pop]Africa","[Pop]Asia","[Pop]Others","[Pop]Total Population","[Dissimilarity Index]Latin America",
                   "[Dissimilarity Index]Western Europe","[Dissimilarity Index]Eastern Europe","[Dissimilarity Index]Africa",
                   "[Dissimilarity Index]Asia","[Dissimilarity Index]Others","[Isolation Index]Latin America",
                   "[Isolation Index]Western Europe","[Isolation Index]Eastern Europe","[Isolation Index]Africa","[Isolation Index]Asia",
                   "[Isolation Index]Others","Population Size","COM", "Percentage_Foreign","Diversity", 
                   "LON","LAT",
                  "[Pop]Total Foreign-born Population")       
x1$Percentage_Spanish <-100 -x1$Percentage_Foreign
x1$IDMUN <- as.character(x1$IDMUN)
x1$MUNICIPALITY2 <-x1$MUNICIPALITY 
x1$MUNICIPALITY3 <-x1$MUNICIPALITY 
x1$MUNICIPALITY4 <-x1$MUNICIPALITY 
x1$MUNICIPALITY5 <-x1$MUNICIPALITY 
x1$MUNICIPALITY6 <-x1$MUNICIPALITY 

x1$Totalpop2 <-x1[, 13]-x1[, 12]
x1$ENTROPYSP<-  (x1[, 6]/x1[, 39])*(log(1/(x1[, 6]/x1[, 39])))
x1$ENTROPYLA<-  (x1[, 7]/x1[, 39])*(log(1/(x1[, 7]/x1[, 39])))
x1$ENTROPYWE<-  (x1[, 8]/x1[, 39])*(log(1/(x1[, 8]/x1[, 39])))
x1$ENTROPYEE<-  (x1[, 9]/x1[, 39])*(log(1/(x1[, 9]/x1[, 39])))
x1$ENTROPYAF<-  (x1[, 10]/x1[, 39])*(log(1/(x1[, 10]/x1[, 39])))
x1$ENTROPYAS<-  (x1[, 11]/x1[, 39])*(log(1/(x1[, 11]/x1[, 39])))

x1$ENTROPYLA[is.na(x1$ENTROPYLA)]<- 0
x1$ENTROPYWE[is.na(x1$ENTROPYWE)]<- 0
x1$ENTROPYEE[is.na(x1$ENTROPYEE)]<- 0
x1$ENTROPYAF[is.na(x1$ENTROPYAF)]<- 0
x1$ENTROPYAS[is.na(x1$ENTROPYAS)]<- 0

x1 <- mutate(x1, ENTROPY =  (ENTROPYSP+
                               ENTROPYLA+                 
                               ENTROPYWE+                      
                               ENTROPYEE+                        
                               ENTROPYAF+
                               ENTROPYAS)/log(6))

x1 <- x1[-c(151,8143,16199,24268,32371,40475,48579,56684,64790,72897,81004,89113,
            97224,105335,113447,121561,129675),]


GROUPS <- structure(c("LatinAmerica", "WesternEurope", "EasternEurope", "Africa", "Asia"), 
                    .Names = c("Latin America", "Western Europe", "Eastern Europe", "Africa", "Asia" ))

TERMS <- structure(c("Absolute", "Relative"), 
                    .Names = c("Absolute", "Relative"))


##### CREAMOS UNA FUNCIONES QUE UTILIZAREMOS EN EL CóDIGO ####

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

a <- function(x) format(x, big.mark = ",", scientific = FALSE) 



######## SHINY ######
shinyServer(function(input, output,session) {    
  
  observe({
    updateSelectizeInput(session, 'MUNICIPALITY', 
                         choices = unique(x1[x1$COM==input$prov, "MUNICIPALITY"]),
                         selected= ifelse(input$prov=="Basque Country", "Bilbao",
                                   ifelse(input$prov=="Castilla la Mancha", "Albacete",
                                   ifelse(input$prov=="Valencia", "Valencia",
                                   ifelse(input$prov=="Andalusia", "Sevilla",
                                   ifelse(input$prov=="Castile and Leon", "Ãvila",       
                                   ifelse(input$prov=="Extremadura", "Cáceres",
                                   ifelse(input$prov=="Balearic Islands", "Palma de Mallorca",
                                   ifelse(input$prov=="Catalonia", "Barcelona",
                                   ifelse(input$prov=="Galicia", "Pontevedra",
                                   ifelse(input$prov=="Arago", "Zaragoza",
                                   ifelse(input$prov=="La Rioja", "Logroño",
                                   ifelse(input$prov=="Madrid", "Madrid",
                                   ifelse(input$prov=="Murcia", "Murcia",
                                   ifelse(input$prov=="Navarra", "Pamplona/Iruña",
                                   ifelse(input$prov=="Asturies", "Gijón",
                                   ifelse(input$prov=="Canary Islands", "Palmas de Gran Canaria, Las",
                                   ifelse(input$prov=="Cantabria", "Santander",
                                    0))))))))))))))))))
  })
  options(shiny.sanitize.errors = FALSE)
  ######## POPULATION PYRAMID ###### 
  
  output$pyramidrel <- renderHighchart({
    
    CATFINAL2014<-PIR[PIR$year %in% input$year & PIR$COM %in% input$provp, ]
    #CATFINAL2014<-PIR[PIR$year==2016 & PIR$COM=="Catalonia", ]
    
    EXT<-CATFINAL2014[,c(1,3:5)]
    EXT$Sex<- paste("Foreign-born", EXT$Sex, sep=" ")
    colnames(EXT)<- c("n", "year", "Sex", "Age")
    EXT$n <-with(EXT, ifelse(Sex=="Foreign-born Males", n*-1,n))
    
    ESP<-CATFINAL2014[,c(2,3:5)]
    ESP$Sex<- paste("Spanish-born", ESP$Sex, sep=" ")
    colnames(ESP)<- c("n", "year", "Sex", "Age")
    ESP$n <-with(ESP, ifelse(Sex=="Spanish-born Males", n*-1,n))
    PIR1<- rbind(ESP,EXT)
    PIR1$Sex <- factor(PIR1$Sex,
                       levels = c("Foreign-born Males",
                                  "Spanish-born Males",
                                  "Spanish-born Females",
                                  "Foreign-born Females"))
    PIR1$nrel<-round((abs(PIR1$n)/sum(abs(PIR1$n)))*100,2)
    PIR1$nrel <-with(PIR1, ifelse((Sex=="Spanish-born Males"|Sex=="Foreign-born Males"), nrel*-1,nrel))
    PIR1<-PIR1[,c(1:5)]
    colnames(PIR1)<-c( "Absolute",  "year", "Sex",  "Age" , "Relative")
    
    PIR1$Sex <- factor(PIR1$Sex ,
                       levels = c("Foreign-born Males", 
                                  "Spanish-born Males",  
                                  "Foreign-born Females",
                                  "Spanish-born Females"))
    
    color1 <-c("#7CAE00","#C77CFF","#00BFC4","#F8766D")
    
    xaxis <- list(categories = sort(unique(PIR1$Age)),
                  reversed = FALSE, tickInterval =5,
                  labels = list(step = 1))
    
    MAXABS<-PIR[PIR$COM%in% input$provp, ]
    
    
    maxabs<-round((max(MAXABS$Spain)+max(MAXABS$Foreign)),0)
   
    maxf <-ifelse(input$terms=="Relative", 1.1,maxabs)
    tickInterval<-ifelse(input$terms=="Relative", 0.1,NA)
    formatter<- ifelse(input$terms=="Relative","function(){ return Math.abs(this.value) + '%'; }",
                                              "function(){ return Math.abs(this.value); }")
    
    
    highchart() %>%
      hc_chart(type = "bar",zoomType= 'xy') %>%
      hc_add_series(name = 'Foreign-born Males', data = c(PIR1[PIR1$Sex=="Foreign-born Males",input$terms])) %>%
      hc_add_series(name = 'Foreign-born Females', data = c(PIR1[PIR1$Sex== "Foreign-born Females",input$terms])) %>%
      hc_add_series(name = 'Spanish-born Males', data = c(PIR1[PIR1$Sex=="Spanish-born Males",input$terms])) %>%
      hc_add_series(name = 'Spanish-born Females', data = c(PIR1[PIR1$Sex== "Spanish-born Females",input$terms])) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=FALSE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_yAxis(labels = list(formatter = JS(formatter)),
               tickInterval=tickInterval, 
               min=-maxf, 
               max=maxf) %>% 
      hc_xAxis(xaxis, rlist::list.merge(xaxis, list(opposite = TRUE, linkedTo = 0))) %>% 
      hc_colors(c(color1)) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_exporting(enabled = FALSE) %>%
      hc_title(text =paste(paste("Population structure - Year: ",unique(PIR1$year),sep=""),paste("Total Population: ", a(sum(abs(PIR1$Absolute))),sep=""),sep=" - "),align = 'left') %>%
      
      hc_subtitle(text =paste(paste("Foreign-born males: ",a(sum(abs(c(PIR1[PIR1$Sex=="Foreign-born Males",1])))),sep=""), 
                              paste("Spanish-born males: ",a(sum(abs(c(PIR1[PIR1$Sex=="Spanish-born Males",1])))),sep=""),
                              paste("Spanish-born females: ",a(sum(abs(c(PIR1[PIR1$Sex=="Spanish-born Females",1])))),sep=""),
                              paste("Foreign-born females: ",a(sum(abs(c(PIR1[PIR1$Sex=="Foreign-born Females",1])))),sep=""),sep=" - "),align = 'left') %>%
      
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(shared = FALSE,
                 formatter = JS("function () { return '<b>' + this.series.name + '<br>' +'Age: ' + this.point.category + '</b><br/>' + 'Population: ' + Math.abs(this.point.y);}"))
    
  }) 
  ######## POPULATION PYRAMID DOWNLOAD IMAGES ###### 
  
  output$downloadPYR <- downloadHandler(
    filename = function() { paste(input$provp,input$year, '.png', sep='') },
    content = function(file) {
      png(file,width = 800, height = 700)
      CATFINAL2014<-PIR[PIR$year %in% input$year & PIR$COM%in% input$provp, ]
      
      EXT<-CATFINAL2014[,c(1,3:5)]
      EXT$Sex<- paste("Foreign-born", EXT$Sex, sep=" ")
      colnames(EXT)<- c("n", "year", "Sex", "Age")
      EXT$n <-with(EXT, ifelse(Sex=="Foreign-born Males", n*-1,n))
      
      ESP<-CATFINAL2014[,c(2,3:5)]
      ESP$Sex<- paste("Spanish-born", ESP$Sex, sep=" ")
      colnames(ESP)<- c("n", "year", "Sex", "Age")
      ESP$n <-with(ESP, ifelse(Sex=="Spanish-born Males", n*-1,n))
      PIR1<- rbind(ESP,EXT)
      PIR1$Sex <- factor(PIR1$Sex,
                         levels = c("Foreign-born Males",
                                    "Spanish-born Males",
                                    "Spanish-born Females",
                                    "Foreign-born Females"))
      PIR1$nrel<-(abs(PIR1$n)/sum(abs(PIR1$n)))*100
      PIR1$nrel <-with(PIR1, ifelse((Sex=="Spanish-born Males"|Sex=="Foreign-born Males"), nrel*-1,nrel))
      PIR1<-PIR1[,c(1:5)]
      
      
      
      PIR1$Sex <- factor(PIR1$Sex ,
                         levels = c("Foreign-born Males", "Spanish-born Males",  "Foreign-born Females","Spanish-born Females"))
      
      a <- function(x) format(x, big.mark = ",", scientific = FALSE) 
     
      MAXABS<-PIR[PIR$COM%in% input$provp, ]
      
      
      maxabs<-round((max(MAXABS$Spain)+max(MAXABS$Foreign)),0)
      
      if(input$terms =='Relative'){
      print(ggplot(PIR1, aes(x=Age, y=nrel, fill=Sex))+ 
              annotate("segment", x = 15, xend = 15, y = -1.1, yend = 1.1,colour = "red")+
              annotate("segment", x = 65, xend = 65, y = -1.1, yend = 1.1,colour = "red")+
              geom_bar(data = PIR1[(PIR1$Sex=="Foreign-born Males"|
                                     PIR1$Sex=="Spanish-born Males"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
              geom_bar(data = PIR1[(PIR1$Sex=="Spanish-born Females"|
                                     PIR1$Sex=="Foreign-born Females"),], 
                       colour = I("Black"),stat="identity", size=.3, colour="black",
                       aes(x = Age, y = nrel,fill = Sex), alpha = 1)+
              coord_flip()+
              scale_y_continuous(limits=c(-1.1,1.1),breaks = c(-1.1,-1,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1), 
                                 labels =paste0(as.character(c(seq(1.1, 0, -0.1), seq(0.1, 1.1, 0.1))), "%")) +
              scale_x_continuous(breaks=seq(0,110,5))+
              #scale_fill_manual(values = c())+
              scale_fill_manual(values = c("#C77CFF", "#7CAE00","#F8766D","#00BFC4" ),
                                breaks=levels(PIR1$Sex))+
              annotate("text", x = 107.5, y = -0.6, label = paste("Males",(a((sum(PIR1$n[PIR1$Sex == "Foreign-born Males"])+
                                                                                sum(PIR1$n[PIR1$Sex == "Spanish-born Males"]))*-1)),sep=": "), size=5)+
              annotate("text", x = 107.5, y = 0.6, label = paste("Females",   a(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"])+
                                                                                  sum(PIR1$n[PIR1$Sex == "Spanish-born Females"])),sep=": "), size=5)+
              annotate("text", x = 67.5, y = -.9, label = paste("65 and more",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males"& PIR1$Age >=65])+
                                                                                     sum(PIR1$n[PIR1$Sex == "Spanish-born Males"& PIR1$Age >=65]))) ,sep=": "),size=4)+
              annotate("text", x = 67.5, y = .9, label = paste("65 and more",a(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"& PIR1$edad >=65])+
                                                                                sum(PIR1$n[PIR1$Sex == "Spanish-born Females"& PIR1$Age >=65])) ,sep=": "),size=4)+
              annotate("text", x = 62.5, y = -.9, label = paste("Between 16 and 64",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males" &(PIR1$Age >15& PIR1$Age <65)])+
                                                                                           sum(PIR1$n[PIR1$Sex == "Spanish-born Males" &(PIR1$Age >15& PIR1$Age <65)]))),sep=": "),size=4)+
              annotate("text", x = 62.5, y = .9, label = paste("Between 16 and 64",  a(1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Females" &(PIR1$Age >15& PIR1$Age <65)])+
                                                                                           sum(PIR1$n[PIR1$Sex == "Spanish-born Females" &(PIR1$Age >15& PIR1$Age <65)]))),sep=": "),size=4)+
              annotate("text", x = 12.5, y = -.9, label = paste("15 or less",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males"& PIR1$Age <=15])+
                                                                                    sum(PIR1$n[PIR1$Sex == "Spanish-born Males"& PIR1$Age <=15]))),sep=": "),size=4)+
              annotate("text", x = 12.5, y = .9, label =  paste("15 or less", a(1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"& PIR1$Age <=15])+
                                                                                    sum(PIR1$n[PIR1$Sex == "Spanish-born Females"& PIR1$Age <=15]))),sep=": "),size=4)+
              
              
              
              labs(title= paste(input$provp,unique(CATFINAL2014$year)),
                   subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
                                    "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
                                    "Foreign-born population:", a(sum(CATFINAL2014[,1])),
                                    "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA.\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))
        
      }else{
        
        
        print(ggplot(PIR1, aes(x=Age, y=n, fill=Sex))+ 
                annotate("segment", x = 15, xend = 15, y = -pretty(maxabs)[2], yend = pretty(maxabs)[2],colour = "red")+
                annotate("segment", x = 65, xend = 65, y = -pretty(maxabs)[2], yend = pretty(maxabs)[2],colour = "red")+
                geom_bar(data = PIR1[(PIR1$Sex=="Foreign-born Males"|
                                        PIR1$Sex=="Spanish-born Males"),], 
                         colour = I("Black"),stat="identity", size=.3, colour="black",
                         aes(x = Age, y = n,fill = Sex), alpha = 1)+
                geom_bar(data = PIR1[(PIR1$Sex=="Spanish-born Females"|
                                        PIR1$Sex=="Foreign-born Females"),], 
                         colour = I("Black"),stat="identity", size=.3, colour="black",
                         aes(x = Age, y = n,fill = Sex), alpha = 1)+
                coord_flip()+
                scale_y_continuous(limits=c(-pretty(maxabs)[2],pretty(maxabs)[2]),breaks = c(-pretty(maxabs)[2],-pretty(maxabs)[2]/2,0,pretty(maxabs)[2]/2,pretty(maxabs)[2]), 
                                   labels =paste0(as.character(c(seq(pretty(maxabs)[2], 0, -pretty(maxabs)[2]/2), seq(pretty(maxabs)[2]/2, pretty(maxabs)[2], pretty(maxabs)[2]/2))), "")) +
                scale_x_continuous(breaks=seq(0,110,5))+
                #scale_fill_manual(values = c())+
                scale_fill_manual(values = c("#C77CFF", "#7CAE00","#F8766D","#00BFC4" ),
                                  breaks=levels(PIR1$Sex))+
                annotate("text", x = 107.5, y = -pretty(maxabs)[2]/2, label = paste("Males",(a((sum(PIR1$n[PIR1$Sex == "Foreign-born Males"])+
                                                                                                    sum(PIR1$n[PIR1$Sex == "Spanish-born Males"]))*-1)),sep=": "), size=5)+
                annotate("text", x = 107.5, y = pretty(maxabs)[2]/2, label = paste("Females",   a(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"])+
                                                                                                      sum(PIR1$n[PIR1$Sex == "Spanish-born Females"])),sep=": "), size=5)+
                annotate("text", x = 67.5, y = -pretty(maxabs)[2]/1.2, label = paste("65 and more",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males"& PIR1$Age >=65])+
                                                                                                           sum(PIR1$n[PIR1$Sex == "Spanish-born Males"& PIR1$Age >=65]))) ,sep=": "),size=4)+
                annotate("text", x = 67.5, y = pretty(maxabs)[2]/1.2, label = paste("65 and more",a(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"& PIR1$edad >=65])+
                                                                                                      sum(PIR1$n[PIR1$Sex == "Spanish-born Females"& PIR1$Age >=65])) ,sep=": "),size=4)+
                annotate("text", x = 62.5, y = -pretty(maxabs)[2]/1.2, label = paste("Between 16 and 64",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males" &(PIR1$Age >15& PIR1$Age <65)])+
                                                                                                                 sum(PIR1$n[PIR1$Sex == "Spanish-born Males" &(PIR1$Age >15& PIR1$Age <65)]))),sep=": "),size=4)+
                annotate("text", x = 62.5, y = pretty(maxabs)[2]/1.2, label = paste("Between 16 and 64",  a(1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Females" &(PIR1$Age >15& PIR1$Age <65)])+
                                                                                                                 sum(PIR1$n[PIR1$Sex == "Spanish-born Females" &(PIR1$Age >15& PIR1$Age <65)]))),sep=": "),size=4)+
                annotate("text", x = 12.5, y = -pretty(maxabs)[2]/1.2, label = paste("15 or less",a(-1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Males"& PIR1$Age <=15])+
                                                                                                          sum(PIR1$n[PIR1$Sex == "Spanish-born Males"& PIR1$Age <=15]))),sep=": "),size=4)+
                annotate("text", x = 12.5, y = pretty(maxabs)[2]/1.2, label =  paste("15 or less", a(1*(sum(PIR1$n[PIR1$Sex == "Foreign-born Females"& PIR1$Age <=15])+
                                                                                                          sum(PIR1$n[PIR1$Sex == "Spanish-born Females"& PIR1$Age <=15]))),sep=": "),size=4)+
                labs(title= paste(input$provp,unique(CATFINAL2014$year)),
                     subtitle = paste("Total population:", a(sum(CATFINAL2014[,1:2])),"|",
                                      "Spanish-born population:", a(sum(CATFINAL2014[,2])),"|",
                                      "Foreign-born population:", a(sum(CATFINAL2014[,1])),
                                      "Foreign-born (%):", round((sum(CATFINAL2014[,1])/sum(CATFINAL2014[,1:2])*100),digits=2)),
                     caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA.\nData: Population Register (Padrón Continuo, INE)")+
                theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                      legend.title = element_blank(),
                      legend.text = element_text(colour="black", size = 10,vjust=2),
                      legend.position="none",
                      legend.background = element_rect(fill="#FfFfFf"),
                      strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                      axis.title.x = element_blank(),
                      axis.text.x  = element_text(angle = 00, colour="black", size=10),
                      axis.title.y = element_text(colour="black",vjust=2, size=10),
                      axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                      panel.background = element_rect(fill = "#f2f2f2"), 
                      panel.grid = element_line(colour="red"),
                      panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                      panel.grid.minor=element_line(colour="#f2f2f2"),
                      plot.background = element_rect(fill = "#ffffff"))+ylab("")+xlab("Age"))}
      
      
      
      dev.off()
    })
  
  ######## POPULATION PYRAMID DOWNLOAD DATA ###### 
  
  output$downloadDataPy <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(paste(input$provp,input$year,sep=" "), input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      CATFINAL2014<-PIR[PIR$year %in% input$year & PIR$COM%in% input$provp, ]
      
      EXT<-CATFINAL2014[,c(1,3:5)]
      EXT$Sex<- paste("Foreign-born", EXT$Sex, sep=" ")
      colnames(EXT)<- c("n", "year", "Sex", "Age")
      EXT$n <-with(EXT, ifelse(Sex=="Foreign-born Males", n*-1,n))
      
      ESP<-CATFINAL2014[,c(2,3:5)]
      ESP$Sex<- paste("Spanish-born", ESP$Sex, sep=" ")
      colnames(ESP)<- c("n", "year", "Sex", "Age")
      ESP$n <-with(ESP, ifelse(Sex=="Spanish-born Males", n*-1,n))
      PIR1<- rbind(ESP,EXT)
      PIR1$Sex <- factor(PIR1$Sex,
                         levels = c("Foreign-born Males",
                                    "Spanish-born Males",
                                    "Spanish-born Females",
                                    "Foreign-born Females"))
      PIR1$nrel<-(abs(PIR1$n)/sum(abs(PIR1$n)))*100
      PIR1$nrel <-with(PIR1, ifelse((Sex=="Spanish-born Males"|Sex=="Foreign-born Males"), nrel*-1,nrel))
      PIR1<-PIR1[,c(1:5)]
      # Write to a file specified by the 'file' argument
      write.table(PIR1, file, sep = sep, row.names = FALSE)
    })

  
  ######## POPULATION COMPOSITION PLOTS ###### 
  
  output$ComposicionPlot2 <-renderHighchart({ 
    Mun2 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun2 <-  Mun2[order(Mun2$YEAR),] 
    highchart() %>%
      hc_chart(type = 'column') %>%
      hc_legend(enabled = TRUE) %>%
      hc_xAxis(categories = Mun2$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Population (k: Thousands)')) %>%
      hc_plotOptions(series = list(stacking = "normal",animation=TRUE),
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth =.25))%>%
      hc_add_series(name = 'Foreign-born', data = Mun2[,32]) %>%
      hc_add_series(name = 'Spanish-born', data = Mun2[,6]) %>%
      
      hc_title(text =paste("Population by place of birth",paste(unique(input$MUNICIPALITY)," ", "(",unique(Mun2$PROVINCE),")",sep=""), 
                            sep=": "),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      # hc_colors(c(color)) %>%
      hc_tooltip(enabled = TRUE)
  })
  
  output$ComposicionPlot3 <-renderHighchart({ 
    Mun3 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,]
    Mun3 <-  Mun3[order(Mun3$YEAR),] 
    Mun3$Percentage_Spanish <-  round(Mun3$Percentage_Spanish, digits=2) 
    Mun3$Percentage_Foreign <-  round(Mun3$Percentage_Foreign, digits=2)
    prop<- highchart() %>%
      hc_chart(type = 'line') %>%
      hc_legend(enabled = TRUE) %>%
      hc_xAxis(categories = Mun3$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Foregin-born population (%)'), ceiling=70) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = FALSE))) %>%
      hc_add_series(name = 'Foreign-born population (%)', data = Mun3[,28]) %>%
      hc_title(text =paste("Foreign-born population (%)", paste(unique(input$MUNICIPALITY)," ",
                                 "(",unique(Mun3$PROVINCE),")", 
                                 sep=""),sep=": "),
               align = 'left') %>% 
      hc_add_theme(hc_theme_smpl()) %>% 
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      # hc_colors(c(color)) %>% 
      hc_tooltip(enabled = TRUE)
    prop
  }) 
  
  ######## POPULATION COMPOSITION PLOTS DOWNLOAD IMAGE ###### 
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() {paste("Stacked bar plot1", paste(input$MUNICIPALITY, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun3 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY,]
      Mun3 <-  Mun3[order(Mun3$YEAR),] 
      Munplot <-data.frame(YEAR=rep(2000:2016,2),
                           POP=c(Mun3[1:17,6],Mun3[1:17,32]),
                           ORIGEN=c(rep("[Pop]Spain", 17),rep("[Pop]Total Foreig-born Population", 17)))
      
      Munplot$ORIGEN <- factor(Munplot$ORIGEN ,
                               levels = c("[Pop]Total Foreig-born Population",
                                          "[Pop]Spain"))
      
      print(ggplot(data=Munplot, aes(x=YEAR, y=POP, fill=ORIGEN)) +
              labs(title= paste(paste(input$MUNICIPALITY," ", "(",Mun3$PROVINCE,")",sep=""),"Population composition", sep=": "),
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA.\nData: Population Register (Padrón Continuo, INE)")+
              geom_bar(stat="identity", colour="black")+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_fill_manual(values=rev(c("#3366cc", "#dc3912")),
                                breaks=c("[Pop]Spain", "[Pop]Total Foreig-born Population"),
                                labels=c("Spanish-born", "Foreign-born"))+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#ffffff",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population"))
      dev.off()
    })
  
  ######## POPULATION COMPOSITION PLOTS DOWNLLOAD DATA ###### 
  
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
  ######## PIE CHARTS ###### 
  
  output$ComposicionPlot6 <-renderHighchart({ 
    Mun6 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun6<- data.frame(t(Mun6[(Mun6$YEAR==2000),7:12]))
    Mun6<- cbind(ORIGIN = rownames(Mun6), Mun6) 
    a <- function(x) format(x, big.mark = ",", scientific = FALSE) 
    color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
    pie1<- highchart() %>%
      hc_chart(type = 'pie') %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = TRUE, 
                                                     format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
      hc_add_series(data = list(
        list(y =  Mun6[1,2], name = "Latin America"),
        list(y =  Mun6[2,2], name = "Western Europe"),
        list(y =  Mun6[3,2], name = "Eastern Europe"),
        list(y =  Mun6[4,2], name = "Africa"),
        list(y =  Mun6[5,2], name = "Asia")))%>%
      
      hc_title(text = paste("Foreign-born population composition year 2000",input$MUNICIPALITY, sep=": "), align = 'left')  %>%
      hc_subtitle(text =  paste("\nTotal Foreign-born population",a(sum(Mun6[,2])), sep=": "))%>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(pointFormat = "Population: {point.y}", enabled = TRUE) 
    pie1
  })
  
  output$ComposicionPlot7 <-renderHighchart({ 
    Mun7 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    Mun7<- data.frame(t(Mun7[(Mun7$YEAR==2016),7:12]))
    Mun7<- cbind(ORIGIN = rownames(Mun7), Mun7) 
    a <- function(x) format(x, big.mark = ",", scientific = FALSE) 
    color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
    pie2<- highchart() %>%
      hc_chart(type = 'pie') %>%
      hc_legend(enabled = TRUE) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = TRUE, 
                                                     format = '<b>{point.name}</b>: {point.percentage:.1f} %'))) %>%
      hc_add_series(data = list(
        list(y =  Mun7[1,2], name = "Latin America"),
        list(y =  Mun7[2,2], name = "Western Europe"),
        list(y =  Mun7[3,2], name = "Eastern Europe"),
        list(y =  Mun7[4,2], name = "Africa"),
        list(y =  Mun7[5,2], name = "Asia")))%>%
      
      hc_title(text = paste("Foreign-born population composition year 2016",input$MUNICIPALITY, sep=": "), align = 'left')  %>%
      
      hc_subtitle(text =  paste("\nTotal Foreign-born population",a(sum(Mun7[,2])), sep=": "))%>%
      
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(pointFormat = "Population: {point.y}", enabled = TRUE)
    pie2
  })
  
  
  output$ComposicionPlot69 <-renderHighchart({ 
    Mun69 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
    
    a <- function(x) format(x, big.mark = ",", scientific = FALSE) 
    color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
    pieline<- highchart() %>%
      hc_chart(type = 'line') %>%
      hc_legend(enabled = TRUE) %>%
      hc_xAxis(categories = Mun69$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Population')) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = FALSE))) %>%
      hc_add_series(name = 'Latin America', data = Mun69[,7]) %>%
      hc_add_series(name = 'Western Europe', data = Mun69[,8]) %>%
      hc_add_series(name = 'Eastern Europe', data = Mun69[,9]) %>%
      hc_add_series(name = 'Africa', data = Mun69[,10]) %>%
      hc_add_series(name = 'Asia', data = Mun69[,11]) %>%
      hc_title(text = paste("Foreign-born population",input$MUNICIPALITY, sep=": "),
               align = 'left')  %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    pieline
  })
  
  
  ######## FOREIGN-BORN POP EVOLUTION DOWNLOAD IMAGE #####
  
  output$fbevolution <- downloadHandler(
    filename = function() {paste("FB_population", paste(input$MUNICIPALITY, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun69 <- x1[x1$MUNICIPALITY %in% input$MUNICIPALITY, ]
      
      DF<-data.frame(GROUP=rep(c('ALatin-America', 'BWestern Europe', 'CEastern Europe', 'DAfrica', 'EAsia'), each= 17),
                     POP=c(Mun69[,7],Mun69[,8],Mun69[,9],
                           Mun69[,10],Mun69[,11]), 
                     YEAR=rep(c(2000:2016), 5))
      color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
      print(ggplot(DF, aes(YEAR, POP, group = GROUP, colour = GROUP)) +
        geom_path(alpha = 1, size=1.2)+
        #scale_y_continuous(limits=c(0, 80))+
        scale_x_continuous(breaks=seq(2000,2016, 1))+
        labs(title= paste(paste(input$MUNICIPALITY," ", "(",Mun69$PROVINCE,")",sep=""),"Foreign-born population by region of birth", sep=": "),
          subtitle = "Years: 2000-2016",
          caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
        scale_colour_manual(values = color,
                            breaks =levels(DF$GROUP), 
                            labels= c("Latin-America", "Western Europe", "Eastern Europe", "Africa", "Asia"))+
        theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 10,vjust=2),
              legend.position="none",
              legend.background = element_rect(fill="#FfFfFf"),
              strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00, colour="black", size=10),
              axis.title.y = element_text(colour="black",vjust=2, size=10),
              axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
              panel.background = element_rect(fill = "#f2f2f2"), 
              panel.grid = element_line(colour="red"),
              panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
              panel.grid.minor=element_line(colour="#f2f2f2"),
              plot.background = element_rect(fill = "#ffffff"))+
        ylab("Population"))
      dev.off()
    })
      
  
  ######## RANKING TOP 10 FOREIGN BORN ###### 
  
  output$ComposicionPlotly2 <-renderHighchart({ 
   # Munplotly2<-CAT_NAC15[CAT_NAC15$MUNICIPALITY %in% input$MUNICIPALITY, ]
    
    Munplotly2<-COUNTRYBIRTH16[COUNTRYBIRTH16$MUNICIPALITY %in% input$MUNICIPALITY, ]
    
    Munplotly2 <- Munplotly2[order(-Munplotly2$n),] 
    Munplotly2$Nom_pais<-factor(Munplotly2$Nom_pais,
                                levels=Munplotly2$Nom_pais)
    
    Munplotly2<-Munplotly2[!Munplotly2$Nom_pais=="Spain", ]
    
    
    rank <- highchart() %>%
      hc_chart(type = 'bar') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = as.character(Munplotly2$Nom_pais), title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Population (k: Thousands)')) %>%
      hc_plotOptions(series = list(dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(name = 'Population 2015', data = c(Munplotly2$n)) %>%
      hc_title(text = paste("Foreign-born population by country of birth",input$MUNICIPALITY, sep=": "),
               align = 'left')  %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      #hc_colors(c(color)) %>%
      hc_tooltip(enabled = TRUE)
    rank
  }) 
  
  ######## RANKING TOP 10 FOREIGN BORN DOWNLOAD IMAGE###### 
  output$rankingd <- downloadHandler(
    filename = function() {paste("FB_population_countries", input$MUNICIPALITY, '.png', sep='')},
    content = function(file) {
      png(file,width = 900, height = 500)
      
      Munplotly2<-COUNTRYBIRTH16[COUNTRYBIRTH16$MUNICIPALITY %in% input$MUNICIPALITY, ]
      #Munplotly2<-COUNTRYBIRTH16[COUNTRYBIRTH16$MUNICIPALITY=='Barcelona', ]
      Munplotly2 <- Munplotly2[order(Munplotly2$n),] 
      Munplotly2$Nom_pais<-factor(Munplotly2$Nom_pais,
                                  levels=Munplotly2$Nom_pais)
      
      Munplotly2<-Munplotly2[!Munplotly2$Nom_pais=="Spain", ]
      
      print(ggplot(data=Munplotly2, aes(x=Nom_pais  , y=n, fill=factor(MUNICIPALITY), label=n)) + 
              geom_bar(stat="identity", colour="black",width=.7)+
              labs(title= paste(paste(input$MUNICIPALITY,sep=""),"Foreign-born population by country of birth", sep=": "),
                   subtitle = "Year: 2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              geom_text(aes(y =n),size=5,colour="Black", hjust = 1)+
              coord_flip()+
              #facet_wrap(~ categoria2, ncol = 1)+
              #scale_fill_manual(values=rev(c(colfunc(length(unique(P429$P_429))))))+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y =  element_blank(),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff")))
      dev.off()
    })
  
  ######## MAP: POPULATION MUNICIPALITIES ##### 
  
  output$mymapbolas <- renderLeaflet({ #OK
    
    x2 <-x1[,c(1:11, 13, 30,31)]
    #x2 <-x1[,c(1:11, 13, 32,15)]
    
    colnames(x2)<- c("MUN", "PROVINCE", "MUNICIPALITY", "Census", "YEAR","Spain", 
                     "LatinAmerica", "WesternEurope", "EasternEurope", "Africa", "Asia","Totalpop", "LON", "LAT" )
    
    MunzBOLAS <- reactive({x2[(x2$PROVINCE %in% input$PROVINCE5 & 
                                 x2$YEAR %in% input$YEAR5) ,c("MUNICIPALITY", 
                                                              "MUN",input$GROUPS5,
                                                              "Totalpop","LON", "LAT" )]})  
    
    
    observe({
      if ("Latin America" %in% input$GROUPS5) {
        # choose all the choices _except_ "Select All"
        selected_choices <- "Latin America"
        updateSelectInput(session, "GROUPS", selected = selected_choices)
      }
    }) 
    
    df<- MunzBOLAS() #OK
    
    colnames(df)<-c("MUNICIPALITY", 
                    "MUN","GROUP", 
                    "Totalpop","LON", "LAT") 
    
    df<-df[df$GROUP>input$obs,]
    
    state_popup <- paste0("<strong>Municipality: </strong>", 
                          df$MUNICIPALITY, 
                          "<br><strong>ID Municipality: </strong>", 
                          df$MUN, 
                          "<br><strong>Foreign-born group population: </strong>", 
                          df$GROUP) #OK
    
    
    
    
    color <-ifelse(input$GROUPS5=="LatinAmerica", "#F8766D",
                   ifelse(input$GROUPS5=="WesternEurope", "#A3A500",
                          ifelse(input$GROUPS5=="EasternEurope", "#00BF7D",
                                 ifelse(input$GROUPS5=="Africa", "#00B0F6",
                                        ifelse(input$GROUPS5=="Asia", "#E76BF3",0))))) 
    
    
    leaflet(df) %>% addTiles() %>%
      addProviderTiles("CartoDB.Positron")%>%
      addCircles(lng = ~LON, lat = ~LAT, weight = 1,
                 radius = ~sqrt(GROUP) * input$size,  
                 stroke = TRUE,
                 color = color,
                 fillColor = color, 
                 fillOpacity = .5,
                 popup = state_popup)
    
    
    
  }) #OK
  
  
  observe({
    
    
    leafletProxy("mymapbolas", data = df) 
    
    
    
  }) #OK
  
  
  
  
  ######## MAP: POPULATION MUNICIPALITIES DOWNLOAD IMAGE ###### 
  
  output$MAP_IMAGE_BOLAS <- downloadHandler(
    filename = function() {paste("MAP", paste(input$GROUPS5, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 1000, height = 1000)
      
      x2 <-x1[,c(1:11, 13, 30,31)]
      
      colnames(x2)<- c("MUN", "PROVINCE", "MUNICIPALITY", "Census", "YEAR","Spain", 
                       "LatinAmerica", "WesternEurope", "EasternEurope", "Africa", "Asia","Totalpop", "LON", "LAT" )
      
      MunzBOLAS <- reactive({x2[(x2$PROVINCE %in% input$PROVINCE5 & 
                                   x2$YEAR %in% input$YEAR5) ,c("MUNICIPALITY", 
                                                                "MUN",input$GROUPS5,
                                                                "Totalpop","LON", "LAT" )]})
      
      df4<- MunzBOLAS() 
      colnames(df4)<-c("MUNICIPALITY", 
                       "MUN","GROUP", 
                       "Totalpop","LON", "LAT") 
      
      df4<-df4[df4$GROUP>input$obs,]
      
      #dfm<- Munzm()
      #df4 <- spTransform(df4, CRS("+proj=utm +zone=31 +ellps=GRS80 +units=m +no_defs"))
      
      color <-ifelse(input$GROUPS5=="LatinAmerica", "#F8766D",
                     ifelse(input$GROUPS5=="WesternEurope", "#A3A500",
                            ifelse(input$GROUPS5=="EasternEurope", "#00BF7D",
                                   ifelse(input$GROUPS5=="Africa", "#00B0F6",
                                          ifelse(input$GROUPS5=="Asia", "#E76BF3",0))))) 
      
      xy <- df4[,c(5,6)]
      
      spdf <- SpatialPointsDataFrame(coords = xy,  data = df4,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      
      b <- bbox(spdf)
      b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
      b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
      
      # RASTERCATGOOGLE <- ggmap(get_map(location = b))
      #RASTERCATOSM <- ggmap(get_map(location = b,source="osm"))
      RASTERCATGOOGLETONER <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", 
                                            crop = T)) 
      
      ESP<-spdf@data
      
     TITLE<-ifelse(input$GROUPS5=="LatinAmerica", "Population born in Latin America",
                   ifelse(input$GROUPS5=="WesternEurope", "Population born in Western Europe",
                   ifelse(input$GROUPS5=="EasternEurope", "Population born in Eastern Europe",
                   ifelse(input$GROUPS5=="Africa", "Population born in Africa",
                   ifelse( input$GROUPS5=="Asia", "Population born in Asia",0)))))
      
      p1 <- ggplot(ESP, aes(LON,LAT))
      
      ss<- RASTERCATGOOGLETONER+
    
        geom_point(data=ESP, aes(LON,LAT,size = GROUP),shape = 21, colour = "black",fill = color, alpha=0.5)
      
      ss1<-ss+scale_size(range = c(0, input$size/2))
      print(ss1 +labs(title= TITLE,
                      subtitle = paste("Year: ", input$YEAR5, sep=""),
                      caption = "\nElaboration: Centre d'Estudis Demogràfics/CERCA.\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
                    legend.title = element_text(angle = 0,vjust=0.5, size=20,colour="black",face="bold"),
                    legend.text = element_text(colour="black", size = 20),
                    legend.position = "NONE",
                    legend.background = element_rect(fill=NA),
                    legend.key.size = unit(2.5, "lines"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=20,colour="black",face="bold"),
                    axis.title.x = element_text(face="bold", colour="black", size=20),
                    axis.text.x  = element_text(angle = 0,vjust=0.5, size=20,colour="black"),
                    axis.title.y = element_text(face="bold", colour="black", size=20),
                    axis.text.y  = element_text(vjust=0.5, size=20,colour="black"),
                    panel.grid.major=element_line(colour="black"),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    panel.background = element_rect(fill = "transparent",colour = NA))+
              ylab("Latitude")+
              xlab("Longitude"))
      
      dev.off()
    }) 
  
  
  ######## MAP: POPULATION MUNICIPALITIES DOWNLOAD DATA ###### 
  
  output$downloadDatabolas <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste(input$GROUPS5, input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      x2 <-x1[,c(1:11, 13, 30,31)]
      
      colnames(x2)<- c("MUN", "PROVINCE", "MUNICIPALITY", "Census", "YEAR","Spain", 
                       "LatinAmerica", "WesternEurope", "EasternEurope", "Africa", "Asia","Totalpop", "LON", "LAT" )
      
      MunzBOLAS <- reactive({x2[(x2$PROVINCE %in% input$PROVINCE5 & 
                                   x2$YEAR %in% input$YEAR5) ,c("MUNICIPALITY", 
                                                                "MUN",input$GROUPS5,
                                                                "Totalpop","LON", "LAT" )]})

      
      df4<- MunzBOLAS() 
      colnames(df4)<-c("MUNICIPALITY", 
                       "MUN","GROUP", 
                       "Totalpop","LON", "LAT") 
      
      df4<-df4[df4$GROUP>input$obs,]
      write.table(df4, file, sep = sep,row.names = FALSE)
    })
  
  
  
 
  
  
  
  
  ######## RESIDENTIAL SEGREGATION ###### 
  observe({
    updateSelectizeInput(session, 
                         'MUNICIPALITY2',
                         choices = unique(x1[x1$COM==input$prov2, "MUNICIPALITY"]),
                         selected= ifelse(input$prov2=="Basque Country", "Bilbao",
                                   ifelse(input$prov2=="Castilla la Mancha", "Albacete",
                                   ifelse(input$prov2=="Valencia", "Valencia",
                                   ifelse(input$prov2=="Andalusia", "Sevilla",
                                   ifelse(input$prov2=="Castile and Leon", "Ãvila",       
                                   ifelse(input$prov2=="Extremadura", "CÃ¡ceres",
                                   ifelse(input$prov2=="Balearic Islands", "Palma de Mallorca",
                                   ifelse(input$prov2=="Catalonia", "Barcelona",
                                   ifelse(input$prov2=="Galicia", "Pontevedra",
                                   ifelse(input$prov2=="Arago", "Zaragoza",
                                   ifelse(input$prov2=="La Rioja", "LogroÃ±o",
                                   ifelse(input$prov2=="Madrid", "Madrid",
                                   ifelse(input$prov2=="Murcia", "Murcia",
                                   ifelse(input$prov2=="Navarra", "Pamplona/IruÃ±a",
                                   ifelse(input$prov2=="Asturies", "GijÃ³n",
                                   ifelse(input$prov2=="Canary Islands", "Palmas de Gran Canaria, Las",
                                   ifelse(input$prov2=="Cantabria", "Santander",
                                          0))))))))))))))))))
  })
  output$ComposicionPlot4 <-renderHighchart({ 
    Mun4 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun4 <-  Mun4[order(Mun4$YEAR),] 
    Mun4 <- cbind(Mun4[,1:16,],round(Mun4[,17:22], digits=2))
    color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
    
    
    DISSI <- highchart() %>%
      hc_chart(type = 'line') %>%
      hc_legend(enabled = TRUE) %>%
      hc_xAxis(categories = Mun4$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Dissimilarity index'), ceiling=100) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = FALSE))) %>%
      hc_add_series(name = 'Latin America', data = round((Mun4[,14]),2)) %>%
      hc_add_series(name = 'Western Europe', data = round((Mun4[,15]),2)) %>%
      hc_add_series(name = 'Eastern Europe', data = round((Mun4[,16]),2)) %>%
      hc_add_series(name = 'Africa', data = round((Mun4[,17]),2)) %>%
      hc_add_series(name = 'Asia', data = round((Mun4[,18]),2)) %>%
      
      hc_title(text =paste("Dissimilarity Index", paste(input$MUNICIPALITY2," ", "(",unique(Mun4$PROVINCE),")",sep=""),
                            sep=": "),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_colors(c(color)) %>%
      hc_tooltip(enabled = TRUE)
    DISSI
  })
  output$ComposicionPlot5 <-renderHighchart({ 
    Mun5 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun5 <-  Mun5[order(Mun5$YEAR),] 
   # Mun5 <- cbind(Mun5[,1:22,],round(Mun5[,23:28], digits=2))
    color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
    IS <- highchart() %>%
      hc_chart(type = 'line') %>%
      hc_legend(enabled = TRUE) %>%
      hc_xAxis(categories = Mun5$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Isolation index'), ceiling=60) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = FALSE))) %>%
      hc_add_series(name = 'Latin America', data = round(Mun5[,20],2)) %>%
      hc_add_series(name = 'Western Europe', data = round(Mun5[,21],2)) %>%
      hc_add_series(name = 'Eastern Europe', data = round(Mun5[,22],2)) %>%
      hc_add_series(name = 'Africa', data = round(Mun5[,23],2)) %>%
      hc_add_series(name = 'Asia', data = round(Mun5[,24],2)) %>%
      
      hc_title(text =paste("Isolation Index",paste(input$MUNICIPALITY2," ", "(",unique(Mun5$PROVINCE),")",sep=""),
                            sep=": "),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_colors(c(color)) %>%
      hc_tooltip(enabled = TRUE)
    IS
    
  })
  
  output$ComposicionPlot8 <-renderHighchart({ 
    Mun8 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
    Mun8 <-  Mun8[order(Mun8$YEAR),] 
    CENSUS <- highchart() %>%
      hc_chart(type = 'line') %>%
      hc_legend(enabled = FALSE) %>%
      hc_xAxis(categories = Mun8$YEAR, title = list(text = '')) %>%
      hc_yAxis(title = list(text = 'Census tracts')) %>%
      hc_plotOptions(column = list(stacking = "normal"),
                     series = list(dataLabels = list(enabled = FALSE))) %>%
      hc_add_series(name = 'Census tracts', data = Mun8[,4]) %>%
      hc_title(text =paste("Number of Census Tract",paste(input$MUNICIPALITY2," ", "(",unique(Mun8$PROVINCE),")",sep=""), sep=": "),
               align = 'left') %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      # hc_colors(c(color)) %>%
      hc_tooltip(enabled = TRUE)
    CENSUS
  })
  

  ######## RESIDENTIAL SEGREGATION DOWNLOAD IMAGES ###### 
  
  output$downloadPlotDIS <- downloadHandler(
    filename = function() {paste("Dissimilarity plot", paste(input$MUNICIPALITY2, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun4 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
      Mun4 <-  Mun4[order(Mun4$YEAR),] 
      MunDIS <-data.frame(YEAR=rep(2000:2016,5),
                          Dissimilarity=c(Mun4[1:17,14],Mun4[1:17,15],
                                          Mun4[1:17,16],Mun4[1:17,17],
                                          Mun4[1:17,18]),
                          Origin=c(rep("[Dissimilarity Index]Latin America", 17),
                                   rep("[Dissimilarity Index]Western Europe", 17),
                                   rep("[Dissimilarity Index]Eastern Europe", 17),
                                   rep("[Dissimilarity Index]Africa", 17),
                                   rep("[Dissimilarity Index]Asia", 17)))
      MunDIS$Origin <- factor(MunDIS$Origin, 
                              levels = c("[Dissimilarity Index]Latin America",
                                         "[Dissimilarity Index]Western Europe", 
                                         "[Dissimilarity Index]Eastern Europe",
                                         "[Dissimilarity Index]Africa",
                                         "[Dissimilarity Index]Asia"))
      color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
      print(ggplot(MunDIS, aes(YEAR, Dissimilarity, group = Origin, colour = Origin)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(0, 80))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              labs(title= paste(paste(input$MUNICIPALITY2," ", "(",Mun4$PROVINCE,")",sep=""),"Residential Segregation", sep=": "),
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo,INE)")+
              scale_colour_manual(values = color,
                                  breaks =levels(MunDIS$Origin), 
                                  labels= c("Latin-America", "Western Europe", "Eastern Europe", "Africa", "Asia"))+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index"))
      dev.off()
    })
  output$downloadPlotIS <- downloadHandler(
    filename = function() {paste("Isolation plot", paste(input$MUNICIPALITY2, '.png', sep=''), sep=" ")},
    content = function(file) {
      png(file,width = 800, height = 500)
      Mun5 <- x1[x1$MUNICIPALITY2 %in% input$MUNICIPALITY2, ]
      Mun5 <-  Mun5[order(Mun5$YEAR),] 
      MunIS <-data.frame(YEAR=rep(2000:2016,5),
                         Isolation=c(Mun5[1:17,20],Mun5[1:17,21],
                                     Mun5[1:17,22],Mun5[1:17,23],
                                     Mun5[1:17,24]),
                         Origin=c(rep("[Isolation Index]Latin America", 17),
                                  rep("[Isolation Index]Western Europe", 17),
                                  rep("[Isolation Index]Eastern Europe", 17),
                                  rep("[Isolation Index]Africa", 17),
                                  rep("[Isolation Index]Asia", 17)))
      MunIS$Origin <- factor(MunIS$Origin, 
                             levels = c("[Isolation Index]Latin America",
                                        "[Isolation Index]Western Europe", 
                                        "[Isolation Index]Eastern Europe",
                                        "[Isolation Index]Africa",
                                        "[Isolation Index]Asia"))
      color =c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
      print(ggplot(MunIS, aes(YEAR, Isolation, group = Origin, colour = Origin)) +
              geom_path(alpha = 1, size=1.2)+
              #scale_y_continuous(limits=c(0, 50))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              labs(title= paste(paste(input$MUNICIPALITY2," ", "(",Mun5$PROVINCE,")",sep=""),"Residential Segregation", sep=": "),
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              scale_colour_manual(values = color,
                                  breaks =levels(MunIS$Origin), 
                                  labels= c("Latin-America", "Western Europe", "Eastern Europe", "Africa", "Asia"))+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="none",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Isolation Index"))
      dev.off()
    })
  
  ######## RESIDENTIAL SEGREGATION DOWNLOAD DATA ###### 
  
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
  
  
  
  ######## POPULATION COMPARE TOTAL #### 
  output$comtotal <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,13)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGLA)[3]<-'Total_pop'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Total_pop,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Popultaion")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE TOTAL DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_tot <- downloadHandler(
    filename = function() {paste("Total_Pop", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,13)]
      colnames(Mun9)[3]<-'Total_pop'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, Total_pop, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Total population",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population"))
      dev.off()
    })
  
  
  ######## POPULATION COMPARE TOTALfb ####
  output$comfbtotal <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,32)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGLA)[3]<-'Total_popfb'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Total_popfb,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Popultaion")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE TOTALfb  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_tot_fb <- downloadHandler(
    filename = function() {paste("Total_Pop_fb", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,32)]
      colnames(Mun9)[3]<-'Total_popfb'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, Total_popfb, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Foreign-born population",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population"))
      dev.off()
    })
  
  
  ######## POPULATION COMPARE FOREING (%) ####
  output$comper <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,28)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'Foreing_born'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Foreing_born,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Foreign-born (%)")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE FOREING (%)  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_tot_fb2 <- downloadHandler(
    filename = function() {paste("Total_Pop_fb_per", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,28)]
      colnames(Mun9)[3]<-'FB'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, FB, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Foreign-born population (%)",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Foreign-born population (%)"))
      dev.off()
    })
  
  
  
  ######## POPULATION COMPARE  LA ####
  output$comla <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,7)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'LA'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(LA,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE  LA  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_la <- downloadHandler(
    filename = function() {paste("Total_LA", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,7)]
      colnames(Mun9)[3]<-'LA'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, LA, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Latin-America",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population)"))
      dev.off()
    })
  
  
  ######## POPULATION COMPARE  WE ####
  output$comwe <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,8)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'WE'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(WE,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE  WE  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_we <- downloadHandler(
    filename = function() {paste("Total_WE", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,8)]
      colnames(Mun9)[3]<-'WE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, WE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Western Europe",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population)"))
      dev.off()
    })
  
  ######## POPULATION COMPARE  EE ####
  output$comee <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,9)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'EE'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(EE,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE  EE  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_ee <- downloadHandler(
    filename = function() {paste("Total_EE", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,9)]
      colnames(Mun9)[3]<-'EE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, EE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Eastern Europe",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population)"))
      dev.off()
    })
  
  
  ######## POPULATION COMPARE  AF ####
  output$comaf <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,10)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'AF'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(AF,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE  AF  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_af <- downloadHandler(
    filename = function() {paste("Total_AF", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,10)]
      colnames(Mun9)[3]<-'AF'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, AF, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Africa",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population)"))
      dev.off()
    })
  
  ######## POPULATION COMPARE  AS #####
  output$comas <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,11)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,28)]
    colnames(MunSEGLA)[3]<-'AS'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(AS,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Population")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## POPULATION COMPARE  AS  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_pop_as <- downloadHandler(
    filename = function() {paste("Total_AS", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,11)]
      colnames(Mun9)[3]<-'AS'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, AS, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Asia",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Population)"))
      dev.off()
    })
  
  
  
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE DOWNLOAD DATA ###### 
  
  output$downloadPOPCOM <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Population", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,
                     c("YEAR", "Census tracts","MUNICIPALITY3", "[Pop]Total Population",
                       "[Pop]Total Foreign-born Population","Percentage_Foreign","[Pop]Latin America",
                       "[Pop]Western Europe","[Pop]Eastern Europe","[Pop]Africa" ,
                       "[Pop]Asia" ) ], 
                  file, sep = sep,row.names = FALSE)
    })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE LA ######
  output$segcomla <-renderHighchart({ 
    
    MunSEGLA <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,c(3,5,14)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGLA)[3]<-'Dissimilarity_LATINOAMERICA'
    MunSEGLA <-  MunSEGLA[order(MunSEGLA$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
     #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGLA)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Dissimilarity_LATINOAMERICA,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Dissimilarity Index"),
               min=input$min, 
               max=input$max) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE LA  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_seg_la <- downloadHandler(
    filename = function() {paste("D_LA", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,14)]
      colnames(Mun9)[3]<-'VALUE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, VALUE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
               scale_y_continuous(limits=c(input$min, input$max))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Dissimilarity Latin-America",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index)"))
      dev.off()
    })
  
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE WE #####
  output$segcomwe <-renderHighchart({ 
    
    MunSEGWE <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,c(3,5,15)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGWE)[3]<-'Dissimilarity_WESTERNEUROPE'
    MunSEGWE <-  MunSEGWE[order(MunSEGWE$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGWE)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Dissimilarity_WESTERNEUROPE,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Dissimilarity Index"),
               min=input$min, 
               max=input$max) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## RESIDENTIAL SEGREGATION COMPARE WE  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_seg_we <- downloadHandler(
    filename = function() {paste("D_WE", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,15)]
      colnames(Mun9)[3]<-'VALUE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, VALUE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(input$min, input$max))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Dissimilarity Western Europe",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index)"))
      dev.off()
    })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE EE ####
  output$segcomee <-renderHighchart({ 
    
    MunSEGWE <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,c(3,5,16)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGWE)[3]<-'Dissimilarity_EASTERNEUROPE'
    MunSEGWE <-  MunSEGWE[order(MunSEGWE$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGWE)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Dissimilarity_EASTERNEUROPE,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Dissimilarity Index"),
               min=input$min, 
               max=input$max) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE EE  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_seg_ee <- downloadHandler(
    filename = function() {paste("D_EE", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,16)]
      colnames(Mun9)[3]<-'VALUE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, VALUE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(input$min, input$max))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Dissimilarity Eastern Europe",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index)"))
      dev.off()
    })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE AF ####
  output$segcomaf <-renderHighchart({ 
    
    MunSEGWE <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,c(3,5,17)]
    #MunSEGLA <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,14)]
    colnames(MunSEGWE)[3]<-'Dissimilarity_AFRICA'
    MunSEGWE <-  MunSEGWE[order(MunSEGWE$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGWE)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Dissimilarity_AFRICA,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Dissimilarity Index"),
               min=input$min, 
               max=input$max) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## RESIDENTIAL SEGREGATION COMPARE Af  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_seg_af <- downloadHandler(
    filename = function() {paste("D_AF", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,17)]
      colnames(Mun9)[3]<-'VALUE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, VALUE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(input$min, input$max))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Dissimilarity Africa",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index)"))
      dev.off()
    })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE AS ####
  output$segcomas <-renderHighchart({ 
    
    MunSEGWE <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,c(3,5,18)]
    #MunSEGWE <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,18)]
    colnames(MunSEGWE)[3]<-'Dissimilarity_ASIA'
    MunSEGWE <-  MunSEGWE[order(MunSEGWE$YEAR),] 
    #Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
    #                                            Diversity = mean(Diversity))
    
    #Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(MunSEGWE)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    COMLA <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Dissimilarity_ASIA,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Dissimilarity Index"),
               min=input$min, 
               max=input$max) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    COMLA
  })
  
  ######## RESIDENTIAL SEGREGATION COMPARE As  DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_seg_as <- downloadHandler(
    filename = function() {paste("D_AS", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3c,c(3,5,18)]
      colnames(Mun9)[3]<-'VALUE'
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      
      MunFINAL <-rbind(Mun9)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, VALUE, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              scale_y_continuous(limits=c(input$min, input$max))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Dissimilarity Asia",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Dissimilarity Index)"))
      dev.off()
    })
  
  
  ######## RESIDENTIAL SEGREGATION COMPARE DOWNLOAD DATA ###### 
  
  output$downloadSEGCOM <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("Dissimilarity_Index", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      # Write to a file specified by the 'file' argument
      write.table(x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3B,
                     c("YEAR", "Census tracts","MUNICIPALITY3", "[Dissimilarity Index]Latin America",
                       "[Dissimilarity Index]Western Europe","[Dissimilarity Index]Eastern Europe","[Dissimilarity Index]Africa",
                       "[Dissimilarity Index]Asia") ], 
                  file, sep = sep,row.names = FALSE)
    })
  
  ######## POPULATION DIVERSITY ###### 
  
  output$ComposicionPlot9 <-renderHighchart({ 
    
    Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(3,5,29)]
    #Mun9 <- x1[(x1$MUNICIPALITY3 =="Barcelona"| x1$MUNICIPALITY3 =="Madrid"),c(3,5,29)]
    
    Mun9 <-  Mun9[order(Mun9$YEAR),] 
    Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
                                                 Diversity = mean(Diversity))
    
    Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(Mun9,Mun14)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    
    SIMPSON <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(Diversity,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories = unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Simpson's Diversity Index")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
    hc_tooltip(enabled = TRUE)
    SIMPSON
  })
  
  
  output$entropy <-renderHighchart({ 
    
    Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(3,5,46)]
    Mun9 <-  Mun9[order(Mun9$YEAR),] 
    Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
                                                 ENTROPY = mean(ENTROPY))
    
    Mun14<- Mun14[c(2,1,3)]
    MunFINAL <-rbind(Mun9,Mun14)
    color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
    ENTROPY <- hchart(MunFINAL, "line", hcaes(x = as.character(YEAR), y = round(ENTROPY,2), group = MUNICIPALITY))%>%
      hc_xAxis(categories =unique(MunFINAL$YEAR), title = list(text = '')) %>%
      hc_yAxis(title = list(text = "Entropy Index")) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_colors(c(color)) %>%
      hc_credits(enabled = TRUE, text = "HIGHCHARTS",href = "http://www.highcharts.com/")%>%
      hc_tooltip(enabled = TRUE)
    ENTROPY
   
  })
  
  ######## POPULATION DIVERSITY DOWNLOAD IMAGES ###### 
  
  output$MAP_IMAGE_div <- downloadHandler(
    filename = function() {paste("Diversity plot", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(3,5,29)]
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
                                                   Diversity = mean(Diversity))
      
      Mun14<- Mun14[c(2,1,3)]
      MunFINAL <-rbind(Mun9,Mun14)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, Diversity, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
             # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Simpson's Diversity Index by municipality",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Simpson's Diversity Index"))
      dev.off()
    })
  
  output$MAP_IMAGE_div_e <- downloadHandler(
    filename = function() {paste("Diversity plot", '.png', sep='')},
    content = function(file) {
      png(file,width = 800, height = 500)
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(3,5,46)]
      #Mun9 <- x1[x1$MUNICIPALITY3=="Barcelona",c(3,5,46)]
      
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
                                                   ENTROPY = mean(ENTROPY))
      
      Mun14<- Mun14[c(2,1,3)]
      MunFINAL <-rbind(Mun9,Mun14)
      
      color <-gg_color_hue(length(unique(MunFINAL$MUNICIPALITY)))
      
      print(ggplot(MunFINAL, aes(YEAR, ENTROPY, group = MUNICIPALITY, colour = MUNICIPALITY)) +
              geom_path(alpha = 1, size=1.2)+
              # scale_y_continuous(limits=c(1, 2))+
              scale_x_continuous(breaks=seq(2000,2016, 1))+
              scale_colour_manual(values = color,
                                  breaks =unique(MunFINAL$MUNICIPALITY), 
                                  labels= c(unique(MunFINAL$MUNICIPALITY)))+
              labs(title= "Entropy Diversity Index by municipality",
                   subtitle = "Years: 2000-2016",
                   caption = "\nElaboration: GEDEM / Centre d'Estudis Demogràfics/CERCA\nData: Population Register (Padrón Continuo, INE)")+
              theme(plot.title = element_text(lineheight=5.6, vjust=1.5,size=12, face="bold"),
                    legend.title = element_blank(),
                    legend.text = element_text(colour="black", size = 10,vjust=2),
                    legend.position="bottom",
                    legend.background = element_rect(fill="#FfFfFf"),
                    strip.text=element_text(angle = 0,vjust=0.5, size=10,colour="black",face="bold"),
                    axis.title.x = element_blank(),
                    axis.text.x  = element_text(angle = 00, colour="black", size=10),
                    axis.title.y = element_text(colour="black",vjust=2, size=10),
                    axis.text.y  = element_text(vjust=0.5, size=10,colour="black"),
                    panel.background = element_rect(fill = "#f2f2f2"), 
                    panel.grid = element_line(colour="red"),
                    panel.grid.major=element_line(colour="#FfFfFf",size = .2), 
                    panel.grid.minor=element_line(colour="#f2f2f2"),
                    plot.background = element_rect(fill = "#ffffff"))+
              ylab("Simpson's Diversity Index"))
      dev.off()
    })
  
  ######## POPULATION DIVERSITY DOWNLOAD DATA ###### 
  
  output$downloadDiv <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("SDI", input$filetype, sep = ".")
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      sep <- switch(input$filetype, "csv" = ",", "txt" = "\t")
      
      
      Mun9 <- x1[x1$MUNICIPALITY3 %in% input$MUNICIPALITY3,c(3,5,29)]
      Mun9 <-  Mun9[order(Mun9$YEAR),] 
      Mun14 <- x1 %>% group_by(YEAR) %>% summarise(MUNICIPALITY="Mean Diversity Spain",
                                                   Diversity = mean(Diversity))
      
      Mun14<- Mun14[c(2,1,3)]
      MunFINAL <-rbind(Mun9,Mun14)
      # Write to a file specified by the 'file' argument
      write.table(MunFINAL,file, sep = sep,row.names = FALSE)
    })
  
  
  
  ######## DATA TABLE ###### 
  
  updateSelectizeInput(session, "MUNICIPALITY6",
                       choices=unique(x1$MUNICIPALITY6),
                       selected="Barcelona")
  updateSelectizeInput(session, "YEAR",
                       choices=unique(x1$YEAR),
                       selected=c("2000","2016"))
  output$usertable <-renderDataTable({ 
    
    MunTA <- x1[(x1$MUNICIPALITY%in%input$MUNICIPALITY6 &
                   x1$YEAR %in%input$YEAR),input$VARIABLES, drop = FALSE ]
    MunTA <-  MunTA[order(MunTA$YEAR),] 
    
  })
  
  ######## DATA TABLE DOWNLOAD DATA ###### 
  
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