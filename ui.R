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

###### PREPARATION ######
load("./data/CATALONIA14UTF2.Rdata")
load("./data/PIR.Rdata")
load("./data/COUNTRYBIRTH.Rdata")


#load("C:\\Users\\jgaleano\\Dropbox\\R\\SHINY\\NEW_DP\\ROSTOSCKN\\CATALONIA\\data\\PIR.Rdata")


lookup1 <- c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015" )
x1<-DATOS
x1$EXT<-NULL
x1$POP_TOTAL_EXT <- x1$POP_TOTAL-x1$POP_SPANISH
x1 <-  x1[order(x1$YEAR),] 
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
#x1<- x1[c(1:13,32,30,33,14:25,31,26:29,34:38)] 
PROVS <-unique(x1$PROVINCE)
PROVS1<-c(PROVS[1:34],PROVS[36:37],PROVS[39:50])

GROUPS <- structure(c("LatinAmerica", "WesternEurope", "EasternEurope", "Africa", "Asia"), 
                    .Names = c("Latin America", "Western Europe", "Eastern Europe", "Africa", "Asia" ))


TERMS <- structure(c("Absolute", "Relative"), 
                   .Names = c("Absolute", "Relative"))



####### SHINY UI #####

shinyUI(fluidPage(
  tags$head(includeScript("google_analytics.js")),
  navbarPage(div(img(src="logotipCED_nuevo.png",height = 60, width = 160),
                       title="WEB CED", href="http://ced.uab.es"),
                       windowTitle="Population change: Spain XXI", 
                       theme = shinytheme("spacelab"),  
                
                  
  ###### POPULATION COMPOSITION #####                 
                   navbarMenu(h5("Population composition"),
                              
                              ##### POPULATION PYRAMID #####
                              
                              tabPanel(h5(span("Population structure by age, sex and place of birth by regions (2000-2016)" )), 
                                       sidebarPanel(
                                         #tags$head(
                                         # tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                         #),
                                        # h5(strong("Population structure by age, sex and place of birth by regions")),
                                         #p("Explore composition by age, sex and place of birth of the population between 2000 and 2016."),
                                         selectInput("provp", "Choose a region from the list or type its name:", 
                                                     choices=unique(PIR$COM), selected="Catalonia"),
                                         
                                         # Slider: choose year
                                         sliderInput("year", "Select a year or push the play button:",  min=2000, max=2016, value=2016,post=000, animate = TRUE),
                                         
                                         selectizeInput("terms", "Select a magnitude:", 
                                                        choices=TERMS, selected="Relative"),
                                         # HTML info
                                         radioButtons("filetype", h5(strong("Download data, choose a file extension:")),
                                                      choices = c("csv", "txt")),
                                         downloadLink('downloadDataPy', strong('Download data')),br(),
                                         h5(strong("Download charts:")),
            
                                         downloadLink('downloadPYR', strong('Population pyramid')),
                                         h5(strong("Elaboration:")),
                                         p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                         h5(strong("Share")),
                                         p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                                       ),
                                       
                                       ### Main Panel
                                       mainPanel(align="center",
                                         # Show the plot
                                         highchartOutput("pyramidrel", height="600px",width="720px")
                                       )),
                              
                              
                              ##### POPULATION COMPOSITION #####
                              
                              tabPanel(h5(span("Population composition (place of birth) by municipality (2000-2016)" )),     
                                       fluidPage(
                                         sidebarPanel(
                                           #tags$head(
                                           #tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                           #),
                                         #  h5(strong("Population composition 2000-2016:")),
                                          # p("Explore population composition (place of birth) of each municipality between 2000 and 2016."),
                                           selectInput("prov", "Choose a region from the list or type its name:", 
                                                       choices=unique(x1$COM), selected="Catalonia"),
                                           selectizeInput(
                                             inputId = "MUNICIPALITY", 
                                             label = "Select a municipality from the list or type its name:",
                                             multiple  = F,
                                             choices =   unique(x1[["MUNICIPALITY"]])
                                           ),
                                         
                                           
                                           h5(strong("Download data:")),
                                           p("Download and re-use data associated with the stacked bar plots as a *csv or *txt file.
                                             Data shows the composition of the population by place of birth (Spanish-born vs foreign-born)
                                             for the selected municipality between 2000 and 2016. Attention: UTF-8 encoding and dot as decimal separator."),
                                           radioButtons("filetype", h5(strong("File extension:")),
                                                        choices = c("csv", "txt")),
                                           downloadLink('downloadData3', strong('Download data')),br(),
                                           h5(strong("Download charts:")),
                                          
                                           downloadLink('downloadPlot2', strong('Population by place of birth')),br(),
                                           downloadLink('fbevolution', strong('Foreign-born population by region of birth')),br(),
                                           downloadLink('rankingd', strong('Foreign-born population by country of birth')),
                                           
                                           h5(strong("Elaboration:")),
                                           p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                           h5(strong("Share")),
                                           p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))),    
                                         mainPanel( 
                                           tabsetPanel(
                                             tabPanel(h6('Population by place of birth'),
                                                      br(),
                                                      splitLayout(cellWidths = c("48.5%", "48.5%"),
                                                                  highchartOutput("ComposicionPlot2", height = 550),
                                                                  highchartOutput("ComposicionPlot3", height = 550))),
                                                  
                                             tabPanel(h6('Foreign-born population'),
                                                      br(),
                                                      
                                                      highchartOutput("ComposicionPlot69", width = "100%", height = "300px"),
                                                      splitLayout(cellWidths = c("49%", "49%"),
                                                                  highchartOutput("ComposicionPlot6"),
                                                                  highchartOutput("ComposicionPlot7"))),
                                             tabPanel(h6("Foreign-born population by country of birth 2016 (Main origins)"),
                                                      br(),
                                                      highchartOutput("ComposicionPlotly2", width = "100%", height = "550px")),
                                             tabPanel(h6('About Population Groups'),
                                                      br(),
                                                      p("Population is grouped by region of birth (Spain, Latin-America, 
                                                        Western Europe*, Eastern Europe*, Africa, Asia and others*.)"),
                                                      p("*Western Europe includes: Germany, Andorra, Austria, Belgium, Denmark, Finland, France, Iceland, Ireland, Italy, 
                                                        Liechtenstein, Luxembourg, Malta, Monaco, Norway, Netherlands, Portugal, United Kingdom, San Marino, Sweden, 
                                                        Switzerland, Vatican City."),
                                                      p("*Eastern Europe includes:  Albania, Armenia, Belarus, Bosnia and Herzegovina, Bulgaria, Croatia, Slovenia, Estonia, 
                                                        Georgia, Hungary, Latvia, Lithuania, Moldova, Poland, Romania, Slovak Republic, Czech Republic, Russia, Serbia and 
                                                        Montenegro, Macedonia Ukraine and Cyprus."),
                                                      p("*Others: those people who could not prove their place of 
                                                        birth when they registered at the municipality."))
                                                      )))),
                              
                              #### MAP POPULATION COMPOSITION MUNICIPALITIES ####
                              
                              tabPanel(h5(span("Foreign-born population distribution by municipalities (2000-2016)" )),                          
                                       fluidPage(
                                         sidebarPanel(
                                           #  tags$head(
                                           #   tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                           #),
                                          # h5(strong("Foreign-born population distribution by municipalities (2000-2016):")),
                                        
                                           sliderInput("YEAR5", "Select a year or push the play button:",  min=2000, max=2016, value=2016,post=000, animate = TRUE),
                                           # selectizeInput('YEAR5', label = 'Select a year', choices = lookup1, selected = "2016"),
                                           selectizeInput("PROVINCE5", "Choose a province from the list or type its name:", 
                                                          choices=unique(x1$PROVINCE),selected = PROVS1,
                                                          multiple = TRUE),
                                           selectizeInput("GROUPS5", "Select a foreign-born population group", 
                                                          choices=GROUPS, selected = "LatinAmerica",multiple = F),
                                           numericInput("obs", "Minimum number of residentes:", 50),
                                           numericInput("size", "Maximum size of bubbles:", 150),
                                           h5(strong("Download data:")),
                                           p("Download and re-use data associated with the map as a *csv or *txt file. 
                                             Attention: UTF-8 encoding and dot as decimal separator."),
                                           radioButtons("filetype", h5(strong("File extension:")),
                                                        choices = c("csv", "txt")),
                                           downloadLink('downloadDatabolas', strong('Download data')),br(),
                                           downloadLink('MAP_IMAGE_BOLAS',strong('Download an image of the map')),
                                           h5(strong("Elaboration:")),
                                           p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                           h5(strong("Share")),
                                           p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                                           ), 
                                         mainPanel( 
                                           
                                       
                                                      leafletOutput("mymapbolas",width = "100%", height = 700))))
                              
                                                      ), 
  
                             
  
  
 
  
   ####### RESIDENTIAL SEGREGATION AND DIVERSITY INDEXES #####                
  navbarMenu(h5("Residential segregation and diversity indexes"),
             
             ##### RESIDENTIAL SEGREGATION #####
             
             tabPanel(h5(span("Residential Segregation by municipality (2000-2016)" )),          
                      sidebarPanel(
                        #tags$head(
                        #  tags$style(type = "text/css", "a{color: #6ec3be;}")
                        #),
                        h5(strong("Residential Segregation 2000-2016:")),
                        p("Explore the evolution of two residential segregation indexes (Dissimilarity and Isolation) 
                          for different population groups between 2000 and 2016. 
                          A line chart with the number of census tracts of the selected municipality is provided 
                          in order to facilitate the interpretation of the observed trends."),
                        selectInput("prov2", "Choose a region from the list or type its name:", 
                                    choices=unique(x1$COM), selected="Catalonia"),
                        selectizeInput(
                          inputId = "MUNICIPALITY2", 
                          label = "Select municipality from the list or type its name:",
                          multiple  = F,
                          choices =   unique(x1[["MUNICIPALITY"]])
                        ),
                        h5(strong("Download data:")),
                        p("Download and re-use data associated with the line plots as a *csv or *txt file.
                          Data shows the evolution of dissimilarity and isolation indexes of different migrant groups 
                          (born in Latin-America, Western Europe, Eastern Europe, Africa and Asia) 
                          for the selected municipality between 2000 and 2016. Attention: UTF-8 encoding and dot as decimal separator."),
                        radioButtons("filetype", h5(strong("File extension:")),
                                     choices = c("csv", "txt")),
                        downloadLink('downloadData4', strong('Download data')),br(),
                        h5(strong("Download charts:")),
                        downloadLink('downloadPlotDIS',strong('Dissimilarity index')),br(),
                        downloadLink('downloadPlotIS', strong('Isolation index')),br(),
                        h5(strong("Elaboration:")),
                        p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                        h5(strong("Share")),
                        p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                        ),    
                      mainPanel( 
                        tabsetPanel(
                          tabPanel(h6('Residential segregation indexes'),
                                   br(),
                                   splitLayout(cellWidths = c("49%", "49%"),
                                               highchartOutput("ComposicionPlot4", height = "550px"),
                                               highchartOutput("ComposicionPlot5", height = "550px"))),
                          tabPanel(h6('Census tracts'),
                                   br(),        
                                   highchartOutput("ComposicionPlot8", width = '100%')),
                          tabPanel(h6('About the indexes'),
                                   h4("Methodology"),
                                   p("Residential segregation can be defined in general terms as the degree to which two or more groups of populations 
                                     live separated from each other in a shared physical space (neighbourhood, municipality, province). 
                                     Here we present the evolution of the dissimilarity and isolation indexes for five different foreign-born groups at the municipal level. 
                                     The first of these indexes of residential segregation shows the degree of shared space between two population groups.
                                     The score of the dissimilarity can be simply interpreted
                                     as the percentage of the foreign-born population that should have changed their place of residence in the province of 
                                     reference in order to replicate the spatial distribution of the Spanish-born population. 
                                     In the case of the isolation index the resulting value can be interpreted as the probability 
                                     of whether, when two people in the reference municipality are taken randomly, 
                                     they would belong to the same group. The formulas for the dissimilarity and isolation indexes 
                                     can be expressed as follows:",
                                     withMathJax("$$D=\\frac{1}{2}{\\sum_{i =1}\\Bigl\\lvert\\frac{x_i}{X}-\\frac{y_i}{Y}\\lvert}*100$$"),
                                     withMathJax("$$P^*=\\sum_{i =1}\\frac{x_i}{X}*\\frac{y_i}{t_i}*100$$"),
                                     p("Where ", em("x"), "is the population of the ", em("X"), " type within the ", em("i")," area, e.g. census tracts; ",
                                       em("y"), "is the population of the type ", em("Y")," within the ", em("i")," area, ", em("X"), " is the total ", em("X"),
                                       " population of the large geographic entity for 
                                       which the index is being calculated, ", em("Y"), " is the total ", em("Y")," population of the large geographic entity for 
                                       which the index is being calculated and", em("t"), " is the total population of the ", em("i"), "area. The indexes are multiplied by 100
                                       to facilitate interpretation of the results."),
                                     h5("Reference:"),
                                     p(a("Duncan, D. & Duncan, B. (1955), A Methodological Analysis of Segregation Indexes, 
                                         American Sociological Review, Vol. 20, 210-217.",
                                         href="http://personal.psc.isr.umich.edu/yuxie-web/files/demtech/Duncan_Duncan1955.pdf")),
                                     p("Lieberson, S. (1981), An asymmetrical approach to segregation . In C. Peach, V. Robinson, & S. Smith, editors, 
                                       Ethnic Segregation in the Cities. London: Croom Helm, 61-82."),
                                     p(a("Massey, D. & Denton, N. (1988) 
                                         The Dimensions of Residential Segregation. Social Forces, Vol. 67, 281-315.",
                                         href="http://sf.oxfordjournals.org/content/67/2/281.abstract")),
                                     h5("Note:"),
                                     p("Population is grouped by region of birth (Spain, Latin-America, 
                                       Western Europe*, Eastern Europe*, Africa, Asia and others*.)"),
                                     p("*Western Europe includes: Germany, Andorra, Austria, Belgium, Denmark, Finland, France, Iceland, Ireland, Italy, 
                                       Liechtenstein, Luxembourg, Malta, Monaco, Norway, Netherlands, Portugal, United Kingdom, San Marino, Sweden, 
                                       Switzerland, Vatican City."),
                                     p("*Eastern Europe includes:  Albania, Armenia, Belarus, Bosnia and Herzegovina, Bulgaria, Croatia, Slovenia, Estonia, 
                                       Georgia, Hungary, Latvia, Lithuania, Moldova, Poland, Romania, Slovak Republic, Czech Republic, Russia, Serbia and 
                                       Montenegro, Macedonia Ukraine and Cyprus."),
                                     p("*Others: residual category that groups those people who could not prove their place of birth when they registered at the municipality.")
                                     )))
                                     )),
             
             
             ##### POPULATION DIVERSITY #####
             
             tabPanel(h5(span("Population diversity by municipality (2000-2016)" )),                          
                      fluidPage(
                        sidebarPanel(
                          h5(strong("Population Diversity 2000-2016:")),
                          p("Explore the evolution of two diversity indexes (Simpson and entropy computed over 6 population groups) of each municipality between 2000 and 2016.
                            The mean diversity of Spain is plotted by default."),
                          selectInput("MUNICIPALITY3", "Choose a municipality from the list or type its name:", 
                                      choices=unique(x1$MUNICIPALITY3),selected=c("Barcelona", "Girona", "Lleida", "Tarragona"),multiple = TRUE),
                          h5(strong("Download data:")),
                          p("Download and re-use data associated with the charts as a *csv or *txt file. 
                            Attention: UTF-8 encoding and dot as decimal separator."),
                          radioButtons("filetype", h5(strong("File extension:")),
                                       choices = c("csv", "txt")),
                          downloadLink('downloadDiv', strong('Download data')),br(),
                          h5(strong("Download charts:")),
                          downloadLink('MAP_IMAGE_div',strong("Simpson's Index")),br(),
                          downloadLink('MAP_IMAGE_div_e',strong('Entropy index')),
                          h5(strong("Elaboration:")),
                          p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                          h5(strong("Share")),
                          p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                          ),    
                        mainPanel( 
                          tabsetPanel(
                            tabPanel(h6("Simpson's Diversity Index"),
                                     br(),
                                     highchartOutput("ComposicionPlot9",width = '100%', height = 500)),
                            tabPanel(h6('Entropy Index'),
                                     br(),
                                     highchartOutput("entropy",width = '100%', height =500)),
                            tabPanel(h6('About the index'),
                                     h4("Methodology"),
                                     p("The Simpson index introduced in 1949 by ",
                                       a("Edward H. Simpson",href="http://en.wikipedia.org/wiki/Edward_H._Simpson"),
                                       "takes into account the richness (number of groups) within a given distribution 
                                       and the relative frequency with which they are present. To assess the level of population diversity the 
                                       proportion of people in each group (including natives) relative to the total population of 
                                       that area is calculated and squared. The squared proportions for all groups are summed, 
                                       and the reciprocal is taken. The computation can be expressed as follows:",
                                       withMathJax("$$SDI=\\frac{1}{\\sum_{i =1}^sP(i)^2}$$"),
                                       "where ",em("s")," is the total number of groups represented in a given 
                                       area, ", em("P(i)"), "is the size of a given ethnic group as a proportion of the total population in the study area.
                                       Since we compute the index over six population groups the values can range between one and six.
                                       For example: A municipality with 120 inhabitants where each group account for 20 people
                                       would score six. At the opposite extreme, if all inhabitants are of the same group the index would 
                                       have a value of one."),
                                     
                                     
                                     p("Additionally we also compute the Entropy Index (E), being ", em("E"),"calculated as:. ",
                                     withMathJax("$$E = {\\sum\\limits_{r=1}^{n}Q_r}ln{\\dfrac{1}{Q_r}}$$"),
                                     "where ",em ("Q(r)"),"is group ", em("r"),"'s proportion in the study area population. 
                                     The maximum value of ", em("E")," then, is the natural log of the number of groups (6) - which would occur when all groups in a municipality are of equal size.
                                      Following Walker (2016), ",em("E")," is scaled by its maximum by dividing by ",em("ln(6)")," setting the range of values from 0 to 1."),
                                     
                                     h5("Reference:"),
                                     p(a("Simpson, E. H. (1949) Measurement of diversity. Nature, 163, 688-688",
                                         href="http://www.nature.com/nature/journal/v163/n4148/abs/163688a0.html")),
                                     p(a("Walker, K. (2016) Locating neighbourhood diversity in the American metropolis. Urban Studies, 1-17",
                                         href="http://journals.sagepub.com/doi/abs/10.1177/0042098016643481")),
                                     h5("Note:"),
                                     p("Population is grouped by region of birth (Spain, Latin-America, 
                                       Western Europe*, Eastern Europe*, Africa, Asia.)"),
                                     p("*Western Europe includes: Germany, Andorra, Austria, Belgium, Denmark, Finland, France, Iceland, Ireland, Italy, 
                                       Liechtenstein, Luxembourg, Malta, Monaco, Norway, Netherlands, Portugal, United Kingdom, San Marino, Sweden, 
                                       Switzerland, Vatican City."),
                                     p("*Eastern Europe includes:  Albania, Armenia, Belarus, Bosnia and Herzegovina, Bulgaria, Croatia, Slovenia, Estonia, 
                                       Georgia, Hungary, Latvia, Lithuania, Moldova, Poland, Romania, Slovak Republic, Czech Republic, Russia, Serbia and 
                                       Montenegro, Macedonia Ukraine and Cyprus.")
                                     )))))
             
                        ),
  
  
  navbarMenu(h5("Compare municipalities"),
             
             ##### Population #####
             tabPanel(h5(span("Population composition (2000-2016)" )),                          
                      fluidPage(
                        sidebarPanel(
                          h5(strong("Compare municipalities 2000-2016:")),
                          p("Compare the evolution of different population groups between municipalities."),
                          selectInput("MUNICIPALITY3c", "Choose a municipality from the list or type its name:", 
                                      choices=unique(x1$MUNICIPALITY3),selected=c("Madrid", "Barcelona", "Valencia", "Sevilla"),multiple = TRUE),
                
                          h5(strong("Download data:")),
                          p("Download and re-use data associated with the charts as a *csv or *txt file. 
                            Attention: UTF-8 encoding and dot as decimal separator."),
                          radioButtons("filetype", h5(strong("File extension:")),
                                       choices = c("csv", "txt")),
                          downloadLink('downloadPOPCOM', strong('Download data')),br(),
                          h5(strong("Download charts:")),
                          downloadLink('MAP_IMAGE_pop_tot',strong("Total population")),br(),
                          downloadLink('MAP_IMAGE_pop_tot_fb',strong('Foreign-born population')),br(),
                          downloadLink('MAP_IMAGE_pop_tot_fb2',strong('Foreign-born population (%)')),br(),
                          downloadLink('MAP_IMAGE_pop_la',strong('Latin-America')),br(),
                          downloadLink('MAP_IMAGE_pop_we',strong('Western Europe')),br(),
                          downloadLink('MAP_IMAGE_pop_ee',strong('Eastern Europe')),br(),
                          downloadLink('MAP_IMAGE_pop_af',strong('Africa')),br(),
                          downloadLink('MAP_IMAGE_pop_as',strong('Asia')),br(),
                          h5(strong("Elaboration:")),
                          p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                          h5(strong("Share")),
                          p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                          ),    
                        mainPanel( 
                          tabsetPanel(
                            tabPanel(h6("Total Population"),
                                     br(),
                                     highchartOutput("comtotal",width = '100%', height = 500)),
                            tabPanel(h6("Foreign-born population"),
                                     br(),
                                     highchartOutput("comfbtotal",width = '100%', height = 500)),
                            tabPanel(h6("Foreign-born (%)"),
                                     br(),
                                     highchartOutput("comper",width = '100%', height = 500)),
                            tabPanel(h6("Latin-America"),
                                     br(),
                                     highchartOutput("comla",width = '100%', height = 500)),
                            tabPanel(h6("Western Europe"),
                                     br(),
                                     highchartOutput("comwe",width = '100%', height = 500)),
                            tabPanel(h6("Eastern Europe"),
                                     br(),
                                     highchartOutput("comee",width = '100%', height = 500)),
                            tabPanel(h6("Africa"),
                                     br(),
                                     highchartOutput("comaf",width = '100%', height = 500)),
                            tabPanel(h6("Asia"),
                                     br(),
                                     highchartOutput("comas",width = '100%', height = 500))
                          )))),
             
             
             
             ##### RESIDENTIAL SEGREGATION COMPARE #####
             tabPanel(h5(span("Residential Segregation: Dissimilarity Index (2000-2016)" )),                          
                      fluidPage(
                        sidebarPanel(
                          h5(strong("Compare municipalities 2000-2016:")),
                          p("Compare the evolution of the dissimilarity index between municipalities."),
                          selectInput("MUNICIPALITY3B", "Choose a municipality from the list or type its name:", 
                                      choices=unique(x1$MUNICIPALITY3),selected=c("Madrid", "Barcelona", "Valencia", "Sevilla"),multiple = TRUE),
                          h5(strong("Set y-axis range:")),
        
                          numericInput("min", label = h5("Minimum"), value = 10),
                          numericInput("max", label = h5("Maximum"), value = 70),
                          h5(strong("Download data:")),
                          p("Download and re-use data associated with the charts as a *csv or *txt file. 
                            Attention: UTF-8 encoding and dot as decimal separator."),
                          radioButtons("filetype", h5(strong("File extension:")),
                                       choices = c("csv", "txt")),
                          downloadLink('downloadSEGCOM', strong('Download data')),br(),
                          h5(strong("Download charts:")),
                          downloadLink('MAP_IMAGE_seg_la',strong("Latin-America")),br(),
                          downloadLink('MAP_IMAGE_seg_we',strong('Western-Europe')),br(),
                          downloadLink('MAP_IMAGE_seg_ee',strong('Eastern Europe')),br(),
                          downloadLink('MAP_IMAGE_seg_af',strong('Africa')),br(),
                          downloadLink('MAP_IMAGE_seg_as',strong('Asia')),br(),
                          h5(strong("Elaboration:")),
                          p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                          h5(strong("Share")),
                          p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                          ),    
                        mainPanel( 
                          tabsetPanel(
                            tabPanel(h6("Latin-America"),
                                     br(),
                                     highchartOutput("segcomla",width = '100%', height = 500)),
                            tabPanel(h6("Western Europe"),
                                     br(),
                                     highchartOutput("segcomwe",width = '100%', height = 500)),
                            tabPanel(h6("Eastern Europe"),
                                     br(),
                                     highchartOutput("segcomee",width = '100%', height = 500)),
                            tabPanel(h6("Africa"),
                                     br(),
                                     highchartOutput("segcomaf",width = '100%', height = 500)),
                            tabPanel(h6('Asia'),
                                     br(),
                                     highchartOutput("segcomas",width = '100%', height =500))
                          ))))
  
  ),
  
  ##### DATA TABLE  #####
  
  tabPanel(h5(span("Data download" )),  
           fluidPage(
             sidebarPanel(
               #  tags$head(
               #   tags$style(type='text/css', ".span4 { max-width: 350px; }")
               #),
               h5(strong("Build and download your own data-table 2000-2016:")),
               selectizeInput("MUNICIPALITY6", "Choose one or more municipalities from the list or type its name:", 
                              choices=unique(x1$MUNICIPALITY6),selected="Barcelona", multiple=TRUE),
               selectizeInput("YEAR", "Choose one or more years from the list:", 
                              choices=unique(x1$YEAR),selected=c("2000","2016"), multiple=TRUE),
               checkboxGroupInput('VARIABLES', 'Columns in data-table to show:',
                                  c(colnames(x1[1:2]),colnames(x1[3:4]),colnames(x1[5:33])), 
                                  selected =c(colnames(x1[2:3]),colnames(x1[4:5]),colnames(x1[13:14]))),
               radioButtons("filetype", h5(strong("Attention: UTF-8 encoding and dot as decimal separator. File extension:")),
                            choices = c("csv", "txt")),
               downloadButton('downloadDatausers', 'Download data'),
               h5(strong("Elaboration:")),
               p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
               h5(strong("Share")),
               p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
             ),    
             mainPanel(
               tabsetPanel(
                 tabPanel(h6("Your table"), 
                          
                          br(),
                          dataTableOutput("usertable"),
                          p(em("GEDEM Data-playground \nVersion 2.1."),align = "right")),
                 tabPanel(h6("About data download"),
                          br(),
                          p("Build and download your own data-table as a *csv or *txt file by selecting the municipalities, years and variables you want."),
                          h5("Citation"),
                          p("Our aim is to open our research to broader audiences (scholars, data-journalists,
                            policy makers and the general public). For this reason we made our database public and available 
                            to anyone interested in the field. We would appreciate if you acknowledge the project when use it.
                            The suggested citation for data from this website is: ", 
                            strong("Centre d'Estudis Demogràfics (2015),",
                                   em("GEDEM Data-playground Version 1.1. Available at "),
                                   a(" http://gedemced.uab.cat/en/",href="http://gedemced.uab.cat/en/"))),
                          p("Database for the project is under license Creative Commons Attribution 4.0 International."),
                          p(includeHTML("http://gedemced.uab.cat/images/LICENSE1.htm")),
                          h5("Note:"),
                          p("Population is grouped by region of birth (Spain, Latin-America, 
                            Western Europe*, Eastern Europe*, Africa, Asia and others*.)"),
                          p("*Western Europe includes: Germany, Andorra, Austria, Belgium, Denmark, Finland, France, Iceland, Ireland, Italy, 
                            Liechtenstein, Luxembourg, Malta, Monaco, Norway, Netherlands, Portugal, United Kingdom, San Marino, Sweden, 
                            Switzerland, Vatican City."),
                          p("*Eastern Europe includes:  Albania, Armenia, Belarus, Bosnia and Herzegovina, Bulgaria, Croatia, Slovenia, Estonia, 
                            Georgia, Hungary, Latvia, Lithuania, Moldova, Poland, Romania, Slovak Republic, Czech Republic, Russia, Serbia and 
                            Montenegro, Macedonia Ukraine and Cyprus."),
                          p("*Others: residual category that groups those people who could not prove their place of birth when they registered at the municipality.")
                          ))))),
  
  ##### PRESENTATION #####
  navbarMenu(h5("About the project"),
  
  tabPanel(h5(span("About the project")), 
           mainPanel(
             #tabsetPanel(
             #  tabPanel(h6('About the project')),
        
             # img(src="GEDEM_CED.png",align = "Center",height = 100, width = 500),
        
             br(),
             h4(strong("Welcome to GEDEM's data-playground")),
             p("The ",a("Group for Demographic and Migration Studies (GEDEM)",href="http://gedemced.uab.cat/en/"), 
               "is an interdisciplinary group associated with the ",
               a("Centre for Demographic Studies (CED) ",href="http://ced.uab.es/"),
               "at the Autonomous University of Barcelona (UAB). It is officially recognised as a Consolidated Research 
               Group by the Government of Catalonia since 2005. One of the objectives of the group is to depict the 
               settlement process of the foreign-born population over time using traditional and new spatial analysis techniques."),
             p("GEDEM's data-playground allows users to explore, visualize and download population data for each municipality 
               in Spain (over 8,000 municipalities) since 2000. The database also includes computations of various indices 
               of segregation and diversity using the smallest geographical units available (Census Tracts)."),
             
             p("GEDEM's data-playground uses data from the Municipal Registers of inhabitants, which is disseminated by ", 
               a("National Statistics Institute (INE).", href="http://www.ine.es/"),"All remaining errors are the responsibility of the authors alone."),
             p("The source code of the application is available on ", 
               a("GITHUB.", href="https://github.com/JuanGaleano/GEDEM_DATAPLAYGROUND")),
             br(),
             h4(strong("Suggested Citation")),
             p("GEDEM's data-playground has been launched as a free online database (Open Access). The idea is that data and results from ongoing 
               research can go out and impact policymakers, civil society, academic and media circles
               Here is our suggested citation if you use the database: ", 
               strong("Centre d'Estudis Demogràfics (2015),",
                      em("GEDEM Data-playground Version 1.1. Available at "),
                      a(" http://gedemced.uab.cat/en/",href="http://gedemced.uab.cat/en/"))),
             p("The database for this project is under license Creative Commons Attribution 4.0 International."),
             p(includeHTML("http://gedemced.uab.cat/images/LICENSE1.htm")),
             br(),
             h4(strong("Credits")),
             p("GEDEM (Group for Demographic and Migration Studies) at the ",a("Center for Demographic Studies(CED) ",href="http://ced.uab.es/"),"Barcelona, Spain."),
             p("Coordination: ",a("Andreu Domingo", href="http://ced.uab.es/es/directori/andreu-domingo-valls/")),
             p("Data management and computations: ",a("Juan Galeano", href="http://gedemced.uab.cat/documents-cv/cv-juan-galeano1.pdf"),"and ",
               a("Albert Sabater", href="https://www.st-andrews.ac.uk/gsd/people/asc6/")),
             p("Web Interface (R & Shiny): ",a("Juan Galeano", href="http://gedemced.uab.cat/documents-cv/cv-juan-galeano1.pdf")),
             br(),
             h4(strong("Acknowledgements")),
             
             p("This application has been developed using the web application framework , ", a("Shiny", href="http://shiny.rstudio.com/"), "for ", 
               a("the R language", href="https://www.r-project.org/"),". For designing the interface we use the package ",
               a("shinythemes", href="https://rstudio.github.io/shinythemes/"), " and for the different plots and maps the packages ", 
               a("Leaflet", href="https://cran.r-project.org/web/packages/leaflet/index.html"),", ", 
               a("highcharter",href="https://cran.r-project.org/web/packages/highcharter/index.html"), ", ", 
               
               a("ggmap",href="https://cran.r-project.org/web/packages/ggmap/index.html")," and ", 
               a("ggplot2",href="https://cran.r-project.org/web/packages/ggplot2/index.html" ),". In addition, for the manipulation of statistical and spatial data we used the packages ",
               a("maptools", href="https://cran.r-project.org/web/packages/maptools/index.html"),", ",
               a("rgeos", href="https://cran.r-project.org/web/packages/rgeos/index.html"),", ",
               a("geosphere", href="https://cran.r-project.org/web/packages/geosphere/index.html"),", ",
               a("rgdal", href="https://cran.r-project.org/web/packages/rgdal/index.html"),", ",
               a("dplyr",href="https://cran.r-project.org/web/packages/dplyr/index.html")," and ",
               a("readr",href="https://cran.r-project.org/web/packages/readr/index.html" ),". 
               We sincerely appreciate the contribution of all those involved in the development of the R community. 
               We also appreciate the valuable feedback from Dr. Clara Cortina
               and Juan Linares during the elaboration process of this platform."),
             br(),  
             h4(strong("Follow Us")),
             p(includeHTML("http://gedemced.uab.cat/images/FOLLOW.htm")),
             tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FGEDEM.CED&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                         scrolling="no",
                         frameborder="0",
                         style="border:none; overflow:hidden; height:80px;",
                         allowTransparency="true")
             )),
  
  tabPanel(h5(span("Other platforms")), 
           mainPanel(
             br(),
             h4(strong("Explore other datasets available at the Demographic Data Hub (DDH)")),
            p(a(strong("Internal migrations in Spain 2015 by municipalities"),href="http://bancdadesced.uab.es/Internal_migrations/")), 
            img(src="INTERNAL.png",align = "Center",height = 250, width = 500)
           )))
  
  
  
             )) )
