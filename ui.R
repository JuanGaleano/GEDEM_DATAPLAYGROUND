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
x1$POP_TOTAL_EXT <- x1$POP_TOTAL-x1$POP_SPANISH
x1 <-  x1[order(x1$YEAR),] 
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
x1<- x1[c(1:13,32,30,33,14:25,31,26:29,34:38)] 

CATSECC2014@data$Prop<-as.factor(CATSECC2014@data$Prop)
CATSECC2014@data$Nom_Mun<-paste(as.character(CATSECC2014@data$Nom_Mun), sep="")
CATSECC2014@data$Nom_Mun1<-CATSECC2014@data$Nom_Mun
choices <- c("Select All", unique(levels(CATSECC2014@data$Prop))) 

palette<-c("#D8D8D8","#FFEDA0","#FED976","#FEB24C","#FD8D3C","#FC4E2A" ,"#E31A1C", "#B10026")
pal <- colorFactor(palette, NULL, n = length(unique(CATSECC2014@data$MUN)))

shinyUI(navbarPage("Catalonia", 
                   
                   ### PRESENTATION PAGE ###
                   tabPanel(h5(span("Presentation",style = "color:#585858" )), 
                            mainPanel(
                              tabsetPanel(
                                tabPanel(h6('About the project')),
                                br(),
                                br(),
                                img(src="GEDEM_CED.png",align = "Center",
                                    height = 100, width = 500),
                                br(),
                                br(),
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
                                
                                p("GEDEM’s data-playground uses data from the Municipal Registers of inhabitants, which is disseminated by ", 
                                  a("National Statistics Institute (INE).", href="http://www.ine.es/"),"All remaining errors are the responsibility of the authors alone."),
                                br(),
                                h4(strong("Suggested Citation")),
                                p("GEDEM’s data-playground has been launched as a free online database (Open Access). The idea is that data and results from ongoing 
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
                                p("Coordination: ",a("Andreu Domingo", href="http://gedemced.uab.cat/documents-cv/cv-andreu-domingo.pdf")),
                                p("Data management and computations: ",a("Juan Galeano", href="http://gedemced.uab.cat/documents-cv/cv-juan-galeano1.pdf"),"and ",
                                  a("Albert Sabater", href="http://gedemced.uab.cat/documents-cv/cv-albert-sabater.pdf")),
                                p("Web Interface (R & Shiny): ",a("Juan Galeano", href="http://gedemced.uab.cat/documents-cv/cv-juan-galeano1.pdf")),
                                br(),
                                h4(strong("Acknowledgements")),
                                p("For the development of this application the following free software has been employed: ", a("R-Project", href="http://www.r-project.org/"), 
                                  "(statistical computing and graphics), the integrated development environment (IDE) for R,", a("RStudio,", href="http://www.rstudio.com/"),
                                  "the web application framework for R,", a("Shiny,", href="http://shiny.rstudio.com/"),
                                  "the package",a("googlevis", href="http://cran.r-project.org/web/packages/googleVis/index.html"),
                                  "developed by Markuss Gessmann and  Diego del Castillo, the package",a("dplyr", href="http://cran.r-project.org/web/packages/dplyr/index.html"), 
                                  "developed by Hadley Wickham, Romain Francois and  RStudio the package",
                                  a("ggplot2", href="http://cran.r-project.org/web/packages/ggplot2/index.html"),
                                  "developed by Hadley Wickham and Winston Chang and the package",a("rgal", href="http://cran.r-project.org/web/packages/rgdal/index.html"),
                                  "developed by Timothy H. Keitt, Roger Bivand, Edzer Pebesma and Barry Rowlingson. 
                                  We sincerely appreciate the contribution of all those involved in the development of the R community."),
                                br(),
                                h4(strong("Follow Us")),
                                p(includeHTML("http://gedemced.uab.cat/images/FOLLOW.htm")),
                                tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FGEDEM.CED&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                            scrolling="no",
                                            frameborder="0",
                                            style="border:none; overflow:hidden; height:80px;",
                                            allowTransparency="true")
                                ))),
                   
                   ### MOTION CHART
                   
                   tabPanel(h5(span("Motion Chart",style = "color:#585858" )),                          
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                h5(strong("Motion Chart:")),
                                selectizeInput("PROVINCE", "Choose a province from the list or type its name:", 
                                               choices=unique(x1$PROVINCE),selected = c("Barcelona","Girona", "Lleida", "Tarragona"),
                                               multiple = TRUE),
                                h5(strong("Download data:")),
                                p("Download and re-use data associated with the motion chart as a *csv or *txt file. 
                                  Data shows the composition of the population by region of birth of each municipality for the 
                                  selected province between 2000 and 2015. Attention: UTF-8 encoding and dot as decimal separator."),
                                radioButtons("filetype", h5(strong("File extension:")),
                                             choices = c("csv", "txt")),
                                downloadButton('downloadData', 'Download data'),
                                h5(strong("Elaboration:")),
                                p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                h5(strong("Share")),
                                p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                                ),    
                              mainPanel( 
                                tabsetPanel(
                                  tabPanel(h6('Motion Chart'),
                                           br(),
                                           p("Google’s Motion Charts offer the opportunity to explore several variables over time.  
                                             By default we plot the Latitude (LAT) and Longitude (LON) coordinates of each municipality 
                                             centroid on the ", em("y"), "axis and",em("x"), "axis respectively. This allows us to obtain a simile 
                                             for the spatial relation between municipalities. Users can (easily) re-define the variables displayed 
                                             on the ", em("x"), "and ", em("y"), "axis of the motion chart, as well as bubbles' colour and size."),
                                           tableOutput("ComposicionPlot"),
                                           p(a("Google Terms of Use", href="https://developers.google.com/terms/")),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Tutorial on Motion Chart'),
                                           br(),
                                           p("Explore multiple variables over time. The maximum capacity of simultaneous representation 
                                             consists of four variables (one which is plotted on the ", em("X"), "axis, one on the ", em("Y"), "axis 
                                             and two others represented on the colour and size of entities)."),
                                           p(img( src="TUTORIAL1.png",
                                                  height = 350, width = 400)),
                                           p("However, the chart allows the user to re-order and represent the variables 
                                             through their selection on the x and y axes, and by colour and size."),
                                           p(img( src="TUTORIAL2.png",
                                                  height = 350, width = 400)),
                                           p("The user can also select the entities to be display by clicking on them."),
                                           p(img( src="TUTORIAL3.png",
                                                  height = 350, width = 400)),
                                           p("Opacity can be adjusted from the spanner icon."),
                                           p(img( src="TUTORIAL4.png",
                                                  height = 350, width = 400)),
                                           p("The chart offers the possibility to explore each of the 
                                             variables over time in a bar or lines graph, which reduces 
                                             the simultaneous display."),
                                           p(img( src="TUTORIAL5.png",
                                                  height = 350, width = 400)),
                                           p(img( src="TUTORIAL6.png",
                                                  height = 350, width = 400)),
                                           p("Finally, it is possible to zoom in over some part of the plot. 
                                             In order to do so, first select the area to zoom, click on zoom 
                                             in and then click the space bar on your keyboard."),
                                           p(img( src="TUTORIAL9.png",
                                                  height = 350, width = 400)),
                                           p("Zoom out by clicking the zoom out button."),
                                           p(img( src="TUTORIAL8.png",
                                                  height = 350, width = 400)),
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
                  
                   
                   ### POPULATION PYRAMID PAGE ###
                   
                   tabPanel(h5(span("Population Pyramid",style = "color:#585858" )), 
                            sidebarPanel(
                              tags$head(
                                tags$style(type='text/css', ".span4 { max-width: 350px; }")
                              ),
                              h5(strong("Population Structure:")),
                              p("Explore composition by age, sex and place of birth of the population between 2000 and 2015."),
                              # Slider: choose year
                              sliderInput("year", "Select a year or push the play button:",  min=2000, max=2015, value=2015,post=000, animate = TRUE),
                              # HTML info
                              radioButtons("filetype", h5(strong("Download data, choose a file extension:")),
                                           choices = c("csv", "txt")),
                              downloadButton('downloadDataPy', 'Download data'),
                              h5(strong("Download plots as a *png files:")),
                              downloadButton('downloadPYR', 'Download Pyramid plot'),
                              h5(strong("Elaboration:")),
                              p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                              h5(strong("Share")),
                              p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                            ),
                            
                            ### Main Panel
                            mainPanel(
                              # Show the plot
                              plotOutput("pyramidrel", height="700px",width="800px"),
                             # tableOutput("ComposicionEDADES"),
                              p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                              p(em("Last Update: 07/02/2016."),align = "right")
                            )),
                   
                   ### POPULATION COMPOSITION PAGE ###
                   
                   tabPanel(h5(span("Population by municipality",style = "color:#585858" )),     
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                h5(strong("Population Composition:")),
                                p("Explore population composition of each municipality between 2000 and 2015 
                                  in absolute and relative terms."),
                                selectInput("MUNICIPALITY", "Choose a municipality from the list or type its name:", 
                                            choices=unique(x1$MUNICIPALITY), selected="Barcelona"),
                                h5(strong("Download data:")),
                                p("Download and re-use data associated with the stacked bar plots as a *csv or *txt file.
                                  Data shows the composition of the population by place of birth (Spanish-born vs foreign-born)
                                  for the selected municipality between 2000 and 2015. Attention: UTF-8 encoding and dot as decimal separator."),
                                radioButtons("filetype", h5(strong("File extension:")),
                                             choices = c("csv", "txt")),
                                downloadButton('downloadData3', 'Download data'),
                                h5(strong("Download stacked bar plots a *png files:")),
                                downloadButton('downloadPlot2', 'Download first plot'),
                                br(),
                                downloadButton('downloadPlot3', 'Download second plot'),
                                h5(strong("Elaboration:")),
                                p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                h5(strong("Share")),
                                p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                                ),    
                              mainPanel( 
                                tabsetPanel(
                                  tabPanel(h6('Total population'),tableOutput("ComposicionPlot2"),
                                           tableOutput("ComposicionPlot3"),                        
                                           p(a("Google Terms of Use", 
                                               href="https://developers.google.com/terms/")),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Foreign-born population 2000 & 2015'),tableOutput("ComposicionPlot6"),
                                           tableOutput("ComposicionPlot7"),                        
                                           p(a("Google Terms of Use", 
                                               href="https://developers.google.com/terms/")),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6("Foreign-born population 2015: Country of birth top 10"),
                                           br(),
                                           h4(strong("Foreign-born population")),
                                           plotlyOutput("ComposicionPlotly2", width = "1200px", height = "500px"),
                                           br(),
                                           p(strong("NOTE: "), "Download plot as png from the camara icon."),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
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
                   
                   
                   #### mapas
                   
                   tabPanel(h5(span("Population by census tracts",
                                           style = "color:#585858" )),                          
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                
                                selectizeInput("Nom_Mun", "Choose a municipality from the list or type its name:", 
                                               choices=unique(CATSECC2014@data$Nom_Mun),selected = "Barcelona",
                                               multiple = TRUE),
                                selectizeInput("Prop", "Filter:", 
                                               choices=choices, selected = "Select All",multiple = TRUE
                                ),
                                h5(strong("Download data:")),
                                p("Download and re-use data associated with the map as a *csv or *txt file. 
                                  Data shows the composition of the population by region of birth of each census tract for the 
                                  selected municipality in 2014. Attention: UTF-8 encoding and dot as decimal separator."),
                                radioButtons("filetype", h5(strong("File extension:")),
                                             choices = c("csv", "txt")),
                                downloadButton('downloadDataz', 'Download data'),
                                h5(strong("Elaboration:")),
                                p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                h5(strong("Share")),
                                p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                              ),    
                              mainPanel( 
                                tabsetPanel(
                                  tabPanel(h6('Foreign-born population (%) by census tracts (YEAR:2014)'),
                                           br(),
                                           leafletOutput("mymap", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right"))
                                )))),
                   
                   
                   #### mapas2
                   
                   tabPanel(h5(span("Location quotiens (foreign-born groups)",
                                    style = "color:#585858" )),                          
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                
                                selectizeInput("Nom_Mun1", "Choose one or more municipalities from the list or type their names:", 
                                               choices=unique(CATSECC2014@data$Nom_Mun1),selected = "Barcelona",
                                               multiple = TRUE),
                               # selectizeInput("Prop", "Filter:", 
                                #               choices=choices, selected = "Select All",multiple = TRUE
                                #),
                                h5(strong("Download data:")),
                                p("Download and re-use data associated with the maps as a *csv or *txt file. 
                                  Data shows the location quotients and composition of the population by region of birth of each census tract for the 
                                  selected municipality (or municipalities) in 2014. Attention: UTF-8 encoding and dot as decimal separator."),
                                radioButtons("filetype", h5(strong("File extension:")),
                                             choices = c("csv", "txt")),
                                downloadButton('downloadDataz1', 'Download data'),
                                h5(strong("Elaboration:")),
                                p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                h5(strong("Share")),
                                p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                              ),    
                              mainPanel( 
                                tabsetPanel(
                                  tabPanel(h6('Location quotients: LATIN-AMERICA'),
                                           br(),
                                           p("Location quotient (LQ) is a valuable way of quantifying how concentrated a particular demographic group 
                                              is in a census tract as compared to the municipality (or group of municipalities) to which it belongs. The formula for the location quotient 
                                             can be expressed as follows:",
                                             withMathJax("$$LQ=\\frac{x_i}{X}/\\frac{ y_i}{Y}$$"),
                                             p("Where ", em("x"), "is the population of the ", em("X"), " type within the ", em("i")," area, e.g. census tracts; ",
                                               em("y"), "is the total population within the ", em("i")," area. ", em("X"), " is the total ", em("X"),
                                               " population of the large geographic entity for 
                                               which the quotient is being calculated and ", em("Y"), " is the total population of the large geographic entity for 
                                               which the quotient is being calculated.")),
                                           p("When LQ takes a value of one, it means that the foreign-born group under analysis represents, within that census tract,
                                             the same proportion over total population that it represents at the geo-administrative reference level 
                                             (a municipality or group of municipalities). We group the resulting values in 5 categories:"),
                                           p(strong("High underrepresentation:"), "value equal or under 0.5."),
                                           p(strong("Underrepresentation:"),"value over 0.5 and equal or under 0.8."),
                                           p(strong("Average representation:"),"value over 0.8 and equal or under 1.2."),
                                           p(strong("Overrepresentation:"),"value over 1.2 and equal or under 1.5."),
                                           p(strong("High overrepresentation:"),"value over 1.5."),
                                           p(strong("NOTE:"),"within this app LQs are reative. If more than one municipality is selected, LQs are re-compute
                                                    summing total populations for the group of municipalities. For this app we use a shapefile of the division 
                                             of Catalonia by census tracts in 2014 provided by the ",a("Institut Catogràfic i Geològic de Catalunya (ICGC)", href="http://www.icgc.cat/")),
                                           
                                           leafletOutput("mymap1", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Location quotients: WESTERN-EUROPE'),
                                           br(),
                                           leafletOutput("mymap2", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Location quotients: EASTERN-EUROPE'),
                                           br(),
                                           leafletOutput("mymap3", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Location quotients: AFRICA'),
                                           br(),
                                           leafletOutput("mymap4", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6('Location quotients: ASIA'),
                                           br(),
                                           leafletOutput("mymap5", width = 1150, height = 700),
                                           p(),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right"))
                                  
                                )))),
                   ### RESIDENTIAL SEGREGATION PAGE ###
                   
                   tabPanel(h5(span("Residential Segregation Indexes by municipality",style = "color:#585858" )),          
                            sidebarPanel(
                              tags$head(
                                tags$style(type='text/css', ".span4 { max-width: 350px; }")
                              ),
                              h5(strong("Residential Segregation:")),
                              p("Explore the evolution of two residential segregation indexes (Dissimilarity and Isolation) 
                                for different population groups between 2000 and 2015. 
                                A line chart with the number of census tracts of the selected municipality is provided 
                                in order to facilitate the interpretation of the observed trends."),
                              selectInput("MUNICIPALITY2", "Choose a municipality from the list or type its name:", 
                                          choices=unique(x1$MUNICIPALITY2),selected="Barcelona"), 
                              h5(strong("Download data:")),
                              p("Download and re-use data associated with the line plots as a *csv or *txt file.
                                Data shows the evolution of dissimilarity and isolation indexes of different migrant groups 
                                (born in Latin-America, Western Europe, Eastern Europe, Africa and Asia) 
                                for the selected municipality between 2000 and 2015. Attention: UTF-8 encoding and dot as decimal separator."),
                              radioButtons("filetype", h5(strong("File extension:")),
                                           choices = c("csv", "txt")),
                              downloadButton('downloadData4', 'Download data'),
                              h5(strong("Download line plots a *png files:")),
                              downloadButton('downloadPlotDIS', 'Download Dissimilarity plot'),
                              br(),
                              downloadButton('downloadPlotIS', 'Download Isolation plot'),
                              h5(strong("Elaboration:")),
                              p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                              h5(strong("Share")),
                              p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                              ),    
                            mainPanel( 
                              tabsetPanel(
                                tabPanel(h6('Dissimilarity Index'),
                                         tableOutput("ComposicionPlot4"),
                                         p(a("Google Terms of Use", 
                                             href="https://developers.google.com/terms/")),
                                         p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                         p(em("Last Update: 07/02/2016."),align = "right")),
                                tabPanel(h6('Isolation Index'),
                                         tableOutput("ComposicionPlot5"),
                                         p(a("Google Terms of Use", 
                                             href="https://developers.google.com/terms/")),
                                         p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                         p(em("Last Update: 07/02/2016."),align = "right")),
                                tabPanel(h6('Census tracts in municipality'),
                                         tableOutput("ComposicionPlot8"),
                                         p(a("Google Terms of Use", 
                                             href="https://developers.google.com/terms/")),
                                         p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                         p(em("Last Update: 07/02/2016."),align = "right")),
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
                   
                   ### POPULATION DIVERSITY PAGE ###
                   
                   tabPanel(h5(span("Population diversity by municipality",style = "color:#585858" )),                          
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                h5(strong("Population Diversity:")),
                                p("Explore the evolution of the Simpson Diversity Index (computed over 6 population groups) of each municipality between 2000 and 2015. 
                                  Make comparisons by selecting up to three municipalities. 
                                  The mean diversity for the autonomous community is plotted by default."),
                                selectInput("MUNICIPALITY3", "Choose a municipality from the list or type its name:", 
                                            choices=unique(x1$MUNICIPALITY3),selected="Barcelona"),
                                selectInput("MUNICIPALITY4", "Choose a municipality from the list or type its name:", 
                                            choices=unique(x1$MUNICIPALITY4),selected="Girona"),
                                selectInput("MUNICIPALITY5", "Choose a municipality from the list or type its name:", 
                                            choices=unique(x1$MUNICIPALITY5),selected="Tarragona"),
                                h5(strong("Elaboration:")),
                                p(a(img( src="GEDEM-color_sin_leyenda.png",height = 110, width = 150), href="http://gedemced.uab.cat/en/")),
                                h5(strong("Share")),
                                p(includeHTML("http://gedemced.uab.cat/images/TWEET.htm"))
                                ),    
                              mainPanel( 
                                tabsetPanel(
                                  tabPanel(h6('Simpson Diversity Index'),tableOutput("ComposicionPlot9"),
                                           p(a("Google Terms of Use", 
                                               href="https://developers.google.com/terms/")),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
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
                                           h5("Reference:"),
                                           p(a("Simpson, E. H. (1949) Measurement of diversity. Nature, 163, 688-688",
                                               href="http://www.nature.com/nature/journal/v163/n4148/abs/163688a0.html")),
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
                                           ))             
                                           )
                                )),
                   
                   #### BUILD YOUR OWN DATA TABLE
                   
                   tabPanel(h5(span("Build your own data table",style = "color:#585858" )),  
                            fluidPage(
                              sidebarPanel(
                                tags$head(
                                  tags$style(type='text/css', ".span4 { max-width: 350px; }")
                                ),
                                h5(strong("Build and download your own table:")),
                                selectizeInput("MUNICIPALITY6", "Choose one or more municipalities from the list or type its name:", 
                                               choices=unique(x1$MUNICIPALITY6),selected="Barcelona", multiple=TRUE),
                                selectizeInput("YEAR", "Choose one or more years from the list:", 
                                               choices=unique(x1$YEAR),selected=c("2000","2005","2010","2015"), multiple=TRUE),
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
                                  tabPanel(h6("Your table"), dataTableOutput("usertable"),
                                           p(em("GEDEM Data-playground \nVersion 1.1."),align = "right"),
                                           p(em("Last Update: 07/02/2016."),align = "right")),
                                  tabPanel(h6("About data download"),
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
                   tabPanel(h5(span("Change Autonomous Community",style = "color:#585858" )),  
                            mainPanel(
                              h4(strong("Select other Autonomous Communities",style = "color:#585858")),
                              br(),
                              p(a(img( src="EN_ANDALUSIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_ANDALUSIA/"),
                                a(img( src="EN_ARAGON.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_ARAGON/"),
                                a(img( src="EN_ASTURIAS.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_ASTURIAS/"),
                                a(img( src="EN_BALEARIC.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_BALEARIC_ISLANDS/"),
                                a(img( src="EN_BASQUE.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_BASQUE_COUNTRY/"),
                                a(img( src="EN_CANARY.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_CANARY_ISLANDS/"),
                                a(img( src="EN_CANTABRIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_CANTABRIA/"),
                                a(img( src="EN_CASTILLE_LEON.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_CASTILE_LEON/"),
                                a(img( src="EN_CASTILLE_MANCHA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_CASTLE_MANCHA/"),
                                a(img( src="EN_CATALONIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_CATALONIA/"),
                                a(img( src="EN_EXTREMADURA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_EXTREMADURA/"),
                                a(img( src="EN_GALICIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_GALICIA/"),
                                a(img( src="EN_LA_RIOJA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_LA_RIOJA/"),
                                a(img( src="EN_MADRID.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_MADRID/"),
                                a(img( src="EN_MURCIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_MURCIA/"),
                                a(img( src="EN_NAVARRE.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_NAVARRE/"),
                                a(img( src="EN_VALENCIA.png",height = 140, width = 150), href="https://gedemced.shinyapps.io/DP_VALENCIA/")),
                              br(),
                              h6(strong("Follow Us")),
                              p(includeHTML("http://gedemced.uab.cat/images/FOLLOW.htm")),
                              tags$iframe(src="//www.facebook.com/plugins/follow.php?href=https%3A%2F%2Fwww.facebook.com%2FGEDEM.CED&amp;width&amp;height=80&amp;colorscheme=light&amp;layout=standard&amp;show_faces=true",
                                          scrolling="no",
                                          frameborder="0",
                                          style="border:none; overflow:hidden; height:80px;",
                                          allowTransparency="true"))) 
                   ))