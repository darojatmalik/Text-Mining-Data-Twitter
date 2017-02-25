library(leaflet)
library(shinydashboard)

navbarPage("Geovisualisasi", id="nav",
           
#--------------------------------------------------Menu Data Explorer--------------------------------------------------#          
           tabPanel("Data explorer",
                    
                    sidebarLayout(fluid = TRUE, 
                                  sidebarPanel(
                                    fileInput('datafile', 'Upload the file'),
                                    actionButton(inputId = "input_action", label = "Show Inputs"),
                                    downloadButton('downloadData', 'Download'),
                                    width=3),
                                  
                                  mainPanel(fluid = TRUE,
                                    
                                            tabsetPanel(
                                              tabPanel("Table", dataTableOutput("td"))
                                            )
                                            
                                  )
                                 
                    )
           ),
#--------------------------------------------------Menu K-Means Clustering----------------------------------------------#
           tabPanel("K-Means Clustering",
                    
                    sidebarLayout(fluid = TRUE, 
                                  sidebarPanel(
                                    #fileInput('datafile', 'Upload the file'),
                                    numericInput("numberofsparse", "Sparsity", 0.99, min = 0.985, max = 0.995),
                                    numericInput("numberofk", "K for clustering", 7, min = 2, max = 7),
                                    br(),
                                    actionButton(inputId = "input_clustering", label = "Show Output"),
                                    downloadButton('downloadData1', 'Download'),
                                    width=3),
                                  
                                  mainPanel(fluid = TRUE,
                                            
                                            tabsetPanel(
                                              tabPanel("Table", dataTableOutput("td1")),
                                              tabPanel("SSE", dataTableOutput("sse")),
                                              tabPanel("TDM", tableOutput("tdm"))
                                              
                                            )
                                            
                                  )
                                  
                    )
           ),
#--------------------------------------------------Menu Interactive Map-------------------------------------------------#
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("Cluster Explorer"),
                                      actionButton(inputId = "show_map", label = "Show Map"),
                                      #show legend
                                      checkboxInput("legend", "Show legend"),
                                      #selectInput("cities", "Cities", c("Cities"=""), multiple=TRUE),   
                                      plotOutput("histCluster", height = 200)
                                      
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Geovisualisasi Data Teks Twitter Terkait Kebakaran Hutan dan Bencana Alam'), ' Oleh : Hamid Darojat (G64144022).'
                        )
                    )
           ),
#--------------------------------------------------Menu Help----------------------------------------------------------#
           tabPanel("Help",
                    hr(),
                    h4("How to Praproses in Data Explorer:"),
                    "1. Browse dataset to upload as data input, make sure data is csv format.", br(),
                    "2. Click ShowInput button to Praproses and show data. ", br(),
                    "3. Click Download button to download praproses result. ",
                    hr(),
                    h4("How to Clustering with K-Means:"),
                    "1. Fill the values of sparsity, default value is 0.99, minimun value is 0.985, and maximum value is 0.995.", br(),
                    "2. Choose the values of K for K-Means Clustering, default value is 7,min = 5, max = 10 .", br(),
                    "3. Click ShowOutput button to Clustering and show data. ", br(),
                    "4. Table sub menu contain Data Twitter and Clustering Result.",br(),
                    "5. SSE sub menu contain Sum Square Error in every class (k).",br(),
                    "6. TDM sub menu contain Result Term before and After sparsity",br(),
                    "7. Click Download button to download Clustering result. ",
                    hr(),  
                    h4("How to use Interactive Map:"),
                    "1. Click ShowMap button to show the result of renderleaflet().", br(),
                    "2. Cek Checkbox button to show legend", br(),
                    "3. Click Marker to get popup information data tweet.", br(),
                    "4. Zoom in/zoom out, drag mouse to show all geovisualization.",
                    hr(),
                    h4("Noted:"),
                    "1. Before use this application, make sure MySQL server has been activated .", br(),
                    "2. MySQL connect in Database tbkatadasar for Normalization and Stemming(Preprocessing Step).", 
                    hr()
           ),
#----------------------------------------------------About-------------------------------------------------------------#
            tabPanel("About",
               tags$h4(verbatimTextOutput("about"),
                       br()))
          
          
)
