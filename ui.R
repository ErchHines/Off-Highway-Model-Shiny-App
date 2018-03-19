

# Choices for drop-downs

choices <- 2025:2011

# AgDataY    <- c("2016","2015")
# AgCensusY  <- AgCensus$Year
# AvDataY    <- AviationState$Year
# BoatDataY  <- StateRecBoating$Year
# NrbsDataY  <- BoatMpg$Year
# FhDataY    <- Mv7$Year
# FfrDataY   <- Ffr43$Year
# EpaDataY   <- EpaNonRoad$Year

fluidPage(id="nav",
  titlePanel("Non Highway Models"),
  
  tags$head(
    # Include the custom CSS
    includeCSS("styles.css")
  ),

  sidebarLayout(

    sidebarPanel(width = 2,

      helpText("Enter the year for each data source"),

      selectInput("Ag", "USDA Annual Survey", 
      choices, multiple = FALSE, selected = 2016),
      
      selectInput("AgC", "USDA Census", 
      choices, multiple = FALSE, selected = 2012),
  
      selectInput("Av", "EIA Prime Supplier Survey", 
      choices, multiple = FALSE, selected = 2016),    
      
      selectInput("Fh", "FHWA Highway Statistics", 
      choices, multiple = FALSE, selected = 2016), 
      
      selectInput("Bo", "USCG Boating Statistics", 
      choices, multiple = FALSE, selected = 2016),   
  
      selectInput("BoC", "USCG NRBS", 
      choices, multiple = FALSE, selected = 2012),
      
      selectInput("Ff", "GSA Federal Fuel Report", 
      choices, multiple = FALSE, selected = 2015),
    
      selectInput("Ep", "EPA Model Run", 
      choices, multiple = FALSE, selected = 2016),
      
      downloadButton("downloadData1", "Combined Model", class = "buttonDownload"),
      br(),
      br(),
      downloadButton("downloadData2", "FHWA Model", width = "1200px", class = "buttonDownload"),
      br(),
      br(),
      downloadButton("downloadData3", "EPA Model", class = "buttonDownload")
      
      
    
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("FHWA + EPA Model",
          DT::dataTableOutput('mytable2')
        ),

        tabPanel("FHWA Model",
          DT::dataTableOutput('mytable1')
        ),
        
        tabPanel("EPA Model",
          DT::dataTableOutput('mytable3')
        )
      )
    )
  )
)
    

   