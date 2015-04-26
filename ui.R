library(shiny)

shinyUI(pageWithSidebar(
    
    headerPanel(
        h2('GEOPHYSICS: Earth\'s Gravity')
    ),
    
    sidebarPanel(
        p('Author:     Joshua Poirier'),
        p('Date:       April 26, 2015'),
        p('School:     Coursera / Johns Hopkins Bloomberg School of Public Health'),
        p('Course:     Developing Data Products (devdataprod-013)'),
        br(),
        h3('Area of Investigation'),
        p('Please select a country and state to begin our geophysical investigation!'),
        uiOutput("choose_country"),
        uiOutput("choose_state"),
        actionButton("DLButton", "Download!")
    ),
    
    mainPanel(
        p('Due to the dynamic nature of this app, it may take a few minutes to render all features.  
          The list of countries and states are downloaded at run-time.  The topography and gravity data have 
          variable download times depending on your internet connection speed and the size of the selected state'),
        h2('Topography - Digital Elevation Model'),
        plotOutput('plotDEM'),
        h2('Gravitational Data - Free Air Anomaly'),
        plotOutput('plotFAA')
    )
))