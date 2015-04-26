library(shiny)
library(RCurl)
library(raster)
library(spatstat)

# retrieve list of countries
countries <- as.data.frame(getData('ISO3'), stringsAsFactors=FALSE)

shinyServer(function(input, output) {

    # retrieve the ISO3 country code (ie USA, MEX, etc)
    # #################################################
    countryCode <- reactive({
        i <- which(countries$NAME == input$country)
        countries$ISO3[i]
    })
    
    # retrieve maps bounding coordinates
    # #################################################
    bbox <- reactive({
        withProgress(message = 'Retrieving state data', value=0, {
            # get the administrative boundaries for the selected country
            incProgress(1/2, detail="Contacting host and downloading data")
            shp <- getData('GADM', country=countryCode(), level=1)
            
            # get the row corresponding to the state selected by the user
            j <- which(shp$NAME_1 == input$state)            
            incProgress(2/2, detail="Complete")
        })
        
        # get extents of state
        findExtents(shp@polygons[[j]])
    })
    
    # drop-down for user to select country
    # #################################################
    output$choose_country <- renderUI({
        selectInput("country", "Country:", countries$NAME)
    })
    
    # drop-down for user to select state/province
    # #################################################
    output$choose_state <- renderUI({   
        withProgress(message = 'Retrieving state data', value=0, {
            # get the administrative boundaries for the selected country
            incProgress(1/2, detail="Contacting host and downloading data")
            shp <- try(getData('GADM', country=countryCode(), level=1))
            if (class(shp) == "try-error") {
                return()
            }
            
            incProgress(2/2, detail="Complete")
            selectInput("state", "State/Province:", shp$NAME_1)            
        })

    })
    
    # plot altitude data for the state
    # #################################################
    output$plotDEM <- renderPlot({
        if (input$DLButton == 0) return()
        
        isolate({
            withProgress(message = 'Topography retrieval', value=0, {
                # retrieve administrative boundaries for the selected country
                incProgress(1/7, detail="Downloading state administrative boundaries")
                validate(validateStateDL(input$country))
                shp <- try(getData('GADM', country=countryCode(), level=1))
                b <- bbox()
                
                ## website where Sandwell & Smith topography/gravity data may be downloaded
                incProgress(2/7, detail="Contacting host")
                url <- "http://topex.ucsd.edu/cgi-bin/get_data.cgi"
                getURL(url)
                
                # download and read Sandwell & Smith topography data
                incProgress(3/7, detail="Downloading data from host")
                ts <- postForm(url, north=b$maxy, west=b$minx, east=b$maxx, south=b$miny, mag=1)
                t <- read.delim(textConnection(ts),header=FALSE,sep="",strip.white=TRUE)
                
                # clean data
                incProgress(4/7, detail="Cleaning data")
                names(t) <- c("Lon", "Lat", "DEM")
                t$Lon <- ifelse(t$Lon > 180, t$Lon - 360, t$Lon)
                
                # processing data
                incProgress(5/7, detail="Data processing")
                r <- raster(nrows=length(table(t$Lat))/2, ncols=length(table(t$Lon))/2,
                            xmn=min(t$Lon), xmx=max(t$Lon),
                            ymn=min(t$Lat), ymx=max(t$Lat))
                dem <- rasterize(t[,1:2], r, t$DEM)
                slope <- terrain(dem, opt='slope')
                aspect <- terrain(dem, opt='aspect')
                hill <- hillShade(slope, aspect, 45, 45)
                
                # rendering data
                incProgress(6/7, detail="Rendering data")
                plot(hill, legend=FALSE, col=grey(0:100/100))
                plot(dem, add=TRUE, alpha=0.65)
                contour(dem, add=TRUE, col=rgb(0,0,0,alpha=0.15))
                plot(shp, add=TRUE, lwd=2.5)
                incProgress(7/7, detail="Complete")
            })
        })
    })
    
    # plot gravity data for the state
    # #################################################
    output$plotFAA <- renderPlot({
        if (input$DLButton == 0) return()
        
        isolate({            
            withProgress(message = 'Gravity retrieval', value=0, {
                # retrieve administrative boundaries for the selected country
                incProgress(1/7, detail="Downloading state administrative boundaries")
                validate(validateStateDL(input$country))
                shp <- getData('GADM', country=countryCode(), level=1)
                b <- bbox()
                
                # website where Sandwell & Smith topography/gravity data may be downloaded
                incProgress(2/7, detail="Contacting host")
                url <- "http://topex.ucsd.edu/cgi-bin/get_data.cgi"
                getURL(url)
                
                # download and read Sandwell & Smith gravity data
                incProgress(3/7, detail="Downloading data from host")
                gs <- postForm(url, north=b$maxy, west=b$minx, east=b$maxx, south=b$miny, mag=0.1)
                g <- read.delim(textConnection(gs),header=FALSE,sep="",strip.white=TRUE)
                
                # clean data
                incProgress(4/7, detail="Cleaning data")
                names(g) <- c("Lon", "Lat", "FAA")
                g$Lon <- ifelse(g$Lon > 180, g$Lon - 360, g$Lon)
                
                # processing data
                incProgress(5/7, detail="Data processing")
                r <- raster(nrows=length(table(g$Lat))/2, ncols=length(table(g$Lon))/2,
                            xmn=min(g$Lon), xmx=max(g$Lon),
                            ymn=min(g$Lat), ymx=max(g$Lat))
                faa <- rasterize(g[,1:2], r, g$FAA)
                slope <- terrain(faa, opt='slope')
                aspect <- terrain(faa, opt='aspect')
                hill <- hillShade(slope, aspect, 45, 45)
                
                # rendering data
                incProgress(6/7, detail="Rendering data")
                plot(hill, legend=FALSE, col=grey(0:100/100))
                plot(faa, add=TRUE, alpha=0.65)
                contour(faa, add=TRUE, col=rgb(0,0,0,alpha=0.15))
                plot(shp, add=TRUE, lwd=2.5)                
                incProgress(7/7, detail="Complete")
            })
        })
    })
    
    # hosts for state administrative boundary data seem unreliable, so validate their functionality before proceeding
    # #################################################
    validateStateDL <- function(input) {
        shp <- try(getData('GADM', country=countryCode(), level=1))
        if (class(shp) == "try-error") {
            paste("We're sorry, the host for the state administrative boundaries of", 
                  input, 
                  "is currently unavailable.  Please try another country or try again later")
        } else if (input == "") {
            NULL
        } else { 
            NULL
        }
    }
})

# determine the extents of the user-selected state
# ie - how far east, west, north, and south the state 
# goes so we know how much data to download
# #################################################
findExtents <- function(x) {
    # minx, maxx, miny, maxy
    bbox <- data.frame(minx=NA, maxx=NA, miny=NA, maxy=NA)
    
    # get number of polygons in the given SpatialPolygons-class x
    L <- length(x@Polygons)
    
    withProgress(message = 'Determining state extents', value = 0, {
        # loop through polygons in SpatialPolygons-class and determine bounding box coordinates
        for (i in 1:L) {
            if (is.na(bbox$minx) || bbox$minx > min(x@Polygons[[i]]@coords[,1])) bbox$minx <- min(x@Polygons[[i]]@coords[,1])
            if (is.na(bbox$miny) || bbox$miny > min(x@Polygons[[i]]@coords[,2])) bbox$miny <- min(x@Polygons[[i]]@coords[,2])
            if (is.na(bbox$maxx) || bbox$maxx < max(x@Polygons[[i]]@coords[,1])) bbox$maxx <- max(x@Polygons[[i]]@coords[,1])
            if (is.na(bbox$maxy) || bbox$maxy < max(x@Polygons[[i]]@coords[,2])) bbox$maxy <- max(x@Polygons[[i]]@coords[,2])
            incProgress(1/L, detail = paste("Examining state", i))
        }        
    })
    
    # expand bounding box of state by 25% - padding for our map extents
    t <- data.frame(minx=NA, maxx=NA, miny=NA, maxy=NA)
    t$minx <- bbox$minx - 0.125 * (bbox$maxx - bbox$minx)
    t$maxx <- bbox$maxx + 0.125 * (bbox$maxx - bbox$minx)
    t$miny <- bbox$miny - 0.125 * (bbox$maxy - bbox$miny)
    t$maxy <- bbox$maxy + 0.125 * (bbox$maxy - bbox$miny)
    
    t
}