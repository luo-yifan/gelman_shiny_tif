#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(raster)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    
    # Sidebar with a slider input for number of bins
    
    
    leafletOutput("mymap"),
    p(),
    sidebarPanel(
        sliderInput(
            "slider",
            "Dates:",
            min = as.Date("1986-02-01", "%Y-%m-%d"),
            max = as.Date("2022-11-01", "%Y-%m-%d"),
            value = as.Date("2016-12-01"),
            timeFormat = "%Y-%m"
        )
    ),
    
    
    # Show a plot of the generated distribution
    
    
)
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
projectPath = getwd()

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$mymap <- renderLeaflet({
        date_time = format(input$slider, "%Y%m")
        imgPath = paste(projectPath,
                        "/data/tif/Conc.",
                        date_time,
                        ".tif",
                        sep = "")
        r <- raster(imgPath)
        color_t = rev(
            c(
                '#152814',
                '#33481C',
                '#696C21',
                '#956720',
                '#C2361C',
                '#CA382D',
                '#D13F42',
                '#D85160',
                '#DE647C',
                '#E47796',
                '#EA8AAE'
            )
        )
        
        pal <-
            colorQuantile(
                color_t,
                c(4 , 7.2 , 85 , 150 , 280 , 500 , 1000 , 1900 , 3000, 5000, 3000000),
                n = 12,
                na.color = "transparent"
            )
        
        leaflet() %>%
            addProviderTiles(providers$Esri.WorldTopoMap,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addRasterImage(r,
                           colors = pal,
                           opacity = 0.8,
                           maxBytes = 123123123) %>%
            setView(lng = -83.792,
                    lat = 42.284,
                    zoom = 14) %>%
            addLegend(
                "bottomright",
                colors = color_t,
                labels = c(
                    '<4' ,
                    '4-7.2' ,
                    '7.2-85' ,
                    '85-150' ,
                    '150-280' ,
                    '280-500' ,
                    '500-1000' ,
                    '1000-1900' ,
                    '1900-3000',
                    '3000-5000',
                    '>5000'
                ),
                opacity = 0.8
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
#
# library(rsconnect)
# rsconnect::setAccountInfo(name='yifanluo',
#                           token='801F6411CB4C8EFD33298155AE6A8725',
#                           secret='0B0CMhurDaBk0BtwqUEx+VxVguLFT+9yt4VYzwx2')
#
# rsconnect::deployApp()
