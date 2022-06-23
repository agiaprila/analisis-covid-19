library(shiny)
library(leaflet)
library(rgdal)
library(openxlsx)
library(dplyr)



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Peta Covid"), #1

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("variable", "Pilih Variabel:",
                      c("Kasus Positif" = 9,
                        "Kasus Meninggal" = 10,
                        "Kasus Sembuh" = 11)),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("peta_covid")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  peta <- readOGR("data/indonesia-prov.geojson")
  data_covid <- read.xlsx("data/Data_Harian_Kasus_per_Provinsi_COVID-19_Indonesia.xlsx")
  
  peta@data <- inner_join(peta@data, data_covid, by = c("kode" = "Kode_Provi"))
  
  output$peta_covid <- renderLeaflet({
    if(input$variable == '9'){
      var_terpilih = "Kasus Positif"
    } else if(input$variable == '10'){
      var_terpilih = "Kasus Meninggal"
    } else{
      var_terpilih = "Kasus Sembuh"
    }
    bins <- c(0, 5000, 7500, 10000, 15000, Inf)
    pal <- colorBin("YlOrRd", domain = peta@data[, as.numeric(input$variable)] , bins = bins)
    
    labels <- sprintf(
      "Nama: <strong>%s</strong><br/>Jumlah %s: %g  ",
      peta@data$Propinsi, var_terpilih, peta@data[, as.numeric(input$variable)]
    ) %>% lapply(htmltools::HTML)
    
    leaflet(peta) %>%
      addTiles()%>%
      addPolygons(
        fillColor = ~pal(peta@data[, as.numeric(input$variable)]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~peta@data[, as.numeric(input$variable)], opacity = 0.7, title = NULL,
                position = "bottomright")

  })

}

# Run the application 
shinyApp(ui = ui, server = server)
