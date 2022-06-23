library(rgdal) #membaca data shp aau geojson
library(openxlsx) #membuka file excel
library(leaflet) #peta interaktif
library(dplyr) #dplyr menyiapkan data


peta <- readOGR("E:/R Course/DQLAB/pertemuan 7 - Analisa Covid/pertemuan 7 - Analisa Covid/data/indonesia-prov.geojson")
data_covid <- read.xlsx("E:/R Course/DQLAB/pertemuan 7 - Analisa Covid/pertemuan 7 - Analisa Covid/data/Data_Harian_Kasus_per_Provinsi_COVID-19_Indonesia.xlsx")
head(data_covid)

peta@data <- inner_join(peta@data, data_covid, by = c("kode" = "Kode_Provi"))
head(peta@data)

bins <- c(0, 5000, 7500, 10000, 15000, Inf)
pal <- colorBin("YlOrRd", domain = peta@data$Kasus_Positif , bins = bins)

labels <- sprintf(
  "Nama: <strong>%s</strong><br/>Jumlah: %g  ",
  peta@data$Propinsi, peta@data$Kasus_Positif
) %>% lapply(htmltools::HTML)


leaflet(peta) %>%
  addTiles() %>%
  addPolygons()


leaflet(peta) %>%
  addTiles()%>%
  addPolygons(
    fillColor = ~pal(peta@data$Kasus_Positif),
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
  addLegend(pal = pal, values = ~peta@data$Kasus_Positif, opacity = 0.7, title = NULL,
            position = "bottomright")

