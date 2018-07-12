library(classInt)

setwd("/dmine/data/counties/")

counties <- readShapePoly('UScounties_conus.shp',
                          proj4string=CRS
                          ("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
projection = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#counties <- counties[grep("Idaho|Washington|Oregon|Montana", counties@data$STATE_NAME),]
#counties <- counties[grep(input$state2, counties@data$STATE_NAME),]


      setwd("/dmine/data/USDA/crop_indemnity_originals")
      files2 <- list.files(pattern = "*.csv")
      myfiles2 = lapply(files2, read.csv, strip.white = TRUE, header = FALSE)
      y <- do.call(rbind, myfiles2)
      x <- as.data.frame(y)
      
      
      
colnames(x) <- c("year", "statecode", "state", "countycode", "county", "commoditycode", "commodity", "insurancecode", "insurance", "stagecode", "damagecausecode", "damagecause", "monthcode", "month", "loss" )      
xx <- aggregate(x$loss, list(x$year, x$state, x$county, x$commodity), FUN = "sum")
colnames(xx) <- c("year", "STATE_NAME", "NAME", "commodity", "loss")

setwd("/dmine/data/USDA/crop_indemnity_originals_aggregated/")
write.csv(xx, file = "commodities.csv")


xx <- read.csv("/dmine/data/USDA/crop_indemnity_originals_aggregated/commodities.csv", header = TRUE)


xx$STATE_NAME <- state.name[match(xx[,3], state.abb)]


xxx <- subset(xx, year == "2013" & commodity == "WHEAT")






m <- merge(counties, xxx, by=c("STATE_NAME", "NAME"))
m$loss[is.na(m$loss)] <- 0
m$log10loss <- log10(m$loss)
m$log10loss <- ifelse(m$log10loss < 0, 0, m$log10loss)

bin_it <- function(START, END, BINS) {
  range <- END-START
  jump <- range/15
  v1 <- c(START, seq(START+jump+1, END, jump))
  v2 <- seq(START+jump-1, END, jump)+1
  data.frame(v1, v2)
}

range1 <- min(m$loss)
range2 <- max(m$loss)
binz <- t(bin_it(range1, range2))

#brewer.pal(n = 10, name = "Spectral")
pal <- colorNumeric(palette = c("white", "yellow", "orange", "darkorange", "red", "darkred"),
                    domain = m$log10loss)
exte <- as.vector(extent(counties))

#pal <- colorBin(palette = "YlOrRd",
#                    domain = m$loss, bins = binz)

label <- paste(sep = "<br/>", m$NAME, round(m$loss, 0))
markers <- data.frame(label)
labs <- as.list(m$loss)

leaflet(data = m) %>% addProviderTiles("Stamen.TonerLite") %>% fitBounds(exte[1], exte[3], exte[2], exte[4]) %>% addPolygons(color = ~pal(log10loss), weight = 1, popup = markers$label) %>%
  addLegend(pal = pal, values = ~log10loss, opacity = 0.5, title = NULL,
            position = "bottomright")



