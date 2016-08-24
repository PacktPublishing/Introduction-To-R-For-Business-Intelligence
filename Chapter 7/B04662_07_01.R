# Copyright 2016 Packt Publishing

# Introduction to R for Business Intelligence
# Chapter 7, Visualizing the Data’s Story

message("Introduction to R for Business Intelligence
        Chapter 7 - Visualizing the Data's Story
        Copyright (2016) Packt Publishing \n
        Let's look at ways to visualize data")

#
# Visualizing Data

#
# Plotting with ggplot2

plot_dat <- read.csv("./data/Ch7_marketing.csv")
plot_dat$emp_size <- cut(plot_dat$employees, breaks = 3,
          labels = c("Employees: 3 - 6", "7 - 9", "10+"))

if(!require("ggplot2")) install.packages("ggplot2")
suppressMessages(suppressWarnings(library(ggplot2)))
if(!require("scales")) install.packages("scales")
suppressMessages(suppressWarnings(library(scales)))
plot <- ggplot(data = plot_dat, aes(x = marketing_total,
                                    y = revenues))

plot <- plot + facet_grid(. ~ emp_size) + 
     geom_point(aes(color = pop_density), shape = 18, size = 4)

plot + scale_y_continuous(labels = dollar,
                          breaks = pretty_breaks(n = 5)) +
     scale_x_continuous(labels = dollar, 
                        breaks = pretty_breaks(n = 5)) +
     scale_color_discrete(guide = guide_legend(
          title = "Population\nDensity")) +
     xlab("Marketing Expenditures ($K)") + ylab("Revenues ($K)")
rm(plot_dat, plot)

#
# Geo-mapping Using Leaflet

# Learning geo-mapping

stations <- read.csv("./data/Ch7_bike_station_locations.csv")

if(!require("magrittr")) install.packages("magrittr")
suppressMessages(suppressWarnings(library(magrittr)))
if(!require("leaflet")) install.packages("leaflet")
suppressMessages(suppressWarnings(library(leaflet)))
leaflet() %>%
     addTiles() %>%
     addMarkers(data = stations, ~longitude, ~latitude)

bike <- makeIcon("./data/bike.png", iconWidth = 20,
                 iconHeight = 20)
stations$popup <- paste0("Station Location #",
                       seq(1, nrow(stations)))
new_tile_url <- "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png"
new_tile_attribution_string <- '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'

leaflet() %>%
     addTiles(urlTemplate = new_tile_url,
              attribution = new_tile_attribution_string,
              options = providerTileOptions(noWrap = TRUE)) %>%
     addMarkers(data = stations, lng = ~longitude, lat = ~latitude,
                popup = ~popup, icon = bike)

rm(stations, bike, new_tile_url, new_tile_attribution_string)

# Extending geo-mapping functionality

short_path <- read.csv("./data/Ch7_optimal_maint_route.csv")
head(short_path)
nrow(short_path)

leaflet() %>%
     addTiles() %>%
     addCircleMarkers(data = short_path, lat = ~latitude,
                      lng = ~longitude, radius = 3,
                      popup = ~popup) %>%
     addPolylines(data = short_path, lat = ~latitude,
                  lng = ~longitude, color = "#A93E36",
                  opacity = .7)

rm(short_path)

#
# Creating Interactive Graphics Using rCharts

# Learning interactive graphing with JavaScript

dat <- read.csv("./data/Ch7_email_marketing_campaign.csv",
                check.names = FALSE)
head(dat)

if(!require("reshape2")) install.packages("reshape2")
suppressMessages(suppressWarnings(library(reshape2)))
dat2 <- melt(dat[, 2:6], id.vars = "Promotion",
             variable.name = "Event", value.name = "Outcome")
head(dat2)

dat2$Outcome <- ifelse(dat2$Outcome == "Y", 1, 0)
aggregate <- aggregate(Outcome ~ Promotion + Event, FUN = sum,
                       data = dat2)
aggregate

if(!require("devtools")) install.packages("devtools")
suppressMessages(suppressWarnings(library(devtools)))

install_github(repo = 'ramnathv/rCharts')
suppressMessages(suppressWarnings(library(rCharts)))
n1 <- nPlot(Outcome ~ Event, group = "Promotion",
            data = aggregate, type = "multiBarChart")

n1$xAxis(axisLabel = "Event Type", staggerLabels = FALSE,
         rotateLabels = 0)
n1$yAxis(axisLabel = "Conversions", width = 40,
         tickFormat = "#! d3.format('.0f') !#",
         showMaxMin = TRUE)
n1$chart(color = c("#006CB8", "#ED1C24"))
n1$params$width <- 700
n1$params$height <- 400
n1

rm(n1, aggregate, dat, dat2)

dat <- read.csv("./data/Ch7_email_marketing_conversions.csv",
                check.names = FALSE)
head(dat)

promotion1 <- dat[dat$Promotion == "10% off", 2:4]
colnames(promotion1) <- c("source", "target", "value")

sankeyPlot <- rCharts$new()
sankeyPlot$setLib("http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey")
sankeyPlot$set(
     data = promotion1,
     nodeWidth = 25,
     nodePadding = 100,
     layout = 800,
     width = 700,
     height = 500
)

sankeyPlot$setTemplate(
     afterScript = "
     <script>
     
     // loop through the nodes and find the total count of emails sent
     // we will use this to determine the percentages of each descendant node
     var total_emails_sent = 0;
     d3.selectAll('#{{ chartId }} svg .node text')
     .text( function(d) { 
     if(d.name == 'Sent Email') { 
     total_emails_sent = d.value 
     }
     })
     
     // loop through the nodes and rename them and format them
     d3.selectAll('#{{ chartId }} svg .node text')
     .text( function(d) { 
     if(d.name == 'Sent Email') { 
     return d.name 
     } else { 
     return d.name + ': ' + d3.format('.0%')(d.value / total_emails_sent)
     } 
     })
     .style('font-size', '13')
     .style('font-weight', 'bold')
     
     // loop through the links and add the conversion percentage
     // we will use this to determine the percentages of each descendant node
     d3.selectAll('#{{ chartId }} svg .link title')
     .text( function(d) { 
     return d.source.name + ' \\u2192 ' + 
     d.target.name + ': ' + d3.format('.0%')(d.value / d.source.value) })
     </script>
     ")

sankeyPlot