library(shiny, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(caret, warn.conflicts = FALSE)
#library(gbm, warn.conflicts = FALSE)
library(leaflet, warn.conflicts = FALSE)
library(ggplot2)
library(cowplot)
library(readr)
library(shinythemes)

NYC <- read_csv("listings.csv", 
                col_types = cols(host_id = col_skip(), 
                                 host_name = col_skip(), id = col_skip(), 
                                 latitude = col_number(), longitude = col_number(), 
                                 price = col_number()))
NYC <- as.data.frame(NYC)

rownames(NYC) <- NULL
choice <- colnames(NYC)


NYC <- NYC %>% 
  mutate(price_group=ifelse(price < 56, "Very Low",
                            ifelse(price < 101, "Low",
                                   ifelse(price < 121, "Moderate",
                                          ifelse(price < 181, "High", "Very High")))))


mean_room_type <- aggregate(list(average_price = NYC$price), list(room_type = NYC$room_type), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type

theme1 <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 19),
  axis.title.y = element_text(size = 19),
  legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
options(warn=-1)





#####Predictions

#### Y and X data for Regression ####

airbnb3 <- read.csv("airbnb_cleaned_data.csv")

colnames(airbnb3)

d <- airbnb3

d[19]

d <-d[,c(19, 5, 6, 8, 9, 10, 14, 16, 17, 18, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)]

names(d)[1] <- "y"

capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

# for (i in names(d[sapply(d, is.numeric)])) {

# d[[i]]=capOutlier(d[[i]])

# }

d$y <- capOutlier(d$y)

# summary(d$y)
# 
# boxplot(d$y)

library(caret)

################################################################################
# Data partitioning
################################################################################


cols <- c("y", "neighbourhood_group_cleansed", "room_type", "beds","bathrooms", "bedrooms")


d2 <- d[cols]


set.seed(42) # set a seed so you can replicate your results

# identify records that will be used in the training set. Here we are doing a
# 80/20 train-test split
inTrain <- createDataPartition(y = d2$y,   # outcome variable
                               p = .8,   # % of training data you want
                               list = F)
# create your partitions
train <- d2[inTrain,]  # training data set
test <- d2[-inTrain,]  # test data set

################################################################################
# Modeling
################################################################################

##### Linear Model

lm_model = lm(formula = y ~ .,data = train)

summary(lm_model)

y_lm_train = predict(lm_model, train)
# 
# library(Metrics)
# 
# rmse.lm<-rmse(train$y, y_lm_train)
# print(rmse.lm)
# 
#
# 
# y_lm_pred = predict(lm_model, test)
# 
# rmse.lm<-rmse(test$y, y_lm_pred)
# print(rmse.lm)
# 


# ##transformation
# lm_model2 = lm(formula = log(y) ~ .,data = train)
# summary(lm_model2)
# 
# y_lm_train = predict(lm_model2, train)
# 
# library(Metrics)
# 
# rmse.lm<-rmse(train$y, y_lm_train)
# print(rmse.lm)
# 
# 
# 
# y_lm_pred = predict(lm_model2, test)
# 
# rmse.lm<-rmse(test$y, y_lm_pred)
# print(rmse.lm)




##################### GBM #####################

# library(gbm)
# 
# set.seed(42)
# gbm.gbm <- gbm(y ~ .
#                , data=train
#                , distribution="gaussian"
#                , n.trees=1000
#                , interaction.depth=3
#                , n.minobsinnode=10
#                , shrinkage=0.1
#                , bag.fraction=0.75
#                , cv.folds=10
#                , verbose=FALSE
# )
# best.iter <- gbm.perf(gbm.gbm, method="cv")
# 
# print(best.iter)
# 
# 
# train.predict <- predict.gbm(object=gbm.gbm, newdata=train, best.iter)


# rmse.gbm<-rmse(train$y, train.predict)
# print(rmse.gbm)


# y_gbm_pred <- predict.gbm(object=gbm.gbm, newdata=test, best.iter)

# rmse.gbm<-rmse(test$y, y_gbm_pred)
# print(rmse.gbm)




test2 <- list(neighbourhood_group_cleansed = "Queens",
              room_type = "Hotel room", bedrooms = 3, beds = 2,
              bathrooms = 2)


test_data2 <- data.frame(lapply(test2, function(x) t(data.frame(x))))

# y_pred_final = predict.gbm(gbm.gbm,test_data2, best.iter)
# y_pred_final


y_pred_final = predict(lm_model,test_data2)
y_pred_final



#######################



shinyServer <- function(input, output, session){
  
  observe({
    selected1=unique(NYC[NYC$neighbourhood_group==input$selected0,'neighbourhood'])
    updateSelectizeInput(
      session,'selected1',
      choices = selected1,
      selected = selected1[1]
    )
  })
  
  
  listingNYC <- reactive({
    selected0 = input$selected0
    selected1 = input$selected1
    selected2 = input$selected2
    NYC %>%
      select(neighbourhood_group, neighbourhood,room_type, longitude,latitude,price) %>%
      filter(neighbourhood_group == selected0 & neighbourhood == selected1 & room_type == selected2)
  })
  output$map <- renderLeaflet({
    leaflet()%>% setView(lng = -73.9059, lat = 40.7128, zoom = 11) %>%
      addTiles() %>% addMarkers(lng = listingNYC()$longitude, lat = listingNYC()$latitude)
  })
  
  output$plotmap <- renderPlot({
    ggplot(listingNYCplot %>% filter(neighbourhood_group == input$selected0 & neighbourhood == input$selected1), aes(price, color=room_type)) + geom_freqpoly()
  })
  
  listingNYCplot <- NYC %>%
    select(neighbourhood_group, neighbourhood, room_type, price, minimum_nights, price_group, longitude,latitude,price)
  
  output$plot1 <- renderPlot({
    ggplot(listingNYCplot %>% filter(minimum_nights >= input$selected4[1] & minimum_nights <= input$selected4[2] &
                                       price >= input$selected3[1] & price <= input$selected3[2]), aes(neighbourhood_group)) + 
      geom_bar(aes(fill=room_type), position = position_dodge())
  })
  
  output$plot2 <- renderPlot({
    # listingNYCplot %>% group_by(neighbourhood_group) %>% su
    ggplot(listingNYCplot %>% filter(minimum_nights >= input$selected6[1] & minimum_nights <= input$selected6[2]),
           aes(neighbourhood_group, price)) + 
      geom_bar(aes(fill=room_type), position = position_dodge(),stat = "summary", fun = "mean")
  })
  output$plot3 <- renderPlot({
    ggplot(listingNYCplot %>% filter(minimum_nights >= input$selected5[1] & minimum_nights <= input$selected5[2] & neighbourhood_group == input$selected7),
           aes(neighbourhood, price)) + 
      geom_bar(aes(fill=room_type), position = position_dodge(),stat = "summary", fun = "mean") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  
  
  output$plot_count_by_nh <- renderPlot({
    ggplot(NYC, aes(neighbourhood_group)) + geom_bar(aes(fill = price_group))
  })
  output$plot_density_by_nh <- renderPlot({
    ggplot(NYC, aes(longitude,latitude)) + geom_density2d(aes(color = neighbourhood_group))
  })
  output$plot_density_by_price <- renderPlot({
    ggplot(subset(NYC, price_group == "Very High" | price_group == "Very Low"), aes(longitude,latitude)) + geom_density2d(aes(color = price_group))
  })
  output$plot_count_by_rt <- renderPlot({
    ggplot(NYC, aes(room_type)) + geom_bar(aes(fill = price_group))
  })
  
  output$pop_den_by_price <- renderPlot({
    ggplot(subset(NYC, price < 500), aes(price)) + geom_density(aes(fill = price_group), alpha=0.35)
  })
  output$pop_den_by_minnights <- renderPlot({
    ggplot(subset(NYC, minimum_nights < 50), aes(minimum_nights)) + geom_density()
  })
  
  th <- theme(axis.title = element_text(), axis.title.x = element_text()) # global theme for ggplot2 objects
  
  output$box_plot <- renderPlot({
    ggplot(NYC, aes(x = room_type, y = price)) +
      geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
      th + 
      xlab("Room type") + 
      ylab("Price") +
      geom_hline(yintercept = mean(NYC$price), color = "purple", linetype = 2)
  })
  
  
  output$price_perRT <- renderPlot({
    ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
      coord_flip() +
      geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
      geom_point(size=7, mapping = aes(color = room_type)) +
      theme_minimal() +
      xlab("") +
      ylab("") +
      theme1
  })
  
  options(repr.plot.width=14, repr.plot.height=6)
  df <- data.frame(price = NYC["price"][NYC["price"] <= 500], room_type = NYC["room_type"][NYC["price"] <= 500])
  
  output$price_density <- renderPlot({
    ggplot(data = df, mapping = aes(x = price, fill = room_type)) +
      geom_density(mapping = aes(fill = room_type), bins = 70, size = 1.3, color = "black", alpha = .6, size = 1.5) +
      theme_minimal() +
      ylab("Density") +
      xlab("Price") +
      theme1 +
      theme(legend.position="right", legend.text = element_text(colour="black", size=20, face="bold"))
  })
  
  top_10_neighbourhood <- aggregate(list(NYC$price), list(NYC$neighbourhood), mean)
  colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
  top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]
  top_10_neighbourhood <- tail(top_10_neighbourhood, 12)
  top_10_neighbourhood <- head(top_10_neighbourhood, 10)
  r <- c()
  for(i in 10:1){r <- c(r, i)}
  row.names(top_10_neighbourhood) <- r
  top_10_neighbourhood
  
  options(repr.plot.width=15, repr.plot.height=11)
  
  output$expensive_nh <- renderPlot({
    ggplot(data = top_10_neighbourhood, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
      geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
      geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
      coord_flip() + 
      xlab("") +
      ylab("") +
      theme1
  })
  
  top_10_neighbourhood_b <- aggregate(list(NYC$price), list(NYC$neighbourhood), mean)
  colnames(top_10_neighbourhood_b) <- c("neighbourhood", "Average_price_per_neighborhood")
  top_10_neighbourhood_b <- top_10_neighbourhood_b[order(top_10_neighbourhood_b$Average_price_per_neighborhood),]
  top_10_neighbourhood_b <- head(top_10_neighbourhood_b, 10)
  r <- c()
  for(i in 1:10){r <- c(r, i)}
  row.names(top_10_neighbourhood_b) <- r
  top_10_neighbourhood_b
  options(repr.plot.width=15, repr.plot.height=6.5)
  
  output$cheapest_nh <- renderPlot({
    ggplot(data = top_10_neighbourhood_b, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
      geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
      geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
      coord_flip() + 
      xlab("") +
      ylab("") +
      theme1
  })
  
  theme2 <- theme(plot.title = element_text(size = 23, hjust = .5),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  legend.text = element_text(colour="black", size=19, face="bold"),
                  legend.position="right",
                  legend.background = element_rect(fill="#F5FFFA", size=0.5, linetype="dashed", colour ="black"))
  
  df <- data.frame(price = NYC["price"][NYC["price"] <= 300], room_type = NYC["room_type"][NYC["price"] <= 300], lat = NYC["latitude"][NYC["price"] <= 300], lon = NYC["longitude"][NYC["price"] <= 300],
                   neighbourhood_group = NYC["neighbourhood_group"][NYC["price"] <= 300], minimum_nights = NYC["minimum_nights"][NYC["price"] <= 300])
  df$minimum_nights <- factor(df$minimum_nights)
  options(repr.plot.width=22, repr.plot.height=14)
  
  output$geoplot <- renderPlot({
    a<-ggplot(data = df, mapping = aes(x = lat, y = lon, colour = neighbourhood_group)) +
      theme_minimal() +
      scale_fill_identity() +
      geom_point(mapping = aes(colour = neighbourhood_group), size = 3) +
      ggtitle("Price <= 300 dollars") +
      xlab("Latitude") +
      ylab("Longitude") +
      theme2
    
    b<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
      theme_minimal() +
      scale_fill_identity() +
      geom_point(mapping = aes(color = price), size = 3) +
      ggtitle("Price <= 300 dollars ") +
      xlab("Latitude") +
      ylab("Longitude") +
      theme2
    
    c<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
      theme_minimal() +
      scale_fill_identity() +
      geom_point(mapping = aes(color = room_type), size = 3) +
      ggtitle("Price <= 300 dollars ") +
      xlab("Latitude") +
      ylab("Longitude") +
      theme2
    
    plot_grid(a, b, c, ncol=3, nrow=1)
  })
  
  
  test_data2 <- eventReactive(input$click,{
    data.frame(
      room_type = input$room_type,
      bedrooms = input$bedrooms,
      neighbourhood_group_cleansed = input$nbhood,
      beds = input$beds,
      bathrooms = input$bathrooms
      # tv = input$tv,
      # Wifi = input$Wifi,
      # Heating = input$Heating
    )})
  
  
  y_pred_final <- eventReactive(input$click,{predict.lm(object=lm_model,test_data2(),)})
  
  airbnb_d <- eventReactive(input$click, {subset(airbnb3,airbnb3$neighbourhood_group_cleansed==input$nbhood)})
  
  heading <- eventReactive(input$click, {input$nbhood})
  
  data_gg_data <- eventReactive(input$click, {subset(month_agg,neighbourhood_group_cleansed==input$nbhood)})
  
  output$Prediction <- renderText(print(paste("Estimated Price:",round(y_pred_final(),2),"Dollars")))
  
  
  
  
}


shinyUI <- dashboardPage(skin = "red", title = "Airbnb",
                         dashboardHeader(title = tags$a(href='https://www.airbnb.com/host/homes',
                                                        tags$img(src='https://cdn.freebiesupply.com/logos/large/2x/airbnb-2-logo-black-and-white.png',height='45',width='60'))),
                         dashboardSidebar(
                           #sidebarUserPanel(),
                           sidebarMenu(
                             #menuItem("Intro", tabName = "intro", icon = icon("image")),
                             menuItem("Map", tabName = "map", icon = icon("map")),
                             menuItem("Bar Plot", tabName = "plot1", icon = icon("bar-chart-o")),
                             menuItem("Price Chart", tabName = "price", icon = icon("chart-line")),
                             menuItem("Exploratory Data Analysis", tabName = "EDA_1", icon = icon("chart-bar")),
                             #menuItem("EDA_2", tabName = "EDA_2", icon = icon("chart-bar")),
                             menuItem("Predictions", tabName = "Predictions", icon = icon("chart-line")),
                             menuItem("About Us", tabName = "me", icon = icon("user-circle")))
                         ),
                         dashboardBody(
                           tabItems(
                             # tabItem(tabName = "intro",
                             #         HTML('<img src="https://images.pexels.com/photos/1525612/pexels-photo-1525612.jpeg?auto=compress&cs=tinysrgb&h=750&w=1260" width=500 height=700>')
                             # ),
                             # 
                             tabItem(tabName = "map",
                                     fluidPage(
                                       
                                       box(
                                         
                                         title = "Listings on Map",
                                         leafletOutput("map"),
                                         selectizeInput("selected0",
                                                        "Select Borough",
                                                        unique(NYC$neighbourhood_group), selected = 'Manhattan'),
                                         
                                         selectizeInput("selected1",
                                                        "Select Neighbourhood",
                                                        unique(NYC$neighbourhood)),
                                         
                                         selectizeInput("selected2",
                                                        "Select Room Type",
                                                        unique(NYC$room_type))),
                                     )),
                             
                             tabItem(tabName = "price",
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Borough",
                                                          fluidRow(
                                                            plotOutput('plot2'),
                                                            box(
                                                              sliderInput("selected6",
                                                                          "Select Number of Nights",
                                                                          min = 1, max = 30, value = c(1, 30))
                                                            )
                                                          )
                                                 ),
                                                 tabPanel("Neighborhood",
                                                          fluidRow(
                                                            plotOutput('plot3'),
                                                            box(
                                                              selectizeInput("selected7",
                                                                             "Select Borough",
                                                                             unique(NYC$neighbourhood_group)),
                                                              sliderInput("selected5",
                                                                          "Select Number of Nights",
                                                                          min = 1, max = 30, value = c(1, 30))
                                                            )
                                                          ))
                                                 
                                     )),
                             
                             tabItem(tabName = "plot1",
                                     fluidRow(
                                       # box(
                                       plotOutput('plot1'),
                                       box(sliderInput("selected3",
                                                       "Choose a Price Range", pre = "$",
                                                       min = 1, max = 400, value = c(1, 400)),
                                           sliderInput("selected4",
                                                       "Select Number of Nights",
                                                       min = 1, max = 30, value = c(1, 30))
                                       )
                                       
                                     )),
                             
                             tabItem(tabName = "EDA_1",
                                     fluidPage(
                                       tabBox(
                                         id = "tabset1",
                                         height = "5000px",
                                         width = "1000px",
                                         tabPanel("1",
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Count of Listings by Neighborhood",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('plot_count_by_nh')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Count of Listings by Room Type",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('plot_count_by_rt')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Listings Density By Neighborhood",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('plot_density_by_nh')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Density of Listings by Price",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('plot_density_by_price')
                                                    )
                                                  )
                                         ),
                                         tabPanel("2",
                                                  fluidRow(
                                                    # box(
                                                    plotOutput('geoplot')
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Listings Density By Price",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('pop_den_by_price')
                                                    )
                                                  ),
                                                  
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Boxplots of Price by Room-Type",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('box_plot')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Average Price Per Room Type",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('price_perRT')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Price Distribution by Room Types",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('price_density')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Top 10 Costliest Neighborhoods",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('expensive_nh')
                                                    )
                                                  ),
                                                  box(
                                                    collapsible = TRUE,
                                                    solidHeader = TRUE,
                                                    title = "Top 10 Cheapest Neighborhoods",
                                                    fluidRow(
                                                      # box(
                                                      plotOutput('cheapest_nh')
                                                    )
                                                  )
                                         )
                                       ))),
                             
                             tabItem(tabName = "Predictions",
                                     fluidPage(
                                       theme = shinytheme("united"),
                                       titlePanel("New York Airbnb Listings Price Estimator"),
                                       fluidRow(
                                         column(
                                           width = 2,
                                           selectInput('nbhood', 'Neighbourhood', choices = unique(d2$neighbourhood_group_cleansed)),
                                           hr()),
                                         column(
                                           width = 2,
                                           selectInput('room_type', 'Room Type', choices = unique(d2$room_type)),
                                           hr()),
                                         column(
                                           width = 2,
                                           sliderInput('bedrooms', 'Number of Bedrooms', min = 1, max = 8, value = 2),
                                           hr()),
                                         column(
                                           width = 2,
                                           sliderInput('beds', 'Number of Beds', min = 1, max = 10, value = 2),
                                           hr()),
                                         column(
                                           width = 2,
                                           sliderInput('bathrooms', 'Number of Bathrooms', min = 1, max = 5, value = 1),
                                           hr()),
                                         column(
                                           width = 2,
                                           actionButton("click", "Get Price"),
                                           hr()),
                                       ),
                                       
                                       fluidRow(
                                         
                                         column(
                                           width = 6,
                                           verbatimTextOutput("Prediction"),
                                           hr())
                                         
                                       ),
                                       
                                       fluidRow(
                                         column(width = 4, plotOutput("gmap_all", height = "350")),
                                         column(width = 4, plotOutput("gmap_neigh", height = "350")),
                                         column(width = 4, plotOutput("gbar", height = "350"))
                                       )
                                       
                                     )
                             ),
                             tabItem(tabName = "me",
                                     
                                     fluidPage(
                                       #h2("About this project:"),
                                       h3("Team Name: We R Shiny"),
                                       br(),
                                       tags$ol(style = "font-size:17px;",
                                               tags$li("Aanchal Arora - 0032113533"),
                                               br(),
                                               tags$li("Harshal Khona - 0032105043"),
                                               br(),
                                               tags$li("Kaashyap Chintala - 0032112635")
                                       ),
                                     ),
                             )
                           )
                         ))

shinyApp(shinyUI, shinyServer)

