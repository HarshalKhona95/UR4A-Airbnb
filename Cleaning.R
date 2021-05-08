

#setwd("C:\\Users\\Kaashyap\\Downloads\\R_Shiny_Project")

#Load data

airbnb <- read.csv("listings_full.csv")

library(shiny)
library(shinythemes)
library(ggplot2)
library (sqldf)
library(caret)
library(Metrics)
library(randomForest)
library(gbm)

library(dplyr)
library(lubridate) 


drop_cols <-
  c(
    'summary',
    'space',
    'description',
    'experiences_offered',
    'neighborhood_overview',
    'notes',
    'transit',
    'access',
    'interaction',
    'house_rules',
    'host_name',
    'host_about',
    'host_total_listings_count',
    'neighbourhood',
    'market',
    'smart_location',
    'country_code',
    'country',
    'requires_license',
    'license',
    'is_business_travel_ready',
    'require_guest_profile_picture',
    'require_guest_phone_verification',
    'weekly_price',
    'monthly_price',
    'has_availability',
    'listing_url',
    'scrape_id',
    'last_scraped',
    'thumbnail_url',
    'medium_url',
    'picture_url',
    'xl_picture_url',
    'host_url',
    'host_thumbnail_url',
    'host_picture_url',
    'calendar_last_scraped',
    'jurisdiction_names'
  )



#Drop columns from data

for (i in drop_cols){
  airbnb[i] <- NULL
}

names <- names(airbnb)

for (i in 1:length(names)){
  
  airbnb[which(airbnb[,names[i]]=='N/A'),names[i]] <- NA 
  
}

for (i in 1:length(names)){
  
  airbnb[which(airbnb[,names[i]]==''),names[i]] <- NA 
  
}

#Load Data Quality to inspect NAs

source("DataQualityReport.R")

DataQualityReport(airbnb)



#Removing further columns from data after making NAs

drop_cols2 <- c('host_location','host_response_rate','host_acceptance_rate','host_listings_count','city','state','square_feet','last_review','neighbourhood_cleansed')

for (i in drop_cols2){
  airbnb[i] <- NULL
}

########### Numeric Cleaning ###########


num_cols <-
  c(
    'extra_people'
    ,'price'
    ,'security_deposit'
    ,'cleaning_fee'
  )

for (i in num_cols){
  airbnb[[i]] <- gsub('[$]','',airbnb[[i]])
  airbnb[[i]] <- as.numeric(gsub(',','',airbnb[[i]]))
}

#Imputing NAs with 0 for security_deposit and cleaning_fee variables

airbnb$security_deposit[is.na(airbnb$security_deposit)] <- 0

airbnb$cleaning_fee[is.na(airbnb$cleaning_fee)] <- 0

#Imputing NAs with median for other numerical variables



median_cols <-
  c(
    'bathrooms'
    ,'bedrooms'
    ,'beds'
    ,'review_scores_rating'
    ,'review_scores_accuracy'
    ,'review_scores_cleanliness'
    ,'review_scores_checkin'
    ,'review_scores_communication'
    ,'review_scores_location'
    ,'review_scores_value'
    ,'reviews_per_month'
    
  )

for (i in median_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- median(airbnb[[i]], na.rm = T)
}


########### Factor ###########

##### Imputing NAs in character vectors using 'Unknown' #####

unknown_cols <-
  c(
    'name'
    ,'host_response_time'
    ,'host_neighbourhood'
  )


for (i in unknown_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- 'Unknown'
}


##### Imputing NAs in character vectors using Mode #####

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_cols <-
  c(
    'host_is_superhost'
    ,'host_has_profile_pic'
    ,'host_identity_verified'
  )


for (i in mode_cols){
  airbnb[[i]][is.na(airbnb[[i]])] <- getmode(airbnb[[i]])
}


factor_cols <-
  c(
    'neighbourhood_group_cleansed'
    ,'property_type'
    ,'room_type'
    ,'bed_type'
    ,'cancellation_policy'
  )


for (i in factor_cols){
  airbnb[[i]] <- as.factor(airbnb[[i]])
}

#### Converting host_response_time into ordered factor ####

airbnb$host_response_time <- factor(airbnb$host_response_time, 
                                    levels = c("within an hour","within a few hours","within a day","a few days or more","Unknown"),
                                    ordered = T)

#### Breakdown top amenities into separate columns ####

library(sqldf)

airbnb <- sqldf("select *,
							case when amenities like '%Wifi%' then 'Yes' else 'No' end as Wifi,
							case when amenities like '%TV%' then 'Yes' else 'No' end as tv,
							case when amenities like '%Internet%' then 'Yes' else 'No' end as Internet,
							case when amenities like '%Kitchen%' then 'Yes' else 'No' end as Kitchen,
							case when amenities like '%Heating%' then 'Yes' else 'No' end as Heating,
							case when amenities like '%Washer%' then 'Yes' else 'No' end as Washer,
							case when amenities like '%Refrigerator%' then 'Yes' else 'No' end as Refrigerator,
							case when amenities like '%Dishwasher%' then 'Yes' else 'No' end as Dishwasher,
							case when amenities like '%Dryer%' then 'Yes' else 'No' end as Dryer,
							case when amenities like '%Microwave%' then 'Yes' else 'No' end as Microwave,
							case when amenities like '%Stove%' then 'Yes' else 'No' end as Stove,
							case when amenities like '%Paid parking on premises%' then 'Yes' else 'No' end as Premise_Part,
							case when amenities like '%Paid parking off premises%' then 'Yes' else 'No' end as Off_Premise_Part
						from airbnb")


str(airbnb)

#### Factor amenities ####

amenities <-
  c(
    'Wifi'
    ,'tv'
    ,'Internet'
    ,'Kitchen'
    ,'Heating'
    ,'Washer'
    ,'Refrigerator'
    ,'Dishwasher'
    ,'Dryer'
    ,'Microwave'
    ,'Stove'
    ,'Premise_Part'
    ,'Off_Premise_Part'
  )


for (i in amenities){
  airbnb[[i]] <- as.factor(airbnb[[i]])
}




#### Convert True/False into 1s and 0s ####

true_false_cols <- 
  c(
    'host_is_superhost'
    ,'host_has_profile_pic'
    ,'host_identity_verified'
    ,'instant_bookable'
  )

for (i in true_false_cols) {
  airbnb[[i]] <- (ifelse(airbnb[[i]]=="t",1,0))
}

airbnb$host_since <- as.Date(airbnb$host_since, origin = "1970-01-01")
airbnb$first_review <- as.Date(airbnb$first_review,origin = "1970-01-01")

max_date <- as.numeric(max(airbnb$host_since, na.rm = T))

min_date <- as.numeric(min(airbnb$host_since, na.rm = T))

final_date_val <- (max_date+min_date)/2

final_date <- as.Date(final_date_val, origin = "1970-01-01")

airbnb[["host_since"]][is.na(airbnb[["host_since"]])] <- final_date


airbnb$first_review <- as.Date(ifelse(is.na(airbnb$first_review), airbnb$host_since, airbnb$first_review), origin = "1970-01-01")


######## Final data for visualization ########

colnames(airbnb)

airbnb2 <- airbnb[1:52]

for (i in true_false_cols){
  airbnb2[[i]] <- as.factor(airbnb2[[i]])
}

airbnb2 <- sqldf("select *,
							case when amenities like '%Wifi%' then 1 else 0 end as Wifi,
							case when amenities like '%TV%' then 1 else 0 end as tv,
							case when amenities like '%Internet%' then 1 else 0 end as Internet,
							case when amenities like '%Kitchen%' then 1 else 0 end as Kitchen,
							case when amenities like '%Heating%' then 1 else 0 end as Heating,
							case when amenities like '%Washer%' then 1 else 0 end as Washer,
							case when amenities like '%Refrigerator%' then 1 else 0 end as Refrigerator,
							case when amenities like '%Dishwasher%' then 1 else 0 end as Dishwasher,
							case when amenities like '%Dryer%' then 1 else 0 end as Dryer,
							case when amenities like '%Microwave%' then 1 else 0 end as Microwave,
							case when amenities like '%Stove%' then 1 else 0 end as Stove,
							case when amenities like '%Paid parking on premises%' then 1 else 0 end as Premise_Part,
							case when amenities like '%Paid parking off premises%' then 1 else 0 end as Off_Premise_Part
						from airbnb2")

#### Drop amenities and host_verifications ####

airbnb$amenities <- NULL
airbnb$host_verifications <- NULL

airbnb2$amenities <- NULL
airbnb2$host_verifications <- NULL


for (i in names(airbnb2)[51:63]){
  airbnb2[[i]] <- as.factor(airbnb2[[i]])
}



write.csv(airbnb2, "airbnb_cleaned_data1.csv", row.names = F)

###### EDA and graphs

dev.off()

par(mfrow=c(3,3), bg="white", fg="black",cex.lab=1.2, cex.axis=.9, cex.main=1.5)
amenities<-c("Wifi", "tv", "Internet", "Kitchen", "Heating", "Washer", "Refrigerator", "Dishwasher", "Dryer")
for(i in amenities){
  plot(airbnb2[,i], main=paste0("Plot of ", names(airbnb2[i])), xlab=names(airbnb2[i]), col="gold")
}


###Geo charts:

airbnb2$price <- as.numeric(airbnb2$price)

par(mfrow=c(1,1))

dev.off()

ggplot(data=subset(airbnb2,price<200))+geom_point(aes(x=latitude, y=longitude, color=price)) + scale_colour_gradientn(colours = terrain.colors(10))+ ggtitle("Price per night across Berlin Airbnb listings") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data=subset(airbnb2,price<200))+geom_point(aes(x=latitude, y=longitude, color=neighbourhood_group_cleansed))+ ggtitle("Distribution of listings across Berlin") + theme(plot.title = element_text(hjust = 0.5))
