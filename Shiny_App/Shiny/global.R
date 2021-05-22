library(readr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(stringr)
library(shinyjs)
library(rintrojs)
# install.packages("rintrojs")

# install.packages('rsconnect')
# library(rsconnect)
# rsconnect::setAccountInfo(name='philipp-schmelzer-hsg',
#                           token='161748CCB122B1478377A8110F1AD8CE',
#                           secret='e5v/w7kA3VIkLeGDYEZvUvqVqZrul2vOTf6OZyCb')
#https://philipp-schmelzer-hsg.shinyapps.io/shiny/

#rsconnect::deployApp("/Users/philippschmelzer/Desktop/Masterarbeit/Shiny_App/Shiny")

# Clean any stored variables, constants,environment
# rm(list=ls())
getwd()

#Preparation_Icons------------------------------------------------------------------------------

www <- "www/"
#Preparation_Intro------------------------------------------------------------------------------

intro <- read_csv2("intro.csv")

# Preparation_Datasets------------------------------------------------------------------------

#import dates
time_series <-  as.data.frame(read.csv("prediction_output/time_series_test.csv"))
time_series$X=NULL
time_series$Date = as.Date(time_series$Date, format="%Y-%m-%d")
# min(time_series$Date)
# max(time_series$Date)

#add features for display of icons------------------------------------------------------------------------
#add feature occuptation------------------------------------------------------------------------
time_series$day <- weekdays(time_series$Date)
time_series$occupation[time_series$day=="Donnerstag"] <- "low"
time_series$occupation[time_series$day=="Freitag"] <- "low"  
time_series$occupation[time_series$day=="Mittwoch"] <- "middle" 
time_series$occupation[time_series$day=="Samstag"] <- "middle" 
time_series$occupation[time_series$day=="Montag"] <- "high" 
time_series$occupation[time_series$day=="Dienstag"] <- "high" 
time_series$occupation[time_series$day=="Sonntag"] <- "high" 
#add feature season------------------------------------------------------------------------
time_series$month <- month(time_series$Date)
time_series$season[time_series$month=="3"] <- "cold"
time_series$season[time_series$month=="4"] <- "warm"
#add feature weather------------------------------------------------------------------------
time_series$week <- week(time_series$Date)
time_series$weather[time_series$week=="11"] <- "sunny"
time_series$weather[time_series$week=="12"] <- "rainy"  
time_series$weather[time_series$week=="13"] <- "cloudy"  
time_series$weather[time_series$week=="14"] <- "sunny"  
#add feature terrace open------------------------------------------------------------------------
time_series$terrace[time_series$weather == "sunny"] <- "terrace"
time_series$terrace[!time_series$weather == "sunny"] <- "no_terrace"
#add feature events------------------------------------------------------------------------
#(=Friday)
time_series$event[time_series$day == "Freitag" ] <- "event"
time_series$event[!time_series$day == "Freitag"] <- "no_event"
#conditionals
time_series$occupation[time_series$event=="event"] <- "high" 
time_series$event[time_series$weather=="rainy"] <- "no_event" 
time_series$terrace[time_series$weather=="rainy"] <- "no_terrace" 
time_series$Date <- as.character(time_series$Date)
time_series$month <- as.character(time_series$month)
time_series$week <- as.character(time_series$week)
#import names of Items------------------------------------------------------------------------
Items <-  as.data.frame(read.csv("prediction_output/fix_items.csv"))
Items$X=NULL
colnames(Items)[1] <- "Item"
Items$Item <- as.character(Items$Item)
#import predictions------------------------------------------------------------------------
predicted_values <- read.csv("prediction_output/predicted_values_df.csv")
predicted_values <- as.data.frame(predicted_values)
colnames(predicted_values)[1] <- "Item"
colnames(predicted_values)[2:29] <- as.character(time_series$Date)
alt_options <- predicted_values
alt_options$Item <- as.character(alt_options$Item)
alt_options[3,1] <- "Hot chocolate"
alt_options[10,1] <- "Farm House"
alt_options[21,1] <- "Spanish Brunch"
rownames(predicted_values) <- Items$Item
predicted_values$Item = NULL

# str(predicted_values)

#import actuals------------------------------------------------------------------------
actual_values <- read.csv("prediction_output/actual_values_df.csv")
actual_values <- as.data.frame(actual_values)
colnames(actual_values)[1] <- "Item"
colnames(actual_values)[2:29] <- as.character(time_series$Date)
rownames(actual_values) <- rownames(predicted_values)
actual_values$Item = NULL
# str(actual_values) 



