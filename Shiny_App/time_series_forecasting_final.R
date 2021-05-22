#Author: Philipp Schmelzer, 14-603-245
#Masterthesis
#Time Series Forecasting model
#dataset: bakery_sales, 6 months of data from a bakery (October 30, 2018 to April 9, 2019)

#1. Preparation------------------------------------------------------------------------------
#Load Libraries
library(forecast)
library(randomForest)
library(data.table)
library(ggplot2)
library(dplyr)
library(ggpubr)
# Clean any stored variables, constants,environment
# rm(list=ls())

#RMSE calculation
RMSE=function(actual, predicted){
  rmse = sqrt(mean((actual-predicted)^2))
  return(rmse)
}

#Import Dataset
sales = fread("bakery_sales.csv")
#View(sales)
#str(sales)

#2. Overview: Dataset structure and content------------------------------------------------------------------------------

# Structure of dataset
nrow(sales) # 21293 rows
sum(is.na(sales)) # 0 N/As
str(sales)
min(sales$Date) # min Date 2018-10-30
max(sales$Date) # max Date 2019-04-09
unique(sales$Item) # 95 different products / items sold in bakery

#Formatting features
# Fix Date
sales$Date = as.Date(sales$Date, format="%Y-%m-%d")
sales$Time = NULL
#View(sales)

#3. Creation of Individual Featurest------------------------------------------------------------------------------
#Create quantity of transaction volume = 1
sales$quantity = 1

#Total Sales
sales[,.(quantity=sum(quantity)),by="Item"][order(-quantity)]
total_sales <- sales[,.(quantity=sum(quantity)),by="Item"][order(-quantity)]
sum(total_sales$quantity)
#in total there were 21293 Items sold over given timeframe, but item "NONE" accounting for 786.
#Therefore relevant total items sold is 20507

#create Product_id
total_sales$product_id <- seq(1:95)
#View(total_sales)

#change Item to product_Id
id_df <- sales %>% 
  select(Item) 
#3.1 Convert Product into Product_id------------------------------------------------------------------------------
#convert product into product_id taking into account demand
id_df[id_df$Item == "Coffee"] <- "1"
id_df[id_df$Item == "Bread"] <- "2"
id_df[id_df$Item == "Tea"] <- "3"
id_df[id_df$Item == "Cake"] <- "4"
id_df[id_df$Item == "Pastry"] <- "5"
id_df[id_df$Item == "NONE"] <- "6"
id_df[id_df$Item == "Sandwich"] <- "7"
id_df[id_df$Item == "Medialuna"] <- "8"
id_df[id_df$Item == "Hot chocolate"] <- "9"
id_df[id_df$Item == "Cookies"] <- "10"
id_df[id_df$Item == "Brownie"] <- "11"
id_df[id_df$Item == "Farm House"] <- "12"
id_df[id_df$Item == "Muffin"] <- "13"
id_df[id_df$Item == "Juice"] <- "14"
id_df[id_df$Item == "Alfajores"] <- "15"
id_df[id_df$Item == "Soup"] <- "16"
id_df[id_df$Item == "Scone"] <- "17"
id_df[id_df$Item == "Toast"] <- "18"
id_df[id_df$Item == "Scandinavian"] <- "19"
id_df[id_df$Item == "Truffles"] <- "20"
id_df[id_df$Item == "Coke"] <- "21"
id_df[id_df$Item == "Spanish Brunch"] <- "22"
id_df[id_df$Item == "Fudge"] <- "23"
id_df[id_df$Item == "Baguette"] <- "24"
id_df[id_df$Item == "Jam"] <- "25"
id_df[id_df$Item == "Tiffin"] <- "26"
id_df[id_df$Item == "Mineral water"] <- "27"
id_df[id_df$Item == "Jammie Dodgers"] <- "28"
id_df[id_df$Item == "Chicken Stew"] <- "29"
id_df[id_df$Item == "Hearty & Seasonal"] <- "30"
id_df[id_df$Item == "Salad"] <- "31"
id_df[id_df$Item == "Frittata"] <- "32"
id_df[id_df$Item == "Smoothies"] <- "33"
id_df[id_df$Item == "Keeping It Local"] <- "34"
id_df[id_df$Item == "The Nomad"] <- "35"
id_df[id_df$Item == "Focaccia"] <- "36"
id_df[id_df$Item == "Vegan mincepie"] <- "37"
id_df[id_df$Item == "Bakewell"] <- "38"
id_df[id_df$Item == "Tartine"] <- "39"
id_df[id_df$Item == "Afternoon with the baker"] <- "40"
id_df[id_df$Item == "Art Tray"] <- "41"
id_df[id_df$Item == "Extra Salami or Feta"] <- "42"
id_df[id_df$Item == "Eggs"] <- "43"
id_df[id_df$Item == "Granola"] <- "44"
id_df[id_df$Item == "Tshirt"] <- "45"
id_df[id_df$Item == "My-5 Fruit Shoot"] <- "46"
id_df[id_df$Item == "Ella's Kitchen Pouches"] <- "47"
id_df[id_df$Item == "Vegan Feast"] <- "48"
id_df[id_df$Item == "Crisps"] <- "49"
id_df[id_df$Item == "Dulce de Leche"] <- "50"
id_df[id_df$Item == "Valentine's card"] <- "51"
id_df[id_df$Item == "Pick and Mix Bowls"] <- "52"
id_df[id_df$Item == "Kids biscuit"] <- "53"
id_df[id_df$Item == "Duck egg"] <- "54"
id_df[id_df$Item == "Mighty Protein"] <- "55"
id_df[id_df$Item == "Christmas common"] <- "56"
id_df[id_df$Item == "Tacos/Fajita"] <- "57"
id_df[id_df$Item == "Postcard"] <- "58"
id_df[id_df$Item == "Chocolates"] <- "59"
id_df[id_df$Item == "Gingerbread syrup"] <- "60"
id_df[id_df$Item == "Muesli"] <- "61"
id_df[id_df$Item == "Nomad bag"] <- "62"
id_df[id_df$Item == "Muesli"] <- "61"
id_df[id_df$Item == "Drinking chocolate spoons"] <- "63"
id_df[id_df$Item == "Victorian Sponge"] <- "64"
id_df[id_df$Item == "Empanadas"] <- "65"
id_df[id_df$Item == "Coffee granules"] <- "66"
id_df[id_df$Item == "Argentina Night"] <- "67"
id_df[id_df$Item == "Basket"] <- "68"
id_df[id_df$Item == "Honey"] <- "69"
id_df[id_df$Item == "Lemon and coconut"] <- "70"
id_df[id_df$Item == "Crepes"] <- "71"
id_df[id_df$Item == "Pintxos"] <- "72"
id_df[id_df$Item == "Half slice Monster"] <- "73"
id_df[id_df$Item == "Bare Popcorn"] <- "74"
id_df[id_df$Item == "Panatone"] <- "75"
id_df[id_df$Item == "Mortimer"] <- "76"
id_df[id_df$Item == "Bread Pudding"] <- "77"
id_df[id_df$Item == "Caramel bites"] <- "78"
id_df[id_df$Item == "Brioche and salami"] <- "79"
id_df[id_df$Item == "Raspberry shortbread sandwich"] <- "80"
id_df[id_df$Item == "Cherry me Dried fruit"] <- "81"
id_df[id_df$Item == "Fairy Doors"] <- "82"
id_df[id_df$Item == "Bowl Nic Pitt"] <- "83"
id_df[id_df$Item == "Chimichurri Oil"] <- "84"
id_df[id_df$Item == "Spread"] <- "85"
id_df[id_df$Item == "Siblings"] <- "86"
id_df[id_df$Item == "Hack the stack"] <- "87"
id_df[id_df$Item == "Chicken sand"] <- "88"
id_df[id_df$Item == "The BART"] <- "89"
id_df[id_df$Item == "Adjustment"] <- "90"
id_df[id_df$Item == "Bacon"] <- "91"
id_df[id_df$Item == "Olum & polenta"] <- "92"
id_df[id_df$Item == "Polenta"] <- "93"
id_df[id_df$Item == "Gift voucher"] <- "94"
id_df[id_df$Item == "Raw bars"] <- "95"
#3.2 Creation of Dataset with all products------------------------------------------------------------------------------
#Create df with Date, quantity and Product_Id
total_df <- sales %>% 
  select(Date) 
total_df$Item <- id_df$Item
total_df$quantity <- sales$quantity
unique(total_df$Item)

# create complete times series
dateseq = data.table(Date=seq(min(sales$Date),max(sales$Date), by="days"))

#Create df in wide format with products summed up for given dateseq
items = unique(total_df$Item)
df_2= data.frame()
collector <- dateseq

for (z in 1:length(items)){
  df_2 <- total_df[Item == items[z], .(quantity = sum(quantity)), by = "Date"]
  collector <- left_join(collector, df_2, by = "Date", all.x=TRUE)
  colnames(collector)[z+1] <- items[z]
}

#Save collected columns in new df
product <- collector
#View(product)
#3.3 Creation of new features------------------------------------------------------------------------------
#is closed feature
product$is_closed = ifelse(is.na(product$`1`),1,0)
product[is.na(1),"1"]=0

# Creation of weekday and number of week feature
product$day_of_week <- weekdays(product$Date)
product$week_of_year <- week(product$Date)

#3.4 Creation of even dataset------------------------------------------------------------------------------
#create even weeks, starting with monday, ending with sunday
product <- product[-c(1:6, 162),]

#fill all NAs with 0
product[is.na(product)] <- 0
product = product[-155,]
#View(product)

#4. Prediction for individual Items------------------------------------------------------------------------------
#create helper 
table = c()
helper = c()
RMSE_indiv = c()
RMSE_help = c()

#Looping through df=products to pick item for prediction
for (i in items) {
  simple_dataset <- product %>% 
    select(Date, i, is_closed, day_of_week, week_of_year)
  
  # Create Test and Training Data of given product
  index = tail(1:nrow(simple_dataset),28)
  train_product = simple_dataset[-index,]
  test_product = simple_dataset[index,]
  # Explore Time Series
  # explore seasonality feature
  product_ts = ts(product[,2], frequency = 7)
  decomposed_ts = decompose(product_ts)
  autoplot(decomposed_ts)
  
  ### Random Forest
  #######################################
  # basic data prep
  train_product$Date=NULL
  test_product$Date=NULL
  
  train_product$day_of_week=as.numeric(as.factor(train_product$day_of_week))
  test_product$day_of_week=as.numeric(as.factor(test_product$day_of_week))
  
  # create lags
  train_product$quantity_lag7 = shift(train_product[,1], n=7, fill=NA, type="lag")
  train_product$quantity_lag8 = shift(train_product[,1], n=8, fill=NA, type="lag")
  train_product$quantity_lag9 = shift(train_product[,1], n=9, fill=NA, type="lag")
  train_product$quantity_lag10 = shift(train_product[,1], n=10, fill=NA, type="lag")
  train_product$quantity_lag11 = shift(train_product[,1], n=11, fill=NA, type="lag")
  train_product$quantity_lag12 = shift(train_product[,1], n=12, fill=NA, type="lag")
  train_product$quantity_lag13 = shift(train_product[,1], n=13, fill=NA, type="lag")
  train_product$quantity_lag14 = shift(train_product[,1], n=14, fill=NA, type="lag")
  #View(train_product)
  
  # decompose time series / Creation of Seasonality Feature
  train_product_decomposed <- decompose(ts(train_product[,1], frequency = 7))
  train_product$seasonal <- train_product_decomposed$seasonal
  # View(train_product)
  #only use complete cases
  train_product <- train_product[complete.cases(train_product),]
  df_forest <- train_product
  
  #rename product id to easily integrate it into random forest model
  colnames(df_forest)[1] <- 'amount'
  
  # create Model
  tsforest = randomForest(amount ~ ., df_forest, ntree=10000)
  colnames(df_forest)[1] <- colnames(train_product)[1]
  
  #set up prediction loop
  history = df_forest
  predictions =c()
  actuals = c()
  performance_collector = c()
  

  #for loop to predict weeks 11, 12, 13, 14
  for (v in unique(test_product$week_of_year)){
    
    dat = tail(history, 14)
    dat = rbind(dat, test_product[week_of_year == v,,], fill=TRUE)
    
    # create lags
    dat$quantity_lag7 = shift(dat[,1], n=7, fill=NA, type="lag")
    dat$quantity_lag8 = shift(dat[,1], n=8, fill=NA, type="lag")
    dat$quantity_lag9 = shift(dat[,1], n=9, fill=NA, type="lag")
    dat$quantity_lag10 = shift(dat[,1], n=10, fill=NA, type="lag")
    dat$quantity_lag11 = shift(dat[,1], n=11, fill=NA, type="lag")
    dat$quantity_lag12 = shift(dat[,1], n=12, fill=NA, type="lag")
    dat$quantity_lag13 = shift(dat[,1], n=13, fill=NA, type="lag")
    dat$quantity_lag14 = shift(dat[,1], n=14, fill=NA, type="lag")
    
    # decompose time series
    dat$seasonal[15:21]=dat$seasonal[1:7]
    
    dat=tail(dat,7)
    
    #make forecast
    predicted = predict(tsforest, dat)
    #create dummy df to save column name
    actual_df <- test_product
    colnames(actual_df)[1] <- "amount"
    actual = actual_df[week_of_year == v,,]$amount
    
    performance = RMSE(actual, predicted)
    
    # update history and collect predictions
    history <- rbind(history, dat)
    predictions <- rbind(predictions, predicted)
    actuals <- rbind(actuals, actual)
    performance_collector <- c(performance_collector, performance)
   
  }
  
  #merge different sets together
  Goods <- total_sales[product_id == i,,]$Item
  helper <- rbind(predictions, actuals)
  helper <- cbind(helper, Goods)
  table <- rbind(table, helper)
  RMSE_help <- merge(Goods, performance_collector)
  RMSE_indiv <- rbind(RMSE_indiv,RMSE_help)
  
  #View(table)
  # print(performance_collector)
  # print(total_sales$Item[i])
  # print(RMSE_indiv)
  print(total_sales[product_id == i,,]$Item)
  
}
#format table headers
colnames(table) <- c("Mo", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun", "Goods")
# creation and formating of RSME table of individual products
options(scipen=999)
RMSE_indiv$y <- round(RMSE_indiv$y, digits = 2)
RMSE_indiv <- aggregate(.~x, data=RMSE_indiv, mean)
RMSE_selected <- RMSE_indiv[c(7,1,10,25,8),]
colnames(RMSE_selected) <- c("Item", "RSME")
# View(RMSE_selected)
#5. Prediction for Total Demand------------------------------------------------------------------------------
#prepare df
intermediate_predict_total <- total_df
intermediate_predict_total$Item=NULL

#Total Sales
intermediate_predict_total <- intermediate_predict_total[,.(quantity=sum(quantity)),by="Date"][order(Date)]

#integrate complete time series
predict_total <- dateseq

#merge datasets
predict_total <- left_join(predict_total, intermediate_predict_total, by = "Date", all.x=TRUE)
# View(predict_total)
#5.1 Creation of new features------------------------------------------------------------------------------
#is closed feature
predict_total$is_closed <- ifelse(is.na(predict_total$quantity),1,0)

# Creation of weekday and number of week feature
predict_total$day_of_week <- weekdays(predict_total$Date)
predict_total$week_of_year <- week(predict_total$Date)

#create even weeks, starting with monday, ending with sunday
predict_total <- predict_total[-c(1:6, 161,162),]

#fill all NAs with 0
predict_total[is.na(predict_total)] <- 0

#creation of seasonal feature
seasonality_total_help =c()
seasonality_total =c()
for (i in items) {
  simple_dataset_total <- product %>%
    select(Date, i, is_closed, day_of_week, week_of_year)

  seasonality_total_help <- decompose(ts(simple_dataset_total[,2], frequency = 7))
  seasonality_total <- rbind(seasonality_total, seasonality_total_help$seasonal)
}
seasonality_total <- t(seasonality_total)
colnames(seasonality_total) <- items

#mean seasonality over given time frame
helper_3 <- c()
helper_4 <- c()
for (u in 1:154) {
  helper_3 <- mean(seasonality_total[u,])
  helper_4 <- c(helper_4,helper_3)
}
#integrate feature into dataframe
predict_total$seasonality <- helper_4
# # View(predict_total)


# Create Test and Training Data of given product
index = tail(1:nrow(predict_total),28)
train_product_total = predict_total[-index,]
test_product_total = predict_total[index,]
# View(test_product_total)

#5.2 Prediction------------------------------------------------------------------------------
# Random Forest

# basic data prep
train_product_total$Date=NULL
test_product_total$Date=NULL

train_product_total$day_of_week=as.numeric(as.factor(train_product_total$day_of_week))
test_product_total$day_of_week=as.numeric(as.factor(test_product_total$day_of_week))

# create lags
train_product_total$quantity_lag7 = shift(train_product_total[,1], n=7, fill=NA, type="lag")
train_product_total$quantity_lag8 = shift(train_product_total[,1], n=8, fill=NA, type="lag")
train_product_total$quantity_lag9 = shift(train_product_total[,1], n=9, fill=NA, type="lag")
train_product_total$quantity_lag10 = shift(train_product_total[,1], n=10, fill=NA, type="lag")
train_product_total$quantity_lag11 = shift(train_product_total[,1], n=11, fill=NA, type="lag")
train_product_total$quantity_lag12 = shift(train_product_total[,1], n=12, fill=NA, type="lag")
train_product_total$quantity_lag13 = shift(train_product_total[,1], n=13, fill=NA, type="lag")
train_product_total$quantity_lag14 = shift(train_product_total[,1], n=14, fill=NA, type="lag")
#View(train_product_total)

#only use complete cases
train_product_total <- train_product_total[complete.cases(train_product_total),]

# create Random Forest
tsforest_total = randomForest(quantity ~ ., train_product_total, ntree=10000)

#set up prediction loop and helpers
history_total = train_product_total
predictions_total =c()
actuals_total = c()
performance_collector_total = c()

#for loop to predict weeks 11, 12, 13, 14
for (w in unique(test_product_total$week_of_year)){
  
  dat_total = tail(history_total, 14)
  dat_total = rbind(dat_total, test_product_total[week_of_year == w,,], fill=TRUE)
  
  # create lags
  dat_total$quantity_lag7 = shift(dat_total[,1], n=7, fill=NA, type="lag")
  dat_total$quantity_lag8 = shift(dat_total[,1], n=8, fill=NA, type="lag")
  dat_total$quantity_lag9 = shift(dat_total[,1], n=9, fill=NA, type="lag")
  dat_total$quantity_lag10 = shift(dat_total[,1], n=10, fill=NA, type="lag")
  dat_total$quantity_lag11 = shift(dat_total[,1], n=11, fill=NA, type="lag")
  dat_total$quantity_lag12 = shift(dat_total[,1], n=12, fill=NA, type="lag")
  dat_total$quantity_lag13 = shift(dat_total[,1], n=13, fill=NA, type="lag")
  dat_total$quantity_lag14 = shift(dat_total[,1], n=14, fill=NA, type="lag")
  
  dat_total=tail(dat_total,7)
  
  #make forecast
  predicted_total = predict(tsforest_total, dat_total)
  actual_total = test_product_total[week_of_year == w,,]$quantity
  performance_total = RMSE(actual_total, predicted_total)
  
  # update history and collect predictions
  history_total = rbind(history_total, dat_total)
  predictions_total = rbind(predictions_total, predicted_total)
  actuals_total = rbind(actuals_total, actual_total)
  performance_collector_total = c(performance_collector_total, performance_total)
  
  # print(predictions_total)
  # print(actuals_total)
  # print(performance_collector_total)
}

#Create Table with predictions and actual values
table_total <- rbind(predictions_total, actuals_total)
#format table headers
colnames(table_total) <- c("Mo", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun")
rownames(table_total) <- c("predicted_Kw11", "predicted_KW12", "predicted_KW13", "predicted_KW14","actual_Kw11", "actual_KW12", "actual_KW13", "actual_KW14" )
# View(table_total)
wd <- getwd()
dir <- "/Shiny/prediction_output"
folderdir <- paste(wd, dir, sep = "")
dir.create("prediction_output", path= folderdir)
#save table as csv
write.csv(table_total, paste0(folderdir, "/prediction_total.csv", sep= ""), row.names = TRUE)

RSME_total <-mean(performance_collector_total)
varImp_total <- varImpPlot(tsforest_total, main = "Total Products")
#6. Analysis------------------------------------------------------------------------------
#What are the relevant products / items?
# Filter for the relevant Items regarding weekly mean of demand
mean_collector = c()
# create mean for every product/column
for (c in items) {
  means <- mean(product[[c]])
  mean_collector <- cbind(mean_collector, means)
  # colnames(mean_collector)[c] <- items[c]
}

# match corresponding product_id to mean
ids <- product[1,]
ids <- ids[,-c(1,97:99)]
colnames(mean_collector) <- colnames(ids)
#transpose matrix
mean_collector <- t(mean_collector)
mean_collector <- sort(mean_collector, decreasing = TRUE)

#exclude all items with mean lower than 1, resulting in 22 "relevant" items
mean_collector <- mean_collector[-c(23:95)]

#match corresponding names to productid
names <- c()
for (d in 1:length(mean_collector)) {
  df_3 <- total_sales[product_id == d,,]$Item
  names <- cbind(names, df_3)
}

names <- t(names)
mean_collector <- cbind(mean_collector, names)

#exclude Item NONE
mean_collector <- mean_collector[-6,]
fix_items <- mean_collector[,2]

#alter table to only show the relevant items
final_table <- table[c(1:24, 33:80,113:120, 129:136, 169:176, 193:200, 217:224, 241:256, 265:272, 361:368, 481:496, 593:600),]
# View(final_table)
# unique(final_table$Goods)
#7. Create dataframes for shiny------------------------------------------------------------------------------
#7.1 Relevant Items------------------------------------------------------------------------------
# releveant items (fix_items =21)
fix_items <- unique(final_table[,8])
#save relevant Items as csv
write.csv(fix_items, paste0(folderdir, "/fix_items.csv", sep= ""), row.names = TRUE)

#7.2 Time Series for prediction weeks------------------------------------------------------------------------------
#create time series for predicted weeks
time_series_test_11 <- predict_total[week_of_year == 11,,]$Date
time_series_test_12 <- predict_total[week_of_year == 12,,]$Date
time_series_test_13 <- predict_total[week_of_year == 13,,]$Date
time_series_test_14 <- predict_total[week_of_year == 14,,]$Date
time_series_test <- c(time_series_test_11, time_series_test_12, time_series_test_13,time_series_test_14)
time_series_test <- as.data.frame(time_series_test)
colnames(time_series_test) <- "Date"
# str(time_series_test)

#save time series as csv
write.csv(time_series_test, paste0(folderdir, "/time_series_test.csv", sep= ""), row.names = TRUE)

#7.3 Final table with prediction and actual values------------------------------------------------------------------------------
#give final_table meaningful rownames
row_names_df <- c("bread_predicted_KW11","bread_predicted_KW12","bread_predicted_KW13","bread_predicted_KW14",
                  "bread_actual_KW11","bread_actual_KW12","bread_actual_KW13","bread_actual_KW14",
                  "Scandinavian_predicted_KW11","Scandinavian_predicted_KW12","Scandinavian_predicted_KW13","Scandinavian_predicted_KW14",
                  "Scandinavian_actual_KW11","Scandinavian_actual_KW12","Scandinavian_actual_KW13","Scandinavian_actual_KW14",
                  "Hot chocolate_predicted_KW11","Hot chocolate_predicted_KW12","Hot chocolate_predicted_KW13","Hot chocolate_predicted_KW14",
                  "Hot chocolate_actual_KW11","Hot chocolate_actual_KW12","Hot chocolate_actual_KW13","Hot chocolate_actual_KW14",
                  "Cookies_predicted_KW11","Cookies_predicted_KW12","Cookies_predicted_KW13","Cookies_predicted_KW14",
                  "Cookies_actual_KW11","Cookies_actual_KW12","Cookies_actual_KW13","Cookies_actual_KW14",
                  "Muffin_predicted_KW11","Muffin_predicted_KW12","Muffin_predicted_KW13","Muffin_predicted_KW14",
                  "Muffin_actual_KW11","Muffin_actual_KW12","Muffin_actual_KW13","Muffin_actual_KW14",
                  "Coffee_predicted_KW11","Coffee_predicted_KW12","Coffee_predicted_KW13","Coffee_predicted_KW14",
                  "Coffee_actual_KW11","Coffee_actual_KW12","Coffee_actual_KW13","Coffee_actual_KW14",
                  "Pastry_predicted_KW11","Pastry_predicted_KW12","Pastry_predicted_KW13","Pastry_predicted_KW14",
                  "Pastry_actual_KW11","Pastry_actual_KW12","Pastry_actual_KW13","Pastry_actual_KW14",
                  "Medialuna_predicted_KW11","Medialuna_predicted_KW12","Medialuna_predicted_KW13","Medialuna_predicted_KW14",
                  "Medialuna_actual_KW11","Medialuna_actual_KW12","Medialuna_actual_KW13","Medialuna_actual_KW14",
                  "Tea_predicted_KW11","Tea_predicted_KW12","Tea_predicted_KW13","Tea_predicted_KW14",
                  "Tea_actual_KW11","Tea_actual_KW12","Tea_actual_KW13","Tea_actual_KW14",
                  "Farm House_predicted_KW11","Farm House_predicted_KW12","Farm House_predicted_KW13","Farm House_predicted_KW14",
                  "Farm House_actual_KW11","Farm House_actual_KW12","Farm House_actual_KW13","Farm House_actual_KW14",
                  "Juice_predicted_KW11","Juice_predicted_KW12","Juice_predicted_KW13","Juice_predicted_KW14",
                  "Juice_actual_KW11","Juice_actual_KW12","Juice_actual_KW13","Juice_actual_KW14",
                  "Soup_predicted_KW11","Soup_predicted_KW12","Soup_predicted_KW13","Soup_predicted_KW14",
                  "Soup_actual_KW11","Soup_actual_KW12","Soup_actual_KW13","Soup_actual_KW14",
                  "Cake_predicted_KW11","Cake_predicted_KW12","Cake_predicted_KW13","Cake_predicted_KW14",
                  "Cake_actual_KW11","Cake_actual_KW12","Cake_actual_KW13","Cake_actual_KW14",
                  "Coke_predicted_KW11","Coke_predicted_KW12","Coke_predicted_KW13","Coke_predicted_KW14",
                  "Coke_actual_KW11","Coke_actual_KW12","Coke_actual_KW13","Coke_actual_KW14",
                  "Sandwich_predicted_KW11","Sandwich_predicted_KW12","Sandwich_predicted_KW13","Sandwich_predicted_KW14",
                  "Sandwich_actual_KW11","Sandwich_actual_KW12","Sandwich_actual_KW13","Sandwich_actual_KW14",
                  "Alfajores_predicted_KW11","Alfajores_predicted_KW12","Alfajores_predicted_KW13","Alfajores_predicted_KW14",
                  "Alfajores_actual_KW11","Alfajores_actual_KW12","Alfajores_actual_KW13","Alfajores_actual_KW14",
                  "Brownie_predicted_KW11","Brownie_predicted_KW12","Brownie_predicted_KW13","Brownie_predicted_KW14",
                  "Brownie_actual_KW11","Brownie_actual_KW12","Brownie_actual_KW13","Brownie_actual_KW14",
                  "Truffles_predicted_KW11","Truffles_predicted_KW12","Truffles_predicted_KW13","Truffles_predicted_KW14",
                  "Truffles_actual_KW11","Truffles_actual_KW12","Truffles_actual_KW13","Truffles_actual_KW14",
                  "Toast_predicted_KW11","Toast_predicted_KW12","Toast_predicted_KW13","Toast_predicted_KW14",
                  "Toast_actual_KW11","Toast_actual_KW12","Toast_actual_KW13","Toast_actual_KW14",
                  "Scone_predicted_KW11","Scone_predicted_KW12","Scone_predicted_KW13","Scone_predicted_KW14",
                  "Scone_actual_KW11","Scone_actual_KW12","Scone_actual_KW13","Scone_actual_KW14",
                  "Spanish Brunch_predicted_KW11","Spanish Brunch_predicted_KW12","Spanish Brunch_predicted_KW13","Spanish Brunch_predicted_KW14",
                  "Spanish Brunch_actual_KW11","Spanish Brunch_actual_KW12","Spanish Brunch_actual_KW13","Spanish Brunch_actual_KW14")

rownames(final_table) <- row_names_df
final_table <- as.data.frame(final_table)
# View(final_table)
#save table as csv
write.csv(final_table, paste0(folderdir, "/completefinalprediction.csv", sep= ""), row.names = TRUE)

#7.4 Table with only prediction values------------------------------------------------------------------------------
goods <- final_table$Goods 
# View(final_table)
#convert factors back to numeric
indx <- sapply(final_table, is.factor)
final_table[indx] <- lapply(final_table[indx], function(x) as.numeric(as.character(x)))
final_table$Goods=NULL
final_table$Goods <- goods

#create separate df with predicted and actual values
final_table$Values <- c("predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual",
                        "predicted","predicted","predicted","predicted","actual","actual","actual","actual")

#subset for predicted values
predicted_values <- subset(final_table, final_table$Values=="predicted")
#construction of dataframe
predicted_values$Goods = NULL
predicted_values$Values = NULL
predicted_values <- t(predicted_values)
predicted_values_df <- as.data.frame(c(predicted_values[,1],predicted_values[,2],predicted_values[,3],predicted_values[,4]))
colnames(predicted_values_df) <- "Bread"
predicted_values_df$Scandinavian <- c(predicted_values[,5],predicted_values[,6],predicted_values[,7],predicted_values[,8])
predicted_values_df$Hot_chocolate <- c(predicted_values[,9],predicted_values[,10],predicted_values[,11],predicted_values[,12])
predicted_values_df$Cookies <- c(predicted_values[,13],predicted_values[,14],predicted_values[,15],predicted_values[,16])
predicted_values_df$Muffin <- c(predicted_values[,17],predicted_values[,18],predicted_values[,19],predicted_values[,20])
predicted_values_df$Coffee <- c(predicted_values[,21],predicted_values[,22],predicted_values[,23],predicted_values[,24])
predicted_values_df$Pastry <- c(predicted_values[,25],predicted_values[,26],predicted_values[,27],predicted_values[,28])
predicted_values_df$Medialuna <- c(predicted_values[,29],predicted_values[,30],predicted_values[,31],predicted_values[,32])
predicted_values_df$Tea <- c(predicted_values[,33],predicted_values[,34],predicted_values[,35],predicted_values[,36])
predicted_values_df$Farm_House <- c(predicted_values[,37],predicted_values[,38],predicted_values[,39],predicted_values[,40])
predicted_values_df$Juice <- c(predicted_values[,41],predicted_values[,42],predicted_values[,43],predicted_values[,44])
predicted_values_df$Soup <- c(predicted_values[,45],predicted_values[,46],predicted_values[,47],predicted_values[,48])
predicted_values_df$Cake <- c(predicted_values[,49],predicted_values[,50],predicted_values[,51],predicted_values[,52])
predicted_values_df$Coke <- c(predicted_values[,53],predicted_values[,54],predicted_values[,55],predicted_values[,56])
predicted_values_df$Sandwich <- c(predicted_values[,57],predicted_values[,58],predicted_values[,59],predicted_values[,60])
predicted_values_df$Alfajores <- c(predicted_values[,61],predicted_values[,62],predicted_values[,63],predicted_values[,64])
predicted_values_df$Brownie <- c(predicted_values[,65],predicted_values[,66],predicted_values[,67],predicted_values[,68])
predicted_values_df$Truffles <- c(predicted_values[,69],predicted_values[,70],predicted_values[,71],predicted_values[,72])
predicted_values_df$Toast <- c(predicted_values[,73],predicted_values[,74],predicted_values[,75],predicted_values[,76])
predicted_values_df$Scone <- c(predicted_values[,77],predicted_values[,78],predicted_values[,79],predicted_values[,80])
predicted_values_df$Spanish_Brunch <- c(predicted_values[,81],predicted_values[,82],predicted_values[,83],predicted_values[,84])
predicted_values_df <-t(predicted_values_df)
predicted_values_df <- as.data.frame(predicted_values_df)
#convert character back to numeric
indx <- sapply(predicted_values_df, is.character)
predicted_values_df[indx] <- lapply(predicted_values_df[indx], function(x) as.numeric(as.character(x)))
predicted_values_df <- round(predicted_values_df, digits=0)
colnames(predicted_values_df) <- as.character(time_series_test$Date)
View(predicted_values_df)

#save table as csv
write.csv(predicted_values_df, paste0(folderdir, "/predicted_values_df.csv", sep= ""), row.names = TRUE)

#7.5 Table with only actual values------------------------------------------------------------------------------
#subset for predicted values
actual_values <- subset(final_table, final_table$Values=="actual")
#construction of dataframe
actual_values$Goods = NULL
actual_values$Values = NULL
actual_values <- t(actual_values)
actual_values_df <- as.data.frame(c(actual_values[,1],actual_values[,2],actual_values[,3],actual_values[,4]))
colnames(actual_values_df) <- "Bread"
actual_values_df$Scandinavian <- c(actual_values[,5],actual_values[,6],actual_values[,7],actual_values[,8])
actual_values_df$Hot_chocolate <- c(actual_values[,9],actual_values[,10],actual_values[,11],actual_values[,12])
actual_values_df$Cookies <- c(actual_values[,13],actual_values[,14],actual_values[,15],actual_values[,16])
actual_values_df$Muffin <- c(actual_values[,17],actual_values[,18],actual_values[,19],actual_values[,20])
actual_values_df$Coffee <- c(actual_values[,21],actual_values[,22],actual_values[,23],actual_values[,24])
actual_values_df$Pastry <- c(actual_values[,25],actual_values[,26],actual_values[,27],actual_values[,28])
actual_values_df$Medialuna <- c(actual_values[,29],actual_values[,30],actual_values[,31],actual_values[,32])
actual_values_df$Tea <- c(actual_values[,33],actual_values[,34],actual_values[,35],actual_values[,36])
actual_values_df$Farm_House <- c(actual_values[,37],actual_values[,38],actual_values[,39],actual_values[,40])
actual_values_df$Juice <- c(actual_values[,41],actual_values[,42],actual_values[,43],actual_values[,44])
actual_values_df$Soup <- c(actual_values[,45],actual_values[,46],actual_values[,47],actual_values[,48])
actual_values_df$Cake <- c(actual_values[,49],actual_values[,50],actual_values[,51],actual_values[,52])
actual_values_df$Coke <- c(actual_values[,53],actual_values[,54],actual_values[,55],actual_values[,56])
actual_values_df$Sandwich <- c(actual_values[,57],actual_values[,58],actual_values[,59],actual_values[,60])
actual_values_df$Alfajores <- c(actual_values[,61],actual_values[,62],actual_values[,63],actual_values[,64])
actual_values_df$Brownie <- c(actual_values[,65],actual_values[,66],actual_values[,67],actual_values[,68])
actual_values_df$Truffles <- c(actual_values[,69],actual_values[,70],actual_values[,71],actual_values[,72])
actual_values_df$Toast <- c(actual_values[,73],actual_values[,74],actual_values[,75],actual_values[,76])
actual_values_df$Scone <- c(actual_values[,77],actual_values[,78],actual_values[,79],actual_values[,80])
actual_values_df$Spanish_Brunch <- c(actual_values[,81],actual_values[,82],actual_values[,83],actual_values[,84])
actual_values_df <-t(actual_values_df)
actual_values_df <- as.data.frame(actual_values_df)
#convert character back to numeric
indx <- sapply(actual_values_df, is.character)
actual_values_df[indx] <- lapply(actual_values_df[indx], function(x) as.numeric(as.character(x)))
actual_values_df <- round(actual_values_df, digits=0)
colnames(actual_values_df) <- as.character(time_series_test$Date)
# View(actual_values_df)

#save actual tables as csv
write.csv(actual_values_df, paste0(folderdir, "/actual_values_df.csv", sep= ""), row.names = TRUE)

#7.6 Table showing weekly means for individual products------------------------------------------------------------------------------
#Creation of table showing mean weekday for the relevant Items
weekday_df <- c("Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag", "Montag")
for (e in 1:21) {
df_4 = total_df[Item == e, .(quantity = sum(quantity)), by = "Date"]
df_5 <- df_4[,sum(quantity),by=weekdays(Date)]
weekday_df <- cbind(weekday_df, df_5[,2])
}
header <- c("Weekday", total_sales$Item[c(1:5,7:22)])
colnames(weekday_df) <- header
t(weekday_df)
#display weeks in correct order
weekday_df <- weekday_df[c(7,1,2,3,4,5,6),]
library(tidyverse)
#set weekdays as factors in order to plot them in the correct order
weekday_df$Weekday <- factor(weekday_df$Weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
# View(weekday_df)
#save table as csv
write.csv(weekday_df, paste0(folderdir, "/weeklymeans.csv", sep= ""), row.names = TRUE)


#7.7 Table showing weekly means for total demand------------------------------------------------------------------------------
# Creation of table showing mean of total demand per weekday 
weekday_df_total <- c("Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag", "Montag")
#View(intermediate_predict_total)
helper_2 <- intermediate_predict_total[,sum(quantity),by=weekdays(Date)]
weekday_df_total <- cbind(weekday_df_total, helper_2[,2])
colnames(weekday_df_total) <- c("Weekday", "Total_Mean")
#display weekdys in correct order
weekday_df_total <- weekday_df_total[c(7,1,2,3,4,5,6),]
#set weekdays as factors in order to plot them in the correct order
weekday_df_total$Weekday <- factor(weekday_df_total$Weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
#save table as csv
write.csv(weekday_df_total, paste0(folderdir, "/weeklymeans_total.csv", sep= ""), row.names = TRUE)

#7.8 Table showing monthly means for individual products------------------------------------------------------------------------------

#create monthly distribution
monthly_means<- as.data.frame(as.numeric(c("10", "11", "12", "1", "2", "3", "4")))
colnames(monthly_means)[1] <- "month"

for (f in 1:21) {
  df_6 <-total_df[Item == f, .(quantity = sum(quantity)), by = "Date"]
  df_8 <- df_6[,sum(quantity),by=month(Date)]
  monthly_means <- full_join(monthly_means, df_8, all.x = TRUE, by ="month")
  monthly_means[is.na(monthly_means)] <- 0

}
header_month <- c("Month", total_sales$Item[c(1:5,7:22)])
colnames(monthly_means) <- header_month
#display month
monthly_means[,1] <- c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April")
# str(monthly_means)
library(tidyverse)
#set weekdays as factors in order to plot them in the correct order
monthly_means[,1] <- factor(monthly_means$Month, levels = c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April"))
#save table as csv
write.csv(monthly_means, paste0(folderdir, "/monthlymeans.csv", sep= ""), row.names = TRUE)

#7.9 Table showing monthly means for total demand------------------------------------------------------------------------------
# Creation of table showing mean of total demand per weekday 
month_df_total <- c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April")
#View(intermediate_predict_total)
helper_2_monthly <- intermediate_predict_total[,sum(quantity),by=month(Date)]
month_df_total <- cbind(month_df_total, helper_2_monthly[,2])
colnames(month_df_total) <- c("Month", "Total_Mean")
#set weekdays as factors in order to plot them in the correct order
month_df_total$Month <- factor(month_df_total$Month, levels = c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April"))
#save table as csv
write.csv(month_df_total, paste0(folderdir, "/monthlymeans_total.csv", sep= ""), row.names = TRUE)

#7.10 Table showing predicted weekly means for indidivual products------------------------------------------------------------------------------
#Setup Dataframe
weekly_predicted <- predicted_values_df
weekdays <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag", "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag","Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag","Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
colnames(weekly_predicted) <- weekdays
weekly_predicted <- as.data.frame(t(weekly_predicted))

#create dataframe summed by weekday
collector <- c()
weekday_pred_indiv <- c()
for (g in 1:7) {
  collector <- weekly_predicted[g,] +weekly_predicted[g+7,]+weekly_predicted[g+14,]+weekly_predicted[g+21,]
  weekday_pred_indiv <- rbind(weekday_pred_indiv, collector)
}

#calculate correct amount
length(unique(product$Date)) #154 days in total, 28 days predicted -> multiple 5.5
weekday_pred_indiv<-round(weekday_pred_indiv[,c(1:21)]* (length(unique(product$Date))/length(weekly_predicted$Bread)),digits = 0)
weekday_pred_indiv$Weekday <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
weekday_pred_indiv$Weekday <- factor(weekday_pred_indiv$Weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
rownames(weekday_pred_indiv) <- c("1", "2", "3", "4", "5", "6", "7")

#Creating long format dataframe with actual and predicted values for plotting
##weekday_predicted_long
###predicted values
weekday_predicted_long<-as.data.frame(weekday_pred_indiv)
value <- c("predicted", "predicted", "predicted","predicted", "predicted", "predicted","predicted")
weekday_predicted_long$value <- value
colnames(weekday_predicted_long)[3] <- "Hot chocolate"
colnames(weekday_predicted_long)[10] <- "Farm House"
colnames(weekday_predicted_long)[21] <- "Spanish Brunch"

##weekday_actual_long
###actual values
weekday_actual_long<-as.data.frame(weekday_df)
value <- c("actual", "actual", "actual","actual", "actual", "actual","actual")
weekday_actual_long$value <- value

##weekly_values_long
str(weekday_actual_long)
str(weekday_predicted_long)
weekday_long <- full_join(weekday_predicted_long, weekday_actual_long)
weekday_long <- weekday_long[c(1,8,2,9,3,10,4,11,5,12,6,13,7,14),]

weekday_long$value[weekday_long$value=="predicted"] <- "1"
weekday_long$value[weekday_long$value=="actual"] <- "2"
weekday_long$value <- as.factor(weekday_long$value)

#7.11 Table showing predicted weekly means for total products------------------------------------------------------------------------------
#predicted values
weekday_pred_intermediate <- weekday_pred_indiv
weekday_pred_intermediate$Weekday <- NULL

collector <- c()
weekday_pred_total <- c()
for (h in 1:7) {
  collector <- sum(weekday_pred_intermediate[h,])
  weekday_pred_total <- rbind(weekday_pred_total, collector)
}
weekday_pred_total_long <- as.data.frame(weekday_pred_total)
weekday_pred_total_long$Weekday <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag")
weekday_pred_total_long$Weekday <- factor(weekday_pred_total_long$Weekday, levels = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"))
rownames(weekday_pred_total_long) <- c("1", "2", "3", "4", "5", "6", "7")
colnames(weekday_pred_total_long)[1] <- "Total"
weekday_pred_total_long<-as.data.frame(weekday_pred_total_long)

value <- c("predicted", "predicted", "predicted","predicted", "predicted", "predicted","predicted")
weekday_pred_total_long$value <- value

#actual values
weekday_act_total_long <- as.data.frame(weekday_df_total)
colnames(weekday_act_total_long)[2] <- "Total"
value <- c("actual", "actual", "actual","actual", "actual", "actual","actual")
weekday_act_total_long$value <- value

##total_values_long
str(weekday_pred_total_long)
str(weekday_act_total_long)
total_values_long <- full_join(weekday_pred_total_long, weekday_act_total_long)
total_values_long <- total_values_long[c(1,8,2,9,3,10,4,11,5,12,6,13,7,14),]

total_values_long$value[total_values_long$value=="predicted"] <- "1"
total_values_long$value[total_values_long$value=="actual"] <- "2"
total_values_long$value <- as.factor(total_values_long$value)
#View(total_values_long)
# str(total_values_long)

#7.12 Table showing predicted monthly means for individual products-------------------
#predicted values
monthly_predicted <- predicted_values_df
#create dataframe summed by march
collector <- c()
march <- c()
for (j in 1:21) {
  collector <- sum(monthly_predicted[j,c(1:21)])
  march <- rbind(march, collector)
}

#calculate for full march instead of 3/4 weeks
length(unique(colnames(predicted_values_df))) #28 weeks in total, 21 in march 7 in april
march<-round(march*(4/3),digits = 0)

#create dataframe summed by april
collector <- c()
april <- c()
for (k in 1:21) {
  collector <- sum(monthly_predicted[k,c(22:28)])
  april <- rbind(april, collector)
}

#join march and april
march_april <- as.data.frame(cbind(march,april))
rownames(march_april) <- rownames(monthly_predicted)
colnames(march_april) <- c("März", "April")

#add columns for not predicted months
march_april$Oktober = 0
march_april$November = 0
march_april$Dezember = 0
march_april$Januar = 0
march_april$Februar = 0
march_april <- as.data.frame(t(march_april))
#rename rows
march_april$Month <- rownames(march_april)
rownames(march_april) <- seq(1:7)
march_april <- march_april[c(3,4,5,6,7,1,2),]
march_april$value <- "predicted"
month_pred_indiv <- march_april
#rename colnames to match other dataframe
colnames(month_pred_indiv)[3] <- "Hot chocolate"
colnames(month_pred_indiv)[10] <- "Farm House"
colnames(month_pred_indiv)[21] <- "Spanish Brunch"

#actual values
month_actual_indiv <- as.data.frame(monthly_means)
month_actual_indiv$value <- "actual"

#merge dateframes
month_values_long <- full_join(month_pred_indiv, month_actual_indiv)
month_values_long <- month_values_long[c(1,8,2,9,3,10,4,11,5,12,6,13,7,14),]
#rename and create factors
month_values_long$value[month_values_long$value=="predicted"] <- "1"
month_values_long$value[month_values_long$value=="actual"] <- "2"
month_values_long$value <- as.factor(month_values_long$value)
month_values_long$Month <- factor(month_values_long$Month, levels = c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April"))


#7.13 Table showing predicted monthly means for total products---------------------
#create datafrae
total_month_intermediate <- as.data.frame(month_pred_indiv[c(1:21)])
#create dataframe summed by total
collector <- c()
total_item <- c()
for (l in 1:7) {
  collector <- sum(total_month_intermediate[l,c(1:21)])
  total_item <- rbind(total_item, collector)
}
#merge dataframes and create total dataframe
total_month_values_pred=NULL
total_month_values_pred$value <- month_pred_indiv$value
total_month_values_pred$Month <-month_pred_indiv$Month
total_month_values_pred <- as.data.frame(total_month_values_pred)
total_month_values_pred <- cbind(total_item, total_month_values_pred)
# View(total_month_values_pred) 

#rename rows and colums
rownames(total_month_values_pred) <- seq(1:7)
colnames(total_month_values_pred)[1] <- "Total"
total_month_values_pred$Month <- factor(total_month_values_pred$Month, levels = c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April"))
# View(total_month_values_pred) 

#actual data
total_month_values_actual <- as.data.frame(month_df_total)
total_month_values_actual$value <- "actual"
colnames(total_month_values_actual)[2] <- "Total"
# View(total_month_values_actual)

#merge pred and actual dateframes
total_month_values_long <- full_join(total_month_values_pred, total_month_values_actual)
total_month_values_long <- total_month_values_long[c(1,8,2,9,3,10,4,11,5,12,6,13,7,14),]
total_month_values_long$value[total_month_values_long$value=="predicted"] <- "1"
total_month_values_long$value[total_month_values_long$value=="actual"] <- "2"
total_month_values_long$value <- as.factor(total_month_values_long$value)
total_month_values_long$Month <- factor(total_month_values_long$Month, levels = c("Oktober", "November", "Dezember", "Januar", "Februar", "März", "April"))

#8. Visualization------------------------------------------------------------------------------
#save path where the visualisations should be saved

wd <- getwd()
direc <- "/Shiny/www/"
results <- paste(wd, direc, sep = "")
# results <- "~/Desktop/Masterarbeit/Shiny_App/Shiny/www/"

#8.1 Plots regarding total sales_weekly------------------------------------------------------------------------------

#create new df
plot_total_sales <- total_sales
#set product_id = 0 and exclude "NONE
plot_total_sales$product_id = NULL
plot_total_sales <- plot_total_sales[-6,]
sum(plot_total_sales$quantity)
#in total there are 21293 Items sold over given timeframe, relevant Items amount is 20507 excluding "NONE"
#Plotting items which account for 70% of Demand
options(scipen=999)
plot_total_sales$Percent <- (round((plot_total_sales$quantity)/sum(plot_total_sales$quantity), digits = 2))*100
plot_total_sales <- plot_total_sales[-c(11:94),]
#View(plot_total_sales)

#8.1.1 Plots of Top10 Product_weekly------------------------------------------------------------------------------
# by Quantity
Top10_Products <- ggplot(plot_total_sales) +
  geom_bar( aes(x= reorder(Item, -quantity), y=quantity), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  ggtitle("Top10 Produkte") +
  xlab("Prodult") + ylab("Anzahl") +
  geom_text(aes(x=Item, y=quantity, label=quantity), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(text = element_text(size=20))

Top10_Products

# save plots as .png
ggsave(Top10_Products, file=paste(results,"Top10_Products.png", sep=''), height=7, width=14, units="in", dpi=150)

# by Percent
plot <- ggplot(plot_total_sales) +
  geom_bar( aes(x= reorder(Item, -Percent), y=Percent), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Top10 Products by Percent") +
  xlab("Produkt") + ylab("Prozent") + 
  geom_text(aes(x=Item, y=Percent, label=Percent), position=position_dodge(width=0.9), vjust=-0.25) +
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Top10_Products_Perc.png", sep=''), height=7, width=10, units="in", dpi=150)

#8.1.2 Plot of total demand_weekdays------------------------------------------------------------------------------
#View(total_values_long)
# with prediction
plot <- ggplot(total_values_long) +
  geom_bar( aes(x=Weekday, y=Total,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Total, label=Total),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Anzahl") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Total_Mean_weekly_w.png", sep=''), height=7, width=10, units="in", dpi=150)

#without prediction 
Total_Mean_week <- ggplot(weekday_df_total) +
  geom_bar( aes(x=Weekday, y=Total_Mean), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_hline(yintercept=mean(weekday_df_total$Total_Mean), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df_total$Total_Mean), digits = 1)
                ,label = round(mean(weekday_df_total$Total_Mean), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) ) +
  xlab("Wochentag") + ylab("Anzahl") +
  geom_text(aes(x=Weekday, y=Total_Mean, label=Total_Mean), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(text = element_text(size=20))
Total_Mean_week

# save plots as .png
ggsave(Total_Mean_week, file=paste(results,"Total_Mean_weekly.png", sep=''), height=7, width=10, units="in", dpi=150)

#8.1.3 Plot of total demand_monthly------------------------------------------------------------------------------
#with prediction

plot <- ggplot(total_month_values_long) +
  geom_bar( aes(x=Month, y=Total,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Total, label=Total),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Anzahl") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Total_Mean_monthly_w.png", sep=''), height=7, width=10, units="in", dpi=150)

#without prediction
Total_Mean_monthly <- ggplot(month_df_total) +
  geom_bar( aes(x=Month, y=Total_Mean), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_hline(yintercept=mean(month_df_total$Total_Mean), color="darkgreen")+
  geom_text(aes(0,round(mean(month_df_total$Total_Mean), digits = 1)
                ,label = round(mean(month_df_total$Total_Mean), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) ) +
  xlab("Monat") + ylab("Anzahl") +
  geom_text(aes(x=Month, y=Total_Mean, label=Total_Mean), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme(text = element_text(size=20))
Total_Mean_monthly

# save plots as .png
ggsave(Total_Mean_monthly, file=paste(results,"Total_Mean_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)

#8.2 Plots regarding individual items_weekly_with_prediction------------------------------------------------------------------------------

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Coffee,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Coffee, label=Coffee),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Coffee") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Coffee.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Bread,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Bread, label=Bread),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Bread") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Bread.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Tea,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Tea, label=Tea),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Tea") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Tea.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Cake,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Cake, label=Cake),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Cake") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Cake.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Pastry,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Pastry, label=Pastry),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Pastry") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Pastry.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Sandwich,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Sandwich, label=Sandwich),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Sandwich") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Sandwich.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Medialuna,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Medialuna, label=Medialuna),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Medialuna") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Medialuna.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=`Hot chocolate`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=`Hot chocolate`, label=`Hot chocolate`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Hot chocolate") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Hot chocolate.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Cookies,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Cookies, label=Cookies),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Cookies") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Cookies.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Brownie,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Brownie, label=Brownie),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Brownie") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Brownie.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=`Farm House`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=`Farm House`, label=`Farm House`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Farm House") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Farm House.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Muffin,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Muffin, label=Muffin),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Muffin") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Muffin.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Juice,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Juice, label=Juice),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Juice") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Juice.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Alfajores,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Alfajores, label=Alfajores),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Alfajores") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Alfajores.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Soup,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Soup, label=Soup),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Soup") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Soup.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Scone,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Scone, label=Scone),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Scone") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Scone.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Toast,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Toast, label=Toast),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Toast") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Toast.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Scandinavian,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Scandinavian, label=Scandinavian),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Scandinavian") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Scandinavian.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Truffles,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Truffles, label=Truffles),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Truffles") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Truffles.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=Coke,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=Coke, label=Coke),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Coke") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Coke.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_long) +
  geom_bar( aes(x=Weekday, y=`Spanish Brunch`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Wochentag") +
  geom_text(aes(x=Weekday, y=`Spanish Brunch`, label=`Spanish Brunch`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Spanish Brunch") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Spanish Brunch.png", sep=''), height=7, width=10, units="in", dpi=150)








#8.2.1 Plots regarding individual items_weekly_without_prediction------------------------------------------------------------------------------

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Coffee), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Coffee, label=Coffee),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Coffee") +
  geom_hline(yintercept=mean(weekday_df$Coffee), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Coffee), digits = 1)
                ,label = round(mean(weekday_df$Coffee), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Coffee_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Bread), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Bread, label=Bread),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Bread") +
  geom_hline(yintercept=mean(weekday_df$Bread), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Bread), digits = 1)
                ,label = round(mean(weekday_df$Bread), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Bread_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Tea), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Tea, label=Tea),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Tea") +
  geom_hline(yintercept=mean(weekday_df$Tea), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Tea), digits = 1)
                ,label = round(mean(weekday_df$Tea), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Tea_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Cake), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Cake, label=Cake),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Cake") +
  geom_hline(yintercept=mean(weekday_df$Cake), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Cake), digits = 1)
                ,label = round(mean(weekday_df$Cake), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Cake_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Pastry), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Pastry, label=Pastry),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Pastry") +
  geom_hline(yintercept=mean(weekday_df$Pastry), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Pastry), digits = 1)
                ,label = round(mean(weekday_df$Pastry), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Pastry_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Sandwich), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Sandwich, label=Sandwich),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Sandwich") +
  geom_hline(yintercept=mean(weekday_df$Sandwich), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Sandwich), digits = 1)
                ,label = round(mean(weekday_df$Sandwich), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Sandwich_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Medialuna), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Medialuna, label=Medialuna),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Medialuna") +
  geom_hline(yintercept=mean(weekday_df$Medialuna), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Medialuna), digits = 1)
                ,label = round(mean(weekday_df$Medialuna), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Medialuna_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=`Hot chocolate`), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=`Hot chocolate`, label=`Hot chocolate`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Hot chocolate") +
  geom_hline(yintercept=mean(weekday_df$`Hot chocolate`), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$`Hot chocolate`), digits = 1)
                ,label = round(mean(weekday_df$`Hot chocolate`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Hot chocolate_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Cookies), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Cookies, label=Cookies),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Cookies") +
  geom_hline(yintercept=mean(weekday_df$Cookies), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Cookies), digits = 1)
                ,label = round(mean(weekday_df$Cookies), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Cookies_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Brownie), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Brownie, label=Brownie),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Brownie") +
  geom_hline(yintercept=mean(weekday_df$Brownie), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Brownie), digits = 1)
                ,label = round(mean(weekday_df$Brownie), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Brownie_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=`Farm House`), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=`Farm House`, label=`Farm House`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Farm House") +
  geom_hline(yintercept=mean(weekday_df$`Farm House`), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$`Farm House`), digits = 1)
                ,label = round(mean(weekday_df$`Farm House`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Farm House_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)


plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Muffin), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Muffin, label=Muffin),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Muffin") +
  geom_hline(yintercept=mean(weekday_df$Muffin), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Muffin), digits = 1)
                ,label = round(mean(weekday_df$Muffin), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Muffin_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)


plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Juice), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Juice, label=Juice),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Juice") +
  geom_hline(yintercept=mean(weekday_df$Juice), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Juice), digits = 1)
                ,label = round(mean(weekday_df$Juice), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Juice_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Alfajores), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Alfajores, label=Alfajores),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Alfajores") +
  geom_hline(yintercept=mean(weekday_df$Alfajores), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Alfajores), digits = 1)
                ,label = round(mean(weekday_df$Alfajores), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Alfajores_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Soup), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Soup, label=Soup),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Soup") +
  geom_hline(yintercept=mean(weekday_df$Soup), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Soup), digits = 1)
                ,label = round(mean(weekday_df$Soup), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Soup_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Scone), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Scone, label=Scone),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Scone") +
  geom_hline(yintercept=mean(weekday_df$Scone), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Scone), digits = 1)
                ,label = round(mean(weekday_df$Scone), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Scone_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Toast), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Toast, label=Toast),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Toast") +
  geom_hline(yintercept=mean(weekday_df$Toast), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Toast), digits = 1)
                ,label = round(mean(weekday_df$Toast), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Toast_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Scandinavian), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Scandinavian, label=Scandinavian),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Scandinavian") +
  geom_hline(yintercept=mean(weekday_df$Scandinavian), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Scandinavian), digits = 1)
                ,label = round(mean(weekday_df$Scandinavian), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Scandinavian_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Truffles), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Truffles, label=Truffles),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Truffles") +
  geom_hline(yintercept=mean(weekday_df$Truffles), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Truffles), digits = 1)
                ,label = round(mean(weekday_df$Truffles), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Truffles_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=Coke), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=Coke, label=Coke),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Coke") +
  geom_hline(yintercept=mean(weekday_df$Coke), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$Coke), digits = 1)
                ,label = round(mean(weekday_df$Coke), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Coke_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)

plot <- ggplot(weekday_df) +
  geom_bar( aes(x=Weekday, y=`Spanish Brunch`), stat="identity", fill="skyblue", position = 'dodge', alpha=0.7) +
  # ggtitle("Mean Demand per Weekday") +
  geom_text(aes(x=Weekday, y=`Spanish Brunch`, label=`Spanish Brunch`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Wochentag") + ylab("Spanish Brunch") +
  geom_hline(yintercept=mean(weekday_df$`Spanish Brunch`), color="darkgreen")+
  geom_text(aes(0,round(mean(weekday_df$`Spanish Brunch`), digits = 1)
                ,label = round(mean(weekday_df$`Spanish Brunch`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
plot

# save plots as .png
ggsave(plot, file=paste(results,"Spanish Brunch_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)



#8.3 Plots regarding individual items_monthly_with_prediction-------------------------------------

plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Coffee,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Coffee, label=Coffee),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Coffee") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Coffee_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Bread,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Bread, label=Bread),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Bread") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Bread_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Scandinavian,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Scandinavian, label=Scandinavian),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Scandinavian") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Scandinavian_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=`Hot chocolate`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=`Hot chocolate`, label=`Hot chocolate`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Hot chocolate") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Hot chocolate_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Cookies,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Cookies, label=Cookies),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Cookies") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Cookies_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Muffin,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Muffin, label=Muffin),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Muffin") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Muffin_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
  geom_bar( aes(x=Month, y=Pastry,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Pastry, label=Pastry),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Pastry") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Pastry_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Medialuna,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Medialuna, label=Medialuna),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Medialuna") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Medialuna_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Tea,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Tea, label=Tea),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Tea") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Tea_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=`Farm House`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=`Farm House`, label=`Farm House`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Farm House") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Farm House_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Juice,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Juice, label=Juice),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Juice") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Juice_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Soup,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Soup, label=Soup),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Soup") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Soup_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Cake,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Cake, label=Cake),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Cake") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Cake_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Coke,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Coke, label=Coke),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Coke") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Coke_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Sandwich,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Sandwich, label=Sandwich),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Sandwich") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Sandwich_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Alfajores,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Alfajores, label=Alfajores),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Alfajores") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Alfajores_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Brownie,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Brownie, label=Brownie),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Brownie") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Brownie_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Truffles,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Truffles, label=Truffles),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Truffles") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Truffles_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=Toast,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=Toast, label=Toast),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Toast") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Toast_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=	Scone,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=	Scone, label=	Scone),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Scone") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Scone_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(month_values_long) +
geom_bar( aes(x=Month, y=`Spanish Brunch`,fill=value), stat="identity",  position = 'dodge', alpha=0.7) +
  ggtitle("Je Monat") +
  geom_text(aes(x=Month, y=`Spanish Brunch`, label=`Spanish Brunch`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Spanish Brunch") +
  scale_fill_discrete(name="Wert",
                      breaks=c(1, 2),
                      labels=c("Vorhersage", "Vergleichsperiode"))+
  theme(text = element_text(size=20))+
  theme(axis.text.x = element_text(size=15),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
plot
# save plots as .png
ggsave(plot, file=paste(results,"Spanish Brunch_monthly.png", sep=''), height=7, width=10, units="in", dpi=150)
##

#8.3.1 Plots regarding individual items_monthly_without_prediction------------------------------------------------------------------------------

plot <- ggplot(monthly_means, aes(x=Month, y=Coffee)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  xlab("Monat") + ylab("Coffee") +
  geom_text(aes(x=Month, y=Coffee, label=Coffee),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  geom_hline(yintercept=mean(monthly_means$Coffee), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Coffee), digits = 1)
                ,label = round(mean(monthly_means$Coffee), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Coffee_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Bread)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Bread, label=Bread),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Bread") +
  geom_hline(yintercept=mean(monthly_means$Bread), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Bread), digits = 1)
                ,label = round(mean(monthly_means$Bread), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Bread_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Tea)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  xlab("Monat") + ylab("Tea") +
  geom_text(aes(x=Month, y=Tea, label=Tea),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  geom_hline(yintercept=mean(monthly_means$Tea), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Tea), digits = 1)
                ,label = round(mean(monthly_means$Tea), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Tea_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Cake)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Cake, label=Cake),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Cake") +
  geom_hline(yintercept=mean(monthly_means$Cake), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Cake), digits = 1)
                ,label = round(mean(monthly_means$Cake), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Cake_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Pastry)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Pastry, label=Pastry),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Pastry") +
  geom_hline(yintercept=mean(monthly_means$Pastry), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Pastry), digits = 1)
                ,label = round(mean(monthly_means$Pastry), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Pastry_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Sandwich)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Sandwich, label=Sandwich),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Sandwich") +
  geom_hline(yintercept=mean(monthly_means$Sandwich), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Sandwich), digits = 1)
                ,label = round(mean(monthly_means$Sandwich), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Sandwich_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Medialuna)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Medialuna, label=Medialuna),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Medialuna") +
  geom_hline(yintercept=mean(monthly_means$Medialuna), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Medialuna), digits = 1)
                ,label = round(mean(monthly_means$Medialuna), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Medialuna_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=`Hot chocolate`)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=`Hot chocolate`, label=`Hot chocolate`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Hot chocolate") +
  geom_hline(yintercept=mean(monthly_means$`Hot chocolate`), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$`Hot chocolate`), digits = 1)
                ,label = round(mean(monthly_means$`Hot chocolate`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Hot chocolate_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Cookies)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Cookies, label=Cookies),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Cookies") +
  geom_hline(yintercept=mean(monthly_means$Cookies), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Cookies), digits = 1)
                ,label = round(mean(monthly_means$Cookies), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Cookies_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##


plot <- ggplot(monthly_means, aes(x=Month, y=Brownie)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Brownie, label=Brownie),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Brownie") +
  geom_hline(yintercept=mean(monthly_means$Brownie), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Brownie), digits = 1)
                ,label = round(mean(monthly_means$Brownie), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Brownie_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##


plot <- ggplot(monthly_means, aes(x=Month, y=`Farm House`)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=`Farm House`, label=`Farm House`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Farm House") +
  geom_hline(yintercept=mean(monthly_means$`Farm House`), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$`Farm House`), digits = 1)
                ,label = round(mean(monthly_means$`Farm House`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Farm House_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##


plot <- ggplot(monthly_means, aes(x=Month, y=Muffin)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Muffin, label=Muffin),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Muffin") +
  geom_hline(yintercept=mean(monthly_means$Muffin), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Muffin), digits = 1)
                ,label = round(mean(monthly_means$Muffin), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Muffin_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Juice)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Juice, label=Juice),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Juice") +
  geom_hline(yintercept=mean(monthly_means$Juice), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Juice), digits = 1)
                ,label = round(mean(monthly_means$Juice), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Juice_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Alfajores)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Alfajores, label=Alfajores),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Alfajores") +
  geom_hline(yintercept=mean(monthly_means$Alfajores), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Alfajores), digits = 1)
                ,label = round(mean(monthly_means$Alfajores), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Alfajores_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Soup)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Soup, label=Soup),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Soup") +
  geom_hline(yintercept=mean(monthly_means$Soup), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Soup), digits = 1)
                ,label = round(mean(monthly_means$Soup), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Soup_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##


plot <- ggplot(monthly_means, aes(x=Month, y=Scone)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Scone, label=Scone),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Scone") +
  geom_hline(yintercept=mean(monthly_means$Scone), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Scone), digits = 1)
                ,label = round(mean(monthly_means$Scone), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Scone_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Toast)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Toast, label=Toast),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Toast") +
  geom_hline(yintercept=mean(monthly_means$Toast), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Toast), digits = 1)
                ,label = round(mean(monthly_means$Toast), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Toast_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Scandinavian)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Scandinavian, label=Scandinavian),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Scandinavian") +
  geom_hline(yintercept=mean(monthly_means$Scandinavian), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Scandinavian), digits = 1)
                ,label = round(mean(monthly_means$Scandinavian), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Scandinavian_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

plot <- ggplot(monthly_means, aes(x=Month, y=Truffles)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Truffles, label=Truffles),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Truffles") +
  geom_hline(yintercept=mean(monthly_means$Truffles), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Truffles), digits = 1)
                ,label = round(mean(monthly_means$Truffles), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Truffles_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##


plot <- ggplot(monthly_means, aes(x=Month, y=Coke)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=Coke, label=Coke),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Coke") +
  geom_hline(yintercept=mean(monthly_means$Coke), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$Coke), digits = 1)
                ,label = round(mean(monthly_means$Coke), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Coke_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##
plot <- ggplot(monthly_means, aes(x=Month, y=`Spanish Brunch`)) +
  geom_bar(stat="identity", fill="skyblue", position = 'dodge', alpha=0.7)+
  geom_text(aes(x=Month, y=`Spanish Brunch`, label=`Spanish Brunch`),size = 4, position = position_dodge2(width = 1), vjust=-0.5)+
  xlab("Monat") + ylab("Spanish Brunch") +
  geom_hline(yintercept=mean(monthly_means$`Spanish Brunch`), color="darkgreen")+
  geom_text(aes(0,round(mean(monthly_means$`Spanish Brunch`), digits = 1)
                ,label = round(mean(monthly_means$`Spanish Brunch`), digits = 1)
                ,vjust = -1, ),position = position_dodge(width = 0.7) )+
  theme(text = element_text(size=20))
# save plots as .png
ggsave(plot, file=paste(results,"Spanish Brunch_monthly_w_o.png", sep=''), height=7, width=10, units="in", dpi=150)
##

# Plots for the thesis
# Total_Mean_monthly
# Top10_Products
# Total_Mean_week

#time series plot
total_ts = ts(intermediate_predict_total$quantity, frequency = 7)

plot_total_ts <- autoplot(total_ts, color="skyblue", alpha = 0.5, size = 2) + 
  ggtitle("Sales over Time") + theme(text = element_text(size=20)) +
  labs(y="Sold Units", x="Weeks")

#combination of the plots
plot <- ggarrange(plot_total_ts, Top10_Products, Total_Mean_monthly, Total_Mean_week,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
plot
# save plots as .png
ggsave(plot, file=paste(results,"explaining.png", sep=''), height=14, width=26, units="in", dpi=150)

#decomposed time series plot of Coffee
Coffee = sales[Item == "Coffee", .(quantity = sum(quantity)), by = "Date"]
Coffee = merge(dateseq, Coffee, by="Date", all.x=TRUE)
Coffee = Coffee[-c(1:6, 161:162),]
Coffee[is.na(quantity)] <- 0
Coffee_ts = ts(Coffee$quantity, frequency = 7)

decomposed_ts = decompose(Coffee_ts)
plot <- autoplot(decomposed_ts)
# save plots as .png
ggsave(plot, file=paste(results,"Coffee_ts.png", sep=''), height=7, width=10, units="in", dpi=150)


##
#



#End_of_code -------------------------------

