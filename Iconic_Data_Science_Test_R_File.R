



#install.packages("RSQLite") #perhaps needed

library("RSQLite")


# connect to the sqlite database file
#I am using my system file path you can replace it with filepath in your system
filename<-"C:/Users/syeds/Documents/My Docs/Iconic Job/datascientist-master/datascientist-master/test_data.db/test_data.db"

#/Users/sajjad/Documents/Iconic Test/datascientist-master/test_data.db   Mac filepath
#C:/Users/syeds/Documents/My Docs/Iconic Job/datascientist-master/datascientist-master/test_data.db/test_data.db  windows filepath


#store the driver in sqlite
sqlite <- dbDriver("SQLite")

#import the database in the Iconic_Database_Test
Iconic_Database_Test <- dbConnect(sqlite,dbname=filename)


#Checking the Data in the Iconic_Database_Test

#Getting the List of Tables in the Database
dbListTables(Iconic_Database_Test)

#Getting the List of all the Columns in the CUstomers Table
dbListFields(Iconic_Database_Test,"customers")


#Question 1*: What was the total revenue to the nearest dollar for customers who have paid by credit card?

#The Query and Output 
dbGetQuery(Iconic_Database_Test,'SELECT  SUM(revenue) as Revenue 
           FROM customers 
           WHERE cc_payments<>0'
)





#Question 2*: What percentage of customers who have purchased female items have paid by credit card?




#The Query and Output 
dbGetQuery(Iconic_Database_Test,'SELECT  Round (AVG(CASE WHEN cc_payments=1 THEN 1 ELSE 0 END ) *100,2)
           as Percentage_Of_Credit_Card_Payments 
           FROM customers 
           WHERE female_items<>0'
)



#Question 3*: What was the average revenue for customers who used either iOS, Android or Desktop?


#The Query and Output 
dbGetQuery(Iconic_Database_Test,'SELECT  round(avg(revenue),2) as Average_Revenue
           FROM customers 
           WHERE (desktop_orders<>0 OR ios_orders <>0 OR android_orders <>0)'
)



#Question 4*: We want to run an email campaign promoting a new mens luxury brand. Can you provide a list of customers we should send to?


#The Query and I have hidden the result because it will take too much space in the Markdown File

Output<-dbGetQuery(Iconic_Database_Test,'SELECT   distinct customer_id 
                   FROM  customers 
                   WHERE male_items <>0 or unisex_items <> 0'
)

# This is the list of customers
Output






# Install package function: it will installs and load multiple R packages.
# Install the package if they are not istalled, then load them into the R session.
r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

Install_Packages <- function(package){
  new.package <- package[!(package %in% installed.packages()[, "Package"])]
  if (length(new.package)) 
    install.packages(new.package,dependencies = TRUE)
  sapply(package, require, character.only = TRUE)
}


packages <- c("readr", "jsonlite", "ggplot2", "dplyr", "tidyverse", "cluster","factoextra","gridExtra","corrplot","randomForestExplainer")
Install_Packages(packages)


install.packages("devtools")
library(devtools)
devtools::install_github('araastat/reprtree')
library(reprtree)



#library(readr)      
#library(jsonlite)   # for reading json file
#library(ggplot2)    # plotting the data
#library(dplyr)      # data manipulation
#library(tidyverse)  # data manipulation
#library(cluster)    # clustering algorithms
#library(factoextra) # clustering algorithms & visualization
#library(gridExtra)  # displaying multiple k value clusters at once
#library(corrplot)




Iconic_Test_Data<-fromJSON("C:/Users/syeds/Documents/My Docs/Iconic #Job/datascientist-master/datascientist-master/test_data/data.json")

#/Users/sajjad/Documents/Iconic Test/datascientist-master/data.json  Mac filepath

#C:/Users/syeds/Documents/My Docs/Iconic #Job/datascientist-master/datascientist-master/test_data/data.json windows file path

#convert the data into dataframe 

Iconic_Test_Data = data.frame(Iconic_Test_Data)

#check the data summary statistics  

summary(Iconic_Test_Data)

#Check the first few rows of data




#Number of corrupt records  in the data 
corrupt_records <- filter(Iconic_Test_Data,days_since_first_order< days_since_last_order) %>% count()

corrupt_records
#Percentage of corrupt recordsin the data 
percetange_corrupt_records  <-round(corrupt_records/count(Iconic_Test_Data) *100,2)

percetange_corrupt_records











#Filtering the Corrupt Records and storing it in the corrupt_records_values
corrupt_records_values <- filter(Iconic_Test_Data,days_since_first_order< days_since_last_order)


#Adding a new coulmn after dividing  it with 24
corrupt_records_values<-mutate (corrupt_records_values, multiple=corrupt_records_values$days_since_last_order/24)

#filtering the days_since_first_order,days_since_last_order,multiple columns and storing them back into the corrupt_records_values and overwritting the existing dataframe

corrupt_records_values<-corrupt_records_values%>%select(days_since_first_order,days_since_last_order,multiple)

#viewing the data
corrupt_records_values







Iconic_Test_Data_Clean <- filter(Iconic_Test_Data,days_since_first_order >= days_since_last_order)

#Orders must be more than or equal to the cancellations.. removing any order cancellation anomalies
Iconic_Test_Data_Clean <- filter(Iconic_Test_Data_Clean,cancels <= orders)





 

#filtering the numeric only values for doing th correlation analysis
Iconic_Test_Data_Clean_Numeric_1<-Iconic_Test_Data_Clean%>%keep(is.numeric)

#casting the Correlation Matrix for the data 
Data_Correlation_Matrix<-cor(as.matrix(Iconic_Test_Data_Clean_Numeric_1))





#correlation plot 
corrplot(as.matrix(Data_Correlation_Matrix))




#Adding Average order value
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, AOV = revenue/orders )

#Adding average number of items per orders
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, ItemsPerOrder = items/orders)

#Cancellation Percentage
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, CancellationRatio = 100 *cancels/orders)


#Adding Return Ratio'
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, ReturnRatio = 100 *returns/orders)


#Vouchers Ratio = Vouchers per order
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, VoucherRatio = 100 *vouchers/orders)

#FemaleItemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, FemaleItemsRatio = 100 *female_items/items)

#MaleItemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MaleItemsRatio = 100 *male_items/items)

#UnisexRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, UnisexItemsRatio = 100 *unisex_items/items)

#wapp_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, WappItemsRatio = 100 *wapp_items/items)

#macc_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MaccItemsRatio = 100 *macc_items/items)


#wftw_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, WftwItemsRatio = 100 *wftw_items/items)


#mapp_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MappItemsRatio = 100 *mapp_items/items)

#wacc_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, WaccItemsRatio = 100 *wacc_items/items)

#mftw_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MftwItemsRatio = 100 *mftw_items/items)

#wspt_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, WsptItemsRatio = 100 *wspt_items/items)

#mspt_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MsptItemsRatio = 100 *mspt_items/items)

#curvy_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, CurvyItemsRatio = 100 *curvy_items/items)


#saccy_itemsRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, SaccItemsRatio = 100 *sacc_items/items)




#msite_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, MsiteOrdersRatio = 100 *msite_orders/orders)

#desktop_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, DesktopOrdersRatio = 100 *desktop_orders/orders)

#android_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, AndroidOrdersRatio = 100 *android_orders/orders)

#ios_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, IosOrdersRatio = 100 *ios_orders/orders)

#other_device_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, OtherDeviceOrdersRatio = 100 *other_device_orders/orders)

#work_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, WorkOrdersRatio = 100 *work_orders/orders)

#home_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, HomeOrdersRatio = 100 *home_orders/orders)


#parcelpoint_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, ParcelPointOrdersRatio = 100 *parcelpoint_orders/orders)


#other_collection_ordersRatio
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, OtherCollectionOrdersRatio = 100 *other_collection_orders/orders)


#Revenue_Per_Order
Iconic_Test_Data_Clean <- mutate(Iconic_Test_Data_Clean, RevenuePerOrder = 100 *revenue/orders)





Iconic_Test_Data_Clean <- Iconic_Test_Data_Clean%>% select(customer_id
                                                           ,days_since_first_order
                                                           ,days_since_last_order
                                                           ,FemaleItemsRatio
                                                           ,MaleItemsRatio
                                                           ,UnisexItemsRatio
                                                           ,average_discount_onoffer
                                                           ,average_discount_used
                                                           ,ItemsPerOrder
                                                           ,CancellationRatio
                                                           ,ReturnRatio
                                                           ,VoucherRatio
                                                           ,WappItemsRatio
                                                           ,MaccItemsRatio
                                                           ,WftwItemsRatio
                                                           ,MappItemsRatio
                                                           ,WaccItemsRatio
                                                           ,MftwItemsRatio
                                                           ,WsptItemsRatio
                                                           ,MsptItemsRatio
                                                           ,CurvyItemsRatio
                                                           ,SaccItemsRatio
                                                           ,MsiteOrdersRatio
                                                           ,DesktopOrdersRatio
                                                           ,AndroidOrdersRatio
                                                           ,IosOrdersRatio
                                                           ,OtherDeviceOrdersRatio
                                                           ,WorkOrdersRatio
                                                           ,HomeOrdersRatio
                                                           ,ParcelPointOrdersRatio
                                                           ,OtherCollectionOrdersRatio
                                                           ,RevenuePerOrder
                                                           
                                                           
)



#First we will check for any non-numeric value being passed in the Normalize function and remove it.


which(apply(Iconic_Test_Data_Clean, 2, var)==0)

Iconic_Test_Data_Clean['OtherDeviceOrdersRatio'] = NULL

Iconic_Test_Data_Clean_Numeric =Iconic_Test_Data_Clean%>%keep(is.numeric)
summary(Iconic_Test_Data_Clean_Numeric)
#creating a normalization funtion here
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(Iconic_Test_Data_Clean_Numeric, normalize))





pca_Iconic_data<-prcomp(na.omit(dfNorm),center = TRUE,scale. = TRUE)
#Iconic_Test_Data_Clean['other_device_orders'] = NULL
summary(pca_Iconic_data)






principle_components <- data.frame(pca_Iconic_data$x[,1:14])
# Plot the principle components
plot(principle_components, pch=16, col=rgb(0,0,0,0.5))





fviz_nbclust(dfNorm, kmeans, method = "wss")
fviz_nbclust(dfNorm, kmeans, method = "silhouette")



set.seed(123)
k2 <- kmeans(na.omit(dfNorm), centers = 2, nstart = 25)
k3 <- kmeans(na.omit(dfNorm), centers = 3, nstart = 25)
k4 <- kmeans(na.omit(dfNorm), centers = 4, nstart = 25)


# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = na.omit(dfNorm))+ ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = na.omit(dfNorm)) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = na.omit(dfNorm)) + ggtitle("k = 4")
grid.arrange(p2, p3, p4, nrow = 2)
k2
#plot(na.omit(dfNorm), k2$cluster)


Iconic_Test_Data_Clean_Clusters <- mutate(Iconic_Test_Data_Clean, Cluster = k2$cluster)


#install the randomforest package if its not already installed
if(!require(randomForest)) install.packages("randomForest",repos ="http://cran.us.r-project.org")

set.seed(123)

#Create the train sample split

train_data <- sample(nrow(Iconic_Test_Data_Clean_Clusters),0.7*nrow(Iconic_Test_Data_Clean_Clusters), replace = FALSE)


#Store the Training data in TrainSet
TrainSet <- Iconic_Test_Data_Clean_Clusters[train_data,]

#Store the Validiation dataset in ValidSet
ValidSet <- Iconic_Test_Data_Clean_Clusters[-train_data,]


#Print the Summary of the train and valid data to see the data split 
summary(TrainSet)
summary(ValidSet)

#Convert the Cluster as factor for prediction 

TrainSet$Cluster <- as.character(TrainSet$Cluster)
TrainSet$Cluster <- as.factor(TrainSet$Cluster)



ValidSet$Cluster <- as.character(ValidSet$Cluster)
ValidSet$Cluster <- as.factor(ValidSet$Cluster)


#RandomForest Model


RandomForest_Model <- randomForest(Cluster ~ 
days_since_first_order
+days_since_last_order
+FemaleItemsRatio
+MaleItemsRatio         
+UnisexItemsRatio
+average_discount_onoffer
+average_discount_used
+ItemsPerOrder
+CancellationRatio
+ReturnRatio
+VoucherRatio
+WappItemsRatio
+MaccItemsRatio
+WftwItemsRatio
+MappItemsRatio
+WaccItemsRatio
+MftwItemsRatio
+WsptItemsRatio
+MsptItemsRatio
+CurvyItemsRatio
+SaccItemsRatio
+MsiteOrdersRatio
+DesktopOrdersRatio
+AndroidOrdersRatio
+IosOrdersRatio
+WorkOrdersRatio
+HomeOrdersRatio
+ParcelPointOrdersRatio
+OtherCollectionOrdersRatio
+RevenuePerOrder, data = TrainSet,importance = TRUE)




RandomForest_Model



# Predicting on train set to see how our model is preforming on the data we used for builing it


predTrain <- predict(RandomForest_Model, TrainSet, type = "class")


# Checking classification accuracy
table(predTrain, TrainSet$Cluster) 

# Predicting on Validation set
predValid <- predict(RandomForest_Model, ValidSet, type = "class")


# Checking classification accuracy
mean(predValid == ValidSet$Cluster)  


table(predValid,ValidSet$Cluster)

# To check importance of different variables
importance(RandomForest_Model)  

#plot the important variable of RandomForest
varImpPlot(RandomForest_Model) 


#Now Lets Plot the Random Forest and Interpert the Gender  

reprtree:::plot.getTree(RandomForest_Model,350)



#Create the Technical Explantory Note for the random forest
explain_forest(RandomForest_Model, interactions = TRUE, data = TrainSet)



RandomForest_Model2 <- randomForest(Cluster ~ FemaleItemsRatio+MaleItemsRatio+WappItemsRatio
                                    +WftwItemsRatio+MappItemsRatio+MftwItemsRatio
                                    , data = TrainSet,importance = TRUE)




RandomForest_Model2



# Predicting on train set to see how our model is preforming on the data we used for builing it


predTrain <- predict(RandomForest_Model2, TrainSet, type = "class")


# Checking classification accuracy
table(predTrain, TrainSet$Cluster) 

# Predicting on Validation set
predValid <- predict(RandomForest_Model2, ValidSet, type = "class")


# Checking classification accuracy
mean(predValid == ValidSet$Cluster)  


table(predValid,ValidSet$Cluster)

# To check importance of different variables
importance(RandomForest_Model2)  

#plot the important variable of RandomForest
varImpPlot(RandomForest_Model2) 




#Now Lets Plot the Random Forest and Interpert the Gender  

reprtree:::plot.getTree(RandomForest_Model2,350)



#Create the Technical Explantory Note for the random forest
explain_forest(RandomForest_Model2, interactions = TRUE, data = TrainSet)



