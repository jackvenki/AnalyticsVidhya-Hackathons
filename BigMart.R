# load library

library(data.table)
library(ggplot2)
# for renaming levels using revalue function
library(plyr)
library(dplyr)
library(dummies)
library(h2o)
#load datasets

train <- fread('C:/Users/Venkatesan/Desktop/Datasets to analyze/AV-BigMart/Train_UWu5bXk.csv')
test <-  fread('C:/Users/Venkatesan/Desktop/Datasets to analyze/AV-BigMart/Test_u94Q5KV.csv')

#checking the dimension in the dataset

dim(train)
dim(test)

#check the varaibles and type

str(train)

#changing datatype of the columns
train$Item_Identifier <- as.factor(train$Item_Identifier)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
train$Item_Type <- as.factor(train$Item_Type)
train$Outlet_Identifier <- as.factor(train$Outlet_Identifier)
train$Outlet_Size <- as.factor(train$Outlet_Size)
train$Outlet_Location_Type <- as.factor(train$Outlet_Location_Type)
train$Outlet_Type <- as.factor(train$Outlet_Type)

test$Item_Identifier <- as.factor(test$Item_Identifier)
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)
test$Item_Type <- as.factor(test$Item_Type)
test$Outlet_Identifier <- as.factor(test$Outlet_Identifier)
test$Outlet_Size <- as.factor(test$Outlet_Size)
test$Outlet_Location_Type <- as.factor(test$Outlet_Location_Type)
test$Outlet_Type <- as.factor(test$Outlet_Type)

#check missing values
table(is.na(train))
colSums(is.na(train))

#getting inferences from data
summary(train)

#visualizing the data
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")

#Merging Test and train

test$Item_Outlet_Sales <-1


#train_merge <- as.data.frame(train[,c(1:11),with=FALSE])
#test <- as.data.frame(test)
merged.data <- rbind(train,test)

#Missing value imputation  since imputation by median is robust
merged.data$Item_Weight[is.na(merged.data$Item_Weight)] <- median (merged.data$Item_Weight,na.rm = TRUE)  #here you did a mistake
merged.data$Item_Visibility <- ifelse(merged.data$Item_Visibility==0, median(merged.data$Item_Visibility),merged.data$Item_Visibility)

#outlet first level is blank (Mismateched level correction)
names(levels(merged.data$Outlet_Size))[1] <- 'other'

merged.data$Item_Fat_Content<-  revalue(merged.data$Item_Fat_Content,c("LF"="Low Fat","low fat" = "Low Fat","reg" = "Regular"))

#Data Manipulation

#Adding new variable count of stores
a <- merged.data %>% group_by(Outlet_Identifier) %>%tally()
names(a)[2] <- "outlet_count"
merged.data <- full_join(a,merged.data,by="Outlet_Identifier")

#Adding new variable Age of stores
c <- merged.data %>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - merged.data$Outlet_Establishment_Year)

merged.data$Outlet_Year <- c$Outlet_Year

#merged.data <- full_join(c,merged.data)

q <- substr(merged.data$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)

merged.data$Item_Type_new <-q
merged.data$Item_Type_new <- as.factor(merged.data$Item_Type_new)

#one hot encoding
merged.data$Item_Fat_Content <- ifelse(merged.data$Item_Fat_Content=="Regular",1,0)
oneHotEncoding <- select(merged.data,Outlet_Location_Type,Outlet_Type,Item_Type_new,Outlet_Size)
oneHotEncoding_new <- data.frame(model.matrix(~.-1,oneHotEncoding))
merged.data <- cbind(merged.data[,-c(1,3,5,7,9,10,11,12)],oneHotEncoding_new)

new_train <- merged.data[1:nrow(train),]
new_test <- merged.data[-(1:nrow(train)),]

#new_train$Item_Outlet_Sales <- train$Item_Outlet_Sales
#new_test$Item_Outlet_Sales <-1

#linear Regression

linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)