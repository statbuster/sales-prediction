# Loading Packages

library(data.table)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(xgboost)


#################  Understanding the Data  #################

# Reading Data

train = fread("data/train.csv")
test = fread("data/test.csv")
submission = fread("data/sample_submission.csv")

# Dimension of the data

dim(train);dim(test)

# Features in the data

names(train)
names(test)

# Structure of the data

str(train)
str(test)

# Combine Train and Test

test[,Item_Outlet_Sales := NA]
combi <- rbind(train, test)

#################  EDA  #################

# Target Variable

ggplot(train) + geom_histogram(aes(x = Item_Outlet_Sales), bins = 50, fill = "blue", color = "black") + 
  labs(title = "Distribution of Item Outlet Sales", x = "Item Outlet Sales", y = "Frequency")

# Independent Variables(numeric variables)

p1 <- ggplot(combi) + geom_histogram(aes(x = Item_Weight), bins = 50, fill = "blue", color = "black")
p2 <- ggplot(combi) + geom_histogram(aes(x = Item_Visibility), bins = 50, fill = "blue", color = "black")
p3 <- ggplot(combi) + geom_histogram(aes(x = Item_MRP), bins = 50, fill = "blue", color = "black")

plot_grid(p1, p2, p3, nrow = 1)

# Independent Variables(categorical variables)

ggplot(combi |> group_by(Item_Fat_Content) |> summarise(count=n())) 
    + geom_bar(aes(x = Item_Fat_Content, y = count), stat = "identity", fill = "coral1") 
    + labs(title = "Distribution of Item Fat", x = "Item Fat", y = "Count")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] <- "Regular"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] <- "Low Fat"

ggplot(combi |> group_by(Item_Fat_Content) |> summarise(count=n())) 
    + geom_bar(aes(x = Item_Fat_Content, y = count), stat = "identity", fill = "coral1") 
    + labs(title = "Distribution of Item Fat Content", x = "Item Fat Content", y = "Count")

p4 <- ggplot(combi |> group_by(Item_Type) |> summarise(count=n())) 
    + geom_bar(aes(x = Item_Type, y = count), stat = "identity", fill = "coral1") 
    + geom_label(aes(Item_Type, count, label = count), vjust = 0.5) 
    + theme(axis.text.x = element_text(angle = 45, hjust=1))

p5 <- ggplot(combi |> group_by(Outlet_Identifier) |> summarise(count=n())) 
    + geom_bar(aes(x = Outlet_Identifier, y = count), stat = "identity", fill = "coral1") 
    + geom_label(aes(Outlet_Identifier, count, label = count), vjust = 0.5) 
    + theme(axis.text.x = element_text(angle = 45, hjust=1))

p6 <- ggplot(combi |> group_by(Outlet_Size) |> summarise(count=n())) 
    + geom_bar(aes(x = Outlet_Size, y = count), stat = "identity", fill = "coral1") 
    + geom_label(aes(Outlet_Size, count, label = count), vjust = 0.5) 
    + theme(axis.text.x = element_text(angle = 45, hjust=1))

second_row <- plot_grid( p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

p7 <- ggplot(combi |> group_by(Outlet_Establishment_Year) |> summarise(count=n())) 
    + geom_bar(aes(x = factor(Outlet_Establishment_Year), y = count), stat = "identity", fill = "coral1") 
    + geom_label(aes(factor(Outlet_Establishment_Year), count, label = count), vjust = 0.5) 
    + xlab("Outlet_Establishment_Year") + theme(axis.text.x = element_text(size=8.5))

p8 <- ggplot(combi |> group_by(Outlet_Type) |> summarise(count=n())) 
    + geom_bar(aes(x = Outlet_Type, y = count), stat = "identity", fill = "coral1") 
    + geom_label(aes(Outlet_Type, count, label = count), vjust = 0.5) 
    + theme(axis.text.x = element_text(size=8.5))

plot_grid(p7, p8, ncol=2)