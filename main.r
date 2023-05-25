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

# Target Variable vs Independent Numerical Variables

p9 <- ggplot(train)  + geom_point(aes(Item_Weight, Item_Outlet_Sales), color = "violet", alpha = 0.5) + theme(axis.title = element_text(size = 8.5))

p10 <- ggplot(train)  + geom_point(aes(Item_Visibility, Item_Outlet_Sales), color = "violet", alpha = 0.5)  + theme(axis.title = element_text(size = 8.5))

p11 <- ggplot(train) + geom_point(aes(Item_MRP, Item_Outlet_Sales), color = "violet", alpha = 0.5)  + theme(axis.title = element_text(size = 8.5))

second_row_2 <- plot_grid(p10, p11, ncol = 2)
plot_grid(p9, second_row_2, nrow=2)


p12 <- ggplot(train) +  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1),  axis.text = element_text(size = 6),  axis.title = element_text(size = 8.5))
p13 <- ggplot(train) +  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1),  axis.text = element_text(size = 8),  axis.title = element_text(size = 8.5))
p14 <- ggplot(train) +  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") + theme(axis.text.x = element_text(angle = 45, hjust = 1),  axis.text = element_text(size = 8), axis.title = element_text(size = 8.5))

second_row_3 <- plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, nrow=2)

ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")

p15 <- ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), fill = "magenta")
p16 <- ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), fill = "magenta")

plot_grid(p15,p16,ncol=1)
