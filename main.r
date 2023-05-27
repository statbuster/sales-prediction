# Loading Packages

library(data.table)
library(dplyr)
library(ggplot2)
library(cowplot)
library(corrplot)
library(xgboost)
library(caret)

#################  Understanding the Data  #################

# Reading Data

train = fread("./sales-prediction/data/train.csv")
test = fread("./sales-prediction/data/test.csv")
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

ggplot(combi |> group_by(Item_Fat_Content) |> summarise(count=n()))  + geom_bar(aes(x = Item_Fat_Content, y = count), stat = "identity", fill = "coral1")  + labs(title = "Distribution of Item Fat", x = "Item Fat", y = "Count")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] <- "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] <- "Regular"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] <- "Low Fat"

ggplot(combi |> group_by(Item_Fat_Content) |> summarise(count=n()))     + geom_bar(aes(x = Item_Fat_Content, y = count), stat = "identity", fill = "coral1") + labs(title = "Distribution of Item Fat Content", x = "Item Fat Content", y = "Count")

p4 <- ggplot(combi |> group_by(Item_Type) |> summarise(count=n()))  + geom_bar(aes(x = Item_Type, y = count), stat = "identity", fill = "coral1") + geom_label(aes(Item_Type, count, label = count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust=1))

p5 <- ggplot(combi |> group_by(Outlet_Identifier) |> summarise(count=n())) + geom_bar(aes(x = Outlet_Identifier, y = count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Identifier, count, label = count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust=1))

p6 <- ggplot(combi |> group_by(Outlet_Size) |> summarise(count=n())) + geom_bar(aes(x = Outlet_Size, y = count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Size, count, label = count), vjust = 0.5) + theme(axis.text.x = element_text(angle = 45, hjust=1))

second_row <- plot_grid( p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)

p7 <- ggplot(combi |> group_by(Outlet_Establishment_Year) |> summarise(count=n())) + geom_bar(aes(x = factor(Outlet_Establishment_Year), y = count), stat = "identity", fill = "coral1") + geom_label(aes(factor(Outlet_Establishment_Year), count, label = count), vjust = 0.5) + xlab("Outlet_Establishment_Year") + theme(axis.text.x = element_text(size=8.5))

p8 <- ggplot(combi |> group_by(Outlet_Type) |> summarise(count=n())) + geom_bar(aes(x = Outlet_Type, y = count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Type, count, label = count), vjust = 0.5) + theme(axis.text.x = element_text(size=8.5))

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

# Missing Values

sum(is.na(combi$Item_Weight))

missing_index <- which(is.na(combi$Item_Weight))

for(i in missing_index){
    item = combi$Item_Identifier[i]
    combi$Item_Weight[i] = mean(combi$Item_Weight[combi$Item_Identifier == item], na.rm = TRUE)
}

sum(is.na(combi$Item_Weight))

ggplot(combi) + geom_histogram((aes(Item_Visibility)), bins=100)

zero_index <- which(combi$Item_Visibility == 0)
for (i in zero_index) {
    item = combi$Item_Identifier[i]
    combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = TRUE)
}

ggplot(combi) + geom_histogram((aes(Item_Visibility)), bins=100)

# Feature Engineering

perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

table(combi$Item_Type_new, substr(combi$Item_Identifier, 1, 2))

combi[,Item_Type_new := substr(Item_Identifier, 1, 2)]

combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st", ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd", ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

# encoding categorical variables

combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0, ifelse(Outlet_Size == "Medium", 1, 2))]
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0, ifelse(Outlet_Location_Type == "Tier 2", 1, 2))]

combi[,c("Outlet_Size", "Outlet_Location_Type") := NULL]

ohe <- dummyVars("~.", data = combi[,-c("Item_Identifier","Outlet_Establishment_Year","Item_Type")], fullRank = T)
ohe_df <- data.table(predict(ohe, combi[,-c("Item_Identifier","Outlet_Establishment_Year","Item_Type")]))

combi <- cbind(combi[,"Item_Identifier"],ohe_df)

##################  Preprocessing Data  #################

# Removing Skewness

combi[, Item_Visibility := log(1 + Item_Visibility)]
combi[, price_per_unit_wt := log(1 + price_per_unit_wt)]

# Scaling numeric predictors

num_vars <- which(sapply(combi, is.numeric))
num_vars_names <- names(num_vars)
combi_numeric <- combi[, setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]

prep_num <- preProcess(combi_numeric, method = c("center", "scale"))
combi_numeric_norm <- predict(prep_num, combi_numeric)

combi[, setdiff(num_vars_names,"Item_Outlet_Sales") := NULL]
combi <- cbind(combi, combi_numeric_norm)

train <- combi[1:nrow(train)]
test <- combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales:= NULL]

cor_train <- cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type="lower", tl.cex = 0.9)
