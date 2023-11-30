library(dplyr)
library("Hmisc")
library(corrplot)
library(cluster)
library(factoextra)
library(lubridate)
library(ggplot2)
install.packages("pheatmap")
library(pheatmap)
library(polycor)
pro.df <- read.csv("mydata.csv")
ncol(pro.df)
nrow(pro.df)
pro22.df <- pro.df %>% filter(Year == 2022) #use data where year=2022
summary(pro22.df)
nrow(pro22.df)
columns_to_replace <- c(5:25,38)
pro22.df <- pro22.df %>% mutate_at(vars(all_of(columns_to_replace)),~ifelse(is.na(.), 0, .)) #replacing NA values in quantitative cols
pro22.df$Total_Unit_Sale <- pro22.df$Unit.Sales.No.Merch+pro22.df$Unit.Sales.Any.Merch
pro22.df$Total_Volume_Sale <- pro22.df$Volume.Sales.No.Merch+pro22.df$Volume.Sales.Any.Merch
pro22.df <- pro22.df[-c(4:11,13,14,16:19,23:27,37)]
pro22.df <- pro22.df[-c(1)] #removing serial_no
pro22.df <- pro22.df[-c(11)] #removing CAG.Category.Value
ncol(pro22.df)
print(colnames(pro22.df))

pro22.df$Time <- sub("Week Ending ", "", pro22.df$Time)
class(pro22.df$Time)
pro22.df$Time <- myd(pro22.df$Time)
pro22.df$Time <- month(ymd(pro22.df$Time))
pro22.df$Time <- month.name[pro22.df$Time]

write.csv(pro22.df, file = "pro_proc.csv", row.names = FALSE)