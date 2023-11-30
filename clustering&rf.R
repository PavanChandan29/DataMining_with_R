library(dplyr)
library (ggplot2)
library (arules)
sales_g.df <- read.csv("pro_proc.csv", header = TRUE)
sales_g.df <- sales_g.df %>% distinct()

sales_g.df[sales_g.df == 0] <- NA

sales_g.df <- na.omit(sales_g.df)
sales_g.df$Geography <- gsub("- IRI Standard - Multi Outlet \\+ Conv", "", sales_g.df$Geography)
sales_g.df$Geography <- gsub("- Multi Outlet \\+ Conv", "", sales_g.df$Geography)
###################### clustering1
GFT.df <- sales_g.df[c("Geography","CAG.Form.Value","total_sales")]
GFT.df <- GFT.df %>%
  filter(Geography != "Total US ")

set.seed(20)
sample_size <- 5000  
samp.df <- GFT.df[sample(1:nrow(GFT.df), sample_size), ]

sum(is.na(samp.df))
samp.df[, "total_sales"] <- scale(samp.df[, "total_sales"])
dummy_df <- as.data.frame(model.matrix(~ . - 1, data = samp.df))

kmed <- pam(dummy_df, 3, metric ="manhattan") #mediods

kmed$cluster
samp.df$cluster <- as.factor(kmed$cluster)

ggplot(samp.df, aes(x = Geography, y = total_sales, color = factor(cluster))) +
  geom_point() +
  facet_wrap(~CAG.Form.Value) +
  ggtitle("K-Meds Clustering of Sales by Geography and CAG.Form.Value") +
  labs(x = "Geography", y = "Sales", color = "Cluster")


############################ Clustering2
FTS.df <- sales_g.df[c("Geography","CAG.Tier.Value","Total_Unit_Sale")]
summary(samp_fts.df)
FTS.df <- FTS.df %>%
  filter(Geography != "Total US ")
FTS.df <- FTS.df %>%
  filter(CAG.Tier.Value != "TBD")

sample_size_fts <- 20000  
samp_fts.df <- FTS.df[sample(1:nrow(FTS.df), sample_size_fts), ]
samp_fts.df[, "Total_Unit_Sale"] <- scale(samp_fts.df[, "Total_Unit_Sale"])
dummy_fts <- as.data.frame(model.matrix(~ . - 1, data = samp_fts.df))

kmed_fts <- pam(dummy_fts, 5, metric ="manhattan") #mediods
kmed_fts$cluster
samp_fts.df$cluster <- as.factor(kmed_fts$cluster)

ggplot(samp_fts.df, aes(x = Geography, y = Total_Unit_Sale, color = factor(cluster))) +
  geom_point() +
  facet_wrap(~CAG.Tier.Value) +
  ggtitle("K-Meds Clustering of Total_Unit_Sale by CAG.Tier.Value and CAG.Form.Value") +
  labs(x = "CAG.Tier.Value", y = "Total_Unit_Sale", color = "Cluster")

########################
library(rpart)
library(rpart.plot)
library(caret)

gtt.df <- sales_g.df[c("CAG.Form.Value", "Geography", "CAG.Tier.Value","Total_Volume_Sale","Total_Unit_Sale")]
gtt.df <- gtt.df %>%
  filter(Geography != "Total US " | CAG.Tier.Value != "TBD" )
gtt.df <- gtt.df %>%
  filter((CAG.Form.Value == "ALL OTHER FORM" | CAG.Form.Value == "STICKS" | 
            CAG.Form.Value =="TUBS" | CAG.Form.Value =="SPRAY/SQUEEZE"))

sum(is.na(gtt.df))

train.index <- sample(c(1:dim(gtt.df)[1]), dim(gtt.df)[1]*0.7)  
train.df <- gtt.df[train.index, ]
valid.df <- gtt.df[-train.index, ]


library(randomForest)
################# random forest
rf <- randomForest(as.factor(CAG.Form.Value) ~ ., data = train.df, ntree = 200, 
                   mtry = 2, nodesize = 4, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, as.factor(valid.df$CAG.Form.Value))
