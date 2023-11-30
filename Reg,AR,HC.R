library (dplyr)
library (arules)
library(cluster)
library(factoextra)
library(caret)
sales.df <- read.csv("pro_proc.csv", header = TRUE)

sales.df <- sales.df %>% distinct()

sales.df[sales.df == 0] <- NA

sales.df <- na.omit(sales.df)

sales.df$Geography <- gsub("- IRI Standard - Multi Outlet \\+ Conv", "", sales.df$Geography)
sales.df$Geography <- gsub("- Multi Outlet \\+ Conv", "", sales.df$Geography)

sales.df <- sales.df %>%
  filter(Geography != "Total US ")

sales.df <- sales.df %>% select(-Time)

sales.df <- sales.df %>% select(-total_sales)

sales.df <- sales.df %>%
  filter(!(Sub.Category.Name == "HAND & BODY LOTION" | Sub.Category.Name == "NA" | Sub.Category.Name =="RFG SAUCE/GRAVY/MARINADE MIXES" | Sub.Category.Name =="RFG FLAVORED MILK" | Form == "NOT STATED ON PACKAGE" | Form == "NA"))


sales_data <- sales.df

categorical_columns <- c("Geography", "Sub.Category.Name", "CAG.Tier.Value", "Form")

numeric_columns <- c("Price.per.Unit", "Price.per.Volume", "Total_Unit_Sale", "Total_Volume_Sale")

### EXploratory Data Analysis ###
# Scatterplot Matrix
pairs(sales_data[numeric_columns])

# Boxplot by Geography
boxplot(Total_Unit_Sale ~ Geography, data = sales_data)

# Heatmap
correlation_matrix <- cor(sales_numeric_data)
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(20),
        main = "Correlation Heatmap")
###

sales_data[categorical_columns] <- lapply(sales_data[categorical_columns], as.factor)

### Association Analysis using arules ###
sales_transactions <- as(sales_data, "transactions")
rules <- apriori(sales_transactions, parameter = list(supp = 0.1, conf = 0.7))
inspect(rules)


### Linear Regression
lm_model <- lm(Total_Unit_Sale ~ Price.per.Unit + Price.per.Volume + Geography + Sub.Category.Name + CAG.Tier.Value + Form, data = sales_data)

# Summary of the linear regression model
summary(lm_model)

# Regression Residuals Plot
plot(lm_model, which = 1)

# Regression Coefficient Plot
plot(lm_model, which = 2)

### Hierarchical Clustering
sales_numeric_data <- sales_data[numeric_columns]

set.seed(123)  
sample_size <- 1000  
sampled_data <- sales_numeric_data[sample(1:nrow(sales_numeric_data), sample_size), ]

scaled_data <- scale(sampled_data)
dist_matrix <- dist(sampled_data)
hclust_model <- hclust(dist_matrix, method = "complete")

k <- 3
cluster_labels <- cutree(hclust_model, k)

fviz_cluster(list(data = sampled_data, cluster = cluster_labels), geom = "point", stand = FALSE, title = "Hierarchical Clustering")
