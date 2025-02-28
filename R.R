
#R script
######################


#0.Loading libraries


























































# Load the necessary libraries
library(dplyr)
library(tidyr)

# Load the dataset
data <- read.csv("EuStockMarkets")
data = EuStockMarkets

# Check for missing values
summary(is.na(data))

# Remove rows with missing values
data <- data[complete.cases(data), ]

# Remove duplicates
data <- data[!duplicated(data), ]

# Convert categorical variables to factors
for (i in 1:ncol(data)) {
  if (is.character(data[, i])) {
    data[, i] <- as.factor(data[, i])
  }
}

# Remove outliers
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    data <- data[data[, i] > quantile(data[, i], 0.01) & data[, i] < quantile(data[, i], 0.99), ]
  }
}

# Remove variables with low variance
data <- data[, sapply(data, function(x) sd(x) > 0.1)]

# Rename variables
names(data) <- paste("var", 1:ncol(data), sep = "_")

# Reorder variables
data <- data[, order(names(data))]

# Save the cleaned dataset
write.csv(data, "cleaned_data.csv")

#3. 
Edist <- dist(df1, method = "euclidean")
Edist <- as.data.frame(Edist)
Ehigh <- Edist[1:nrow(df1),1]
Elow <- Edist[nrow(Edist)-nrow(df1):nrow(Edist),1]
K <- kmeans(df1, 4)