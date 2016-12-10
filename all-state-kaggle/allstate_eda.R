
# loading required libs
library(FSelector)
library(data.table)
library(Matrix)
library(xgboost)
library(DT)
library(e1071)
# Setting up the path
setwd('D:\\Kaggle\\All State')

# reading the data
train <- read.csv('train.csv', sep = ',', header = TRUE, stringsAsFactors = FALSE)
test <- fread('test.csv', sep = ',', header = TRUE, showProgress = TRUE, stringsAsFactors = FALSE)

# Convert Categorical variables as factor variables

for(i in 1:ncol(train))
  {
  
  if (sapply(train[,i], class) == 'character')
  {
    train[,i] <- as.factor(train[,i])
  }
}

boruta.tr <- Boruta(loss ~. -id, data = train, doTrace = 2)
plot(boruta.tr, xlab = "", xaxt = "n")

getSelectedAttributes(boruta.tr)

lz <- lapply(1:ncol(boruta.tr$ImpHistory), function(i) 
  boruta.tr$ImpHistory[is.finite(boruta.tr$ImpHistory[,i]),i])

names(lz) <- colnames(boruta.tr$ImpHistory)
labels <- sort(sapply(lz, median))
axis(side = 1,las=2,labels = names(labels),
     at = 1:ncol(boruta.tr$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.tr)
print(final.boruta)


