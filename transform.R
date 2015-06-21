library(caret)
library(data.table)
library(randomForest)


options(java.parameters = "-Xmx3g")
library(extraTrees)

dat <- read.csv("pml-training.csv")
nms <- names(dat)
# find and remove near zero vars
nzv <- nearZeroVar(dat, saveMetrics = TRUE)
dat.filtered <- dat[, !nzv$nzv]

#possibly, eliminate correlated groups
##highCor <- findCorrelation(dat.filtered, cutoff = 0.75)

#detecting and removing NAs
na.values <- is.na(dat.filtered)
to.remove <- colSums(na.values) > (dim(dat.filtered)[1] / 2)
dat.filtered <- dat.filtered[, !to.remove]

# removing uninformative variables
dat.filtered <- dat.filtered[, 6:59]

#printing dimensions and names
dim(dat.filtered)
names(dat.filtered)
#the outcome variable havsn't been removed by transformation.

#train the model
inTraining <- createDataPartition(dat.filtered$classe, p = 0.35, list = FALSE)
dat.training <- dat.filtered[inTraining,]
dat.testing <- dat.filtered[-inTraining,]

# train control
control <- trainControl(method = "cv")

#random forest
rfFit <- train(classe ~ .,
               data = dat.training,
               method = "rf",
               trControl = control)

#testing
testPred <- predict(rfFit, dat.testing)
postResample(testPred, dat.testing$classe)
