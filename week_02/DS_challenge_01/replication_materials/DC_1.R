setwd("/Users/Gul/Desktop/Data_Challenge_1")

ci_names_S <- scan("adult.names", skip = 96, sep = ':', what = list(''))
ci_names_S <- unlist(ci_names_S)
ci_names_S <- ci_names_S[c(TRUE, FALSE)]
ci_names_S <- c(ci_names_S, 'annual-income')
summary(ci_names_S)

cis_df <- read.table("adult.data", header = FALSE, sep=",",
                     col.names = ci_names_S)


cis_df_test <- read.table("adult.test", skip = 1, header = FALSE, sep=",",
                          col.names = ci_names_S)

levels(cis_df_test$annual.income)[levels(cis_df_test$annual.income)==" <=50K."] <- " <=50K"
levels(cis_df_test$annual.income)[levels(cis_df_test$annual.income)==" >50K."] <- " >50K"
prop_split <- as.numeric(dim(cis_df)[1]/(dim(cis_df)[1]+dim(cis_df_test)[1]))
dataset <- rbind(cis_df, cis_df_test)
library(plyr); library(dplyr); library(psych); library(caret); library(car); library(C50); library(e1071); 
library(caTools); library(ggplot2)
library(randomForest); library(ROCR); library(MASS)
library(rpart); library(MASS); library(GoodmanKruskal); library(corrplot); library(caretEnsemble)
#devtools::install_github("collectivemedia/tictoc")
#devtools::install_github("easyGgplot2", "kassambara")
library(tictoc); library(easyGgplot2); library(ggthemes); library(PRROC); library(pROC); library(randomcoloR)
set.seed(7)
test.index <- createDataPartition(dataset$annual.income, p=0.7, list=FALSE)
testdf.unprocessed <- dataset[-test.index,]
traindf.unprocessed <- dataset[test.index,]
head(traindf.unprocessed)
dim(traindf.unprocessed)
str(traindf.unprocessed)
percentage_cis <- prop.table(table(traindf.unprocessed$annual.income)) * 100
cbind(freq=table(traindf.unprocessed$annual.income), percentage=percentage_cis)
sapply(traindf.unprocessed, class)
sapply(traindf.unprocessed, levels)
summary(traindf.unprocessed, maxsum = 42) 

ggplot(traindf.unprocessed, aes(annual.income, fill = sex)) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("annual.income") +
  ylab("count") +
  scale_fill_discrete(name = "sex") + 
  ggtitle("Gender and Income")

ggplot(traindf.unprocessed, aes(annual.income, fill = race)) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("annual.income") +
  ylab("Count") +
  scale_fill_discrete(name = "race") + 
  ggtitle("Race and Income")
prop.table(table(traindf.unprocessed$annual.income[traindf.unprocessed$race==' Black']))
prop.table(table(traindf.unprocessed$annual.income[traindf.unprocessed$race==' White']))
prop.table(table(traindf.unprocessed$annual.income[traindf.unprocessed$race==' Other']))
prop.table(table(traindf.unprocessed$annual.income[traindf.unprocessed$sex==' Male']))
prop.table(table(traindf.unprocessed$annual.income[traindf.unprocessed$sex==' Female']))

par(mfrow=c(2,2))
par(las=2)
for (i in 1:length(traindf.unprocessed)) {
  if (class(traindf.unprocessed[,i])=="factor") {
    plot(traindf.unprocessed[,i], main=colnames(traindf.unprocessed)[i], cex.names=0.65, horiz=TRUE)
  }
}

par(mfrow=c(2,3))
par(cex=0.7, mai=c(0.3,0.8,0.5,1))
for (i in 1:length(traindf.unprocessed)) {
  if (class(traindf.unprocessed[,i])=="integer" | class(traindf.unprocessed[,i])=="numeric" ) {
    boxplot(traindf.unprocessed[,i]~traindf.unprocessed[,15], main=colnames(traindf.unprocessed)[i])
  }
}

par(mfrow=c(2,1))
par(cex=0.7, mai=c(0.3,0.8,0.5,1))
for (i in 1:length(traindf.unprocessed)) {
  if (class(traindf.unprocessed[,i])=="integer" | class(traindf.unprocessed[,i])=="numeric" ) {
    hist(traindf.unprocessed[,i], breaks=12, col="red", main=colnames(traindf.unprocessed)[i])
    plot(density(traindf.unprocessed[,i]), main=colnames(traindf.unprocessed)[i])
  }
}

pairs.panels(traindf.unprocessed[,c(1,3,5,11:13)], gap=0, bg=c('red', 'blue')[traindf.unprocessed$annual.income], pch=2)


qqnorm(traindf.unprocessed$age); qqline(traindf.unprocessed$age)

library(nortest)
ad.test(traindf.unprocessed$age)
cvm.test(traindf.unprocessed$age)

par(mfrow=c(1,2))
par(cex=0.7, mai=c(0.3,0.8,0.5,1))
boxplot(traindf.unprocessed$capital.gain[traindf.unprocessed$capital.gain>0]~traindf.unprocessed$annual.income[traindf.unprocessed$capital.gain>0])

library(Hmisc)
describeBy(traindf.unprocessed$capital.gain, group = traindf.unprocessed$annual.income, mat = TRUE )
summary(traindf.unprocessed$annual.income)
summary(traindf.unprocessed$annual.income[traindf.unprocessed$capital.gain>0])
summary(traindf.unprocessed$annual.income[traindf.unprocessed$capital.gain==0])
describe(traindf.unprocessed$annual.income[traindf.unprocessed$capital.loss>0])
describe(traindf.unprocessed$annual.income[traindf.unprocessed$capital.loss==0])

others <- levels(dataset[,14])[-40]
dataset[,14] <- recode(dataset[,14], "c(others)='Others'")
others_n <- levels(dataset$race)[1:2]
dataset$race <- recode(dataset$race, "c(others_n)=' Other'")
others_e <- levels(dataset$education)[c(1:7,14)]
dataset$education <- recode(dataset$education, "c(others_e)='Dropout'")
others_e2 <- levels(dataset$education)[c(1:2)]
dataset$education <- recode(dataset$education, "c(others_e2)='Associates'")
others_e3 <- levels(dataset$education)[c(3,6)]
dataset$education <- recode(dataset$education, "c(others_e3)='HSGrad'")
others_e3 <- levels(dataset$education)[c(2,4)]
dataset$education <- recode(dataset$education, "c(others_e3)='PhD'")
others_wc <- levels(dataset$workclass)[c(6,7)]
dataset$workclass <- recode(dataset$workclass, "c(others_wc)='Self-employed'")
others_wc1 <- levels(dataset$workclass)[c(4,7)]
dataset$workclass <- recode(dataset$workclass, "c(others_wc1)='Not-working'")
others_wc2 <- levels(dataset$workclass)[c(3,5)]
dataset$workclass <- recode(dataset$workclass, "c(others_wc2)='Non-federal-gov'")
others_o <- levels(dataset$occupation)[c(4,6,7,8,15)]
dataset$occupation <- recode(dataset$occupation, "c(others_o)='Blue-collar'")
others_o1 <- levels(dataset$occupation)[c(5,6)]
dataset$occupation <- recode(dataset$occupation, "c(others_o1)='Service'")
others_o2 <- levels(dataset$occupation)[c(6,8)]
dataset$occupation <- recode(dataset$occupation, "c(others_o2)='Other-occ'")
others_ms <- levels(dataset$marital.status)[c(4,6)]
dataset$marital.status <- recode(dataset$marital.status, "c(others_ms)='Not-w-spouse'")
others_ms1 <- levels(dataset$marital.status)[c(2,3)]
dataset$marital.status <- recode(dataset$marital.status, "c(others_ms1)='Married'")
levels(dataset$annual.income) <- c("less", "more")

test.index
testdf <- dataset[-test.index,]
traindf <- dataset[test.index,]

table(traindf$marital.status, traindf$relationship)
chisq.test(traindf$marital.status, traindf$relationship, correct=FALSE)
subset <- c(2,4,6:10,14,15)
GKmatrix <- GKtauDataframe(traindf[,subset])
plot(GKmatrix)
GKtau(traindf$marital.status, traindf$relationship)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
cor <- cor(traindf[,c(1,3,5,11,12,13)])
p.mat <- cor.mtest(traindf[,c(1,3,5,11,12,13)])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

table(traindf$annual.income)

percentage_cis_resampled <- prop.table(table(traindf.resampled$annual.income)) * 100
cbind(freq=table(traindf.resampled$annual.income), percentage=percentage_cis_resampled)

library(ROSE)
traindf.resampled.over <- ovun.sample(annual.income ~ ., data = traindf, method = "over",N = 52018, seed=7)$data
table(traindf.resampled.over$annual.income)
traindf.resampled.under <- ovun.sample(annual.income ~ ., data = traindf, method = "under",N = 16362, seed=7)$data
table(traindf.resampled.under$annual.income)
traindf.resampled.both <- ovun.sample(annual.income ~ ., data = traindf, method = "both", p=0.5, N = 34190, seed=7)$data
table(traindf.resampled.both$annual.income)

library(caret)
stats <- function (data, lev = NULL, model = NULL)  {
  c(postResample(data[, "pred"], data[, "obs"]),
    Sens = sensitivity(data[, "pred"], data[, "obs"]),
    Spec = specificity(data[, "pred"], data[, "obs"]))
}
control <- trainControl(method="cv", number=10, summaryFunction = stats, classProbs = TRUE)

metric <- "Kappa" 

set.seed(7)
fit.logit <- train(annual.income~.-fnlwgt, data=traindf, method="glm", family="binomial", 
                   metric=metric, trControl=control)

set.seed(7)
fit.logit5 <- train(annual.income~.-fnlwgt, data=traindf, method="glm", family="binomial",
                    preProcess = c('BoxCox'),
                    metric=metric, trControl=control)


set.seed(7)
fit.logit6 <- train(annual.income~.-fnlwgt, data=traindf.resampled.both, method="glm", family="binomial",
                    preProcess = c('BoxCox'),
                    metric=metric, trControl=control)

set.seed(7)
fit.logit7 <- train(annual.income~.-fnlwgt, data=traindf.resampled.over, method="glm", family="binomial",
                    preProcess = c('BoxCox'),
                    metric=metric, trControl=control)
set.seed(7)
fit.logit8 <- train(annual.income~.-fnlwgt, data=traindf.resampled.under, method="glm", family="binomial",
                    preProcess = c('BoxCox'),
                    metric=metric, trControl=control)
results.logit <- resamples(list(logit=fit.logit, logit5=fit.logit5, logit6=fit.logit6, logit7=fit.logit7, logit8=fit.logit8))
summary(results.logit)
dotplot(results.logit, metric='Kappa')

predictions.logit5 <- predict(fit.logit5, newdata = testdf)
predictions.logit5.prob <- predict(fit.logit5, newdata = testdf, type='prob')$less
predictions.logit6 <- predict(fit.logit6, newdata = testdf)
predictions.logit6.prob <- predict(fit.logit6, newdata = testdf, type='prob')$less
roc(testdf$annual.income, predictions.logit6.prob)
predictions.logit7 <- predict(fit.logit7, newdata = testdf)
predictions.logit7.prob <- predict(fit.logit7, newdata = testdf, type='prob')$less
roc(testdf$annual.income, predictions.logit7.prob)

predictions.logit8 <- predict(fit.logit8, newdata = testdf)
confusionMatrix(predictions.logit5, testdf$annual.income)
confusionMatrix(predictions.logit6, testdf$annual.income)
confusionMatrix(predictions.logit7, testdf$annual.income)
confusionMatrix(predictions.logit8, testdf$annual.income)

set.seed(7)
fit.lda <- train(annual.income~.-fnlwgt, data=traindf, method='lda', preProcess = c('scale', 'center'),
                 metric=metric, trControl=control)
set.seed(7)
fit.lda1 <- train(annual.income~.-fnlwgt, data=traindf, method='lda', preProcess = 'BoxCox',
                  metric=metric, trControl=control)
set.seed(7)
fit.lda2 <- train(annual.income~.-fnlwgt, data=traindf.resampled.over, method='lda', preProcess = 'BoxCox',
                  metric=metric, trControl=control)

set.seed(7)
fit.lda3 <- train(annual.income~.-fnlwgt, data=traindf.resampled.under, method='lda', preProcess = 'BoxCox',
                  metric=metric, trControl=control)

set.seed(7)
fit.lda4 <- train(annual.income~.-fnlwgt, data=traindf.resampled.both, method='lda', preProcess = 'BoxCox',
                  metric=metric, trControl=control)

results.lda <- resamples(list(lda=fit.lda, lda1=fit.lda1, lda2=fit.lda2, lda3=fit.lda3, lda4=fit.lda4))
summary(results.lda)
dotplot(results.lda, metric=metric)

predictions.lda <- predict(fit.lda, newdata = testdf)
predictions.lda1 <- predict(fit.lda1, newdata = testdf)
predictions.lda1.prob <- predict(fit.lda1, newdata = testdf, type='prob')$less
predictions.lda2 <- predict(fit.lda2, newdata = testdf)
predictions.lda3 <- predict(fit.lda3, newdata = testdf)
predictions.lda4 <- predict(fit.lda4, newdata = testdf)
confusionMatrix(predictions.lda, testdf$annual.income)
confusionMatrix(predictions.lda1, testdf$annual.income)
confusionMatrix(predictions.lda2, testdf$annual.income)
confusionMatrix(predictions.lda3, testdf$annual.income)
confusionMatrix(predictions.lda4, testdf$annual.income)

tic('fit.cart')
set.seed(7)
fit.cart <- train(annual.income~.-fnlwgt, data=traindf, method="rpart", 
                  parms = list(split = "information"), #or 'information'
                  metric=metric, trControl=control, tuneLength = 10)
toc()



tic('fit.cart1')
set.seed(7)
fit.cart1 <- train(annual.income~.-fnlwgt, data=traindf.resampled.over, method="rpart", 
                   parms = list(split = "information"), #or 'information'
                   metric=metric, trControl=control, tuneLength = 10)
toc()

tic('fit.cart2')
set.seed(7)
fit.cart2 <- train(annual.income~.-fnlwgt, data=traindf.resampled.under, method="rpart", 
                   parms = list(split = "information"), #or 'information'
                   metric=metric, trControl=control, tuneLength = 10)
toc()

tic('fit.cart3')
set.seed(7)
fit.cart3 <- train(annual.income~.-fnlwgt, data=traindf.resampled.both, method="rpart", 
                   parms = list(split = "information"), #or 'information'
                   metric=metric, trControl=control, tuneLength = 10)
toc()


results.cart <- resamples(list(cart=fit.cart, cart1=fit.cart1, cart2=fit.cart2, cart3=fit.cart3 ))
summary(results.cart)
dotplot(results.cart, metric=metric)
library(rattle)
fancyRpartPlot(fit.cart$finalModel)

predictions.cart <- predict(fit.cart, newdata = testdf)
predictions.cart.prob <- predict(fit.cart, newdata = testdf, type='prob')$less
predictions.cart1 <- predict(fit.cart1, newdata = testdf)
predictions.cart2 <- predict(fit.cart2, newdata = testdf)
predictions.cart3 <- predict(fit.cart3, newdata = testdf)
confusionMatrix(predictions.cart, testdf$annual.income)
confusionMatrix(predictions.cart1, testdf$annual.income)
confusionMatrix(predictions.cart2, testdf$annual.income)
confusionMatrix(predictions.cart3, testdf$annual.income)

grid.c50 <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(5,10,15,20,25), .model="tree" )

set.seed(7)
fit.C5.0 <- train(annual.income~.-fnlwgt, data=traindf,
                  method="C5.0", tuneGrid=grid.c50, metric=metric, trControl=control, verbose = FALSE)

set.seed(7)
fit.C5.01 <- train(annual.income~.-fnlwgt, data=traindf.resampled.over,
                   method="C5.0", tuneGrid=grid.c50, metric=metric, trControl=control, verbose = FALSE)

set.seed(7)
fit.C5.02 <- train(annual.income~.-fnlwgt, data=traindf.resampled.under,
                   method="C5.0", tuneGrid=grid.c50, metric=metric, trControl=control, verbose = FALSE)

set.seed(7)
fit.C5.03 <- train(annual.income~.-fnlwgt, data=traindf.resampled.both,
                   method="C5.0", tuneGrid=grid.c50, metric=metric, trControl=control, verbose = FALSE)

results.c50 <- resamples(list(c5.0=fit.C5.0, C5.01=fit.C5.01, C5.02=fit.C5.02, C5.03=fit.C5.03))
summary(results.c50)
dotplot(results.c50, metric=metric)
summary(fit.C5.0$finalModel)
predictions.C50 <- predict(fit.C5.0, newdata = testdf)
predictions.C50.prob <- predict(fit.C5.0, newdata = testdf, type='prob')$less
predictions.C501 <- predict(fit.C5.01, newdata = testdf)
predictions.C502 <- predict(fit.C5.02, newdata = testdf)
predictions.C503 <- predict(fit.C5.03, newdata = testdf)
confusionMatrix(predictions.C50, testdf$annual.income)
confusionMatrix(predictions.C501, testdf$annual.income)
confusionMatrix(predictions.C502, testdf$annual.income)
confusionMatrix(predictions.C503, testdf$annual.income)
results.all <- resamples(list(logit=fit.logit5, lda=fit.lda1, cart=fit.cart, C5.0=fit.C5.0)) 
summary(results.all)
dotplot(results.all)
dotplot(results.all, metric=metric)
print(fit.C5.0)

modelCor(results.all)
trainControl <- trainControl(method="cv", number=10,
                             savePredictions=TRUE, classProbs=TRUE)

tic('model_list_big2')
model_list_big2 <- caretList(
  annual.income~.-fnlwgt, data=traindf,
  trControl=trainControl,
  tuneList=list(
    cart<-caretModelSpec(method = 'rpart', parms = list(split = "information"), tuneLength=10),
    lda<-caretModelSpec(method="lda", preProcess= 'BoxCox'),
    c50<-caretModelSpec(method="C5.0", tuneGrid=data.frame(.trials = 15, .model='tree', .winnow=FALSE))
  )
)
toc()
results.stack <- resamples(model_list_big2)

summary(results.stack)
dotplot(results.stack)


modelCor(results.stack)
splom(results.stack)

stackControl <- trainControl(method="repeatedcv", number=10, repeats=3,
                             savePredictions=TRUE, classProbs=TRUE)

set.seed(7)
fit.stack5 <- caretStack(model_list_big2, method="glm", metric=metric, trControl=stackControl)
set.seed(7)
fit.stack6 <- caretStack(model_list_big2, method="rpart", metric=metric, trControl=stackControl)


predictions.stack.glm5 <- predict(fit.stack5, newdata = testdf)
predictions.stack.glm5.prob <- predict(fit.stack5, newdata = testdf, type='prob')
predictions.stack.rpart6 <- predict(fit.stack6, newdata = testdf)
predictions.stack.rpart6.probs <- predict(fit.stack6, newdata = testdf, type='prob')


confusionMatrix(predictions.stack.glm5, testdf$annual.income)
confusionMatrix(predictions.stack.rpart6, testdf$annual.income)

colors <- randomColor(count = 10, hue = c("random"), luminosity = c("dark"))
roc1 <- roc(testdf$annual.income, predictions.stack.glm5.prob, col=colors[1], percent=TRUE, asp = NA,
            plot=TRUE, print.auc=TRUE, grid=TRUE, main="ROC comparison", print.auc.x=70, print.auc.y=80)
roc2 <- roc(testdf$annual.income, predictions.stack.rpart6.probs, plot=TRUE, add=TRUE, 
            percent=roc1$percent, col=colors[2], print.auc=TRUE, print.auc.x=70, print.auc.y=70)
roc3 <- roc(testdf$annual.income, predictions.C50.prob, plot=TRUE, add=TRUE, 
            percent=roc1$percent, col=colors[3], print.auc=TRUE, print.auc.x=70, print.auc.y=60)
roc4 <- roc(testdf$annual.income, predictions.cart.prob, plot=TRUE, add=TRUE, 
            percent=roc1$percent, col=colors[4], print.auc=TRUE, print.auc.x=70, print.auc.y=50)
roc5 <- roc(testdf$annual.income, predictions.lda1.prob, plot=TRUE, add=TRUE, 
            percent=roc1$percent, col=colors[5], print.auc=TRUE, print.auc.x=70, print.auc.y=40)
roc6 <- roc(testdf$annual.income, predictions.logit5.prob, plot=TRUE, add=TRUE, 
            percent=roc1$percent, col=colors[6], print.auc=TRUE, print.auc.x=70, print.auc.y=30)
legend("bottomright", legend=c("stack.glm", "stack.rpart", "C5.0", "CART", "LDA", "logistic"), col=c(colors[1:6]), lwd=2)

