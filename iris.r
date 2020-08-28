# Workshop - Hands-on practice of ANN Classifier
# 問題定義：基於倒傳遞ANN演算法對鳶尾花建立品種分類預測模型
# 資料集：R內建資料集-iris

# Load libraries
require(neuralnet)
require(nnet)
require(caret)

# import & Inspect the default dataset
library(datasets)
data <- iris
str(iris)


head(class.ind(data$Species))
data <- cbind(data,class.ind(data$Species))
head(data)
View(data)

formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
bpn <-  neuralnet(formula = formula.bpn,
                  data = data,
                  hidden = c(2),
                  learningrate = 0.01,
                  threshold = 0.01,
                  stepmax = 5e5
)

plot(bpn)

smp.size <- floor(0.8 * nrow(data))

set.seed(131)

train.ind <-  sample(seq_len(nrow(data)),smp.size)

train <- data[train.ind, ]
test <-  data[-train.ind, ]

pred <-  compute(bpn, test[,1:4])

pred$net.result

model <- train(form = formula.bpn,
               data = train,
               method = "neuralnet",
               tuneGrid = expand.grid(.layer1 = c(1:4),
                                      .layer2 = c(0:4),
                                      .layer3 = c(0)),
               learningrate = 0.01,
               threshold = 0.01,
               stepmax = 5e5
)

model

plot(model)

bpn <-  neuralnet(formula = formula.bpn,
                  data = train,
                  hidden = c(1,2),
                  learningrate = 0.01,
                  threshold = 0.01,
                  stepmax = 5e5
)

plot(bpn)

pred.result <- round(pred$net.result)
pred.result

pred.result <- as.data.frame(pred.result)

pred.result$Species <- ""

for(i in 1:nrow(pred.result)){
  if(pred.result[i, 1]==1){ pred.result[i, " Species"] <- "setosa"}
  if(pred.result[i, 2]==1){ pred.result[i, " Species"] <- "versicolor"}
  if(pred.result[i, 3]==1){ pred.result[i, " Species"] <- "virginica"}
}

pred.result

table( real = test$Species, predict = pred.result$Species)
