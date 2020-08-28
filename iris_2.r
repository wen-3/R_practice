
library(datasets)
data("iris")
str(iris)

data <- iris[, -5]
head(data)

E.dist <- dist(data, method = "euclidean") # 歐式距離
M.dist <- dist(data, method = "manhattan") # 曼哈頓距離

par(mfrow = c(1,2))

h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab = "歐式距離")

h.M.cluster <- hclust(M.dist)
plot(h.M.cluster, xlab = "曼哈頓距離")

E.dist <- dist(data, method = "euclidean")
h.cluster <- hclust(E.dist, method = "ward.D2")

par(mfrow = c(1,1))
plot(h.cluster)
abline(h=9, col="red")

cut.h.cluster <- cutree(h.cluster, k=3)
cut.h.cluster

table(cut.h.cluster, iris$Species)





