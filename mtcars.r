str(mtcars)
ls(mtcars)
head(mtcars)
attach(mtcars)

# cyl vs am gear carb 轉 factor
cyl = factor(cyl)
vs = factor(vs)
am = factor(am)
gear = factor(gear)
carb = factor(carb)
str(mtcars)

# 各變數間的相關係數
library("GGally")
ggcorr(data = mtcars, palette = "RdYlGn",
       label = TRUE, label_color = "black")
# 使用 ggpairs() 函數產生多個變數間關係的視覺化矩陣散佈圖
ggpairs(data = mtcars)


par(mfrow=c(2,2))
scatter.smooth(x = cyl, y = mpg,main = "mpg ~ cyl")
scatter.smooth(x = wt, y = mpg,main = "mpg ~ wt")
scatter.smooth(x = drat, y = mpg,main = "mpg ~ drat")
scatter.smooth(x = vs, y = mpg,main = "mpg ~ vs")

# 離群值檢測
par(mfrow=c(2,2))
boxplot(mpg, main="mpg", 
        sub=paste("Outlier rows: ", 
                                    paste(boxplot.stats(mpg)$out, collapse = ",")))

boxplot(wt, main="wt", 
        sub=paste("Outlier rows: ", 
                  paste(boxplot.stats(wt)$out, collapse = ",")))

boxplot(drat, main="drat", 
        sub=paste("Outlier rows: ", 
                  paste(boxplot.stats(drat)$out, collapse = ",")))


boxplot.stats(wt)$out

wtData <- mtcars
outlier <- function(x) {
  x[x > quantile(x,probs = 0.75,na.rm = T) + 1.5 * IQR(x, na.rm = T) |
      x < quantile(x,probs = 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T)] <- NA
  x
}
# apply()的第一個參數只能接受「矩陣」物件，意味著變數物件中所有的資料必須具備相同的資料型態
# 若用在其他結構的物件，如data.frame，需要先轉換成matrix資料結構
wtData <- apply(X = wtData,MARGIN = 2, FUN = outlier)
wtData<- as.data.frame(wtData)

par(mfrow=c(1,2))
boxplot(wtData$wt, main="wt", sub=paste("Outlier rows: ", 
                                              paste(boxplot.stats(wtData$wt)$out, collapse = ","))) 
scatter.smooth(x = wtData$wt, y = wtData$mpg, main = "mpg ~ wt")

# 3.2 資料標準化(Scaling/Normalization, 又稱正規化)處理
# 目的：可使後續進行變數間相關性衡量時，其分析結果不會因各變項的單位尺規大小不一致而影響
# 將所有數值型變數以MinMax標準化方法統一轉換至(0 to 1)的尺規區間
wtData2 <- apply(wtData, 2, function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))})
wtData2 <- as.data.frame(wtData2)


# 變數轉換 - 使變數接近常態分佈，以符合模型的統計假設
# 透過變數log10轉換
subwtData <- wtData2[,c(1)]
subwtData2 <- sapply(subwtData, function(x){x <- log10(x+1)})
wtData2$qsec_log10 <- as.vector(subwtData2)

# 檢視變數轉換後wt的機率密度分佈圖
plot(density(wtData2$qsec_log10, na.rm = TRUE), main="Density Plot: wt", 
     ylab="Frequency", sub=paste("Skewness:", 
                                 round(e1071::skewness(wtData2$wt_log10, na.rm = TRUE), 2)))

# 迴歸模型
car_lm <- lm(mpg ~ cyl + wt + drat + vs , data = wtData2)
summary(car_lm)

# 殘差分析
par(mfrow = c(2,2))
plot(car_lm) 

