library(Hmisc) 
library(stringr)
library(rvest)
library(TTR)
library(xts)
library(zoo)

#Prep
#Read in the file
stock_data <- read.table("merged_all.csv", header = TRUE, sep = ",")[, c(1, 2, 6, 11, 14, 16)]
#The first six observations in this dataset.
head(stock_data)
#Transform the format of tradedate
stock_data[,2] = gsub(pattern = "-", replacement = "", x = stock_data$TRDDT)
stock_data[,2] <- as.Date(as.character(stock_data[,2]),"%Y%m%d")
stock_data <- stock_data[order(stock_data$TRDDT),]
stock_data$NNINDCD <- as.character(stock_data$NNINDCD)
stock_data$STKCD <- as.numeric(stock_data$STKCD)
head(stock_data)
#deal with the industry type
old_type = unique(stock_data$NNINDCD)
#new_type <- character(length = nrow(stock_data))
#new_type = NULL
#stock_data = cbind(stock_data, new_type)
for (type in old_type) {
  newtype = sub(pattern = type, replacement = (substr(type,1,1)), type)
  stock_data$new_type[stock_data$NNINDCD == type] = newtype
}
head(stock_data)
dim(stock_data) 
#########################################################################
#Explore the data
#count the number of stocks in each sector
stock_type = unique(stock_data$new_type)
amount <- numeric(length = length(stock_type))
stock_type_number <- as.data.frame(cbind(stock_type, amount))
stock_type_number$amount <- as.numeric(stock_type_number$amount)
for(type in stock_type_number$stock_type){
  num <- length(unique(stock_data$STKCD[stock_data$new_type == type]))
  stock_type_number$amount[stock_type_number$stock_type == type] <- num
}
stock_type_number <- stock_type_number[order(-stock_type_number$amount),]
head(stock_type_number)
#filter the data
stock_data1 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[1])
stock_data2 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[2])
stock_data3 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[3])
stock_data4 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[4])
#insample and outofsample
stock_data1 <- subset(stock_data1, stock_data1$TRDDT < "2018-07-01")
#count the latest record, the recency, total day, monetary for each stock
stockid1 <- unique(stock_data1$STKCD)
stockname1 <- character(length = length(stockid1))
stocklatest1 <- character(length = length(stockid1))
stockrecency1 <- numeric(length = length(stockid1))
stockrecentgoup1 <- character(length = length(stockid1))
stocktotalgoup1 <- numeric(length = length(stockid1))
stocktotal1 <- numeric(length = length(stockid1))
stockmonetary1 <- numeric(length = length(stockid1))
dataset1 <- as.data.frame(cbind(stockid1, stockname1, stocklatest1, stockrecentgoup1, stocktotalgoup1, stockrecency1, stocktotal1, stockmonetary1))
dataset1$stockid1 <- as.numeric(stockid1)
dataset1$stocklatest1 <- as.Date(stocklatest1, "%Y%m%d")
dataset1$stockrecentgoup1 <- as.Date(stockrecentgoup1, "%Y%m%d")
dataset1$stockrecency1 <- as.numeric(stocktotal1)
dataset1$stocktotalgoup1 <- as.numeric(stocktotalgoup1)
dataset1$stocktotal1 <- as.numeric(stocktotal1)
dataset1$stockmonetary1 <- as.numeric(stockmonetary1)
dataset1$stockname1 <- as.character(stockname1)
for (stock in dataset1$stockid1){
  dataset1$stockname1[dataset1$stockid1 == stock] <- as.character(stock_data1$CONME_EN[stock_data1$STKCD == stock][1])
  dataset1$stocklatest1[dataset1$stockid1 == stock] <- max(stock_data1$TRDDT[stock_data1$STKCD == stock])
  dataset1$stockrecentgoup1[dataset1$stockid1 == stock] <- max(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0])
  dataset1$stocktotalgoup1[dataset1$stockid1 == stock] <- length(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0])
  dataset1$stockrecency1[dataset1$stockid1 == stock] <- as.double(Sys.Date()) - as.double(max(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0]))
  dataset1$stockmonetary1[dataset1$stockid1 == stock] <- sum(stock_data1$DRETWD[stock_data1$STKCD == stock])
  dataset1$stocktotal1[dataset1$stockid1 == stock] <- length(stock_data1$STKCD[stock_data1$STKCD == stock])
}
#filter the data
dataset1new <- subset(dataset1, dataset1$stocklatest1 > "2018-05-15" & dataset1$stocktotal1 > 100)

#calculate RFM
recency <- NULL
frequecy <- NULL
monetary <- NULL
sum <- NULL

  dataset1new$recency <- dataset1new$stockrecency1 / dataset1new$stocktotal1
  dataset1new$frequency <- dataset1new$stocktotalgoup1 / dataset1new$stocktotal1
  dataset1new$monetary <- dataset1new$stockmonetary1 / dataset1new$stocktotal1



# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:

  dataset1new$sum <- (dataset1new$monetary*100
                      + dataset1new$frequency*10
                      + dataset1new$recency)
dataset1new <- dataset1new[order(-dataset1new$sum),]
rank <- dataset1new[,c(1,2,12)]
rank <- rank[order(-rank$sum),]
head(rank)


####################################back-testing################
stock_data1 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[1])
stock_data2 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[2])
stock_data3 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[3])
stock_data4 <- subset(stock_data, stock_data$new_type == stock_type_number$stock_type[4])
stock_data1 <- subset(stock_data1, stock_data1$TRDDT > "2018-06-30")
#count the latest record, the recency, total day, monetary for each stock
stockid1 <- unique(stock_data1$STKCD)
stockname1 <- character(length = length(stockid1))
stocklatest1 <- character(length = length(stockid1))
stockrecency1 <- numeric(length = length(stockid1))
stockrecentgoup1 <- character(length = length(stockid1))
stocktotalgoup1 <- numeric(length = length(stockid1))
stocktotal1 <- numeric(length = length(stockid1))
stockmonetary1 <- numeric(length = length(stockid1))
dataset1 <- as.data.frame(cbind(stockid1, stockname1, stocklatest1, stockrecentgoup1, stocktotalgoup1, stockrecency1, stocktotal1, stockmonetary1))
dataset1$stockid1 <- as.numeric(stockid1)
dataset1$stocklatest1 <- as.Date(stocklatest1, "%Y%m%d")
dataset1$stockrecentgoup1 <- as.Date(stockrecentgoup1, "%Y%m%d")
dataset1$stockrecency1 <- as.numeric(stocktotal1)
dataset1$stocktotalgoup1 <- as.numeric(stocktotalgoup1)
dataset1$stocktotal1 <- as.numeric(stocktotal1)
dataset1$stockmonetary1 <- as.numeric(stockmonetary1)
dataset1$stockname1 <- as.character(stockname1)
for (stock in dataset1$stockid1){
  dataset1$stockname1[dataset1$stockid1 == stock] <- as.character(stock_data1$CONME_EN[stock_data1$STKCD == stock][1])
  dataset1$stocklatest1[dataset1$stockid1 == stock] <- max(stock_data1$TRDDT[stock_data1$STKCD == stock])
  dataset1$stockrecentgoup1[dataset1$stockid1 == stock] <- max(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0])
  dataset1$stocktotalgoup1[dataset1$stockid1 == stock] <- length(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0])
  dataset1$stockrecency1[dataset1$stockid1 == stock] <- as.double(Sys.Date()) - as.double(max(stock_data1$TRDDT[stock_data1$STKCD == stock & stock_data1$DRETWD > 0]))
  dataset1$stockmonetary1[dataset1$stockid1 == stock] <- sum(stock_data1$DRETWD[stock_data1$STKCD == stock])
  dataset1$stocktotal1[dataset1$stockid1 == stock] <- length(stock_data1$STKCD[stock_data1$STKCD == stock])
}
#filter the data
#dataset1new <- subset(dataset1, dataset1$stocklatest1 > "2018-07-01" & dataset1$stocktotal1 > 10)

#calculate RFM
recency <- NULL
frequecy <- NULL
monetary <- NULL
sum <- NULL
for(stock in dataset1new$stockid1){
  dataset1new$recency[dataset1new$stockid1 == stock] <- dataset1new$stockrecency1[dataset1new$stockid1 == stock] / dataset1new$stocktotal1[dataset1new$stockid1 == stock]
  dataset1new$frequency[dataset1new$stockid1 == stock] <- dataset1new$stocktotalgoup1[dataset1new$stockid1 == stock] / dataset1new$stocktotal1[dataset1new$stockid1 == stock]
  dataset1new$monetary[dataset1new$stockid1 == stock] <- dataset1new$stockmonetary1[dataset1new$stockid1 == stock] / dataset1new$stocktotal1[dataset1new$stockid1 == stock]
  dataset1new$sum[dataset1new$stockid1 == stock] <- dataset1new$recency[dataset1new$stockid1 == stock]+10* dataset1new$frequency[dataset1new$stockid1 == stock] + 100* dataset1new$monetary[dataset1new$stockid1 == stock]
}
dataset1new <- dataset1new[order(-dataset1new$sum),]
dataset1new <- subset(dataset1new, dataset1new$stocklatest1 > "2018-05-15" & dataset1$stocktotal1 > 100)

top20 <- dataset1new[c(1:20),]
top20id <- top20$stockid1
dataset1new <- dataset1new[order(dataset1new$sum),]
low20 <- dataset1new[c(1:20),]
top20record <- dataset1
for stockid in top20$stockid1{
  ggplot(top20, aes(x = Time, y = demand)) +
    # 折线图函数
    geom_line()
}

max <- max(dataset1new$stockmonetary1)
min <- min(dataset1new$stockmonetary1)
mean <- mean(dataset1new$stockmonetary1)
median <- median(dataset1new$stockmonetary1)
quantile_dataset <- quantile(dataset1new$stockmonetary1,probs=seq(0,1,0.25))
hist(dataset1new$stockmonetary1,main = "monetary",xlab="monetary",breaks = 400)
variance <- var(dataset1new$stockmonetary1)
table <- rbind(c("max", "min", "mean", "median", "variance"), c(max, min, mean, median, variance))
dataset1new$sum <- 100000 * dataset1new$sum
head(dataset1new)

########################Logistic Regression####################################
#MA & MACD
MA <- SMA(stock_data$CLSPRC[stock_data$STKCD == 4], 5)
MACD <- MACD(stock_data1$DRETWD[stock_data1$STKCD == 4], 12, 26, 9, maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)))
#DJK
stock_data <- read.table("merged_all.csv", header = TRUE, sep = ",", stringsAsFactors = F)[, c(2, 1, 3, 4, 5, 6, 14)]
#The first six observations in this dataset.
head(stock_data)
str(stock_data)

stock_data$TRDDT =as.Date(stock_data$TRDDT)

summary(stock_data)

stock_xts <- xts(stock_data, order.by = stock_data$TRDDT)





## 数据表KDJ用最终的数据

l_temp <- length(stock_data$TRDDT[stock_data$STKCD ==4])         # 数据长度
KDJ <- matrix(NA, l_temp, 3)      # 构建存放数据的矩阵
KDJ <- as.data.frame(KDJ)         # 转换为data.frame
colnames(KDJ) <- c('K', 'D', 'J') # 1-3列的名称分别命名为K、D、J
KDJ[1:8, ]  <- 50                 # 前8天的K、D、J均设为50
## 计算rvs
# 计算9日内最高价
high_9 <- lag.xts(xts(stock_xts$HIPRC), k = 0:8)



high_max <- apply(high_9, MARGIN = 1, FUN = max)
# 计算9日内最低价
low_9 <- lag.xts(stock_xts$LOPRC, k = c(0:8))
low_min <- apply(low_9, MARGIN = 1, FUN = min)
# rsv
rsv <- (stock_xts$CLSPRC - low_min) / (high_max - low_min) * 100
## 计算KDJxts
for (i in 9:l_temp) {
  # 计算K值
  KDJ[i, 1] <- 2/3 * KDJ[(i - 1), 1] + 1/3 * rsv[i, ]
  # 计算D值
  KDJ[i, 2] <- 2/3 * KDJ[(i - 1), 2] + 1/3 * KDJ[i, 1]
  # 计算J值
  KDJ[i, 3] <- 3 * KDJ[i, 1] - 2 * KDJ[i, 2]
}
## 将KDJ转化为xts格式
KDJ <- as.xts(KDJ, order.by = index(rsv))
#mean
stock_dataNew <- cbind(stock_data, (stock_data$CLSPRC + stock_data$HIPRC + stock_data$OPNPRC + stock_data$LOPRC)/4)
#model
glm()
