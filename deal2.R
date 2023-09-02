rm(list = ls())
library(lattice)
library(h2o)
h2o.init(nthreads = -1)
h2o.removeAll()
dat1 <- read.csv('./R/2021年样带数据.csv')
dat <-dat1
dat <-aggregate(dat,by=list(dat$Site), FUN = mean)
dat1 <- dat1[c(-1,-2,-10,-11,-13,-14)]
da.hex <- as.h2o(dat1, destination_frame = 'da.hex')
split = h2o.splitFrame(da.hex,ratios = 0.8,seed = 123)
train <- split[[1]]
test <- split[[2]]
rf <- h2o.randomForest(x = c('LON','LAT'),y = c('PL_total'),
                       training_frame = train,model_id = 'our.rf',
                       seed = 1234)
x <- seq(96,102,2)
y <- seq(20,36,2)
b <- cbind(LON=rep(x,each= length(y)),LAT=rep(y,length(x)))
b
db. <- as.data.frame(b,col.names = c('LON','LAT'))
db.
db.hex <- as.h2o(db., destination_frame = 'db.hex')
prediction <- h2o.predict(rf,db.hex)
print(prediction)
array(prediction)
db.['Pre'] <- array(prediction)
db.
pre <- as.matrix(prediction)
pre<-matrix(data = prediction,ncol = length(y),byrow = TRUE)
rownames(pre) <- x
colnames(pre) <- y
pre
contour(round(pre),method = "edge")
contour(x=x,y=y,round(pre),method = "flattest", vfont = c("sans serif", "plain"))

table(round(pre))

levelplot(pre~rep(x,each= length(y))*rep(y,length(x)))
levelpl
dat <- dat[c(1,2,23,24)]
dat <-aggregate(dat,by=list(dat$Site), FUN = mean)
dat <-aggregate(dat,by=list(dat$Site), FUN = mean)
lon <- dat$LON
range(lon)
lat <- dat$LAT
range(lat)
# plot(lon,lat)
# points(lon,lat)
# levelplot(pre~rep(x,each= length(y))*rep(y,length(x)))
