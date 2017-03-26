
train<-read.csv("microdata_processed/train_train.csv", header=T)
test<-read.csv("microdata_processed/train_test_set.csv", header=T)

train= train[,-c(1:3)]
test= test[,-c(1:3)]

train.cip.naic = train[,-c(113:122, 156:167)]
test.cip.naic = test[,-c(113:122, 156:167)]
train.mfs.naic = train[,-c(60:71, 156:167)]
test.mfs.naic = test[,-c(60:71, 156:167)]
train.cip.nochrd = train[, -c(113:122, 136:155)]
test.cip.nochrd = test[, -c(113:122, 136:155)]
train.mfs.nochrd = train[,-c(60:71, 136:155)]
test.mfs.nochrd = test[, - c(60:71, 136:155)]

library(C50)
#full
fit.c50<-C5.0(morethan60kyr~., data = train, trials = 10)
predict.c50<-predict(fit.c50, test)
table(predict.c50, test[,4])
#> (2064+1719)/5000
#[1] 0.7566

#cip/naic
fit2.c50<-C5.0(morethan60kyr~., data = train.cip.naic, trials = 10)
predict2.c50<-predict(fit2.c50, test.cip.naic)
table(predict2.c50, test.cip.naic[,4])
#> (2065+1693)/5000
#[1] 0.7516

#cip/nochrd
fit3.c50<-C5.0(morethan60kyr~., data = train.cip.nochrd, trials = 10)
predict3.c50<-predict(fit3.c50, test.cip.nochrd)
table(predict3.c50, test.cip.nochrd[,4])
#> (2037+1705)/5000
#[1] 0.7484

#mfs/naic
fit4.c50<-C5.0(morethan60kyr~., data = train.mfs.naic, trials = 10)
predict4.c50<-predict(fit4.c50, test.mfs.naic)
table(predict4.c50, test.mfs.naic[,4])
#> (2077+1664)/5000
#[1] 0.7482


#mfs/nochrd
fit5.c50<-C5.0(morethan60kyr~., data = train.mfs.nochrd, trials = 10)
predict5.c50<-predict(fit5.c50, test.mfs.nochrd)
table(predict5.c50, test.mfs.nochrd[,4])
#> (2055+1690)/5000
#[1] 0.749


library(nnet)
fit.nnet<-nnet(morethan60kyr~., train.reduced, size = 5,maxit=10000,decay=.001)
pred.nnet<-predict(fit.nnet, newdata = test.reduced, type = "class")
table(test.reduced$morethan60kyr, pred.nnet)
#> (2000+1709)/5000
#[1] 0.7418