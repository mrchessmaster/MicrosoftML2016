train<-read.csv("microdata_processed/train_train.csv", header=T)
test<-read.csv("microdata_processed/train_test_set.csv", header=T)
validation<-read.csv("microdata_processed/train_validation.csv", header = T)
combined = rbind(train, test)

combined = combined[, -c(1:2)]
validation = validation[,-c(1:2)]
combined <- rbind(combined,validation)


dat_size = dim(combined)[1]
richforeachhrs <- c()
index <- c()
for (i in unique(combined$hrswrk)) {
  index[i] = i
  richforeachhrs[i] = sum(subset(combined,hrswrk == i)$morethan60kyr)/dim(subset(combined,hrswrk == i))[1]
}
want = cbind(index,richforeachhrs)
plot(want[,2] ~ want[,1], xlab = "Hours worked", ylab = "Proportion earning more than 60k")
abline(lm(want[,2]~want[,1]),col = "red")


