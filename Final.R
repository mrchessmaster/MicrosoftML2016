#Final

train<-read.csv("microdata_processed/train_train.csv", header=T)
test<-read.csv("microdata_processed/train_test_set.csv", header=T)
validation<-read.csv("microdata_processed/train_validation.csv", header = T)


combined = rbind(train, test)

combined = combined[, -c(1:2)]
validation = validation[,-c(1:2)]


levels <- unique(c(combined[[5]]))
combined[[5]] <- factor(combined[[5]],labels=make.names(levels))
validation[[5]] <- factor(validation[[5]],labels=make.names(levels))

library(C50)
#full
fit.c50<-C5.0(morethan60kyr~., data = combined, trials = 100)
predict.c50<-predict(fit.c50, validation)
table(predict.c50, validation[,5])
#for 20 trials:
(1970+1757)/5000
#for 20 trials with CASEID:
(1996+1746)/5000
#for 50 trials:
(2015+1752)/5000
#for 50 trials with CASEID:
(2030+1769)/5000
#for 100 trials:
(2038+1748)/5000
#for 100 trials with CASEID:
(2032+1764)/5000


# start the actual thing
final_prediction_test0 = read.csv("microdata_processed/test.csv", header=T)
final_prediction_test = final_prediction_test0[, -c(1:2)]
final_prediction_test$morethan60kyr = 1
final_prediction_test$morethan60kyr = as.factor(final_prediction_test$morethan60kyr)
final_prediction_test$cma_Halifax = 0
final_prediction_test$cma_Hamilton = 0

full_train = rbind(combined, validation)

fit_final = C5.0(morethan60kyr~., data = full_train, trials = 100)

zz <- file("microdata_processed/final_100.txt", open = "wt")
sink(zz)
summary(fit_final)
sink()

predict_final <- predict(fit_final, final_prediction_test)
predict_final <- as.numeric(predict_final)-1
predict_final <- as.logical(predict_final)

CASEID = final_prediction_test0$CASEID
morethan60kyr = predict_final

write.csv(data.frame(CASEID, morethan60kyr), file = "microdata_processed/final_100.csv", row.names = F)
morethan60_prev <- read.csv("microdata_processed/final_just_2_columns.csv")[,2]

sum(!(morethan60_prev == morethan60kyr))





#  actual thing WITH CASEID
train<-read.csv("microdata_processed/train_train.csv", header=T)
test<-read.csv("microdata_processed/train_test_set.csv", header=T)
validation<-read.csv("microdata_processed/train_validation.csv", header = T)

combined = rbind(train, test)
combined = combined[, -c(1)]
validation = validation[,-c(1)]

levels <- unique(c(combined[[6]]))
combined[[6]] <- factor(combined[[6]],labels=make.names(levels))
validation[[6]] <- factor(validation[[6]],labels=make.names(levels))

fit.c50<-C5.0(morethan60kyr~., data = combined, trials = 100)
predict.c50<-predict(fit.c50, validation)
table(predict.c50, validation[,6])





# START HERE IF PREVIOUS ALREADY RAN -- REMEMBER TO CHECK C.50 FIT
final_prediction_test0 = read.csv("microdata_processed/test.csv", header=T)
final_prediction_test_wCASEID = final_prediction_test0[, -c(1)]
final_prediction_test_wCASEID$morethan60kyr = 1
final_prediction_test_wCASEID$morethan60kyr = as.factor(final_prediction_test_wCASEID$morethan60kyr)
final_prediction_test_wCASEID$cma_Halifax = 0
final_prediction_test_wCASEID$cma_Hamilton = 0

full_train_wCASEID = rbind(combined, validation)

fit_final_wCASEID = C5.0(morethan60kyr~., data = full_train_wCASEID, trials = 100)

zz <- file("microdata_processed/final_100_wCASEID.txt", open = "wt")
sink(zz)
summary(full_train_wCASEID)
sink()

predict_final_wCASEID <- predict(fit_final_wCASEID, final_prediction_test_wCASEID)
predict_final_wCASEID <- as.numeric(predict_final_wCASEID)-1
predict_final_wCASEID <- as.logical(predict_final_wCASEID)

CASEID = final_prediction_test0$CASEID
morethan60kyr_wCASEID = predict_final_wCASEID

write.csv(data.frame(CASEID, morethan60kyr_wCASEID), file = "microdata_processed/final_100_wCASEID.csv", row.names = F)
morethan60_prev_noCASEID <- read.csv("microdata_processed/final_100.csv")[,2]

sum(!(morethan60kyr_wCASEID == morethan60_prev_noCASEID))
