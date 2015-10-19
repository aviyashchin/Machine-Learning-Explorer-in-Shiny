idata <- titanic3

sum(is.na(idata))
as.matrix(idata)==NULL

test <- preProcess(idata,method = c("center"),thresh = 0.95,
           pcaComp = NULL,
           na.remove = TRUE,
           k = 5,
           knnSummary = mean,outcome = NULL)

result<-predict(test,idata)

sum(is.na(result))

#preProc  <- preProcess(bbbDescr[1:100,-3])
#training <- predict(preProc, bbbDescr[1:100,-3])
#test     <- predict(preProc, bbbDescr[101:208,-3])
