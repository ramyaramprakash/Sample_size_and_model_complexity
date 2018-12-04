data=read.csv("C:\\Users\\Ramya Ramprakash\\Python Praxis\\Machine Learning\\cars.csv")

data$MPG[data$MPG==0]<- NA
new=na.omit(data)

set.seed(0)
rand = sample(1:nrow(new),350)
train = new[rand, ]
test = new[-rand, ]

train_accuracy<- c()
test_accuracy <- c()

sample_size=seq(10,350,10)
for (n in sample_size )
{
  
  sample1=sample(nrow(train),n)
  train1=train[sample1,]
  
  
  m1=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6) + I(Weight^7),train1)
  #plot(train1$Weight,train1$MPG, pch=19, cex=0.5)
  #lines(sort(train1$Weight), fitted(m1)[order(train1$Weight)], col='blue', type='l', pch=19) 
  traina=sum(m1$residuals^2)
  train_accuracy<-c(train_accuracy,traina)
  pred = predict(m1, newdata=test)
  testa=sum((pred-test$MPG)^2)
  test_accuracy<-c(test_accuracy,testa)
  
  
  filename=paste(c("sample",n,".png"),collapse = " ")
  chartname=paste(c("sample size=",n,"Degree of polynomial=7"),collapse = " ")
  png(filename =filename )
  
  plot(train1$Weight,train1$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG",main = chartname)
  lines(sort(train1$Weight), fitted(m1)[order(train1$Weight)], col='blue', type='l', pch=19)
 
  metrics=paste(c("Train_Accuracy=",traina,"Test_Accuracy=",testa),collapse = " ")
  mtext(metrics,side = 3)
  dev.off()
  
  
  
  
}



accuracy=data.frame(sample_size,train_accuracy,test_accuracy)



filename=("Sample Size Vs Test Accuracy.png")
png(filename =filename )
plot(sample_size,test_accuracy,xlab = "Sample size", ylab = "Test Error",main = "Sample size Vs Test Accuracy")
lines(sample_size,test_accuracy,type="l")
dev.off()

filename=("Sample Size Vs Train Accuracy.png")
png(filename =filename )
plot(sample_size,train_accuracy,xlab = "sample size",ylab = "Train Error",main="Sample size Vs Train Accuracy")
lines(sample_size,train_accuracy,type="l")
dev.off()


samples=split(train,sample(1:nrow(train),5,replace=F))






colnames(train)
colnames<-colnames(train) 

for (i in 1:5)
{
    sample=data.frame(samples[i])
    colnames(sample)<-colnames
    
    train_accuracy<- c()
    test_accuracy <- c()
    RMSE_test<-c()
    RMSE_train<-c()
    accuracy=data.frame()
    
    
    m1=lm(MPG ~Weight,sample)
    m2=lm(MPG ~Weight+ I(Weight^2),sample)
    m3=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3),sample)
    m4=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4),sample)
    m5=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5),sample)
    m6=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6),sample)
    m7=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6) + I(Weight^7),sample)
    m8=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6) + I(Weight^7) +I(Weight^8),sample)
    m9=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6) + I(Weight^7) +I(Weight^8) +I(Weight^9),sample)
    m10=lm(MPG ~Weight+ I(Weight^2)+ I(Weight^3)+ I(Weight^4)+ I(Weight^5)+ I(Weight^6) + I(Weight^7) +I(Weight^8) ++I(Weight^9) +I(Weight^10),sample)
     
    train1=sum(m1$residuals^2)
    train_accuracy<-c(train_accuracy,train1)
    pred = predict(m1, newdata=test)
    test1=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test1)
    
    train2=sum(m2$residuals^2)
    train_accuracy<-c(train_accuracy,train2)
    pred = predict(m2, newdata=test)
    test2=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test2)
    
    train3=sum(m3$residuals^2)
    train_accuracy<-c(train_accuracy,train3)
    pred = predict(m3, newdata=test)
    test3=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test3)
    
    train4=sum(m4$residuals^2)
    train_accuracy<-c(train_accuracy,train4)
    pred = predict(m4, newdata=test)
    test4=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test4)
    
    train5=sum(m5$residuals^2)
    train_accuracy<-c(train_accuracy,train5)
    pred = predict(m5, newdata=test)
    test5=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test5)
    
    train6=sum(m6$residuals^2)
    train_accuracy<-c(train_accuracy,train6)
    pred = predict(m6, newdata=test)
    test6=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test6)
    
    train7=sum(m7$residuals^2)
    train_accuracy<-c(train_accuracy,train7)
    pred = predict(m7, newdata=test)
    test7=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test7)
    
    train8=sum(m8$residuals^2)
    train_accuracy<-c(train_accuracy,train8)
    pred = predict(m8, newdata=test)
    test8=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test8)
    
    train9=sum(m9$residuals^2)
    train_accuracy<-c(train_accuracy,train9)
    pred = predict(m9, newdata=test)
    test9=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test9)
  
    
    train10=sum(m10$residuals^2)
    train_accuracy<-c(train_accuracy,train10)
    pred = predict(m10, newdata=test)
    test10=sum((pred-test$MPG)^2)
    test_accuracy<-c(test_accuracy,test10)
    
    #plot(sample$Weight,sample$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG")
    #lines(sort(sample$Weight), fitted(m1)[order(sample$Weight)], col='blue', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m2)[order(sample$Weight)], col='Red', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m3)[order(sample$Weight)], col='Green', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m4)[order(sample$Weight)], col="yellow", type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m5)[order(sample$Weight)], col='black', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m6)[order(sample$Weight)], col='blue', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m7)[order(sample$Weight)], col='blue', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m8)[order(sample$Weight)], col='blue', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m9)[order(sample$Weight)], col='blue', type='l', pch=19)
    #lines(sort(sample$Weight), fitted(m10)[order(sample$Weight)], col='blue', type='l', pch=19)
    
    filename=paste(c("sample",i,".png"),collapse = " ")
    chartname=paste(c("sample size= 70","sample number=",i),collapse = " ")
    png(filename =filename )
    plot(sample$Weight,sample$MPG, pch=19, cex=0.5,xlab = "Weight",ylab = "MPG",main =chartname )
    lines(sort(sample$Weight), fitted(m1)[order(sample$Weight)], col='blue', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m2)[order(sample$Weight)], col='Red', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m3)[order(sample$Weight)], col='Green', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m4)[order(sample$Weight)], col="yellow", type='l', pch=19)
    lines(sort(sample$Weight), fitted(m5)[order(sample$Weight)], col='black', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m6)[order(sample$Weight)], col='blue', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m7)[order(sample$Weight)], col='brown', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m8)[order(sample$Weight)], col='purple', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m9)[order(sample$Weight)], col='tan', type='l', pch=19)
    lines(sort(sample$Weight), fitted(m10)[order(sample$Weight)], col='blue', type='l', pch=19)
    
    dev.off()
    
    RMSE_train<-sqrt((1/(70-2))*train_accuracy)
    RMSE_test<-sqrt((1/(70-2))*test_accuracy)
    #print(RMSE_test)
    accuracy=data.frame(1:10,train_accuracy,test_accuracy,RMSE_train,RMSE_test)
    #print(train_accuracy)
    #print(test_accuracy)
    colnames(accuracy)<- c('Order','Train Accuracy','Test Accuracy',"Train_RMSE","Test_RMSE")
    print(accuracy)
    #View(accuracy)
    
    filename=paste(c("Model_Complexity_RSS",i,".png"),collapse = " ")
    png(filename =filename )
    main1=paste(c("Model Complexity Vs Test RSS for sample ",i),collapse = " ")
    plot(1:10,test_accuracy,xlab = "Model Complexity", ylab = "Test_RSS",main=main1)
    lines(1:10,test_accuracy,type="l")
    dev.off()
    
    filename=paste(c("Model_Complexity_Train_RMSE",i,".png"),collapse = " ")
    png(filename =filename )
    main1=paste(c("Model Complexity Vs Train RMSE for sample",i),collapse = " ")
    plot(1:10,RMSE_train,xlab = "Model Complexity", ylab = "RMSE_Train",main=main1)
    lines(1:10,RMSE_train,type="l",col="blue")
    #lines(1:10,RMSE_test,type="l",col="Red")
    dev.off()
    
    filename=paste(c("Model_Complexity_Test_RMSE",i,".png"),collapse = " ")
    png(filename =filename )
    main1=paste(c("Model Complexity Vs Test RMSE for sample",i),collapse = " ")
    plot(1:10,RMSE_test,xlab = "Model Complexity", ylab = "RMSE_Test",main=main1)
    lines(1:10,RMSE_test,type="l",col="Red")
    dev.off()
    
    View(accuracy)
    
   
    
}



