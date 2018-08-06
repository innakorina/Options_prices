library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(e1071)
library(caret)
library(doSNOW)
library(ipred)
library(xgboost)

#setting the path to open all files
setwd("~/R/OptionalProfit")
listsp<-list.files(path = "./Data_Option",full.names = T)
all<-data.frame()
# binding 22 files into 1 dataframe called "all"
for (i in 1:length(listsp)){
  sp<-fread(listsp[i])
  all<-rbind(all,sp)
  print(i)
}
#creating new df "AMZN" from "all"
AMZN<-all%>%
  filter(UnderlyingSymbol=='AMZN')%>%
  filter(Last!=0,Volume!='0')%>%
  rename(Stock.Price= UnderlyingPrice)%>%
  rename(Option.Price= Last)%>%
  mutate(DataDate=as.Date(DataDate,"%m/%d/%Y"))%>%
  mutate(Expiration=as.Date(Expiration,"%m/%d/%Y"))%>%
  #mutate(Expiration=as.factor(as.Date(Expiration,"%m/%d/%Y")))%>%
  mutate(Type=as.factor(Type))
#not to open all next time
fwrite(AMZN, "AMZN.csv")
AMZN<-fread("AMZN.csv", stringsAsFactors=F)

AMZN<-AMZN%>%
  mutate(DataDate=as.Date(DataDate,"%Y-%m-%d"))%>%
  mutate(Expiration=as.Date(Expiration,format="%Y-%m-%d"))%>%
  #mutate(Expiration=as.factor(as.Date(Expiration,"%m/%d/%Y")))%>%
  mutate(Type=as.factor(Type))

  
#subset df for plotting convinience and order
AMZN1<-AMZN%>%
  filter(DataDate=='2015-10-01', Type == 'call')%>%
  mutate(Expiration=as.factor(as.Date(Expiration,"%m/%d/%Y")))%>%
  mutate(DataDate=as.factor(as.Date(DataDate,"%m/%d/%Y")))%>%
  arrange(Expiration)
#____________________________________________
#plotting filtered data in diferent ways
ggplot(data = AMZN1, mapping = aes(x=Strike, y= Option.Price))+
  ggtitle("Premiun on 10/01/2015")+
  labs(y="Premium")+
  geom_line( aes(color=Expiration, group=Expiration),size=1)


#Check bids  
ggplot(data = AMZN1, mapping = aes(x=Strike, y= Bid))+
  ggtitle("Bids on 10/01/2015")+
  geom_line(aes(colour=Expiration,group=Expiration), size=1)

#if the bid and Option price correlate
ggplot(data = AMZN1, mapping = aes(x=Bid, y= Option.Price))+
  ggtitle("Correlations")+
  geom_line(aes(colour=Expiration,group=Expiration), size=1)

#compare ask and bid
ggplot(data = AMZN1)+
  ggtitle("Ask and bid on 10/01/2015")+
  labs(y="Ask/Bid Price")+
  geom_line(aes(x=Strike, y= Ask,colour=Expiration,group=Expiration), size=1)+
  geom_line(aes(x=Strike, y= Bid,colour=Expiration,group=Expiration))

AMZN.to.plot<-AMZN%>%
  #mutate(DataDate=as.factor(DataDate))%>%
  #mutate(Expiration=as.factor(Expiration))%>%
  arrange(DataDate)%>%
  arrange(Option.Price)

#Historical Stock price
ggplot(data = AMZN.to.plot, aes(x=DataDate, y= Stock.Price))+
  ggtitle("Stock Price")+
  geom_point(size=1.5)+
  geom_line(aes(x=DataDate, y= Stock.Price), alpha=0.2)

#plotting price evolution of the option with exp=10-23-2015
AMZN.to.plot%>%
  filter(Expiration=="2015-10-30",Type=="call" )%>%
  #filter(DataDate<"2015-10-21" )%>%
  mutate(DataDate=as.factor(DataDate))%>%
  mutate(Expiration=as.factor(Expiration))%>%
  ggplot(mapping = aes(x=Strike, y= Option.Price))+
  ggtitle("Premium for the options expiring on 10.30.2015")+
  labs(y="Premium")+
  geom_line(aes(colour=DataDate,group=DataDate), size=1)
 
#compare premium of the option with exp date=10.30 on 10.28 and 10.29
date.to.buy='2015-10-28'
date_to_predict='2015-10-29'
expiration_choice='2015-10-30'
mix<-AMZN.to.plot%>%
  filter(DataDate == date.to.buy | DataDate==date_to_predict)%>%
  filter(Expiration==expiration_choice & Type=="call")
p<-mix%>%
  mutate(DataDate=as.factor(DataDate))%>%
  ggplot(aes(x=Strike,y=Option.Price))

mix1<-mix%>%
  filter(DataDate == date.to.buy)
mix2<-mix%>%
  filter(DataDate == date_to_predict)
#what is the stock price change
(mix2$Stock.Price[1]-mix1$Stock.Price[1])/mix1$Stock.Price[1]*100

print(p+ggtitle("Price evolution of the options expiring on 10.30.2015 for 2 trading days, type=call")+
        labs(y="Premium")+
        geom_point(aes(group=Expiration, color=DataDate))+
        coord_cartesian(xlim = c(480, 700), ylim = c(0, 130))+
        geom_vline(xintercept =mix1$Stock.Price[1],color="red")+
        geom_vline(xintercept= mix2$Stock.Price[1], color="#009E73"))

#Creating clean data df for models
Clean.data<-AMZN%>%
  select(Stock.Price,Type,Strike,Bid,Ask,Expiration,DataDate,IV, Option.Price)%>%
  select(-Bid,-Ask)%>%
  filter(complete.cases(.))

Sum_clean<-Clean.data%>%
  group_by(DataDate)%>%
  summarise(Stock.Price=unique(Stock.Price))%>%
  mutate(Stock.Lag.Price=lag(Stock.Price,1))%>%
  #mutate(Stock.Lag.Price=lead(Stock.Price,1))#%>%
  select(-Stock.Price)
#%>%
#  mutate(Lag.Stock=lag(Stock.Price,1))
Clean.data<-merge(Clean.data,Sum_clean,by="DataDate")%>%
  select(-Stock.Price)%>%
  filter(complete.cases(.))
#splitting data into few sets by data of transactions
train<-Clean.data%>%
  filter(DataDate<=as.Date("2015-10-28"))

validation<-Clean.data%>%
  filter(DataDate>as.Date("2015-10-28")&DataDate<=as.Date("2015-10-30"))
#test<-Clean.data%>%
#  filter(DataDate>=as.Date("2015-10-25"))

#Setting seeds for complete reproducibility in case its needed
seeds <- vector(mode = "list", length = 30+1)
set.seed(1234)
for(i in 1:(length(seeds)-1)) {
  seeds[[i]]<- sample.int(n=10000, 451)
}
set.seed(1234)
#for the last model
seeds[[length(seeds)]]<-sample.int(10000, 1)

#Custom control panel
custom<-trainControl(method= "repeatedcv",
                     #seeds = seeds,
                     number = 3,
                     repeats = 2,
                     verboseIter = T)
#-------------Models--------------
#1. glmnet
#model.grid <- expand.grid(.alpha=0:10*0.1,
                          #.alpha=0,
                          #.lambda=0:100*0.01)
#2. brnn
model.grid<-expand.grid(.neurons=14)

#Implementing one model at a time on train dataset
start_time <- Sys.time()

model<-train(Option.Price~.,
             train,
             #method = 'glmnet',
             method = 'brnn',
             tuneGrid=model.grid,
             trControl = custom)
#model_brnn_30n<-model
#save(model_brnn_30n, file="model_brnn_30n.RData")
load(file="model_brnn_30n.RData")
#save(model, file="brnn-14_neurons.RData")
model<-model_brnn_30n
end_time <- Sys.time()
#keeping track of execution time
end_time - start_time
#Results
model$results
model$bestTune
#Plotting the model parameters and searching for optimal
ggplot(model,highlight = T)
summary(model)
dfs<-as.data.frame(model$bestTune)
#coeficient of variation in %
best_rmse<-min(model$results$RMSE)/mean(train$Option.Price)*100
best_rmse
#Data validation on the test data, removing the results
valid_test<-validation%>%
  select(-Option.Price)

#predicting options prices on the valid_test data
model.pred<-predict(model,valid_test)
#set to 0 if <0
model.pred[model.pred<0]<-0

#creating new df(both), comparing the predicted and real data for testing set (Oct 21-30, 2015)
both<-validation
both$predicted<-model.pred
both$counter<-1:nrow(both)

#putting filtering into function for plotting convinience
filter_data<- function(df1,trade_day,exp_date){
   df2<-df1%>%
     filter(DataDate==as.Date(trade_day), Type=="call")%>%
     filter(Expiration==as.Date(exp_date))
  return(df2)
}

df_s<-filter_data(both,date_to_predict,expiration_choice)
#choose exp day and prediction day here
date.to.buy='2015-10-28'
date_to_predict="2015-10-29"
expiration_choice='2015-10-30'

#plot real and predicted data on trade day and expiration on demand
ggplot(data=filter_data(both,date_to_predict,expiration_choice), aes(x=Strike, y=Option.Price))+
  ggtitle("Real and predicted option prices for selected day, exp=10.30, call type",date_to_predict)+
  geom_point(aes(x=Strike, y=Option.Price),size=1, color="black")+
  geom_line(aes(x=Strike, y=predicted),color="red", size=1)


#plot all the predicted and real data for exp=10-30
both%>%
  filter(Type=="call")%>%
  filter(Expiration==as.Date(expiration_choice))%>%
  ggplot(aes(x=Strike, y=predicted,color=factor(DataDate)),size=1)+
  geom_point(aes(x=Strike, y=Option.Price),size=1, color="black",alpha=0.5)+
  geom_line(aes(x=Strike, y=predicted), size=1)+
  ggtitle("Real and predicted options prices, exp=10.30, call type")+
  geom_point(data=filter_data(train,date.to.buy,expiration_choice), aes(x=Strike, y=Option.Price),color='red', alpha=0.5)+
  coord_cartesian(xlim = c(475, 670), ylim = c(0, 160))

 
#plot real data on fixed day(10-28) and predicted on chosen day, exp=10.30, call type
ggplot(data=filter_data(train,date.to.buy,expiration_choice), aes(x=Strike, y=Option.Price))+
  ggtitle("Actual options prices and predicted for chosen day, exp=10.30, call type",date_to_predict)+
  geom_point(aes(x=Strike, y=Option.Price,color='actual on 10-28-2015'),size=1)+
  geom_line(data=filter_data(both,date_to_predict,expiration_choice),aes(x=Strike, y=predicted,color='predicted for 10-29-2015'), size=1)+
  scale_colour_manual(name = 'Explaination',values =c("actual on 10-28-2015"="black","predicted for 10-29-2015"="red"))+
  coord_cartesian(xlim = c(480, 700), ylim = c(0, 100))

both_with_RMSE<-both%>%
  mutate(RMSE=(predicted-Option.Price)^2)%>%
  summarize(RMSE=mean(RMSE))
RMSE_test<-both_with_RMSE$RMSE/mean(both$Option.Price)*100

#RMSE_test<-sum(both_with_RMSE$RMSE)/(mean(both_with_RMSE$predicted)*nrow(both_with_RMSE))  
RMSE_test  

#saving df into csv file
#write.csv(both,file="fitted_14neurons.cvs")

#Splinig function, needed to compare prices with uniform Strikes for dif datasets, reality-a flag either data is real(T) or predicted(F) 
splining<- function(df,exp_date,trade_date,reality){
  df2<-df%>%
    filter(DataDate==as.Date(trade_date), Type=="call")%>%
    filter(Expiration==as.Date(exp_date))
  
  if (reality== 'T')
    dat <- data.frame(
      x = df2$Strike,
      y = df2$Option.Price
    )
  else
    dat <- data.frame(
      x = df2$Strike,
      y = df2$predicted
    )
  #removing outliers
  lower<-quantile(dat$y[dat$y>0],0.005)[[1]]
  higher<-quantile(dat$y,0.995)[[1]]
  dat<-dat%>%
    filter(y>lower&y<higher)
  #need to define vector of strikes(x) to plug in the spline

  xout<-train%>%
    filter(DataDate==as.Date(date.to.buy), Type=="call")%>%
    filter(Expiration==as.Date(exp_date))
  lower<-quantile(xout$Option.Price[xout$Option.Price>0],0.005)[[1]]
  higher<-quantile(xout$Option.Price,0.995)[[1]]
  xout<-xout%>%
    filter(Option.Price>lower&Option.Price<higher)
  
  #SplineData <- data.frame(with(dat, 
  #                              spline(x, y, xout = xout$Strike)
  SplineData<-data.frame(with(dat,approx(x,y,method="linear",xout=xout$Strike)))
  #)
  #)
  SplineData$y[SplineData$y<0]<-0.1
  return (SplineData)
}

#df_to_spline=both
df_to_spline<-both%>%
  filter(DataDate==as.Date(date_to_predict)& Type=="call")%>%
  filter(Expiration==as.Date(expiration_choice))
  
#spline predicted data for 10-29
#lower<-quantile(filter_data(df_to_spline,date_to_predict,expiration_choice)$predicted,0.005)[[1]]
#higher<-quantile(filter_data(df_to_spline,date_to_predict,expiration_choice)$predicted,0.995)[[1]]
#lower<-quantile(df_to_spline$predicted[df_to_spline$predicted>0],0.005)[[1]]
#higher<-quantile(df_to_spline$predicted,0.995)[[1]]
splined_predicted_10_29<-splining(df_to_spline,expiration_choice,date_to_predict,'F')

#plotting splined predicted for 10-29
ggplot(data=filter_data(df_to_spline,date_to_predict,expiration_choice))+
  geom_point(aes(x=Strike, y=predicted))+
  geom_point(data=splining(df_to_spline,expiration_choice,date_to_predict,'F'),aes(x=x,y=y),color='red', alpha=0.2)+
  ggtitle('Splined predicted data for 10-29-2015')+
  labs(x = "Strike",y='Interpolated Option price')

#spline real data for 10-29
#lower<-quantile(filter_data(df_to_spline,date_to_predict,expiration_choice)$Option.Price,0.005)[[1]]
#higher<-quantile(filter_data(df_to_spline,date_to_predict,expiration_choice)$Option.Price,0.995)[[1]]
#lower<-quantile(df_to_spline$Option.Price[df_to_spline$Option.Price>0],0.005)[[1]]
#higher<-quantile(df_to_spline$Option.Price,0.995)[[1]]


splined_real_10_29<-splining(df_to_spline,expiration_choice,date_to_predict,'T')
#plotting splined for real for 10-29
ggplot(data=filter_data(df_to_spline,date_to_predict,expiration_choice))+
  geom_point(aes(x=Strike, y=Option.Price))+
  geom_point(data=splined_real_10_29,aes(x=x,y=y),color='red', alpha=0.2)+
  ggtitle('Splined real data for 10-29-2015')+
  labs(x = "Strike",y='Interpolated Option price')






lower<-quantile(df_to_spline$Option.Price[df_to_spline$Option.Price>0],0.005)[[1]]
higher<-quantile(df_to_spline$Option.Price,0.995)[[1]]
train1<-train%>%
  filter(DataDate==as.Date('2015-10-28'), Type=="call")%>%
  filter(Expiration==as.Date(expiration_choice))%>%
  filter(Option.Price>lower&Option.Price<higher)

threshold<-0.05
ROI_dat<-data.frame(Strike=train1$Strike,buy=train1$Option.Price,predicted_price=splined_predicted_10_29$y[c(1:length(train1$Strike))])%>%
  mutate(Predicted_ROI=(predicted_price-buy)/buy)%>%
  filter(Predicted_ROI>threshold)

volu<-AMZN%>%
  filter(DataDate=="2015-10-28"&Expiration=="2015-10-30")%>%
  filter(Type=="call")%>%
  filter(Option.Price>lower&Option.Price<higher)


ROI_real<-data.frame(Strike=train1$Strike,real_price_on_predicted_date=splined_real_10_29$y,buy=train1$Option.Price,volume=volu$Volume*100)%>%
  mutate(Real_ROI=(real_price_on_predicted_date-buy)/buy)


ROI_final<-merge(ROI_dat,ROI_real,by='Strike')%>%
  rename(Option_price_on_date_to_buy= buy.x)%>%
  mutate(Invest=Option_price_on_date_to_buy*volume)%>%
  select(Strike,Option_price_on_date_to_buy,predicted_price,Predicted_ROI,real_price_on_predicted_date,Real_ROI,volume,Invest,-buy.y)

ROI_average<-mean(ROI_final$Real_ROI)
Total_Investment=sum(ROI_final$Invest)
Profit<-Total_Investment*ROI_average
Collect<-Profit+Total_Investment



col=c("actual"="black","predicted"="red")
#check how predicted and actual ROI are correlated
ggplot(data=ROI_final)+
  geom_point(aes(x=Strike,y=Predicted_ROI*100,color="predicted"))+
  geom_point(aes(x=Strike,y=Real_ROI*100,color='actual'))+
  scale_colour_manual(name = 'ROI',values =col)+
  ggtitle('Predicted and actual ROI in %')+
  labs(x = "Strike",y='ROI')
