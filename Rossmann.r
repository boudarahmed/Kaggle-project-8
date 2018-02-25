library(readr)  
library(xgboost)  
library(lubridate)  
train<-read.csv('train_V2.csv')  
test<-read.csv('test_V2.csv')  
store<-read.csv('store.csv')#store是对店铺属性的补充  
train<-merge(train,store)#将两个数据集按列合并  
test<-merge(test,store)#将两个数据集按列合并  
train$Date<-as.POSIXct(train$Date)#将日期字符变成时间格式  
test$Date<-as.POSIXct(test$Date)#将日期字符变成时间格式  
train[is.na(train)]<-0#将空值置为零  
test[is.na(test)]<-0  
train<-train[which(train$Open=='1'),]#选择开门的且销售额不为0的样本  
train<-train[which(train$Sales!='0'),]  
train$month<-month(train$Date)#提取月份  
train$year<-year(train$Date)#提取年份  
train$day<-day(train$Date)#提取日  
train<-train[,-c(3,8)]#删除日期列和缺失值较多的列  
test<-test[,-c(4,7)]#删除日期列和缺失值较多的列  
feature.names<-names(train)[c(1,2,5:19)]#这一步主要使测试集和训练集的结构一致。  
for(f in feature.names){  
  if(class(train[[f]])=="character"){  
    levels<-unique(c(train[[f]],test[[f]]))  
    train[[f]]<-as.integer(factor(train[[f]],levels = levels))  
    test[[f]]<-as.integer(factor(test[[f]],levels = levels))  
  }  
}  
tra<-train[,feature.names]  
RMPSE<-function(preds,dtrain){ #定义一个评价函数，Kaggle官方给的评价函数作为xgboost中的评价函数。  
  labels<-getinfo(dtrain,"label")  
  elab<-exp(as.numeric(labels))-1  
  epreds<-exp(as.numeric(preds))-1  
  err<-sqrt(mean((epreds/elab-1)^2))  
  return(list(metric="RMPSE",value=err))  
}  
h<-sample(nrow(train),10000)#进行10000次抽样  
dval<-xgb.DMatrix(data=data.matrix(train[h,]),label=log(train$Sales+1)[h])#用于以下构建watchlist   
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Sales+1)[-h])#构建xgb特定的矩阵形式  
watchlist<-list(val=dval,train=dtrain)#构建模型参数的watchlist,watchlist是用于监听每次模型运行时的模型性能情况。  
param<-list(objective="reg:linear",  
            booster="gbtree",  
            eta=0.02,  
            max_depth=12,  
            subsample=0.9,  
            colsample_bytree=0.7,  
            num_parallel_tree=2,  
            alpha=0.0001,  
            lambda=1)  
clf<-xgb.train(  params=param,  
                 data=dtrain,  
                 nrounds = 3000,  
                 verbose = 0,  
                 early_stopping_rounds=100,  
                 watchlist = watchlist,  
                 maximize = FALSE,  
                 feval = RMPSE
)  
ptest<- predict(clf,dtest,outputmargin=TRUE)
hist(exp(ptest))
hist(train$Sales)
test$Sales[test$Open==0]=0
test$Sales[test$Open==0]
hist(test$Sales)
sub_xgb=data.frame(Id=test$Store, test$Sales)
write.csv(sub_xgb,"sub_xgb.csv")
test[test$Store==292,]



