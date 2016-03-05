data<-as.data.frame(DriverData)
data1<-data
mile<-NULL
timemile<-NULL
plantimemile<-NULL
data2.Date<-NULL
data2.DriverName<-NULL
data2.Event<-NULL
data2.ActTime<-NULL
data2.TotPlan<-NULL
data2.Unit<-NULL
data2.StopType<-NULL
data2.Packages<-NULL
data2.Miles<-NULL
data2.count<-NULL
j=1
i=1
# Now to handle the zero miles
for(i in 3:dim(data1)[1]){
  if(data1$Event[i]=='DEL' || data1$Event[i]=='PU'){
    j=i
  }
  if(data1$Miles[i]>1){
    #it isnt advisable to use data which says 1 mile because it maybe very close to 1.9 and it would still record as 1. So it is better to start from larger numbers
    mile<-rbind(mile,data1$Miles[i])
    timemile<-rbind(timemile,data1$Act.Time[i])
    plantimemile<-rbind(plantimemile,data1$Tot.Plan[i])
    
  }
}
#zero miles handled


#linearmile<- lm ( mile ~  0 + timemile  )
#The coefficient of the above regression
newmiles <- newtime*43.9470
data1$Miles<-newmiles



j=1
sum1=0
sum2=0
sum3=0
i=1
j=1
k=1
for(i in 1:dim(data1)[1]){
  if(data1$Event[i]=='Inside AM'){
    j=i
  }
  if(data1$Event[i]=='DEL' || data1$Event[i]=='PU'){
    for(k in j:i){
      sum1=sum1+(data1$Act.Time[k])
      sum2=sum2+(data1$Tot.Plan[k])
      sum3=sum3+(data1$Miles[k,1])
    }
    
    j=k+1
    data2.ActTime<-rbind(data2.ActTime,sum1)
    data2.TotPlan<-rbind(data2.TotPlan,sum2)
    data2.Miles<-rbind(data2.Miles,sum3)
    sum1=0
    sum2=0
    sum3=0
    data2.Event<-rbind(data2.Event,toString(data1$Event[i]))
    data2.DriverName<-rbind(data2.DriverName,toString(data1$Driver.Name[i]))
    data2.Date<-rbind(data2.Date,toString(data1$Date[i]))
    data2.Unit<-rbind(data2.Unit,data1$Unit..[i])
    data2.StopType<-rbind(data2.StopType,toString(data1$Stop.Type[i]))
    data2.Packages<-rbind(data2.Packages,data1$Packages[i])
    
    
  }
}
data2<-NULL
data2<-cbind(data2.Event,data2.ActTime,data2.TotPlan,data2.DriverName,data2.Date,data2.Unit,data2.StopType,data2.Packages,data2.Miles)
tree<- rpart::rpart(formula = data2.ActTime ~ as.factor(data2.Event) +as.factor(data2.DriverName) + as.factor(data2.Unit) +as.factor(data2.StopType) + data2.Miles)
fancyRpartPlot(tree)
#Code testing.
#data2<-cbind(data1$Event,data1$Act.Time,data1$Tot.Plan,data1$Driver.Name,data1$Date,data1$Unit..,data1$Stop.Type,data1$Packages)
#tree<- rpart(formula = data1$Act.Time ~ as.factor(data1$Event) +as.factor(data1$Driver.Name) + as.factor(data1$Unit..) +as.factor(data1$Stop.Type) + data1$Miles)
#plot(tree)
