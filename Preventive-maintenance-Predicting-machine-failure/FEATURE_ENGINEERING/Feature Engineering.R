setwd("D:\\Data Science Course\\PHD\\Rohit_assign")
rm(list=ls(all.names = T))
complaints<-read.csv("ComplaintsLog.csv")
Comp_ser<-read.csv("ComponentServiceLog.csv")
Comp_re<-read.csv("ComponentReplacementLog.csv")

Machines<-read.csv("MachinesDetails.csv")

colnames(Machines)
summary(Comp_re)
summary(Comp_serx)


Comp_fi<-merge(Comp_re,Comp_ser,by.x =c("Year","Month","Day","Time","ComponentReplacement","MachineID") ,by.y = c("Year","Month","Day","Time","ComponentAttended","MachineID"))

library(dplyr)

#######################################################Start of Service log and replacement log processing######################################################

### There are some services in the service file which are listed as Component Repair. On the same date for the same machine\
### for the same component the repair done is listed as component replaced in componentreplacemen log. These were corrected and\
#all the data was brought into a single dataset.
Comp_ser1<-anti_join(Comp_re,Comp_fi[,1:6])
colnames(Comp_fi)[5]=colnames(Comp_ser)[6]
Comp_ser2<-merge(Comp_ser,Comp_fi,by =c("Year","Month","Day","Time","ComponentAttended","MachineID"),all.x = T)
Comp_ser2$ServiceType.x=ifelse(is.na(Comp_ser2$ServiceType.y),as.character(Comp_ser2$ServiceType.x),"ComponentReplacement")
Comp_ser2$ServiceType.y=NULL
Comp_ser1["ServiceType"]="ComponentReplacement"
colnames(Comp_ser2)[7]="ServiceType"
colnames(Comp_ser1)[6]="ComponentAttended"
colnames(Comp_ser1)==colnames(Comp_ser2)
Comp_ser_final<-rbind.data.frame(Comp_ser1,Comp_ser2)

###Converting into date format
Comp_ser<-Comp_ser_final
Comp_ser["Date"]<-(paste(Comp_ser$Year,Comp_ser$Month,Comp_ser$Day,sep="-"))
Comp_ser$Date<-as.Date(Comp_ser$Date)
str(Comp_ser)
rm(Comp_fi,Comp_ser1,Comp_ser2,Comp_ser_final,Comp_re)
Comp_ser[1:3,"Date"]-Comp_ser[1401:1403,"Date"]



str(complaints)
##Features: No of Schedule repairs
##Features:No. of Component Replacements
###Features: No. of Component Repairs

Ser_type<-as.character(unique(Comp_ser$ServiceType))
Machines[Ser_type]=1

for (i in seq(1,length(Ser_type),1))
{

 Comp_ser%>%group_by(MachineID,ServiceType)%>%tally()%>%filter(ServiceType==Ser_type[i])->d
 s<-merge(Machines,d,all.x = T,by=c("MachineID"))
 Machines[Ser_type[i]]=s["n"]

 }
Machines$ScheduledService=ifelse(is.na(Machines$ScheduledService),0,Machines$ScheduledService)
Machines[c("ServiceType","n")]=NULL
rm(d,s,i)

colnames(Machines)[4:6]=c("No.Replacements","No.schservice","No.Repairs")
colnames(Machines)
####Features: Average time between Components replaced
M<-as.character(Machines$MachineID)
Comp_repl<-list()
Service<-list()
Repair<-list()

Comp_ser%>%group_by(MachineID,ServiceType,Date)%>%summarise()->d

   for (j in seq(1,length(M),1))
     {
      d%>%filter(MachineID==M[j],ServiceType==Ser_type[1])->f
     c<-c()
     if (length(f$Date)<1)
      {
       next
       }
     if (length(f$Date)==1)
      {
       c<-c(0)
       Comp_repl[[M[j]]]<-c
       print(c)
       next
      }

      for (i in seq(1,length(f$Date)-1,1))
       {
        c[i]<-as.numeric(f[i+1,"Date"]-f[i,"Date"])
        
      }
     print(c)
      Comp_repl[[M[j]]]<-c
   }
saveRDS(Comp_repl,file = "Comp_replaced.rds")

#####Features: Average time between Service
for (j in seq(1,length(M),1))
{
  d%>%filter(MachineID==M[j],ServiceType==Ser_type[2])->f
  c<-c()
  if (length(f$Date)<1)
  {
    c<-c(0)
    Service[[M[j]]]<-c
    print(c)
    next
    
  }
  if (length(f$Date)==1)
  {
    c<-c(0)
    Service[[M[j]]]<-c
    print(c)
    next
  }
  
  for (i in seq(1,length(f$Date)-1,1))
  {
    c[i]<-as.numeric(f[i+1,"Date"]-f[i,"Date"])
    
  }
  print(c)
  Service[[M[j]]]<-c
}
saveRDS(Service,file = "Service.rds")
######Features: Average time Between Component Repairs.
for (j in seq(1,length(M),1))
{
  d%>%filter(MachineID==M[j],ServiceType==Ser_type[3])->f
  c<-c()
  if (length(f$Date)<1)
  {
    c<-c(0)
    Repair[[M[j]]]<-c
    print(c)
    next
    
  }
  if (length(f$Date)==1)
  {
    c<-c(0)
    Repair[[M[j]]]<-c
    print(c)
    next
  }
  
  for (i in seq(1,length(f$Date)-1,1))
  {
    c[i]<-as.numeric(f[i+1,"Date"]-f[i,"Date"])
    
  }
  print(c)
  Repair[[M[j]]]<-c
}
saveRDS(Repair,file = "Repair.rds")

Comp_repl<-readRDS("Comp_replaced.rds")
Repair<-readRDS("Repair.rds")
Service<-readRDS("Service.rds")


Machines["mean_Compreplaced"]<-1
Machines["min_Compreplaced"]<-1
Machines["max_Compreplaced"]<-1
for (i in seq(1,1150,1))
{
Machines[i,"mean_Compreplaced"]<-as.numeric(mean(Comp_repl[[i]]))
Machines[i,"min_Compreplaced"]<-as.numeric(min(Comp_repl[[i]]))
Machines[i,"max_Compreplaced"]<-as.numeric(max(Comp_repl[[i]]))
}

Machines["mean_schrepair"]<-1
Machines["min_schrepair"]<-1
Machines["max_schrepair"]<-1
for (i in seq(1,1150,1))
{
  Machines[i,"mean_schrepair"]<-as.numeric(mean(Service[[i]]))
  Machines[i,"min_schrepair"]<-as.numeric(min(Service[[i]]))
  Machines[i,"max_schrepair"]<-as.numeric(max(Service[[i]]))
}

Machines["mean_comprepair"]<-0
Machines["min_comprepair"]<-0
Machines["max_comprepair"]<-0
for (i in seq(1,1150,1))
{
  Machines[i,"mean_comprepair"]<-as.numeric(mean(Repair[[i]]))
  Machines[i,"min_comprepair"]<-as.numeric(min(Repair[[i]]))
  Machines[i,"max_comprepair"]<-as.numeric(max(Repair[[i]]))
}
################################ END of Service Log Feature extraction#####################################################

#############################################Complaints Log and Operating data##########################

Sensordata<-read.csv("OperatingConditionsData.csv")
Sensordata["Date"]<-paste(Sensordata$Year,Sensordata$Month,Sensordata$Day,sep = "-")
Sensordata["Date"]<-paste(Sensordata$Date,as.character(Sensordata$Time),sep=" ")
Sensordata["Date"]<-as.POSIXct(Sensordata$Date,origin="2017-01-01 00:00:00 ")
saveRDS(Sensordata,file = "Sensordata.rds")
c<-load("Sensordata")
complaints["Date"]<-paste(complaints$Year,complaints$Month,complaints$Day,sep = "-")
complaints["Date"]<-paste(complaints$Date,as.character(complaints$Time),sep=" ")
complaints["Date"]<-as.POSIXct(complaints$Date,origin = "2017-01-01 00:00:00 ")

Complet_data<-data.frame(readRDS("Complete.rds"))
#Complet_data<-merge(Sensordata,complaints,all.x = T)

saveRDS(object = Complet_data,"Complete.rds")

Complet_data%>%group_by(MachineID,ErrorID)%>%summarise(min(Sensor1),max(Sensor1))

library(ggplot2)

Complet_data%>%filter(MachineID==M[2])%>%ggplot(aes(x=Sensor1))+ geom_density(fill='beige') +
  geom_vline(aes(xintercept = mean(Sensor1)), 
             linetype = "dashed", size = 0.6,color = "#FC4E07")+theme_bw()+theme(rect = element_rect(colour = "black"))

Complet_data%>%filter(MachineID==M[2])%>%ggplot(aes(x=Sensor2))+ geom_density(fill='beige') +
  geom_vline(aes(xintercept = mean(Sensor2)), 
             linetype = "dashed", size = 0.6)+theme(rect = element_rect(colour = "blue"))

Complet_data%>%filter(MachineID==M[2])%>%ggplot(aes(x=Sensor3))+ geom_density(fill='beige') +
  geom_vline(aes(xintercept = mean(Sensor3)), 
             linetype = "dashed", size = 0.6)+theme(rect = element_rect(colour = "blue"))
Complet_data%>%filter(MachineID==M[2])%>%ggplot(aes(x=Sensor4))+ geom_density(fill='beige') +
  geom_vline(aes(xintercept = mean(Sensor4)), 
             linetype = "dashed", size = 0.6)+theme(rect = element_rect(colour = "blue"))

Complet_data%>%group_by(MachineID)%>%summarise(min(Sensor1),max(Sensor1),min(Sensor2),max(Sensor2),min(Sensor3),max(Sensor3),min(Sensor4),max(Sensor4))->d

Machines[colnames(d[,-1])]=d[,-1]

Err_type<-as.character(unique(Complet_data$ErrorID))

Y=Machines
Y[Err_type[2:6]]=1
for (i in seq(2,6,1))
{
i=1
Complet_data%>%group_by(MachineID,ErrorID)%>%tally()%>%filter(ErrorID==Err_type[i])->s
Y=merge(Machines,s,all.x = T)
Machines[Err_type[i]]=Y$n
}
Machines[is.na(Machines)]<-0

write.csv(x = Machines,file = "Machine_Train.csv")

X<-Complet_data[,]
###################Instance with no action################################

complaints["Date"]<-paste(complaints$Year,complaints$Month,complaints$Day,sep = "-")
complaints$Date<-as.Date(complaints$Date)

Comp_serx<-Comp_ser[c("Date","MachineID","ComponentAttended","ServiceType")]
Comp_serx$Date<-Comp_serx$Date-1
complaints$ErrorID<-as.character(complaints$ErrorID)
Complaints_repair<-merge(complaints[c("Date","MachineID","ErrorID")],Comp_serx,all.x = T)
str(Complaints_repair)
Complaints_repair$ErrorID<-as.character(Complaints_repair$ErrorID)
Complaints_repair[is.na(Complaints_repair)]<-"NoAction"
d<-Complaints_repair%>%group_by(MachineID,ServiceType)%>%tally()%>%filter(ServiceType=="NoAction")
d<-merge(Machines,d,all.x = T)
Machines["Noaction_Errors"]<-d["n"]
##################################Time Run without any errors###################
install.packages("smbinning")
library(smbinning)
Complet_data<-readRDS("complete.RDS")


Err_type<-as.character(unique(Complet_data$ErrorID))
Sens<-colnames(Complet_data)[7:10]

Complet_data%>%group_by(MachineID,ErrorID)%>%summarise(max(Sensor1),mean(Sensor1),min(Sensor1),max(Sensor2),mean(Sensor2),min(Sensor2),max(Sensor3),mean(Sensor3),min(Sensor3),max(Sensor4),mean(Sensor4),min(Sensor4))->d
Machines<-read.csv("Machine_train.csv")
for (i in c(2:6))
{

d$ErrorID<-as.character(d$ErrorID)
d%>%filter(ErrorID==Err_type[i])->s
s<-as.data.frame(s)
s$ErrorID=NULL
X<-merge(Machines,s,all.x = T)
Machines<-X

column<-colnames(Machines)[(length(colnames(Machines))-11):length(colnames(Machines))]
column<-paste(column,Err_type[i],sep = "_")
colnames(Machines)[(length(colnames(Machines))-11):length(colnames(Machines))]<-column
colnames(Machines)
a<-c("ERR","ERR")
k<-stringr::str_glue(a)
k<-as.character(toString(a))
paste(a,collapse = "",sep = "&")
}
paste(".",Err_type[i])
k[1]
