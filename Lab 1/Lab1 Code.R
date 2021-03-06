#SSC 442 Lab 1
#Team Awesome - Sayem Lincoln, Joshua Schwimmer, John Townshend.


library(tidyverse)
library(gapminder)
library(ggplot2)

data <- mpg

data1<- read.csv(file = "bank.csv")

#Exercise  1a
#Graph 1
plot1 <- ggplot(data, aes(x=displ, y=hwy))+ geom_point()+
  ggtitle("Displ vs Hwy Graph")+
  labs(x="Hwy(car's fuel efficiency rate)", y="Displ(engine size, litres)", subtitle = "Graph 1")

#Graph 2
plot2 <- ggplot(data, aes(x=class, y=drv))+ geom_point()+
  ggtitle("Class vs Drv Graph")+
  labs(x="Class(Types of Vehicle)", y="Drv(Types of drive)", subtitle = "Graph 2")

#Exercise  1b
#Graph 3
plot3 <- ggplot(data, aes(x=displ, y=hwy, color=class))+ geom_point()+
  ggtitle("Displ vs Hwy Graph with Class representation")+
  labs(x="Hwy(car's fuel efficiency rate)", y="Displ(engine size, litres)", color="Class\n(Types of\n Vehicle)", subtitle = "Graph 3")

#Exercise  2 
#Graph 1
#Building linear model 
model <- lm(campaign ~ duration, data = data1)

#Adding predictions 
pred.int <- predict(model, interval = "prediction")
data2 <- cbind(data1, pred.int)

plot4 <- ggplot(data2, aes(x=duration, y=campaign, color=y)) +
  geom_point() +
  stat_smooth(method = lm)+
  #prediction line in black
  geom_line(aes(y = lwr), color = "black", linetype = "dashed")+
  geom_line(aes(y = upr), color = "black", linetype = "dashed")+ 
  ggtitle("Effectiveness of the Campaign")+
  labs(x="Number of Campaigns", y="Duration of Phone Calls", color="People who\n brought the\n product", subtitle = "Graph 4")

#Graph 2
data3 <- data1[data1$y != "no",]

#Function for algorithm
funSumy <- function(df,edu){
  
  dfadmin <- df[df$job == "admin.",]
  dfadmin <- dfadmin[dfadmin$education == edu,]
  admin <- NROW(dfadmin)
  cadmin <- c("Admin", edu, strtoi(admin))
  
  dfbluecollar <- df[df$job == "blue-collar",]
  dfbluecollar <- dfbluecollar[dfbluecollar$education == edu,]
  bluecollar <- NROW(dfbluecollar)
  cbluecollar <- c("Bluecollar", edu, strtoi(bluecollar))
  
  dfentrepreneur <- df[df$job == "entrepreneur",]
  dfentrepreneur <- dfentrepreneur[dfentrepreneur$education == edu,]
  entrepreneur <- NROW(dfentrepreneur)
  centrepreneur <- c("Entrepreneur", edu, strtoi(entrepreneur))
  
  dfhousemaid <- df[df$job == "housemaid",]
  dfhousemaid <- dfhousemaid[dfhousemaid$education == edu,]
  housemaid <- NROW(dfhousemaid)
  chousemaid <- c("Housemaid", edu, strtoi(housemaid))
  
  dfmanagement <- df[df$job == "management",]
  dfmanagement <- dfmanagement[dfmanagement$education == edu,]
  management <- NROW(dfmanagement)
  cmanagement <- c("Management", edu, strtoi(management))
  
  dfretired <- df[df$job == "retired",]
  dfretired <- dfretired[dfretired$education == edu,]
  retired <- NROW(dfretired)
  cretired <- c("Retired", edu, strtoi(retired))
  
  dfselfemployed <- df[df$job == "self-employed",]
  dfselfemployed <- dfselfemployed[dfselfemployed$education == edu,]
  selfemployed <- NROW(dfselfemployed)
  cselfemployed <- c("Selfemployed", edu, strtoi(selfemployed))
  
  dfservices <- df[df$job == "services",]
  dfservices <- dfservices[dfservices$education == edu,]
  services <- NROW(dfservices)
  cservices <- c("Services", edu, strtoi(services))
  
  dfstudent <- df[df$job == "student",]
  dfstudent <- dfstudent[dfstudent$education == edu,]
  student <- NROW(dfstudent)
  cstudent <- c("Student", edu, strtoi(student))
  
  dftechnician <- df[df$job == "technician",]
  dftechnician <- dftechnician[dftechnician$education == edu,]
  technician <- NROW(dftechnician)
  ctechnician <- c("Technician", edu, strtoi(technician))
  
  dfunemployed <- df[df$job == "unemployed",]
  dfunemployed <- dfunemployed[dfunemployed$education == edu,]
  unemployed <- NROW(dfunemployed)
  cunemployed <- c("Unemployed", edu, strtoi(unemployed))
  
  dfunknown <- df[df$job == "unknown",]
  dfunknown <- dfunknown[dfunknown$education == edu,]
  unknown <- NROW(dfunknown)
  cunknown <- c("Unknown", edu, strtoi(unknown))
  
  ysum <- data.frame(cadmin, cbluecollar, centrepreneur, chousemaid, cmanagement, cretired, cselfemployed, cservices, cstudent, ctechnician, cunemployed, cunknown)
  ysum <- t(ysum)
  rownames(ysum) <- NULL
  colnames(ysum) <- c("job", "education", "y")
  return(ysum)
}
ysum1 = funSumy(data3, "primary")
ysum2 = funSumy(data3, "secondary")
ysum3 = funSumy(data3, "tertiary")
ysum4 = funSumy(data3, "unknown")


ysum <- rbind(ysum1,ysum2,ysum3,ysum4)
dfSum <- data.frame(ysum)

plot5 <- ggplot(dfSum, aes(y=strtoi(y), x=job, fill=education)) +
  geom_bar(stat= 'identity', width=1.4, position = position_dodge(width = 0.5)) +
  ggtitle("Demographic of the customers who brought the product")+
  labs(x="Types of Job", y="Number of customers", fill="Education\nlevel", subtitle = "Graph 5")

plot1
plot2
plot3
plot4
plot5
