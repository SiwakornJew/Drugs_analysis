library(readxl)
library("ggplot2")
library(dplyr)
library(reshape)
library("psych")
library(ggplot2)
library(broom)
install.packages("readxl")
install.packages("dplyr")
install.packages("reshape")
install.packages("ggplot2")
install.packages("broom")
DrugData2560 <- read_excel("C:/Users/Jew/Desktop/Rcoding/file/DrugData2560.xlsx")
View(DrugData2560)
summary(DrugData2560$DrugStore_Cases)
sd(DrugData2560$DrugStore_Cases)
getmode(DrugData2560$DrugStore_Cases)
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
newOne <- na.omit(DrugData2560)

test <- subset(newOne, select = -c(Years, Province_Code))
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(as.matrix(test[,-1]))

cortest <- cor(test[sapply(test,is.numeric)])
cortest
totalcaseset <-subset(test,select = -c(DrugProduction_Cases
                                       ,DrugProduct_person
                                       ,DrugImport_person
                                       ,DrugImport_Cases
                                       ,DrugExport_person
                                       ,DrugConspiracy_Cases
                                       ,DrugConspiracy_person
                                       ,DrugExport_Cases
                                       ,DrugConspiracy_Cases
                                       ,DrugConspiracy_person

))
dat <- data.frame(totalcaseset)
totalcaseset <- dat[-c(1),]
#histogram
hist(totalcaseset$DrugStore_Cases,xlab = "DrugStore_Cases",main = " DrugStore_Cases")

#boxplot
boxplot(totalcaseset$Total_Cases,ylab="Total_Cases",main="Total_Cases")
boxplot.stats(totalcaseset$Total_Cases)$out
boxplot(totalcaseset$DrugUse_Cases,ylab="DrugUse_Cases",main="DrugUse_Cases")
boxplot.stats(totalcaseset$DrugUse_Cases)$out
boxplot(totalcaseset$DrugStore_Cases,ylab="DrugStore_Cases",main="DrugStore_Cases")
boxplot.stats(totalcaseset$DrugStore_Cases)$out
#stem
stem(totalcaseset$DrugUse_Cases)

#pdf 
pdf = dnorm(x=totalcaseset$Total_Cases,mean=mean(totalcaseset$Total_Cases)
            ,sd = sd(totalcaseset$Total_Cases))
plot(totalcaseset$Total_Cases,pdf,main="Probability Density Function:TotalCases"
     ,ylab="PDF",
     xlab="TotalCases")
lines(smooth.spline(totalcaseset$Total_Cases,pdf),col='red',lwd=2)

pdf = dnorm(x=totalcaseset$DrugUse_Cases,mean=mean(totalcaseset$DrugUse_Cases)
            ,sd = sd(totalcaseset$DrugUse_Cases))
plot(totalcaseset$DrugUse_Cases,pdf,main="Probability Density Function:DrugUse_Cases"
     ,ylab="PDF",
     xlab="DrugUse_Cases")
lines(smooth.spline(totalcaseset$DrugUse_Cases,pdf),col='red',lwd=2)

pdf = dnorm(x=totalcaseset$DrugStore_Cases,mean=mean(totalcaseset$DrugStore_Cases)
            ,sd = sd(totalcaseset$DrugStore_Cases))
plot(totalcaseset$DrugStore_Cases,pdf,main="Probability Density Function:
     DrugStore_Cases"
     ,ylab="PDF",
     xlab="DrugStore_Cases")
lines(smooth.spline(totalcaseset$DrugStore_Cases,pdf),col='red',lwd=2)

#scatter
plot(totalcaseset$DrugUse_Cases,totalcaseset$Total_Cases, type="p",col="black",
     main = "DrugUse_Cases:Total_Cases",xlab="DrugUse_Cases",ylab="Total_Cases")
lines(lowess(totalcaseset$DrugUse_Cases, totalcaseset$Total_Cases), col = "blue")

plot(totalcaseset$DrugDealer_Cases,totalcaseset$Total_Cases, type="p",col="black",
     main = "DrugDealer_Cases:Total_Cases",xlab="DrugDealer_Cases",ylab="Total_Cases")
lines(lowess(totalcaseset$DrugDealer_Cases, totalcaseset$Total_Cases), col = "blue")


#CPF
cpf = pnorm(q=totalcaseset$Total_Cases,mean=mean(totalcaseset$Total_Cases)
            ,sd = sd(totalcaseset$Total_Cases))
plot(totalcaseset$Total_Cases,cpf
     ,main="Cumulative Probabilty Function:TotalCases"
     ,ylab="CPF",
     xlab="TotalCases")
lines(smooth.spline(totalcaseset$Total_Cases,cpf),col='red',lwd=2)

cpf = pnorm(q=totalcaseset$DrugUse_Cases,mean=mean(totalcaseset$DrugUse_Cases)
            ,sd = sd(totalcaseset$DrugUse_Cases))
plot(totalcaseset$DrugUse_Cases,cpf
     ,main="Cumulative Probabilty Function:DrugUse_Cases"
     ,ylab="CPF",
     xlab="DrugUse_Cases")
lines(smooth.spline(totalcaseset$DrugUse_Cases,cpf),col='red',lwd=2)

cpf = pnorm(q=totalcaseset$DrugStore_Cases,mean=mean(totalcaseset$DrugStore_Cases)
            ,sd = sd(totalcaseset$DrugStore_Cases))
plot(totalcaseset$DrugStore_Cases,cpf
     ,main="Cumulative Probabilty Function:DrugStore_Cases"
     ,ylab="CPF",
     xlab="DrugStore_Cases")
lines(smooth.spline(totalcaseset$DrugStore_Cases,cpf),col='red',lwd=2)


nSample = 40
sampleAVG = sample(totalcaseset$Total_Cases,nSample)
sampleMean = mean(sampleAVG)
sampleSD = sd(sampleAVG)
print(sampleMean)
print(sampleSD)

getCI <- function(cl,n,x){
    m <- mean(x)
    s <- sd(x)
    
    se <- s/sqrt(n)
    z <- qnorm(cl)
    me<- se*z
    ci <- c(m -me,m+me)
    return(ci)
}
getCI(0.90,nSample,totalcaseset$Total_Cases)
getCI(0.95,nSample,totalcaseset$Total_Cases)
getCI(0.99,nSample,totalcaseset$Total_Cases)

getCI <- function(cl,n,x){
    m <- mean(x)
    s <-sd(x)
    se<- s/sqrt(n)
    z <- qnorm(cl)
    me <- se*z
    ci <- c(m-me,m+me)
    return(ci)
}

cl = 0.99
k = data.frame()
for(a in 1:50){
    nSample = 40
    sampleAVG = sample(totalcaseset$Total_Cases,nSample)
    sampleMean = mean(sampleAVG)
    sampleSD = sd(sampleAVG)
    d = data.frame(
        CL = c("0.9"),
        Mean = c(sampleMean),
        lower = c(getCI(cl,nSample,sampleAVG)[1]),
        upper = c(getCI(cl,nSample,sampleAVG)[2])
    )
    k = rbind(k,d)
}


qplot(
    x = Mean,
    y = 1:50,
    color = 1:50,
    data = k,main = "Confidence Interval of Mean : TotalCases",
    xlab = "AVG Cases",
    ylab = "Lap"
)+geom_errorbar(aes(xmin = lower, xmax = upper, width = 0.2)) + geom_vline(xintercept = mean(totalcaseset$Total_Cases))
print(mean(totalcaseset$Total_Cases))
#Linear  Regression

ggplot(totalcaseset,aes(x=Total_Cases,y=DrugUse_Cases),
       main="Linear Regression(Total_cases:DrugUse_cases")+geom_point()+geom_smooth(method = "lm",se=F,size=1,alpha=1)+theme(axis.title=element_text(size=20))
model <- lm(data=totalcaseset,DrugUse_Cases~Total_Cases)
tidy(model)
summary(model)$r.squared

ggplot(totalcaseset,aes(x=Total_Cases,y=DrugStore_Cases))+geom_point()+geom_smooth(method = "lm",se=F,size=1,alpha=1)+theme(axis.title=element_text(size=20))
model <- lm(data=totalcaseset,DrugStore_Cases~Total_Cases)
tidy(model)
summary(model)$r.squared

ggplot(totalcaseset,aes(x=Total_Cases,y=DrugDealer_Cases ))+geom_point()+geom_smooth(method = "lm",se=F,size=1,alpha=1)+theme(axis.title=element_text(size=20))
model <- lm(data=totalcaseset,DrugDealer_Cases ~Total_Cases)
tidy(model)
summary(model)$r.squared




































