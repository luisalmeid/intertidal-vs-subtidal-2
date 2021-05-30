### setup
rm(list=ls())
setwd("C:/Users/Lula/Desktop/Reefolution/R")

### packages
install.packages("car")
install.packages("dplyr")
install.packages("Rmisc")
install.packages("ggplot2")

library(car)
library(dplyr)
library(Rmisc) 
library(ggplot2)
library(moments)


### load data
sur <- read.csv("surv.csv")
info <- read.csv("info.csv")

### explore data
summary(sur)
table(sur$treatment)
table(sur$stressed)
table(sur$serie)
str(sur)

### October - December ###

## prepare data
sur.oct.dec <- sur$oct.dec <- sur %>%
  transmute(tree,
            survival = rowMeans(select(., 31:38),na.rm = TRUE))

r <- merge(info,sur.oct.dec,by="tree")

octdec <-  r%>% filter(stressed == "no", serie == "original" )

## explore data, normality + homocedasticity 
summary (octdec)

boxplot(survival~treatment,data=octdec,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0.6,1.2))

hist(octdec$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(octdec$survival), sd=sd(octdec$survival)), add=TRUE, col="blue")

skewness(octdec$survival)

shapiro.test(octdec$survival)
leveneTest(survival~treatment,data=octdec)

# data non-normal but homocedastic -> arcsin transformation
asin.octdec <- octdec %>% 
               mutate(asin = asin(octdec$survival))

hist(asin.octdec$asin, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(asin.octdec$asin), sd=sd(asin.octdec$asin)), add=TRUE, col="blue")

skewness(asin.octdec$asin)
shapiro.test(asin.octdec$asin)
leveneTest(asin~treatment,data=asin.octdec)


## Kruskal-Wallis (accounting for non normality)
kruskal.test(survival ~ treatment, data = octdec)


## ANOVA
aov<-aov(asin~treatment, data=asin.octdec)
summary(aov)

plot(aov, 1)
plot(aov, 2)

TukeyHSD(x=aov,"treatment",conf.level = 0.95)


### bar graphs
od<-summarySE(octdec, measurevar="survival",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(od, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ ggtitle("Survival Oct - Dec")+ labs(y= "survival", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))


### December - March ###

## prepare data
sur.dec.mar <- sur$dec.mar <- sur %>%
  transmute(tree,
            survival = rowMeans(select(., 39:46),na.rm = TRUE))

s <- merge(info,sur.dec.mar,by="tree")

decmar <-  s %>% filter(stressed == "no")

## explore data, normality + homocedasticity 
summary (decmar)

boxplot(survival~treatment,data=decmar,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0,1.2))

hist(decmar$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(decmar$survival), sd=sd(decmar$survival)), add=TRUE, col="blue")

skewness(decmar$survival)
shapiro.test(decmar$survival)
leveneTest(survival~treatment,data=decmar)

# data non-normal but homocedastic -> arcsin transformation
asin.decmar <- decmar %>% 
  mutate(asin = asin(decmar$survival),na.rm = TRUE)

hist(asin.decmar$asin, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(asin.decmar$asin), sd=sd(asin.decmar$asin)), add=TRUE, col="blue")

shapiro.test(asin.decmar$asin)
leveneTest(asin~treatment,data=asin.decmar)


### ANOVA
aov1<-aov(asin~treatment, data=asin.decmar)
summary(aov1)

plot(aov1, 1)
plot(aov1, 2)

TukeyHSD(x=aov1,"treatment",conf.level = 0.95)

### bar graphs
dm <-summarySE(decmar, measurevar="survival",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(dm, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ ggtitle("Survival Dec - Mar")+ labs(y= "survival", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))


### October - March ###

## prepare data
sur.oct.mar <- sur$oct.mar <- sur %>%
  transmute(tree,
            survival = rowMeans(select(., 47:54),na.rm = TRUE))

t <- merge(info,sur.oct.mar,by="tree")

octmar <-  t %>% filter(stressed == "no", serie == "original")

## explore data, normality + homocedasticity 
summary (octmar)

boxplot(survival~treatment,data=octmar,
        main = "",
        xlab="Treatment",
        ylab="Survival",
        ylim = c(0,1.2))

hist(octmar$survival, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(octmar$survival), sd=sd(octmar$survival)), add=TRUE, col="blue")

skewness(octmar$survival)
shapiro.test(octmar$survival)
leveneTest(survival~treatment,data=octmar)


# data non-normal but homocedastic -> arcsin transformation
asin.octmar <- octmar %>% 
  mutate(asin = asin(octmar$survival))

hist(asin.octmar $asin, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(asin.octmar$asin), sd=sd(asin.octmar$asin)), add=TRUE, col="blue")

shapiro.test(asin.octmar$asin)
leveneTest(asin~treatment,data=asin.octmar)


### ANOVA
aov2<-aov(asin~treatment, data=asin.octmar)
summary(aov2)

plot(aov2, 1)
plot(aov2, 2)

TukeyHSD(x=aov2,"treatment",conf.level = 0.95)

### bar graphs
om <-summarySE(octmar, measurevar="survival",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(om, aes(x=treatment, y=survival, fill=treatment)) + 
  theme_classic()+ ggtitle("Survival Oct - Mar")+ labs(y= "survival", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,1))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=survival-se, ymax=survival+se), width=.15,
                position=position_dodge(.9))
