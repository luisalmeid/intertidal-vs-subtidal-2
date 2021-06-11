### setup
rm(list=ls()) # Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved locally)

### packages
install.packages("car")
install.packages("dplyr")
install.packages("Rmisc")
install.packages("ggplot2")

library(car)
library(dplyr)
library(Rmisc) 
library(ggplot2)
library(agricolae) # Post hoc letters

##### GROWTH #####

### load data
grow <- read.csv("grow.csv")
info <- read.csv("info.csv")

### explore data
summary(grow)
table(grow$treatment)
table(grow$stressed)
table(grow$serie)
str(grow)


### October - December ###

## prepare data
SGR.oct.dec <- grow$oct.dec <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 103:110),na.rm = TRUE))

r <- merge(info,SGR.oct.dec,by="tree")

octdec <-  r%>% filter(stressed == "no", serie == "original" )

## explore data, normality + homocedasticity 
summary (octdec)

boxplot(SGR~treatment,data=octdec,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR <- octdec$SGR

hist(SGR, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR), sd=sd(SGR)), add=TRUE, col="blue")

shapiro.test(SGR)
leveneTest(SGR~treatment,data=octdec)

### ANOVA
##library(nlme) # gls
##aov <- gls(SGR ~ treatment, data = octdec, weights = varIdent(form = ~1 | treatment))

aov<-aov(SGR~treatment, data=octdec)
summary(aov)

plot(aov, 1)
plot(aov, 2)

TukeyHSD(x=aov,"treatment",conf.level = 0.95)
HSD.test(aov(SGR~treatment, data=octdec), trt = c("treatment"), group = T)$groups

### bar graphs
od <-summarySE(octdec, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(od, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ ggtitle("Growth Oct - Dec")+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))


### December - March ###

## prepare data
SGR.dec.mar <- grow$dec.mar <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 111:118),na.rm = TRUE))

s <- merge(info,SGR.dec.mar,by="tree")

decmar <-  s%>% filter(stressed == "no")

## explore data, normality + homocedasticity 
summary (decmar)

boxplot(SGR~treatment,data=decmar,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR1 <- decmar$SGR

hist(SGR1, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR1), sd=sd(SGR1)), add=TRUE, col="blue")

shapiro.test(SGR1)
leveneTest(SGR1~treatment,data=decmar)

### ANOVA
aov1<-aov(SGR~treatment, data=decmar)
summary(aov1)

plot(aov1, 1)
plot(aov1, 2)

TukeyHSD(x=aov1,"treatment",conf.level = 0.95)
HSD.test(aov(SGR~treatment, data=decmar), trt = c("treatment"), group = T)$groups

### bar graphs
dm <-summarySE(decmar, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(dm, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ ggtitle("Growth Dec - Mar")+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))


### October - March ###

## prepare data
SGR.oct.mar <- grow$oct.mar <- grow %>%
            transmute(tree,
            SGR = rowMeans(select(., 119:126),na.rm = TRUE))

t <- merge(info,SGR.oct.mar,by="tree")

octmar <-  t%>% filter(stressed == "no", serie == "original")

## explore data, normality + homocedasticity 
summary (octmar)

boxplot(SGR~treatment,data=octmar,
        main = "",
        xlab="Treatment",
        ylab="SGR",
        ylim = c(0,0.02))

SGR2 <- octmar$SGR

hist(SGR2, freq=FALSE,breaks = 5, xlab="", ylab = "", main = "")
curve(dnorm(x, mean=mean(SGR2), sd=sd(SGR2)), add=TRUE, col="blue")

shapiro.test(SGR2)
leveneTest(SGR2~treatment,data=octmar)

### ANOVA
aov2<-aov(SGR~treatment, data=octmar)
summary(aov2)

plot(aov2, 1)
plot(aov2, 2)

TukeyHSD(x=aov2,"treatment",conf.level = 0.95)
HSD.test(aov(SGR~treatment, data=octmar), trt = c("treatment"), group = T)$groups

### bar graphs
om<-summarySE(octmar, measurevar="SGR",groupvars=c("treatment","treatment"),na.rm = TRUE)

ggplot(om, aes(x=treatment, y=SGR, fill=treatment)) + 
  theme_classic()+ ggtitle("Growth Oct - Mar")+ labs(y= "SGR/d", x = "Treatment")+guides(fill=FALSE)+
  geom_bar(stat="identity", position=position_dodge()) + theme_classic()+
  coord_cartesian(ylim=c(0,0.020))+ 
  scale_fill_brewer(palette = "RdBu")+
  geom_errorbar(aes(ymin=SGR-se, ymax=SGR+se), width=.15,
                position=position_dodge(.9))