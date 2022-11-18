##########################################################################
###   This file contains the command lines to reproduce the examples   ### 
###   presented in ExploratoryDataAnalysis.pdf                         ###
##########################################################################

library(tidyverse)
library(ggplot2)

# Set your working directory
setwd()

####################
###   Slide 16   ###
####################

load("MATH1041-2022T1.RData")
table(tennis.tv)

####################
###   Slide 17   ###
####################

prop.table(table(tennis.tv)) * 100

tbl <- table(tennis.tv)
cbind(tbl, prop.table(tbl) * 100)

####################
###   Slide 19   ###
####################

c(mean(unsw.satisfaction), median(unsw.satisfaction))

####################
###   Slide 20   ###
####################

quantile(unsw.satisfaction, c(0.25, 0.75))

####################
###   Slide 22   ###
####################

diff(quantile(unsw.satisfaction, c(0.25, 0.75)))
c( sd(unsw.satisfaction), sd(unsw.satisfaction)^2, var(unsw.satisfaction))

summary(unsw.satisfaction)

####################
###   Slide 25   ###
####################

barplot(table(tennis.tv),
        names.arg=c("No", "Yes"),
        main="Did you watch the Australian Open Women Tennis Final?",
        xlab="Answer", ylab="Counts",col="darkred")

####################
###   Slide 27   ###
####################

tennis <- data.frame(answer=c("No", "Yes"), count=as.vector(tbl))
head(tennis)

p <- ggplot(data=tennis, aes(x=answer, y=count)) +
  geom_bar(stat="identity", color="blue",fill="steelblue")+
  geom_text(aes(label=count), vjust=1.6, color="white", 
            size=8.5)
p

####################
###   Slide 29   ###
####################

p + 
  ggtitle("Did you watch the Australian Open Women Tennis Final?") +
  xlab("Answer") + 
  ylab("Count") + 
  theme(
    plot.title = element_text(color="red", size=20, face="bold.italic"),
    axis.text = element_text(size=16),
    axis.title.x = element_text(color="blue", size=20, face="bold"),
    axis.title.y = element_text(color="#993333", size=20, face="bold")
  )

####################
###   Slide 32   ###
####################

sat <- data.frame(satisfaction = unsw.satisfaction)
p2 <- ggplot(data=sat, aes(x="", y=satisfaction)) +
  geom_boxplot(fill = "white", colour = "#3366FF",
               outlier.colour = "red", outlier.shape = 1) +
  ylab("Score") + 
  xlab("") + 
  theme_bw()
p2

####################
###   Slide 34   ###
####################

p2 + geom_jitter(width = 0.2) + 
  theme(
    axis.text = element_text(size=16),
    axis.title.y = element_text(color="#993333", size=20, face="bold")
  )

####################
###   Slide 36   ###
####################

p3 <- ggplot(data=sat, aes(satisfaction)) +
  geom_histogram(fill = "#69b3a2", colour = "purple",
                 bins=10, size=2) +
  ylab("Score") + 
  xlab("") + 
  theme_classic() +
  theme(
    axis.text = element_text(size=16),
    axis.title.y = element_text(color="magenta", size=20, face="bold")
  )
p3

####################
###   Slide 39   ###
####################

table(sex, stress)

####################
###   Slide 40   ###
####################

tbl2 <- table(sex, stress)
round(prop.table(tbl2, margin =2),3) # for column
round(prop.table(tbl2, margin =1),3) # for row

####################
###   Slide 41   ###
####################

summary(hair.cost[sex=="Female"])
summary(hair.cost[sex=="Male"])

####################
###   Slide 44   ###
####################

SS <- data.frame(stress=rep(colnames(tbl2), each=2), 
                 sex = c(rep(rownames(tbl2), 5)),
                 count=as.vector(tbl2))
p4 <- ggplot(data=SS, aes(x=stress, y=count, fill=sex)) +
  geom_bar(stat="identity",
           position="dodge") +
  ggtitle("Are you stressed?") +
  ylab("Count") + 
  xlab("Stress") + 
  theme_classic() +
  theme(
    axis.text = element_text(size=16),
    axis.title = element_text(color="magenta", size=20)
  )
p4

####################
###   Slide 46   ###
####################

SH <- data.frame(sex=sex, hair.cost=hair.cost)

p5 <- ggplot(data=SH, aes(x=sex, y=hair.cost)) +
  geom_boxplot() +
  ggtitle("How much do you spend on a hair cut?") +
  ylab("Amount($)") + 
  xlab("Sex") + 
  theme_classic() +
  theme(
    axis.text = element_text(size=16),
    axis.title = element_text(color="magenta", size=20)
  )
p5

####################
###   Slide 48   ###
####################

p5 <- ggplot(data=SH[-which.max(hair.cost),], aes(x=sex, y=hair.cost, fill=sex)) +
  geom_boxplot() +
  ggtitle("How much do you spend on a hair cut?") +
  ylab("Amount($)") + 
  xlab("Sex") + 
  scale_fill_brewer(palette="Dark2") +
  theme_classic() +
  theme(
    axis.text = element_text(size=16),
    axis.title = element_text(color="magenta", size=20)
  )
p5 + stat_summary(fun=mean, geom="point", shape=23, size=4) # adding means

####################
###   Slide 51   ###
####################

AH <- data.frame(age=age, hours=hours.sleep)

p6 <- ggplot(data=AH, aes(x=age, y=hours.sleep)) +
  ggtitle("Hours slept as function of age") +
  ylab("Hours slept") + 
  xlab("Age") + 
  theme(
    axis.text = element_text(size=16),
    axis.title = element_text(color="magenta", size=20)
  )
p6 + geom_point(shape = 21, colour = "pink", fill = "black", size = 3, stroke = 3)

####################
###   Slide 53   ###
####################

AH$sex <- sex

p6 + geom_jitter(aes(shape=factor(sex), colour = factor(sex), size=2)) +
  scale_x_continuous(limits = c(18, 45)) +
  scale_y_continuous(limits = c(1, 12)) +
  scale_colour_discrete("Sex") +
  guides(size = "none", shape = "none") +
  theme(legend.text = element_text(size=16))

####################
###   Slide 57   ###
####################

head(AH[,1:2])

####################
###   Slide 58   ###
####################

cor(AH[,1:2]) # Produces NA!!!
cor(na.omit(AH)[,1:2])

