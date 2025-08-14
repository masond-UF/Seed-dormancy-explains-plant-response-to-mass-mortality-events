#Decay Rate 
#Abby Jones
#June 6th, 2019

#Measuring the rate of decay of different MMEs and single carcasses over time
#Justification for stats test:
#ANOVA as also used in other papers focused on decay rate
#Also has both a categorical (treatment) and numerical (ADD) affecting variables

#Clear
rm(list=ls())

#Library downloads
library(ggplot2)
library(dplyr)
library(plyr)
library(ggfortify)
library(lme4)
library(lattice)

#Open Themes
theme_aj <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(size=16, colour = "black"),
        plot.title=element_text(face="bold", size = 16,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=16),
        axis.text.y  = element_text(size=16,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_rect(fill = NA , colour = NA ,size=0.7),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = c(1,1),
        legend.justification=c(1,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(1.2)),
        legend.title =  element_text(size = rel(1.2), face = "bold", hjust = 0)) 
}

#Theme for angling text on axises - different from previous theme_aj2
theme_aj2 <- function (base_size=16, font=NA) { 
  theme(axis.title.x = element_text(face="bold", size=16),
        axis.text.x  = element_text(angle = 45, hjust = 1,size=12, colour = "black"),
        plot.title=element_text(face="bold", size = 16,hjust=0.01),
        axis.title.y = element_text(face="bold",angle=90,size=16),
        axis.text.y  = element_text(size=12,colour = "black"),
        plot.background = element_rect(fill = NA ,colour = NA),
        plot.margin = unit(c(1, 1, 0.5, 0.5), "lines"),
        panel.background =   element_rect(fill = NA , colour = NA ,size=0.7),
        panel.border = element_rect(colour = "black", fill=NA, size=2),
        legend.position = c(0,1),
        legend.justification=c(0,1),
        legend.background =element_blank(),
        legend.key = element_blank(),
        legend.text =   element_text(size = rel(1.2)),
        legend.title =  element_text(size = rel(1.2), face = "bold", hjust = 0)) 
}


#Open Days to Reach_Separate.csv
day <- read.csv(file.choose())
View(day)


###### Graph(s) ######
ggplot(day, aes(x=Treatment, y=Day.Stage.Reached, color = Decay.Stage)) +
  geom_boxplot(stat = "boxplot", position = "dodge2")+
  ylab("Days to Reach Each Decay Stage") + xlab("Treatment") +
  scale_x_discrete(limits = c("Open Single Carcass", "Herbivore Single Carcass",
                              "Scavenger Single Carcass", "Open MME", "Herbivore MME", "Scavenger MME"))+
  scale_color_manual(breaks = c("Early", "Advanced", "Skeletal/Missing"),
                     values = c("red","blue","black"))+
  theme_aj2()


######Statistical Analysis: Mixed Effects Model######

### Skeletal and Missing different (Missing = NA)
#Model 1: Early Decomposition
#Open Decay Rate_Early.csv
early <- read.csv(file.choose())
View(early)

hist(early$Day.Stage.Reached)

shapiro.test(early$Day.Stage.Reached)
#Non-normal

#Skeletal and Missing different counts (Missing = NA)
decay.model <- glmer(Day.Stage.Reached~Biomass*Functional+(1|Site),data=early,family=poisson)
anova(decay.model)
summary(decay.model)
#No significance, all one day so wouldn't run


#Model 2: Advanced Decomposition
#Open Decay Rate_AdvancedCalculations.csv
advanced <- read.csv(file.choose())
View(advanced)

hist(advanced$Day.Stage.Reached)

shapiro.test(advanced$Day.Stage.Reached)
#Non-normal

#Skeletal and Missing different counts (Missing = NA)
decay.model <- glmer(Day.Stage.Reached~Biomass*Functional+(1|Site),data=advanced,family=poisson)
anova(decay.model)
summary(decay.model)
#Functional scavenger exclusion: 0.00827
#Interaction

library(emmeans)
emmeans(decay.model,list (pairwise ~ Biomass*Functional),adjust="tukey")

emmeans(decay.model,list (pairwise ~ Biomass),adjust="tukey")
emmeans(decay.model,list (pairwise ~ Functional),adjust="tukey")


#Model 3: Skeletal Decomposition
#Open Decay Rate_Skeletal.csv
skeletal <- read.csv(file.choose())
View(skeletal)

hist(skeletal$Day.Stage.Reached)

shapiro.test(skeletal$Day.Stage.Reached)
#Non-normal

#Skeletal and Missing different counts (Missing = NA)
decay.model <- glmer(Day.Stage.Reached~Biomass*Functional+(1|Site),data=skeletal,family=poisson)
anova(decay.model)
summary(decay.model)


library(emmeans)
emmeans(decay.model,list (pairwise ~ Biomass*Functional),adjust="tukey")

emmeans(decay.model,list (pairwise ~ Biomass),adjust="tukey")
emmeans(decay.model,list (pairwise ~ Functional),adjust="tukey")


#Skeletal and Missing the same (TBS = 35). Open decay rate non-int.csv
tog <- read.csv(file.choose())
View(tog)

shapiro.test(tog$Average.TBS)
#Not normal 

#Skeletal and missing as same count (35)
decay.model1 <- glmer(Average.TBS~Treatment*Fence+(1|Site.N),data=tog,family=poisson)
anova(decay.model1)
summary(decay.model1)
#Treatment/fencing combination significant

library(emmeans)
emmeans(decay.model1, list(pairwise ~ Treatment*Fence),adjust="tukey")

decay.model1 <- glmer(Average.TBS~Treatment*Date*Fence+(1|Site.N),data=tog,family=poisson)
anova(decay.model1)
summary(decay.model1)

