library(readr)
finaldata <- read_csv("finally_final 1.csv")
library(haven)
gravdata <- read_dta("gravdata.dta")
View(gravdata)

#visualize
library(ggplot2)
ggplot(finaldata, aes(x=year, y=log(import_value), color=country_code)) +
  geom_line()
#gravity model
#corelation check
install.packages("GGally")
library(GGally)
ggcorr(finaldata)

m_grav.1 <- lm(log(import_value) ~ log(distance)+log(popjp)+log(GDP)+log(GDPjp)+
                 +log(pop)+log(popjp)+toprunner, data = finaldata)
summary(m_grav.1)

#1999 toprunner
finaldata$top1999 <- ifelse(finaldata$year <=1999, "0", "1")
finaldata$top2003 <- ifelse(finaldata$year <=2003, "0", "1")
finaldata$top2004 <- ifelse(finaldata$year <=2004, "0", "1")
finaldata$top2010 <- ifelse(finaldata$year <=2010, "0", "1")
finaldata$top2009 <- ifelse(finaldata$year <=2009, "0", "1")
finaldata$top2005 <- ifelse(finaldata$year <=2005, "0", "1")
finaldata$top2006 <- ifelse(finaldata$year <=2006, "0", "1")
finaldata$poor <- ifelse(finaldata$GDP <=  ) #to be countinued

#devide year


m_grav.2 <- lm(log(import_value) ~ log(distance)+log(GDP)+log(GDPjp)+
               +log(pop)+log(popjp)+toprunner+top1999+top2003+top2004+top2005+
                 top2009+top2010, data = finaldata)
summary(m_grav.2)

#divide distance
finaldata_close <- filter(finaldata, distance <=5500) 
finaldata_far <- filter(finaldata, distance >5500)

m_close <- lm(log(import_value) ~ log(distance)+log(GDP)+log(GDPjp)+
                +log(pop)+log(popjp)+toprunner+ top1999+top2003+
                top2004+ top2005+ top2009+ top2010 , 
              data = finaldata_close)
summary(m_close)
coef(m_close)
m_far <- lm(log(import_value) ~ log(distance)+log(GDP)+log(GDPjp)+
              +log(pop)+log(popjp)+toprunner+ top1999+top2003+
              top2004+ top2005+ top2009+ top2010, 
            data = finaldata_far)
summary(m_far)

#lmm
library(lme4)
lmm_year <- lmer(log(import_value) ~ log(distance)+log(GDP)+log(GDPjp)+
               +log(pop)+log(popjp)+(log(GDP)|country_code)+(log(GDPjp)|year)+
                 +(log(pop)|country_code)+(log(popjp)|year) , 
             data = finaldata_far)



(gravity model | Year)
(population_i + population_j + distance_i_j | Year)
(population_i + population_j + distance_i_j | year into prieod)
difference and difference model 
#simplify the assumptions



country>>>>year
are>>>>year


#fixed effect
library(gplots)
plotmeans(log(import_value)~country_code, data=finaldata)
detach("package:gplots")
library(foreign)
fixed <- lm(log(import_value)~
              log(distance)+log(GDP)+log(GDPjp)
            +log(pop)+log(popjp)+
              (factor(year)-1)+(factor(country_code)-1),
            data=finaldata )
summary(fixed)

library(plm)
fixed2<- plm(log(import_value)~
               log(distance)+log(GDPjp)+log(GDP)
             +log(pop)+log(popjp),data=finaldata,
             index=c("year","country_code"),model="within")

summary(fixed2)
fixef(fixed2)
pFtest(fixed2,m_grav.2)
#shouldn't use fixed effect model cos p>0.05

random <- plm(log(import_value)~
                log(distance)+log(GDP)+log(GDPjp)
              +log(pop)+log(popjp),data=finaldata,
              index=c("year","country_code"),model="random")
summary(random)

phtest(fixed2,random)
#use ramdon effect model cos p>0.05


timefixed <- plm(log(import_value)~
                   log(distance)+log(GDP)+log(GDPjp)
                 +log(pop)+log(popjp)+
                   factor(year),data=finaldata,
                 index=c("country"),model="within")
summary(timefixed)

pFtest(fixed,timefixed)
plmtest(fixed1,c("time"),type=("bp"))

stargazer(random, type="html", out="model3.doc")
fixef(fixed2)

ranef(random)

AIC(fixed,fixed2,random,timefixed)


m <- lm(import_value ~ toprunner+top1999+top2004+top2005+top2009+top2010, data = finaldata )
summary(m)


install.packages("FSA", "psych")
install.packages("lmerTest", "nlme","car")            
library(FSA, psych)
if(!require(FSA)){install.packages("FSA")}
if(!require(psych)){install.packages("psych")}
if(!require(lme4)){install.packages("lme4")}
if(!require(lmerTest)){install.packages("lmerTest")}
if(!require(nlme)){install.packages("nlme")}
if(!require(car)){install.packages("car")}

random.effects(m_close)


#hausman test
install.packages("plm")
library(plm)
phtest(m)

#put into tables
install.packages("stargazer")
detach("package:stargazer")
library(stargazer)
stargazer(attitude,type="text")
stargazer(m_grav.2,m_close,m_far, type="html", out="model1.doc")