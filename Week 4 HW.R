#Week 4 HW ZOO 955
#Authors: Ian & Jess
#Date Created: 2022-10-04

library(tidyverse)
bees <- read.table('Bees.txt', header = TRUE)
#spobee - density of p larvae spores per bee
#infection - degree of infection

view(bees)

bees$Hive = as.factor(bees$Hive)
bees$Infection = ifelse(bees$Infection == 0, 0, 1)

# Q1: Does variance of spore density appear homogenous among hives? Why or why not?
# A1: No, highly infected hives seem to have more variation in their spore density, see plot

ggplot(bees, aes(x = Hive, y = Spobee))+
  geom_point()

rawlm <- lm(Spobee ~ Hive, data = bees)
plot(rawlm, select = c(1))

#Q2: Try some transformations of the response variable to homogenize the variances (or at least improve it). Which transformation of spore density seems reasonable? Why?

#Start with the classic log
ggplot(bees, aes(x = Hive, y = log(Spobee + 1))) +
  geom_point()

loglm <- lm(log(Spobee +1) ~ Hive, data = bees)
plot(loglm, select = c(1))
#Looks somewhat better

#Square root transformation
ggplot(bees, aes(x = Hive, y = sqrt(Spobee))) +
  geom_point()

sqrtlm <- lm(sqrt(Spobee) ~ Hive, data = bees)
plot(sqrtlm, select = c(1))
#Not much better than raw data - log much more effective

#Resiprocal transform
ggplot(bees, aes(x = Hive, y = 1/(Spobee + 1))) +
  geom_point()

reciplm <- lm((1/(Spobee +1)) ~ Hive, data = bees)
plot(reciplm, select = c(1))
#Actually not that bad - but opposite of raw data issue where infected hives are now suppresed

#Log transform seems best here, looking at residuals and variance

# Q3 Develop a simple linear model for transformed spore density. Include infection (fInfection01), number of bees (sBeesN) and their interaction as explanatory variables. Check for a hive effect by plotting standardized residuals (see the residuals(yourmodel, type='pearson') function) against hive ID (fhive). Show your code and your plots. Do residuals look homogenous among hives?

ilm = lm(Spobee ~ Infection*BeesN, data = bees)
bees$Res = residuals(ilm, type = 'pearson')
ggplot(bees, aes(x = Hive, y = Res))+
  geom_point()
# residuals do not look homogenous

