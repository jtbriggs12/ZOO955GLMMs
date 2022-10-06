#Week 4 HW ZOO 955
#Authors: Ian & Jess
#Date Created: 2022-10-04

library(tidyverse)
library(nlme)
library(lme4)
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

ilm = lm(log(Spobee+1) ~ Infection * BeesN, data = bees)
bees$Res = residuals(ilm, type = 'pearson')
ggplot(bees, aes(x = Hive, y = Res))+
  geom_point()
# residuals do not look homogenous - 

#Q4 - What are the advantages of including hive as a random effect, rather than as a fixed effect?
#As a random effect, it gives the model more degrees of freedom. Since we don't care about how each individual hive is responding to the stressor, adding it as a random effect allows for the model to be developed understanding that each hive may start from a different baseline but has the same reaction (random intercept), or that each hive has different responses that can all be sumamrized into a singluar response (random intercept and slope)

# Q5 Step 3. Choose a variance structure or structures (the random effects). What random effects do you want to try?

# We will use Hive as a random effect

# Q6 Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the lme4 package (transformed spore density is response, fInfection01, sBeesN, and interaction are the explanatory variables). Show your code.
Q5.lme <- lmer(log(Spobee +1) ~ Infection*BeesN + (1 | Hive), data =bees)

summary(Q5.lme)

#Q7 Compare the linear regression and ME model(s) with a likelihood ratio test, including correction for testing on the boundary if needed. Use the anova() command. This will re-fit your lmer model with maximum likelihood, but this is OK (note there are some debates about exactly how to best compare an lm and lmer model). Show your work and the results. Which random effect structure do you choose based on the results?

anova(Q5.lme,ilm)
#Based on the AIC values we would choose the ME model - we went with a random intercept structure. 

# Q8 Check the model: plot standardized residuals vs. fitted values and vs. each predictor. (You can get standardized residuals with residuals(yourmodel, type='pearson')). How do they look?

bees$lmeRes = residuals(Q5.lme, type = 'pearson')
bees$F2 = fitted(Q5.lme)

ggplot(bees, aes(x = as.factor(Infection), y = lmeRes))+
  geom_boxplot()

ggplot(bees, aes(x = BeesN, y = lmeRes))+
  geom_point()

ggplot(bees, aes(x = as.factor(Infection), y = F2))+
  geom_boxplot()

ggplot(bees, aes(x = BeesN, y = F2))+
  geom_point()

# they look reasonably normally distributed

#Q9. Step 7. Re-fit the full model with ML (set REML=FALSE) and compare against a reduced model without the interaction term, also fit with ML. Use anova() to compare the models. Which model do you choose? Why?
full.lm <- lmer(log(Spobee +1) ~ Infection*BeesN + (1 | Hive), data =bees, REML = FALSE)
red.lm <- lmer(log(Spobee +1) ~ Infection+BeesN + (1 | Hive), data =bees, REML = FALSE)

anova(full.lm, red.lm)
#The reduced model with no interact has a lower AIC value, so we will continue with that.

#Q10. Step 8. Iterate #7 to arrive at the final model. Show your work. What is your final set of fixed effects?
red.lm <- lmer(log(Spobee +1) ~ Infection+BeesN + (1 | Hive), data =bees, REML = FALSE)
red2.lm <- lmer(log(Spobee +1) ~ Infection + (1 | Hive), data =bees, REML = FALSE)
red3.lm <- lmer(log(Spobee +1) ~ BeesN + (1 | Hive), data =bees, REML = FALSE)
anova(red.lm, red2.lm, red3.lm)

#The model with the lowest AIC value is the reduced model from Q9 - our final set of fixed effects is thus infection presence or absence & number of bees

# Q11. Step 9. Fit the final model with REML. Check assumptions by plotting a histogram of residuals, plotting Pearson standardized residuals vs. fitted values, and plotting Pearson standardized residuals vs. explanatory variables. Are there issues with the model? If so, how might you address them?

red.lm <- lmer(log(Spobee +1) ~ Infection+BeesN + (1 | Hive), data =bees, REML = T)
hist(residuals(red.lm))

bees$F2 = fitted(red.lm)
bees$lmeRes = residuals(red.lm, type = 'pearson')

ggplot(bees, aes(x = F2, y = lmeRes))+
  geom_point()

ggplot(bees, aes(x = Infection, y = lmeRes))+
  geom_boxplot()

ggplot(bees, aes(x = BeesN, y = lmeRes))+
  geom_point()

# everything looks normal

#Q12
summary(red.lm)

# In smaller hives, there's a higher density of spores because a singluar worker bee is more likely to encounter an infected larvae and thus come in contact with the spores. It is also possible that the smaller hives sampled here have already been impacted by the bacteria and thus have a smaller population. 
