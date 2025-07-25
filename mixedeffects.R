### Mixed effects models of Pinyon N transformations ###
# Used to analyze data with grouped or hierarchical structures 

rm(list=ls()); graphics.off()
library(tidyverse)
library(here)
library(nlme)
library(lme4)
library(readxl)
library(emmeans)
library(Matrix)
library(Hmisc) #check if it can handle lnme model to calculate pairwaise comparisons
library(multcompView)

### Data ###
d <- read_excel("GrossNRatesData.xlsx") 

d <- d %>% rename_all(funs(make.names(.))) #renames all columns to replace spaces with .

winter <- filter(d, Season %in% c("Winter")) #winter season only
summer <- filter(d, Season %in% c("Summer")) #summer season only

a <- aov(lm(as.numeric(Net.Nit) ~ Treatment, data = d))
summary(a)
TukeyHSD(a, conf.level = .95)
### Mixed effects models ###

#this mixed model assuming same/pooled variance 
#lmer(response ~ fixed effects + (random effects))
mod1 <- lmer(as.numeric(Net.Min) ~ Season*Treatment + (1|Plot.number), data = d)
    #  * indicates interaction between these two things
    #lmer = linear mixed effects

#random_effects_only <- lmer(Gross.Min ~ 1 + (1|Plot.number), data = d)

summary(mod1)

qqnorm(resid(mod1))
qqline(resid(mod1))
#if we plotted residuals against fitted, since we have treatments as the indep var,
  # linear models along is harder to interpret. 

# Thus, we can make a linear mixed model and plot that instead:
plot(mod1) #ideally it is randomly scattered horizontally
anova(mod1)


#if  plot(mod1) shows us unequal variance, we need to model the variance according to Treatment
vartrt <- varIdent(form =~1|Treatment) 
#lme(fixed = response ~ fixed effects, random = ~ random effects | grouping variable)
mod2 <- lme(as.numeric(Net.Min) ~ Season*Treatment, (random =~1|Plot.number), weights = vartrt, data = d) #gls, no random
#this second mixed model fixed the variance issue with the first one by assigning a variance structure
  # to the weights

qqnorm(resid(mod2))
qqline(resid(mod2))
plot(mod2) 

summary(mod2)
anova(mod2)

## If ANOVA shows significance in season, For pairwise comparisons only:
mod2emm = emmeans(mod2, ~ Season * Treatment)
pairwise_comparisons <- emmeans(mod1, pairwise ~ Season * Treatment, adjust = "tukey")

pairs(mod2emm)

pairs_result <- pairwise_comparisons$contrasts
print(pairs_result)
cld <- multcomp::cld(pairwise_comparisons$emmeans, adjust = "tukey")

# Print the results with letters
print(cld)

test <- cld %>%
  mutate(letters = recode(.group, "A" = 4, "B" = 3, "C" = 2, "D" = 1))


#alternative to pairwise^^
# Create interaction term
st_int <- with(d, interaction(Season, Treatment))

# Re-run model specifying the interaction term
mod_int <- lme(Gross.Min ~ st_int, (random =~1|Plot.number), weights = vartrt, data = d)

# Call Tukey's HSD on the model that explicitly specifies the interaction term
TukeyHSD(mod_int)
