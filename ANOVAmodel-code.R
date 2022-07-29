# Necessary Libraries
library(readxl)
library(ggplot2)
library (dplyr)
library (multcomp)
library (emmeans)

# Read in Data
Cotton <- read_excel("G:/My Drive/Project 1 328/MATH 328 Project Data/Cotton.xlsx")

# Attach data
head(Cotton)
attach(Cotton)
data(Cotton)

# Estimating the signficance of the main effects:
ggplot(Cotton, aes(x=as.factor(Day), y=Measurment)) + 
  geom_boxplot()
ggplot(Cotton, aes(x=as.factor(Spindle), y=Measurment)) + 
  geom_boxplot()
ggplot(Cotton, aes(x=as.factor(Position), y=Measurment)) + 
  geom_boxplot()

# Estimating the Significance of interaction effect between Position and  Day:
qplot (Day, Measurment, data=Cotton, color=as.factor(Position)) + stat_summary (fun=mean, geom="line") +
  facet_wrap (vars(Spindle), labeller="label_both")

# The following is an interaction plot between Position and Day:
qplot (Day, Measurment, data=Cotton, color=as.factor(Position)) + stat_summary (fun=mean, geom="line")
qplot (Day, Measurment, data=Cotton, color=as.factor(Spindle)) + stat_summary (fun=mean, geom="line") +
  facet_wrap (vars(Position), labeller="label_both")

# The interaction between Spindle and Day adjusted for Position:
qplot (Day, Measurment, data=Cotton, color=as.factor(Spindle)) + stat_summary (fun=mean, geom="line")

# Fit the ANOVA model
cotton1 = aov (Measurment ~ factor(Position)*factor(Day)*factor(Spindle) - factor(Position):factor(Day):factor(Spindle), data=Cotton)
summary (cotton1)

# Model R-Squared
Rsquared_cotton1 = summary (lm (Measurment ~ Position*Day*Spindle, data=Cotton))$adj.r.squared
Rsquared_cotton1

# Model Evaluation (1. residual vs fitted plot 2. Normal Q-Q plot 3. scale location plot)
plot (cotton1, which = 1:3)

# Response Transformation Analysis
# Power Transformation from Boxcox:
MASS::boxcox (cotton1)

# Power Transformation from log.sd vs log.mean:
Diam.summ = Cotton %>% group_by (Day) %>% summarise (mean.Measurment = mean (Measurment),
                                                     sd.Measurment = sd (Measurment))
Diam.summ$log.mean = log10 (Diam.summ$mean.Measurment)
Diam.summ$log.sd   = log10 (Diam.summ$sd.Measurment)
plot ( log.sd ~ log.mean, data=Diam.summ)
fit0 = lm ( log.sd ~ log.mean, data=Diam.summ)
abline (fit0)
summary(fit0)
confint (fit0)

# Second Order model with response transfomation:
transformed_measurement = ((Measurment^(-2))/(-2))
cotton2 = aov (transformed_measurement ~ factor(Position)*factor(Day)*factor(Spindle) - factor(Position):factor(Day):factor(Spindle), data=Cotton)
Rsquared_cotton2 = summary (lm (transformed_measurement ~ Position*Day*Spindle, data=Cotton))$adj.r.squared
summary (cotton2)
Rsquared_cotton2

# Model Evaluation (1. residual vs fitted plot 2. Normal Q-Q plot 3. scale location plot)
plot (cotton2, which = 1:3)

# Analysing the significant main effect, Position:
summary (emmeans (cotton2, pairwise ~ Position), infer=c(T,T))
emmip (cotton2, Position ~ as.factor(Day))

# Analysing the significant interaction effect between Position and Spindle:
cld(emmeans (cotton2, ~ Position|Spindle), Letters=LETTERS)
emmip (cotton2, Position ~ as.factor(Spindle))

cld(emmeans (cotton2, ~ Spindle|Position), Letters=LETTERS)
emmip (cotton2, Spindle ~ as.factor(Position))
