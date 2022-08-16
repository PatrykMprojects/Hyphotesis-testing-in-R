install.packages("tidyverse")
library("tidyverse")
install.packages("fitdistrplus")
library("fitdistrplus")
install.packages("ggplot2")
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
  # generate data -----------------------------------------------------------


set.seed(4200)
mice_b4 <- rnorm(200, mean = 20, sd=sqrt(2))
set.seed(4200)
mice_after <- rnorm(200, mean = 21, sd=sqrt(2.5))

set.seed(4200)
rat_b4 <- rweibull(200, shape = 10, scale = 20 )
set.seed(4200)
rat_after <- rweibull(200, shape = 9, scale = 21 )




# data validation ---------------------------------------------------------

print(mean(mice_b4))
print(var(mice_b4))
print(mean(mice_after))
print(var(mice_after))


# qplot -------------------------------------------------------------------


# Mice --------------------------------------------------------------------


group_m <- c(rep("MiceBefore", 200), rep("MiceAfter", 200))
weightMice <- c(mice_b4,mice_after)
df_mice <- data.frame(weightMice, group_m)


ggplot(df_mice, aes(x = weightMice, colour = group_m)) +
  geom_density()

qplot(df_mice$weightMice, data = df_mice, geom = "density", col = df_mice$group_m)

qplot(df_mice$weightMice, data = df_mice, geom = "boxplot", col = df_mice$group_m)

ggqqplot(df_mice$weightMice)


# RAT ---------------------------------------------------------------------



group <- c(rep("RatBefore", 200), rep("RatAfter", 200))
weightRat <- c(rat_b4,rat_after)
df_rat <- data.frame(weightRat, group)


ggplot(df_rat, aes(x = weightRat, colour = group)) +
  geom_density()

qplot(df_rat$weightRat, data = df_rat, geom = "density", col = df_rat$group)

qplot(df_rat$weightRat, data = df_rat, geom = "boxplot", col = df_rat$group)

ggqqplot(df_rat$weightRat)

# plot all at once --------------------------------------------------------

groupAll <-  c(rep("MiceBefore", 200), rep("MiceAfter", 200), rep("RatBefore", 200), rep("RatAfter", 200))
weightAll <- c(mice_b4,mice_after,rat_b4,rat_after)df_all <- data.frame(weightAll, groupAll)

qplot(df_all$weightAll, data = df_all, geom = "density", col = df_all$groupAll)

qplot(df_all$weightAll, data = df_all, geom = "boxplot", col = df_all$groupAll)


# Task2 -------------------------------------------------------------------


#From the output, the p-value > 0.05 implying that the distribution 
#of the data are not significantly different from
#normal distribution. In other words, we can assume the normality.


# a -----------------------------------------------------------------------

qqnorm(df_mice$weightMice)
qqline(df_mice$weightMice, col="green", lwd=2)

shapiro.test(df_mice$weightMice)



# b -----------------------------------------------------------------------


qqnorm(df_rat$weightRat)
qqline(df_rat$weightRat, col="green", lwd=2)

shapiro.test(df_rat$weightRat)
shapiro.test(rat_b4)


# Task3 -------------------------------------------------------------------


# a) Mice t-test -----------------------------------------------------------------

#t is the t-test statistic value (t = -25.55),
#df is the degrees of freedom (df= 9),
#p-value is the significance level of the t-test (p-value = 1.03910^{-9}).
#conf.int is the confidence interval of the mean of the differences at 95% (conf.int = [-217.1442, -181.8158]);
#sample estimates is the mean of the differences (mean = -199.48).
#https://www.datanovia.com/en/lessons/how-to-do-a-t-test-in-r-calculation-and-reporting/how-to-do-paired-t-test-in-r/

mice_Tt <- t.test(weightMice ~ group_m, data = df_mice, paired = TRUE)
mice_Tt



# b -----------------------------------------------------------------------


 

wilcox.test(weightRat ~ group, data = df_rat, paired = TRUE)



# TASK4 -------------------------------------------------------------------



gamma_rat_b4 <-fitdist(rat_b4, "gamma" )
plot(gamma_rat_b4)
summary(gamma_rat_b4)
weibull_rat_b4 <- fitdist(rat_b4, "weibull")
plot(weibull_rat_b4)
summary(weibull_rat_b4)
lnorm_rat_b4 <- fitdist(rat_b4, "lnorm")
plot(lnorm_rat_b4)
summary(lnorm_rat_b4)

par(mfrow=c(2,2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(gamma_rat_b4, weibull_rat_b4, lnorm_rat_b4), legendtext = plot.legend)
cdfcomp (list(gamma_rat_b4, weibull_rat_b4, lnorm_rat_b4), legendtext = plot.legend)
qqcomp  (list(gamma_rat_b4, weibull_rat_b4, lnorm_rat_b4), legendtext = plot.legend)
ppcomp  (list(gamma_rat_b4, weibull_rat_b4, lnorm_rat_b4), legendtext = plot.legend)
# after -------------------------------------------------------------------


gamma_rat_after <- fitdist(rat_after, "gamma")
plot(gamma_rat_after)
summary(gamma_rat_after)
weibull_rat_after <- fitdist(rat_after, "weibull")
plot(weibull_rat_after)
summary(weibull_rat_after)
lnorm_rat_after <- fitdist(rat_after, "lnorm")
plot(lnorm_rat_after)
summary(lnorm_rat_after)
