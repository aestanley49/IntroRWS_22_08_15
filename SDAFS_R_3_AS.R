rm(list=ls())

###3.1#############
# Simple linear regression
# Determining relationship between continuous response variable (y) and some predictor variables (x_n)
# y_i= B_0 +B_1x_i1 +...+ B_nx_in +E_i, where E_i ~N(0,sigma)
# Predictor variables can be categorical (ANOVA, Ttest), Continuous (regression), or combination (ANCOVA)

# Enters data set
dat <- read.csv('sockeye.csv')

str(dat)
summary(dat)

dat$type <- as.factor(dat$type)
dat <- read.csv('sockeye.csv', stringsAsFactors = T)

# Take a look at the data
head(dat)

# Fit a simple linear regression (fecundity vs weight)
# lm(y~x, data= dat)
# y is response variable and x is predictor variable
fit1 <- lm(fecund ~ weight, data= dat)

fit1

# Take a look at the coefficients of the model (Intercept and slope)
# Interpretation- at theoretical zero weight, fecundity is (coeff 1), and increases with each unit increase of weight
summary(fit1)

#Plot data


plot(fecund~ weight, data= dat, col= 'grey', pch= 16, cex= 1.5)
abline(fit1)

par(mar= c(5, 6, 4, 2))
windowsFonts(A= windowsFont('TT Times New Roman'))
plot(fecund ~ weight, data = dat, col = "grey", pch = 16, cex = 1.5, cex.main= 2, 
     xlab= 'Weight (g)', ylab= '', main= 'Simple linear regression', 
     xlim= c(500, 3000), ylim= c(1000, 3500), xaxs= 'i', yaxs= 'i', cex.lab= 2, cex.axis= 1.5, 
     las= 1, bty= 'n', family= 'A')
title(ylab= 'Fecundity', line= 4, cex.lab= 2, family= 'A')
abline(fit1)
###################


# T-test

# Determining if there is a difference in means between two groups (Hatchery and Wild)
# Create two object that contain the hatch and wild fecundity
hatch <- dat$fecund[dat$type=='hatch']
wild <- dat$fecund[dat$type=='wild']

boxplot(dat$fecund~ dat$type)

# Test the difference in means between hatch and wild; 
mean(hatch, na.rm= T)
mean(wild, na.rm= T)
# Groups have equal variances; 
# Data are not paired
# Alternative hypothesis: two sided
t.test(hatch, wild, var.equal = T, paired= F, alternative= 'two.sided')

# Can check for equal variances (Nonparametric options)
var.test(hatch, wild, data= dat, alternative= 'two.sided')
# P-value >0.05 and CI contains 1


# ANOVA

# Lets do this in a more complex framework (ANOVA)
# Similar to T-test but for more groups (in this case it will be the same)
# Should have same or very similar sample sizes
class(dat$type) 

levels(dat$type)

pairs <-  cbind(
  as.character(dat$type),
  as.numeric(dat$type)
)

head(pairs) 

tail(pairs)

#dat$type <-  relevel(dat$type, ref = "wild")    Changes reference level

# Fit second model looking at fecundity by type
fit2 <-  lm(fecund ~ type, data = dat)

summary(fit2)
# Hatch will be reference level (alphabetical) and Wild is hatch + 713.4

m <-  tapply(dat$fecund, dat$type, mean, na.rm = T)

m[1]

m[2]

m[2] - m[1]
###################

# ANCOVA
# You recognize that these two groups should have their own regression lines (intercept should not be the same)
fit3 = lm(fecund ~ type + weight, data = dat)

summary(fit3)$coef
# Again, Hatch is the reference level; Fecundity increases with increasing weight

# Plot the data using two different point types
plot(fecund ~ weight, data = dat, col = "blue", pch = ifelse(dat$type== 'hatch', 1, 16), cex = 1.5, cex.main= 2, 
     xlab= 'Weight (g)', ylab= '', main= 'Simple linear regression', 
     xlim= c(500, 3000), ylim= c(1000, 3500), xaxs= 'i', yaxs= 'i', cex.lab= 2, cex.axis= 1.5, 
     las= 1, bty= 'n', family= 'A')
title(ylab= 'Fecundity', line= 4, cex.lab= 2, family= 'A')

# Add regression line for Hatchery data
# A is intercept and B is slope
abline(coef(fit3)[c(1,3)], lty = 2)

# Add regression line for Wild data
# A is Hatch estimate plus Wild 'adjustment' and B is slope
abline(sum(coef(fit3)[c(1,2)]), coef(fit3)[3])

legend("bottom", legend = c("Hatchery", "Wild"), 
       pch = c(1,16), lty = c(2,1),
       col = "blue", pt.cex = 1.5, bty = "n", horiz = T)
###################
# Notice that this allows the intercept to vary (Wild adjustment) but it does not allow the slope to vary

# Interactions
# Accounts for the interaction term (allows slope to change depending on which type)
fit4 = lm(fecund ~ type + weight + type:weight, data = dat)
fit4
summary(fit4)
# Plot
plot(fecund ~ weight, data = dat, col = "blue", pch = ifelse(dat$type== 'hatch', 1, 16), cex = 1.5, cex.main= 2, 
     xlab= 'Weight (g)', ylab= '', main= 'Simple linear regression', 
     xlim= c(500, 3000), ylim= c(1000, 3500), xaxs= 'i', yaxs= 'i', cex.lab= 2, cex.axis= 1.5, 
     las= 1, bty= 'n', family= 'A')
title(ylab= 'Fecundity', line= 4, cex.lab= 2, family= 'A')

abline(coef(fit4)[c(1,3)], lty = 2)

abline(sum(coef(fit4)[c(1,2)]), sum(coef(fit4)[c(3,4)]))

legend("bottom", legend = c("Hatchery", "Wild"), pch = c(1,16), lty = c(2,1),
       col = "blue", pt.cex = 1.5, bty = "n", horiz = T)

summary(fit4)$coef
# How do we interpret these?
# Intercept is Hatch at weight= 0; Weight is increase in fecundity for each unit weight
# Typewild is Hatch - Wild adjustment; Typewild:weight is Hatch + Wild increase in fecundity for each unit weight

# compare to fit seperate
fitH = lm(fecund ~ weight, data = dat[dat$type=="hatch",])
fitW = lm(fecund ~ weight, data = dat[dat$type=="wild",])
summary(fitH)$coef
summary(fitW)$coef

###################

# Model selection

# Create AIC table
tab = AIC(fit1, fit2, fit3, fit4)

tab[order(tab$AIC),]    # Simple AIC table

n <- nrow(data)                                                     # Better looking AIC
logL <- c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4))
K <- c(3, 3, 4, 5)
AIC <- -2*logL + 2*K
delta <- AIC- min(AIC)
w <- exp(-0.5*delta)/sum(exp(-0.5*delta))
ms <- data.frame(logL, K, AIC, delta, w)
rownames(ms) <- c('fecund~weight', 'fecund~type', 'fecund~type+weight', 'fecund~type+weight+type:weight')
round(ms, digits= 2)
ms <- ms[order(ms$AIC),]
AIC_tab <- round(ms, digits= 2)
AIC_tab
###################






###3.2#############
# GLM's
logit = function(p) {
  log(p/(1 - p))
}

expit = function(eta) {  # lp stands for logit(p)
  exp(eta)/(1 + exp(eta))
}

dat$binary = ifelse(dat$survival < 70, 0, 1)

fit1 = glm(binary ~ weight, data = dat, family = binomial)

fitted(fit1)
predict(fit1)
plogis(predict(fit1))


summary(fit1)

wt_seq = seq(min(dat$weight, na.rm = T),
             max(dat$weight, na.rm = t),
             length = 100)

p = expit(coef(fit1)[1] + coef(fit1)[2] * wt_seq)

plot(p ~ wt_seq, type = "l", lwd = 3, ylim = c(0,1), xlim= c(500, 3000), las = 1, 
     xlab= 'Weight', ylab= 'Probability of survival', 
     cex.lab= 1.5, cex.axis= 1.2, bty= 'n')

fit2 = glm(binary ~ type, data = dat, family = binomial)

summary(fit2)

########
# fitted vs predict
########

predict(fit2,
        newdata = data.frame(type = c("hatch", "wild")),
        type = "response")

fit3 = glm(binary ~ type + weight, data = dat, family = binomial)

summary(fit3)

p_hatch = predict(
  fit3, newdata = data.frame(type = "hatch", weight = wt_seq),
  type = "response"
)

p_wild = predict(
  fit3, newdata = data.frame(type = "wild", weight = wt_seq),
  type = "response"
)

plot(p_wild ~ wt_seq, type = "l", lwd = 3, lty = 1,
     ylim = c(0,1), xlim= c(500, 3000), las =1,
     xlab = "Weight (g)", ylab = "Pr(>70% Egg Survival)", cex.lab= 1.5, cex.axis= 1.2, bty= 'n'
)

lines(p_hatch ~ wt_seq, lwd = 3, lty = 2)

legend("topright", legend = c("Hatchery", "Wild"),
       lty = c(2,1), lwd = 3, bty = "n")

fit4 <- glm(binary~ type*weight, data= dat, family= binomial)

summary(fit4)

p_hatch = predict(
  fit4, newdata = data.frame(type = "hatch", weight = wt_seq),
  type = "response"
)

p_wild = predict(
  fit4, newdata = data.frame(type = "wild", weight = wt_seq),
  type = "response"
)

plot(p_wild ~ wt_seq, type = "l", lwd = 3, lty = 1,
     ylim = c(0,1), xlim= c(500, 3000),  las =1,
     xlab = "Weight (g)", ylab = "Pr(>70% Egg Survival)", cex.lab= 1.5, cex.axis= 1.2, bty= 'n'
)

lines(p_hatch ~ wt_seq, lwd = 3, lty = 2)

legend("topright", legend = c("Hatchery", "Wild"),
       lty = c(2,1), lwd = 3, bty = "n")
###################

# AIC model selection
tab = AIC(fit1, fit2, fit3, fit4)

tab[order(tab$AIC),]

n <- nrow(data)                                                     # Better looking AIC
logL <- c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4))
K <- c(2, 2, 3, 4)
AIC <- -2*logL + 2*K
delta <- AIC- min(AIC)
w <- exp(-0.5*delta)/sum(exp(-0.5*delta))
ms <- data.frame(logL, K, AIC, delta, w)
rownames(ms) <- c('binary~weight', 'binary~type', 'binary~type+weight', 'binary~type*weight')
round(ms, digits= 2)
ms <- ms[order(ms$AIC),]
AIC_tab <- round(ms, digits= 2)
AIC_tab
###################





###3.3#############
# Probability distributions
# parameters
mu = 500; sig = 50
# a sequence of possible random variables (fish lengths)
lengths = seq(200, 700, length = 100)
# a sequence of possible cumulative probabilities
cprobs = seq(0, 1, length = 100)
# use the four functions
densty = dnorm(x = lengths, mean = mu, sd = sig)  # takes specific lengths
cuprob = pnorm(q = lengths, mean = mu, sd = sig)  # takes specific lengths
quants = qnorm(p = cprobs, mean = mu, sd = sig)   # takes specific probabilities
random = rnorm(n = 1e4, mean = mu, sd = sig)      # takes a number of random deviates to make

# set up plotting region: see ?par for more details
# notice the tricks to clean up the plot
par(
  mfrow = c(2,2),    # set up 2x2 regions
  mar = c(3,3,3,1),  # set narrower margins
  xaxs = "i",        # remove "x-buffer"
  yaxs = "i",        # remove "y-buffer"
  mgp = c(2,0.4,0),  # bring in axis titles ([1]) and tick labels ([2])
  tcl = -0.25        # shorten tick marks
)
plot(densty ~ lengths, type = "l", lwd = 3, main = "dnorm()",
     xlab = "Fish Length (mm)", ylab = "Density", las = 1,
     yaxt = "n") # turns off y-axis
axis(side = 2, at = c(0.002, 0.006), labels = c(0.002, 0.006), las = 2)
plot(cuprob ~ lengths, type = "l", lwd = 3, main = "pnorm()",
     xlab = "Fish Length (mm)", ylab = "Cumulative Probability", las = 1)
plot(quants ~ cprobs, type = "l", lwd = 3, main = "qnorm()",
     xlab = "P", ylab = "P Quantile Length (mm)", las = 1)
hist(random, breaks = 50, col = "grey", main = "rnorm()",
     xlab = "Fish Length (mm)", ylab = "Frequency", las = 1)
box() # add borders to the histogram



###3.4#############
# Non linear regression
rm(list=ls())

# Read in data
dat <- read.csv('feeding.csv')

summary(dat)

# Plot 
par(mar= c(5, 6, 4, 2))
windowsFonts(A= windowsFont('TT Times New Roman'))
plot(cons ~ prey, data = dat, col = "grey", pch = 16, cex = 1.5, cex.main= 2, 
     xlab= 'Prey', ylab= '', main= 'Non-linear regression', 
     xlim= c(0, 50), ylim= c(0, 16), xaxs= 'i', yaxs= 'i', cex.lab= 2, cex.axis= 1.5, 
     las= 1, bty= 'n', family= 'A')
title(ylab= 'Prey items consumed', line= 4, cex.lab= 2, family= 'A')


# Holling Type II response: y = ax/(1 + ahx)
# At low densities, consumption is high; at high densities consumption levels off
# Relies on starting values
fit = nls(cons ~ (a * prey)/(1 + a * h * prey), data = dat,
          start = c(a = 3, h = 0.1))
# A is how high does it rise
# H is how sharply does it level off

summary(fit)

# Create a sequence of values within the range or our data
prey_seq = seq(min(dat$prey), max(dat$prey), length = 100)

# Predict the Consumption from that new data sequence
cons_seq = predict(fit, newdata = data.frame(prey = prey_seq))

# Same plot as before
plot(cons ~ prey, data = dat, col = "grey", pch = 16, cex = 1.5, cex.main= 2, 
     xlab= 'Prey', ylab= '', main= 'Non-linear regression', 
     xlim= c(0, 50), ylim= c(0, 16), xaxs= 'i', yaxs= 'i', cex.lab= 2, cex.axis= 1.5, 
     las= 1, bty= 'n', family= 'A')
title(ylab= 'Prey items consumed', line= 4, cex.lab= 2, family= 'A')

# Use the consumption prediction to fit line
# Can not use abline because we no longer have only two parameters to fit and relationship is not linear
lines(cons_seq ~ prey_seq, lwd = 3)
###################




###Exercise 3######

#Part 1############
rm(list=ls())

dat <- read.csv('sockeye.csv')
fit1 <- lm(egg_size ~ weight, data= dat)
fit1
summary(fit1)
plot(egg_size ~ weight, data = dat, col = "grey", pch = 16, cex = 1.5)
abline(fit1)

fit2 = lm(egg_size ~ type, data = dat)
summary(fit2)
fit3 = lm(egg_size ~ type + year, data = dat)
summary(fit3)$coef
plot(egg_size ~ year, data = dat, col = "grey",
     pch = ifelse(dat$type == "hatch", 1, 16), cex = 1.5)
abline(coef(fit3)[c(1,3)], lty = 2)
abline(sum(coef(fit3)[c(1,2)]), coef(fit3)[3])
legend("topright", legend = c("Hatchery", "Wild"), pch = c(1,16), lty = c(2,1),
       col = "grey", pt.cex = 1.5, bty = "n", horiz = T)

fit4 = lm(egg_size ~ type + year + type:year, data = dat)
plot(egg_size ~ year, data = dat, col = "black",
     pch = ifelse(dat$type == "hatch", 1, 16), cex = 1.5)
abline(coef(fit4)[c(1,3)], lty = 2)
abline(sum(coef(fit4)[c(1,2)]), sum(coef(fit4)[c(3,4)]))
legend("topright", legend = c("Hatchery", "Wild"), pch = c(1,16), lty = c(2,1),
       col = "black", pt.cex = 1.5, bty = "l", horiz = F, bg= 'lightblue')
###################



#Exercise 3 Part 2############
dat$binary = ifelse(dat$survival < 80, 0, 1)
fit1 = glm(binary ~ egg_size, data = dat, family = binomial)
summary(fit1)
egg_size_seq = seq(min(dat$egg_size, na.rm = T),
             max(dat$egg_size, na.rm = t),
             length = 100)
p = expit(coef(fit1)[1] + coef(fit1)[2] * egg_size_seq)
plot(p ~ egg_size_seq, type = "l", lwd = 3, ylim = c(0,1), las = 1)

fit2 = glm(binary ~ type, data = dat, family = binomial)
summary(fit2)
predict(fit2,
        newdata = data.frame(type = c("hatch", "wild")),
        type = "response")

fit3 = glm(binary ~ type + egg_size, data = dat, family = binomial)
summary(fit3)
p_hatch = predict(
  fit3, newdata = data.frame(type = "hatch", egg_size = egg_size_seq),
  type = "response"
)
p_wild = predict(
  fit3, newdata = data.frame(type = "wild", egg_size = egg_size_seq),
  type = "response"
)
plot(p_wild ~ egg_size_seq, type = "l", lwd = 3, lty = 1,
     ylim = c(0,1), las =1,
     xlab = "Egg size (mm)", ylab = "Pr(>80% Egg Survival)"
)
lines(p_hatch ~ egg_size_seq, lwd = 3, lty = 2)
legend("bottomright", legend = c("Hatchery", "Wild"),
       lty = c(2,1), lwd = 3, bty = "n")

fit4 <- glm(binary~ type*egg_size, data= dat, family= binomial)
summary(fit4)
p_hatch = predict(
  fit4, newdata = data.frame(type = "hatch", egg_size = egg_size_seq),
  type = "response"
)
p_wild = predict(
  fit4, newdata = data.frame(type = "wild", egg_size = egg_size_seq),
  type = "response"
)
plot(p_wild ~ egg_size_seq, type = "l", lwd = 3, lty = 1,
     ylim = c(0,1), las =1,
     xlab = "Egg size (mm)", ylab = "Pr(>80% Egg Survival)"
)
lines(p_hatch ~ egg_size_seq, lwd = 3, lty = 2)
legend("bottomright", legend = c("Hatchery", "Wild"),
       lty = c(2,1), lwd = 3, bty = "n")
###################