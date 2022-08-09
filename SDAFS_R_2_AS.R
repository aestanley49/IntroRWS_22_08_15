###Chapter 2#######

# Scatter Plot#
x = 0:10
y = x^2
plot(x = x, y = y,
     xlab = "x-axis label goes here", 
     ylab = "y-axis label goes here",
     main = "Main Plot Title Goes Here")
points(x = rev(x), y = y, col = "blue", pch = 16)
lines(x = x, y = x, lty = 2, col = "red")
text(x = 5, y = 80, "This is Text", cex = 1.5, col = "grey", font = 4)
abline(h = 80, col = "grey")
abline(v = 5, col = "grey")
###################



# Bar Graph#
x1 = c(2,4,6)
barplot(x1)

barplot(x1, names.arg = c("a", "b", "c"))

x2 = c(3,5,7)
x3 = rbind(x1, x2)
barplot(x3, names.arg = c("a", "b", "c"), beside = T)
###################



# Box and Whisker Plot#
dat = read.csv("creel.csv")
summary(dat)

# Need to convert fishery to a factor
dat$fishery <- as.factor(dat$fishery)

plot(x = dat$fishery, y = dat$hours)

plot(hours ~ fishery, data = dat)
###################



# Histograms#
hist(dat$hours[dat$fishery == "Tournament"])
t_hrs = dat$hours[dat$fishery == "Tournament"]

nbins = 20
breaks = seq(from = min(t_hrs), to = max(t_hrs), length = nbins + 1)
hist(t_hrs, breaks = breaks, main = "Tournament", col = "grey")

par(xaxs = "i", yaxs = "i", mar = c(4,4,2,1))
hist(t_hrs, breaks = breaks, main = "Tournament", col = "grey")
###################



# Multi-panel Plots#
par(mfrow = c(1,3), mar = c(4,1,2,1), oma = c(0,3,0,0))
sapply(levels(dat$fishery), function(f) {
  hist(dat$hours[dat$fishery == f], main = f, xlab = "")
})
mtext(side = 2, outer = T, line = 1.5, "Frequency", cex = 0.8)
###################



# Legends#
par(mar = c(4,4,2,1))
barplot(x3, beside = T, 
        names.arg = c("a", "b", "c"),
        col = c("skyblue2", "tomato2"))
legend("topleft", legend = c("Group 1", "Group 2"),
       fill = c("skyblue2", "tomato2"))

t_hrs = sort(dat$hours[dat$fishery == "Tournament"])
n_hrs = sort(dat$hours[dat$fishery == "Non.Tournament"])

par(mar = c(4,4,1,1))  
plot(x = t_hrs, y = 1:length(t_hrs),
     type = "o", lty = 2, xlim = range(c(t_hrs, n_hrs)),
     xlab = "Hours Fished/Year", ylab = "Rank within Fishery Samples",
     las = 1)
points(x = n_hrs, y = 1:length(n_hrs), type = "o", lty = 1, pch = 16)
legend("topleft", legend = c("Tournament", "Non-Tournament"), 
       lty = c(2,1), pch = c(1, 16), bty = "n")
###################



# Exporting plots to a file#
# step 1: Make a pixels per inch object
ppi = 600

# step 2: Call the figure file creation function
png("TestFigure.png", h = 8 * ppi, w = 8 * ppi, res = ppi)

# step 3: Run the plot 
# put all of your plotting code here (without windows())

# step 4: Close the device
dev.off()
###################


# Bonus: Error bars
x_bar = tapply(dat$hours, dat$fishery, mean)
par(mar = c(4,4,1,1))
barplot(x_bar)

calc_se = function(x) {
  sqrt(sum((x - mean(x))^2)/(length(x)-1))/sqrt(length(x))
}

se = tapply(dat$hours, dat$fishery, calc_se)

lwr = x_bar - 1.96 * se
upr = x_bar + 1.96 * se

par(mar = c(4,4,1,1))
mp = barplot(x_bar, ylim = range(c(0, upr)))
arrows(x0 = mp, y0 = lwr, x1 = mp, y1 = upr, length = 0.1, angle = 90, code = 3)
###################





### Exercise 2 ####
dat = read.csv("sockeye.csv")
summary(dat)

dat$type <- as.factor(dat$type)

hist(dat$weight[dat$type == "hatch"], breaks = 10)

plot(fecund ~ weight, data = dat[dat$type == "wild",],
     main = "Fecundity vs. Weight",
     pch = 17, col = "red", cex = 1.5,
     xlab = "Weight (g)", xlim = c(600, 3000),
     ylab = "Fecundity (#eggs)", ylim = c(0, 3500))

points(fecund ~ weight, data = dat[dat$type == "hatch",],
       pch = 15, col = "blue", cex = 1.5)

legend("bottomright",
       legend = c("Wild", "Hatchery"),
       col = c("blue", "red"),
       pch = c(15, 17),
       bty = "n",
       pt.cex = 1.5
)

vars = c("weight", "fecund", "egg_size")
par(mfrow = c(1,3))
sapply(vars, function(v) {
  plot(dat[,v] ~ dat[,"type"], xlab = "", ylab = v)
})

ppi = 600
png("SockeyeComparisons.png", h = 5 * ppi, w = 7 * ppi, res = ppi)
par(mfrow = c(1,3))
sapply(vars, function(v) {
  plot(dat[,v] ~ dat[,"type"], xlab = "", ylab = v)
})
dev.off()

### Bonus
calc_se = function(x, na.rm = F) {
  # include a option to remove NAs before calculating SE
  if (na.rm) x = x[!is.na(x)]
  
  sqrt(sum((x - mean(x))^2)/(length(x)-1))/sqrt(length(x))
}

mean_surv = tapply(dat$survival, dat$type, mean, na.rm = T)
se_surv = tapply(dat$survival, dat$type, calc_se, na.rm = T)

lwr_ci_surv = mean_surv - 1.96 * se_surv
upr_ci_surv = mean_surv + 1.96 * se_surv

mp = barplot(mean_surv, ylim = c(0, max(upr_ci_surv)))
arrows(mp, lwr_ci_surv, mp, upr_ci_surv, length = 0.1, code = 3, angle = 90)


par(mar = c(2,5,2,1))
mp = barplot(mean_surv, ylim = c(0, max(upr_ci_surv)),
             main = "% Survival to Eyed-Egg Stage by Origin",
             ylab = "% Survival to Eyed-Egg Stage",
             names.arg = c("Hatchery", "Wild"))
arrows(mp, lwr_ci_surv, mp, upr_ci_surv, length = 0.1, code = 3, angle = 90)

