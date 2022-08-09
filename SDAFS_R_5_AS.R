rm(list=ls())


###5.1#############
# Getting packages into R
install.packages("dplyr")
install.packages("reshape2")

library(dplyr)
library(reshape2)

# Note: There are function names in dplyr that already exist in base R. You will need to make sure that you
# are using the correct version of the function. To do that, R will assume that you want to use the function
# from the last package that you installed. If instead, you want to use a function from another package or 
# from base R, you will need to use base::filter().



###5.2#############
# Examining the data
catch <- read.csv('daily_catch.csv')
esc <- read.csv('daily_escape.csv')

# Note: There is a time lag between the two data sets which represents the time in between the fish being
# harvested at the fishing grounds and the fish passing the observation tower upstream


###5.3#############
# Changing the data format using melt()
long.catch = melt(catch, id.var = "doy",
                  variable.name = "year",
                  value.name = "catch")
head(long.catch)

long.esc = melt(esc, id.var = "doy",
                variable.name = "year",
                value.name = "esc")
head(long.esc)


# Check to make sure the data is still the same
all(catch$y_2015 == long.catch[long.catch$year == "y_2015", "catch"])


# A way to reverse the process (go from long format to wide format)
dcast(long.catch, doy ~ year, value.var = "catch")



###5.4#############
# Joining data sets using merge()
dat = merge(x = long.esc, y = long.catch,
            by = c("doy", "year"), all = T)
head(dat)


# Ordering by year instead of doy
head(arrange(dat, year))



###5.5#############
# Dealing with lagged vectors

# Let's first filter just the 2015 data
y15 = filter(dat, year == "y_2015")
head(y15)

# Plotting the raw data
plot(catch ~ doy, data = y15, type = "b", pch = 16, col = "blue",
     xlab = "DOY", ylab = "Count", main = "Raw Data")
lines(esc ~ doy, data = y15, type = "b", pch = 16, col = "red")
legend("topright", legend = c("Catch", "Escapement"), 
       col = c("blue", "red"), pch = 16, lty = 1, bty = "n", cex = 0.8)

# Including a lag time of one day
plot(lag(catch, n = 1) ~ doy, data = y15, type = "b", pch = 16, col = "blue",
     xlab = "DOY", ylab = "Count", main = "With lag(catch)")
lines(esc ~ doy, data = y15, type = "b", pch = 16, col = "red")
legend("topright", legend = c("Catch", "Escapement"),
       col = c("blue", "red"), pch = 16, lty = 1, bty = "n", cex = 0.8)


###5.6#############
# Adding columns to data frame with mutate()
lag_sum = function(x, y, n = 1){
  # x is lagged from y by n days
  rowSums(cbind(lead(x, n), y), na.rm = T)
}

lag_sum(y15$esc, y15$catch)

y15 = mutate(y15, run = lag_sum(esc, catch))

y15 = mutate(y15, crun = cumsum(run), cprun = crun/sum(run, na.rm = T))

plot(cprun ~ doy, data = y15, type = "l", ylim = c(0, 1),
     xlab = "DOY", ylab = "Cumulative Run Proportion", lwd = 2)



###5.7#############
# Applying this to all years of data


# Using pipes with dplyr
rnorm(10) %>% length

x = rnorm(10); length(x); rm(x)

dat = dat %>%
  group_by(year) %>%
  mutate(run = lag_sum(esc, catch), 
         crun = cumsum(run), 
         cprun = crun/sum(run, na.rm = T))

arrange(dat, year)


# Plot all years
par(sp)
# make an empty plot
plot(x = 0, y = 0, type = "n", xlim = range(dat$doy), ylim = c(0,1),
     xlab = "DOY", ylab = "Cumulative Run Proportion")

years = levels(dat$year)

# use sapply to "loop" through years, drawing lines for each
tmp = sapply(years, function(x) {
  lines(cprun ~ doy, data = filter(dat, year == x),
        col = ifelse(x == "y_2015", "blue", "grey"))
})


###5.8#############
# Calculating daily means using summarize()
mean_cprun = dat %>% 
  ungroup %>% 
  group_by(doy) %>%
  summarize(cprun = mean(cprun))
head(mean_cprun)

# Doing this with tapply
tapply(dat$cprun, dat$doy, mean)[1:6]

# Note: Using tapply creates a vector whereas using summarize creates a data frame

lines(cprun ~ doy, data = mean_cprun, lwd = 3)



###5.9#############
# More about summarize()
ind = which(dat$cprun == 0.5)
dat$jday[ind]

find_median_doy = function(p,doy) {
  ind = which.min(abs(p - 0.5))
  doy[ind]
}

# use function
med_doy15 = find_median_doy(p = y15$cprun, doy = y15$doy)

# pull out the day it called the median to verify it works
filter(y15, doy == med_doy15) %>% select(cprun)

med_doy = 
  dat %>% 
  group_by(year) %>% 
  summarize(doy = find_median_doy(cprun, doy))
head(med_doy)

# get years as a number
med_doy$year = 
  ungroup(med_doy) %>%
  # extract only the year column
  select(year) %>%
  # turn it to a vector and extract unique values
  unlist %>% unname %>% unique %>%
  # replace "y_" with nothing
  gsub(pattern = "y_", replacement = "") %>%
  # turn to a integer vector
  as.integer

plot(doy ~ year, data = med_doy, pch = 16)

fit = lm(doy ~ year, data = med_doy)
summary(fit)$coef

plot(doy ~ year, data = med_doy, pch = 16)
abline(fit, lty = 2)



###5.10#############
# Fitting a model
sr_dat = 
  dat %>%
  summarize(S = sum(esc, na.rm = T),
            R = sum(run, na.rm = T))

head(sr_dat)

ricker = function(S, alpha, beta) {
  log(alpha * S * exp(-beta * S))
}

# the number of years
nyrs = nrow(sr_dat)
# the spawners to fit to:
S_fit = sr_dat$S[1:(nyrs - 1)]
# the recruits to fit to:
R_fit = sr_dat$R[2:nyrs]

fit = nls(log(R_fit) ~ ricker(S_fit, alpha, beta),
          start = c(alpha = 6, beta = 0))
coef(fit); summary(fit)$sigma

# plot the S-R pairs
plot(R_fit ~ S_fit, pch = 16, col = "grey", cex = 1.5,
     xlim = c(0, max(S_fit)), ylim = c(0, max(R_fit)))
# extract the estimates
ests = coef(fit)
# obtain and draw on a fitted line
S_line = seq(0, max(S_fit), length = 100)
R_line = exp(ricker(S = S_line,
                    alpha = ests["alpha"],
                    beta = ests["beta"]))
lines(R_line ~ S_line, lwd =3)
# draw the 1:1 line
abline(0, 1, lty = 2)


Smax = 1/ests["beta"]
Seq = log(ests["alpha"]) * Smax
Smsy = Seq * (0.5 - 0.07 * log(ests["alpha"]))
brps = round(c(Smax = unname(Smax),
               Seq = unname(Seq),
               Smsy = unname(Smsy)), -3)

abline(v = brps, col = "blue")
text(x = brps, y = max(R_fit) * 1.08, 
     labels = names(brps), xpd = T, col = "blue")
