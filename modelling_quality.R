# A script for modeling good vs bad game production over time
library(ggplot2)
library(dplyr)
library(scales)

# get the game data
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
score = games$AvgRating
year = games$YearPublished

# Select data range (inclusive)
start_year = 1977 # Not much data before this, first year to have 100 games produced, before that so small that proportions weren't meaningful
end_year = 2017   # Three years back to account for lag effect

# Check hist of scores and try to fit normal
hist(score, breaks=100, freq=FALSE, main="", xlab="Ratings")
p <- seq(0, 10, by=.01)
points(p, dnorm(p, mean=mean(score), sd=sd(score)), type='l')
# Should I do anything about the skew?

# From BGG website guidelines (obviously don't have to be followed)
# 8 - Very good game. I like to play. Probably I'll suggest it and will never turn down a game.
# 7 - Good game, usually willing to play.
# 6 - Ok game, some fun or challenge at least, will play sporadically if in the right mood.
# 5 - Average game, slightly boring, take it or leave it.
# 4 - Not so good, it doesn't get me but could be talked into it on occasion.
# One reddit post says they think most 'good games' are 7.5+
good.cutoff = qnorm(.9, mean=mean(score), sd=sd(score))
bad.cutoff = qnorm(.1, mean=mean(score), sd=sd(score))
abline(v=good.cutoff)  # 7.62
abline(v=bad.cutoff)   # 5.23

# Set up data frame with this distinction
year_bins = cut(year, breaks=seq(start_year, end_year, by=1), include.lowest=TRUE)
game_status = cut(score, breaks=c(0, bad.cutoff, good.cutoff, 10), include.lowest=TRUE, labels=FALSE)
filtered_game_status =  game_status[game_status!=2]
filtered_game_status = factor(filtered_game_status, levels = c(3, 1), labels = c("Good Game", "Bad Game"))
filtered_year_bins = year_bins[game_status!=2]
status_df = data.frame(filtered_year_bins, filtered_game_status)

binary_game_status = cut(score, breaks=c(0, good.cutoff, 10), include.lowest=TRUE, labels=FALSE)
binary_game_status = factor(binary_game_status, levels = c(2, 1), labels = c("Good Game", "Bad Game"))
binary_df = data.frame(year_bins, binary_game_status)
binary_df$year = as.Date(as.character(year), format="%Y")
clean = binary_df[complete.cases(binary_df$year_bins), ]

# For true binary split
ggplot(clean, aes(x=year, fill=binary_game_status)) + 
  geom_bar() +
  scale_fill_manual(values=c(rgb(.4, .82, .771, 1), rgb(.96, .312, .182, .85))) +
  scale_x_date(labels=date_format("%Y"))  + 
  xlab("Year") + 
  ylab("Number of Games")

ggplot(clean, aes(x=year, fill=binary_game_status)) + 
  geom_bar(position='fill') +
  scale_fill_manual(values=c(rgb(.4, .82, .771, 1), rgb(.96, .312, .182, .85))) +
  scale_x_date(labels=date_format("%Y"))  + 
  xlab("Year") + 
  ylab("Fraction of Games")

# Model quality split using binomial where p = at+b -> exponential?
good.years = year[binary_game_status == "Good Game" & year >= start_year & year <= end_year]
counts = table(factor(good.years, levels=start_year:end_year))
total.counts = table(factor(year, levels=start_year:end_year))
x = as.numeric(names(counts))                                 # years
z = as.vector(total.counts) # total counts each year
y = as.vector(counts)                                         # good game counts
actual.ps = y/z

# Model using poisson with exponential rate of occurrence
prob = function(x, a, b){
  return (exp((a * x) + b))
  # return ((a*x**2) + (b*x) + c)
}

propose.vals = function(guess){
  a = guess[1]; b = guess[2];
  a = a + runif(1, -.005, .005);
  b = b + runif(1, -1, 1);
  return (c(a, b))
}

calc.llk = function(guess, x, y, z){
  a = guess[1]; b = guess[2]
  ps = prob(x, a, b)
  binoms = dbinom(y, z, ps, log=TRUE)
  llk = sum(binoms)
  if (is.na(llk)){
    return (-Inf)
  } else {
    return (llk)
  }
}

stochastic.search = function(initial.guess, num.iterations, y, z){
  
  T = num.iterations
  a = initial.guess[1]; b = initial.guess[2]
  results = matrix(0,nrow=T,ncol=3)
  results[1,] = c(a, b, -Inf)
  for (t in 2:T){
    
    new.guess = propose.vals(results[t-1,])
    new.llk = calc.llk(new.guess, x, y, z)
    
    if (new.llk > results[t-1, 3]){
      results[t,] <- c(new.guess, new.llk)
    } else
    {
      results[t,] <- results[t-1,]
    }
  }
  return (results[T,])
}

# Initial Guesses
# a = .00005 # for linear p
# b = 0
a = .07 # for exponential
b = -143
# a = .0000007 # for quadratic
# b = -.001342
# c = -.05
initial.guess = c(a, b)

# Plot initial guess
#y[13] = 0; y[20] = 0; y[22]=0;
par(mfrow=c(1,2))
plot(x, actual.ps, xlab="Year", ylab="P of Good Game Produced", main="Binomial Probability")
ps = prob(x, a, b)
pred = ps * z
points(x, ps, type='l', col="red")

mle = stochastic.search(initial.guess, 10000, y, z)

# Check results of stochastic search
ps = prob(x, mle[1], mle[2])
y.pred = ps * z
points(x, ps, type='l', col="blue")
legend(1980, .1, c("Initial Estimate", "Stochastic Search Result"),fill=c("red", "blue"), cex=.6)

# Fraction of unexplained variation
residuals = y-y.pred
TSS<- sum((y-mean(y))^2)
RSS<- sum((residuals)^2)
R.2.mle <- 1-RSS/TSS  # .9906

residuals = ps-actual.ps
TSS<- sum((actual.ps-mean(actual.ps))^2)
RSS<- sum((residuals)^2)
R.2.mle.ps <- 1-RSS/TSS  # .8675

# Plot predicted counts
plot(x, y, xlab="Year", ylab="Good Games Produced", main="Count")
points(x, y.pred, type='l', col='blue')
points(x, pred, type='l', col='red')
legend(1980, 140, c("Initial Estimate", "Stochastic Search Result"),fill=c("red", "blue"), cex=.6)

# Plot residuals
plot(x, residuals, xlab="Year", ylab="Residual")

# Calculate log probability of seeing a greater residual - check for outliers
p.tail = pnorm(abs(residuals), mean(residuals), sd(residuals), log=TRUE, lower.tail=FALSE)
plot(x, p.tail)
outliers = x[p.tail < -5]  # 2016, 2017

# observed and theoretical quantiles for probs
s <- seq(0.01,0.99,by=0.01)
obs.q <- quantile(actual.ps, s)
the.q <-  quantile(ps, s)

# QQ-Plot - probabilities
plot(the.q,obs.q,col=grey(0.5,0.5),pch=20,cex=1.5,
     xlab="Model quantiles",ylab="Observed quantiles", main="Probability Model")
soft.red <- rgb(0.9,0.1,0.1,0.5)
abline(a=0,b=1,col=soft.red,lwd=2)

# observed and theoretical quantiles for counts
s <- seq(0.01,0.99,by=0.01)
obs.q <- quantile(y, s)
the.q <-  quantile(y.pred, s)

# QQ-plot - predicted counts
plot(the.q,obs.q,col=grey(0.5,0.5),pch=20,cex=1.5,
     xlab="Model quantiles",ylab="Observed quantiles", main="Count Model")
soft.red <- rgb(0.9,0.1,0.1,0.5)
abline(a=0,b=1,col=soft.red,lwd=2)



# Bootstrap the model to get confidence intervals
# M = 1000
# N = length(year)
# storage <- matrix(nrow=M, ncol=(2))
# for (i in 1:M) {
#   positions <- sample(1:N, N, replace=TRUE)
#   bs.years = year[positions]
#   bs.status = binary_game_status[positions]
#   bs.good.years = bs.years[bs.status == "Good Game" & bs.years >= start_year & bs.years <= end_year]
#   bs.counts = table(factor(bs.good.years, levels=start_year:end_year))
#   bs.y = as.vector(bs.counts)
#   bs.z = as.vector(table(bs.years[bs.years>=start_year & bs.years<=end_year]))
# 
#   bs.mle = stochastic.search(initial.guess, 10000, bs.y, bs.z)
#   storage[i, 1] <- bs.mle[1]
#   storage[i, 2] <- bs.mle[2]
# }
# 
# cis <- apply(storage, 2, function(x) quantile(x, probs=c(0.005, 0.995)))
# # .0658-.0731 and -149.633--135.026
