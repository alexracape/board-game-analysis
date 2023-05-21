# Script for modeling quantity of games produced over time
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
score = games$AvgRating
year = games$YearPublished

# Select data range (inclusive)
start_year = 1950 # Not much data before this, very sparse kind of arbitrary right now
end_year = 2017   # Three years back to account for lag effect

# Get our principal variables from the data
select_years = year[year>=start_year & year<=end_year]
year_counts = table(select_years)
x = start_year:end_year
y = as.vector(year_counts)

# Number of games produced per year
log_year_counts = log(year_counts)
plot(log_year_counts, type="p")
log_lm = lm(log_year_counts~as.numeric(names(log_year_counts)))
abline(log_lm)
lm_residuals = log_lm$residuals  # Could come back and do qq plot


# Model using poisson with exponential rate of occurrence
lambda = function(x, a, b){
  return (exp((a * x) + b))
}

propose.vals = function(guess){
  a = guess[1]; b = guess[2];
  a = a + runif(1, -.0005, .0005);
  b = b + runif(1, -1, 1);
  return (c(a, b))
}

calc.llk = function(guess, x, y){
  a = guess[1]; b = guess[2];
  lambdas = lambda(x, a, b)
  poiss = dpois(y, lambdas, log=TRUE)
  llk = sum(poiss)
  return (llk)
}

stochastic.search = function(inital.guess, num.iterations, y){

  T = num.iterations
  a = initial.guess[1]
  b = initial.guess[2]
  results = matrix(0,nrow=T,ncol=3)
  results[1,] = c(a, b, -Inf)
  for (t in 2:T){
    
    new.guess = propose.vals(results[t-1,])
    new.llk = calc.llk(new.guess, x, y)
    
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
a = .05
b = -94
initial.guess = c(a, b)

# Plot inital guess
plot(x, y, xlab="Year", ylab="Games Produced")
lambdas = lambda(x, a, b)
points(x, lambdas, type='l', col="red")

# Check results of stochastic search
mle = stochastic.search(initial.guess, 100000, y)
lambdas = lambda(x, mle[1], mle[2])
points(x, lambdas, type='l', col="blue")
legend(1950, 1000, c("Initial Estimate", "Stochastic Search Result"),fill=c("red", "blue"), cex=.6)

# Fraction of unexplained variation
y.pred = lambdas
residuals = y-y.pred
TSS<- sum((y-mean(y))^2)
RSS<- sum((residuals)^2)
R.2.mle <- 1-RSS/TSS  # .983

# Plot the residuals
plot(x, residuals, xlab="Year", ylab="Residual")

# Calculate log probability of seeing a greater residual - check for outliers
p.tail = pnorm(abs(residuals), mean(residuals), sd(residuals), log=TRUE, lower.tail=FALSE)
plot(x, p.tail)
outliers = x[p.tail < -5]  # 2015, 2016, 2017

# observed and theoretical quantiles
s <- seq(0.01,0.99,by=0.01)
obs.q <- quantile(y, s)
the.q <-  quantile(y.pred, s)

# QQ-plot
plot(the.q,obs.q,col=grey(0.5,0.5),pch=20,cex=1.5,
     xlab="Model quantiles",ylab="Observed quantiles")
soft.red <- rgb(0.9,0.1,0.1,0.5)
abline(a=0,b=1,col=soft.red,lwd=2)

# Bootstrap the model to get confidence intervals
M = 100
N = length(year)
storage <- matrix(nrow=M, ncol=(2)) # create a matrix to store the coefficients
for (i in 1:M) {
  positions <- sample(1:N, N, replace=TRUE)
  bs.years = year[positions]
  bs.select_years = bs.years[bs.years>=start_year & bs.years<=end_year]
  bs.year_counts = table(bs.select_years)
  bs.y = as.vector(bs.year_counts)

  bs.mle = stochastic.search(initial.guess, 10000, bs.y)
  storage[i, 1] <- bs.mle[1]
  storage[i, 2] <- bs.mle[2]
}

cis <- apply(storage, 2, function(x) quantile(x, probs=c(0.005, 0.995)))
# a = .0742 [.0625, .0754]
# b = -142.1 [-145.1, -119.2]


