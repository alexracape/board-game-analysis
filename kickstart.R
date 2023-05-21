# Hypothesis test for kickstarted games - is it a different distribution?
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")

score = games$AvgRating
year = games$YearPublished
weight = games$GameWeight # 1-5 scale
kickstarted = games$Kickstarted

# Kickstarter vs Without
kickstarted.scores = score[kickstarted == 1]
non.kickstarted.scores = score[kickstarted == 0]
set.seed(42)
p1 = hist(kickstarted.scores, col=rgb(0,0,1,1/4), xlim=c(2,10), freq=FALSE, main="", breaks=50, xlab="Rating")
p2 = hist(non.kickstarted.scores, col=rgb(1,0,0,1/4), freq=FALSE, breaks=50, add=TRUE)
legend(2, .5, c("Kickstarted", "Not Kickstarted"),fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), cex=.6)

# Bootstrap to get 99% confidence mean and sd, and check if new dist is in range
M = 10000
N = length(score)
storage <- matrix(nrow=M, ncol=(4)) # create a matrix to mean and sd for dist of mean
for (i in 1:M) {
  positions <- sample(1:N, N, replace=TRUE)
  bs_scores = score[positions]
  bs_kick = kickstarted[positions]
  bs_kickstarted_scores = bs_scores[bs_kick == 1]
  bs_non_kickstarted_scores = bs_scores[bs_kick == 0]
  storage[i, 1] <- mean(bs_kickstarted_scores)
  storage[i, 2] <- mean(bs_non_kickstarted_scores)
  storage[i, 3] <- sd(bs_kickstarted_scores)
  storage[i, 4] <- sd(bs_non_kickstarted_scores)
}
cis <- apply(storage, 2, function(x) quantile(x, probs=c(0.005, 0.995)))

# Hypothesis test
m.kick = mean(kickstarted.scores)
m.non.kick = mean(non.kickstarted.scores)
sd.kick = sd(kickstarted.scores)
sd.non.kick = sd(non.kickstarted.scores)

test.stat = m.kick - m.non.kick
null.sd = sqrt((sd.kick**2)/length(kickstarted.scores) + (sd.non.kick**2)/length(non.kickstarted.scores))
p.value = pnorm(test.stat, 0, null.sd, lower.tail = FALSE)
# 0, not enough precision I guess to show, maybe check
