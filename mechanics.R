# Hypothesis for different themes essentially having no effect
library(corrplot)
library(ggplot2)

# Prep data
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
mechanics <- read.csv("bgg/mechanics.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")

score = games$AvgRating
year = games$YearPublished
start_year = 1977
end_year = 2017

# Group mechanics
resource.management <- c("Auction.Bidding", "Hand.Management", "Investment", "Market", "Stock.Holding", "Trading", "Income", "Commodity.Speculation", "Resource.to.Move", "Ownership", "Loans", "Automatic.Resource.Growth")
territory.control <- c("Area.Majority...Influence", "Enclosure", "Zone.of.Control", "Grid.Coverage", "King.of.the.Hill")
randomness <- c("Dice.Rolling", "Push.Your.Luck", "Random.Production", "Roll...Spin.and.Move", "Re.rolling.and.Locking")
cooperative <- c("Trading", "Alliances", "Cooperative.Game", "Team.Based.Game", "Semi.Cooperative.Game", "Single.Loser.Game")
anti.cooperative <- c("Race", "Player.Elimination", "Take.That", "Campaign...Battle.Card.Driven", "Traitor.Game", "Kill.Steal", "Prisoner.s.Dilemma")
player.interaction <- c("Alliances", "Negotiation", "Trading", "Bribery", "Voting", "Player.Judge")
anti.interaction <- c("Solo...Solitaire.Game", "Communication.Limits", "Prisoner.s.Dilemma")
building <- c("Network.and.Route.Building", "Investment", "Worker.Placement", "TableauBuilding", "Deck.Construction", "Deck..Bag..and.Pool.Building", "Connections")
secrets <- c("Betting.and.Bluffing", "Hidden.Roles", "Hidden.Movement", "Hidden.Victory.Points", "Roles.with.Asymmetric.Information", "Traitor.Game") 

mech.groups <- list(resource.management, territory.control, randomness, cooperative, anti.cooperative, player.interaction, anti.interaction, building, secrets)
mech.group.names <- c("resource.management", "territory.control", "randomness", "cooperative", "anti.cooperative", "player.interaction", "anti.interaction", "building", "secrets")

# group the games into themes
games_from_themes = function(theme_titles){
  return (which(unname(rowSums(mechanics[, theme_titles])) > 0))
}

resource.management.games <- games_from_themes(resource.management)
territory.control.games <- games_from_themes(territory.control) # 3426
randomness.games <- games_from_themes(randomness) # 4705
cooperative.games <- games_from_themes(cooperative) # 1895
anti.cooperative.games <- games_from_themes(anti.cooperative) # 3231
player.interaction.games <- games_from_themes(player.interaction) # 2637
anti.interaction.games <- games_from_themes(anti.interaction) # 2754
building.games <- games_from_themes(building) # 1508
secrets.games <- games_from_themes(secrets) # 1508

game.groups = list(resource.management.games, territory.control.games, randomness.games, cooperative.games,
                   anti.cooperative.games, player.interaction.games, anti.interaction.games,
                   building.games, secrets.games)

# Find pairwise p values for null distribution: test stat is dif of means
p.vals = matrix(nrow=9, ncol=9)
alpha = .01
means = matrix(nrow=1, ncol=9)
for (i in 1:9) {
  group.games = unlist(game.groups[i])
  group.scores = score[group.games]
  m.group = mean(group.scores)
  means[i] = m.group
  sd.group = sd(group.scores)
  for (j in 1:9) {
    
    other.group.games = unlist(game.groups[j])
    other.group.scores = score[other.group.games]
    m.other = mean(other.group.scores)
    sd.other = sd(other.group.scores)
    
    test.stat = abs(m.group - m.other)
    null.sd = sqrt((sd.group**2)/length(group.scores) + (sd.other**2)/length(other.group.scores))
    p.vals[i, j] = pnorm(test.stat, 0, null.sd, lower.tail = FALSE) * 2
  }
  hist(group.scores, breaks=50, xlim=c(0,10), main=i)
  select.years = year[year>start_year & year<end_year]
  # hist(select.years[group.games], breaks=50, main=i)
}

# Show results in corrplot format
colnames(p.vals) = mech.group.names
rownames(p.vals) = mech.group.names
corrplot(p.vals, method = "circle", sig.level=alpha, tl.col="black")

# Check out differences in model for significantly different groups
good.cutoff = 7.62
for (i in 1:9){
  group.games = unlist(game.groups[i])
  group.scores = score[group.games]
  group.years = year[group.games]
  year_bins = cut(group.years, breaks=seq(start_year, end_year, by=1), include.lowest=TRUE)
  binary_game_status = cut(group.scores, breaks=c(0, good.cutoff, 10), include.lowest=TRUE, labels=FALSE)
  binary_game_status = factor(binary_game_status, levels = c(2, 1), labels = c("Good Game", "Bad Game"))
  binary_df = data.frame(year_bins, binary_game_status)
  
  p = ggplot(binary_df, aes(x=year_bins, fill=binary_game_status)) + 
    geom_bar(position='fill') +
    scale_fill_manual(values=c("#00f0d1","#eC4E2A"))
  print(p)
}
# 
# binary_game_status = cut(score, breaks=c(0, good.cutoff, 10), include.lowest=TRUE, labels=FALSE)
# binary_game_status = factor(binary_game_status, levels = c(2, 1), labels = c("Good Game", "Bad Game"))
# binary_df = data.frame(year_bins, binary_game_status)
# 
# ggplot(binary_df, aes(x=year_bins, fill=binary_game_status)) + 
#   geom_bar(position='fill') +
#   scale_fill_manual(values=c("#00f0d1","#eC4E2A"))


