library(corrplot)
library(dplyr)

# Read in Data
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
  # id, name, description, rating, best num players, weight (with num Votes), 
  # num_owned, playtime, kickstarted, ranks, categories
mechanics <- read.csv("bgg/mechanics.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
  # game, booleans for each mechanic
# ratings <- read.csv("bgg/user_ratings.csv", sep=",", header=TRUE, fill=TRUE)
  # game, rating, user, 18,942,215 entries
themes <- read.csv("bgg/themes.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
  # game, then boolean for each theme
subcats <- read.csv("bgg/subcategories.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")

# Artist and Designer sets are Game Id by Name with booleans
id = games$BGGId
score = games$AvgRating
name = games$Name
year = games$YearPublished
weight = games$GameWeight # 1-5 scale
kickstarted = games$Kickstarted
best_num_players = games$BestPlayers
owned = games$NumOwned
age_rec = games$ComAgeRec

# Try pairwise plots
numeric_cols <- games %>% select_if(is.numeric) %>% colnames()
pdf("pairs_plot.pdf", width=12, height=10)
pairs(games[c("YearPublished", "GameWeight", "AvgRating", "BestPlayers", "ComAgeRec", "MfgPlaytime", "NumUserRatings", "Kickstarted", "NumComments")])
dev.off()
# Some exploratory plots
plot(year, score) # Shows that lots of 0's probably filled 
hist(year)

png('weight_vs_rating.png')
plot(score[weight>0], weight[weight>0], xlab="Rating ", ylab="Weight", col=rgb(.1,0,.8,1/4))
dev.off()
hist(weight)

plot(score, best_num_players)
hist(best_num_players)

plot(score, owned)
hist(owned)

plot(score, age_rec)
hist(age_rec)

outlier_years = name[year == 0]
best_sellers = name[owned > 50000]

# Idea: plot histogram of ratings for games grouped by mechanic
num_mechanics = ncol(mechanics)
correlations = matrix(0,nrow=num_mechanics,ncol=1)
ids = mechanics$BGGId
for (mechanic in 1:num_mechanics) {
  col = mechanics[, mechanic]
  mechanic.scores = score[col == 1]
  hist(mechanic.scores, breaks=50, main=colnames(mechanics)[mechanic])
  correlations[mechanic, 1] = cor(col, score)
}

# Explore Mechanic Data Set
percol = colSums(mechanics) # Max 6486 (Dice.Rolling), min 0, mean 433
overlapping = rowSums(mechanics) # Max 20, min 0, mean 3.1

# Kickstarter vs Without
kickstarted.scores = score[kickstarted == 1]
non.kickstarted.scores = score[kickstarted == 0]
set.seed(42)
p1 = hist(kickstarted.scores, col=rgb(0,0,1,1/4), xlim=c(2,10), freq=FALSE, breaks=50, main="Kickstarter Effect on Ratings")
p2 = hist(non.kickstarted.scores, col=rgb(1,0,0,1/4), freq=FALSE, breaks=50, add=TRUE)
legend(2, .5, c("Kickstarted", "Not Kickstarted"),fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), cex=.6)


# Start with time and then theme as space
# Measure correlation
# Bivariate plot with two humps to look for separation
# Break apart by controversial? popularity?
# Use reviews because less sparse
# What makes a good game?


# Each game can have multiple, most are roughly 0-4 max is 12 themes ]
# However there is likely overlap that is not counted 
  # Ex: fighting, world.war.II, modern.warfare are all separate
theme.totals = sort(colSums(themes))
game.theme.totals = rowSums(themes)
total.themes = sum(unname(theme.totals))

# Subcategory may be a bit easier, only 10, most are in none max is 4
subcat.totals = sort(colSums(subcats))
game.subcat.totals = rowSums(subcats)
total.subcats = sum(unname(subcat.totals))

# define the themes
warfare = c("Fighting", "Civil.War", "Post.Napoleonic", 
            "World.War.II", "World.War.I", 
            "Napoleonic", "American.Indian.Wars",
            "American.Revolutionary.War", "Vietnam.War", 
            "American.Civil.War", "Korean.War", 
            "Theme_Colonial", "Theme_Gladiators", 
            "Theme_Nuclear.option", "Modern.Warfare", 
            "Theme_Mech.Warfar", "Theme_Battle.Royal",
            "Theme_Knights.Templar", "Theme_French.Foreign.Legion")

historical = c("Civilization", "Age.of.Reason", "Renaissance", "Medieval", "Ancient",
               "Post.Napoleonic", "Prehistoric", "Napoleonic", "Political",
               "Theme_Colonial", "Theme_Archaeology...Paleontology", 
               "Theme_King.Arthur...The.Knights.of.the.Round.Table...Camelot",
               "Theme_Aztecs", "Theme_Mayans", "Theme_Silk.Road", 
               "Theme_Spanish.Political.Games", "Theme_Hanseatic.Leagu")

sci_fi_and_fantasy <- c("Fantasy", "Science.Fiction", "Space.Exploration", 
                                 "Comic.Book...Strip", "Theme_Robots", "Theme_Time.Travel",
                                 "Theme_Steampunk", "Theme_Cyberpunk","Theme_Alchemy", 
                                 "Theme_Fictional.Games", "Theme_UFOs", 
                                 "Theme_Sci.Fi.Sports", "Theme_Psychic.Powers")

science_and_knowledge = c("Medical", "Age.of.Reason", "Number", "Trivia", "Math",
                          "Theme_Art", "Theme_Mining", "Theme_Biology", "Theme_Weather",
                          "Theme_Books...Libraries", "Theme_Evolution", 
                          "Theme_Climate.Chang", "Theme_Psychology", "Theme_Scienc",
                          "Theme_Chemistry", "Theme_Astronomy", 
                          "Theme_School...College...University", 
                          "Theme_Computer...Information.Technology.Industry", 
                          "Theme_Journalis", "Theme_Teaching.Programming", 
                          "Theme_Ecology", "Theme_Flags.identification", 
                          "Political")

adventure_and_exploration <- c("Adventure", "Transportation", "Space.Exploration",
                               "American.West", "Nautical", "Travel", "Trains",
                               "Aviation...Flight", "Theme_Tropical", "Theme_Submarines",
                               "Theme_Amusement.Parks...Theme.Parks", 
                               "Theme_Airships...Blimps...Dirigibles...Zeppelins",
                               "Theme_Safaris", "Theme_Trucks", "Theme_Hot.Air.Balloons",
                               "Theme_Motorcycles", "Theme_Helicopters")

nature_and_animals <- c("Environmental", "Animals", "Nautical", "Farming",
                        "Theme_Anthropomorphic.Animals", "Theme_Archaeology...Paleontology",
                        "Theme_Deserts", "Theme_Tropical", "Theme_Gardening",
                        "Theme_Flowers", "Theme_Natur", "Theme_Biology", "Theme_Weather",
                        "Theme_Tropical.Islands", "Theme_Safaris", "Theme_Evolution",
                        "Theme_Animal.Battles", "Theme_Climate.Chang", "Theme_Beaches",
                        "Theme_Under.the.Sea", "Theme_Trees.and.Forests", 
                        "Theme_US.National.Parks", "Theme_Volcanoes", "Theme_Earthquakes",
                        "Theme_Mushrooms", "Theme_Hike.Hiking", "Theme_Camping",
                        "Theme_Geocaching", "Theme_Ecology", "Theme_Rivers")

building_and_economics = c("Economic", "Industry...Manufacturing", "Transportation",
                           "Civilization", "Farming", "City.Building", "Theme_Colonial",
                           "Theme_Mining", "Theme_Gardening", "Theme_Construction",
                           "Theme_City", "Theme_Oil...Gas...Petroleu", "Theme_Silk.Road",
                           "Theme_Automotive.Industry")

cultural = c("Mythology", "Ancient", "Arabian", "American.Indian.Wars", "Theme_Cthulhu.Mythos",
             "Theme_Vikings", "Theme_Colonial", "Theme_Native.Americans...First.Peoples",
             "Theme_Ninjas", "Theme_Samurai", "Theme_Aztecs", "Theme_Mayans",
             "Theme_Templ", "Theme_African.Americans", "Theme_Tiki.Cultur", 
             "Theme_Māori", "Theme_Inuit.Peoples", "Theme_Apache.Tribes")

categories = list(warfare, historical, sci_fi_and_fantasy, science_and_knowledge,
               adventure_and_exploration, nature_and_animals, building_and_economics,
               cultural)

# group the games into themes
games_from_themes = function(theme_titles){
  return (which(unname(rowSums(themes[, theme_titles])) > 0))
}

warfare.games <- games_from_themes(warfare) # 4520
historical.games <- games_from_themes(historical) # 3426
sci_fi_and_fantasy.games <- games_from_themes(sci_fi_and_fantasy) # 4705
science_and_knowledge.games <- games_from_themes(science_and_knowledge) # 1895
adventure_and_exploration.games <- games_from_themes(adventure_and_exploration) # 3231
nature_and_animals.games <- games_from_themes(nature_and_animals) # 2637
building_and_economics.games <- games_from_themes(building_and_economics) # 2754
cultural.games <- games_from_themes(cultural) # 1508

game.groups = list(warfare.games, historical.games, sci_fi_and_fantasy.games, science_and_knowledge.games,
                  adventure_and_exploration.games, nature_and_animals.games, building_and_economics.games,
                  cultural.games)
group.titles = c("Warfare", "Historical", "Sci-Fi and Fantasy", "Knowledge-Based", 
                 "Adventure and Exploration", "Nature and Animals", "Building and Economics",
                 "Cultural")

# Total games per cat, word cloud based on number of games per sub cat
make_word_cloud = function(theme_titles){
  library(wordcloud)
  counts = colSums(themes[,theme_titles])
  wordcloud(theme_titles, counts, random.order=FALSE, colors=brewer.pal(8,"Dark2"), rot.per=0)
}

par(mfrow=c(2,4))
for (i in 1:length(categories)){
  make_word_cloud(categories[[i]])
}

# Use theme to divide space
num_themes = length(game.groups)
cat_corrs = matrix(0,nrow=num_themes,ncol=1)
palette = paste(brewer.pal(8, "Set1"), "40", sep = "")
par(mfrow=c(2,4))
for (group in 1:num_themes) {
  group.scores = score[game.groups[[group]]]
  color = sample(palette, 1)
  hist(group.scores, breaks=50, freq=FALSE, col=color, xlim=c(1,10), main=group.titles[group], xlab="Rating")
}


# Try for subcats
num_subcats = 10
for (cat in 1:num_subcats){
  cat.scores = score[subcats[cat]==1]
  hist(cat.scores, breaks = 50, xlim=c(1,10), main=colnames(subcats)[cat])
}


# Make some traces across themes
num_themes = length(game.groups)
cat_corrs = matrix(0,nrow=num_themes,ncol=1)
group.names = c("warfare", "historical", "sci-fi and fantasy", "science and knowledge",
               "adventure and exploration", "nature and animals", "building and economics",
               "cultural")

# Red to green scatter plots
color.map = colorRampPalette(c("#ff0010", "#fdd49e", "#0fff50"))(10)
for (group in 1:num_themes) {
  select.games = game.groups[[group]]
  group.scores = score[select.games]
  group.weights = weight[select.games]
  group.years = year[select.games]
  colors = color.map[findInterval(group.scores, seq(2, 8))]
  plot(group.years[group.years>1900], group.weights[group.years>1900],col=colors, main=group.names[group])
}

plot(year[year>1950], weight[year>1950], col = color.map[findInterval(score[year>1950], seq(2, 8))], main="All")


# Check exponential production rate
hist(year[year>1950], breaks = 70)


# Segment and use stacked bar plot
library(ggplot2)
library(scales)
library(patchwork)
breaks = seq(1950, 2020, by=1)
year_bins = cut(year, breaks=breaks, include.lowest=TRUE)
score_bins = cut(score, breaks=seq(0,10), labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), include.lowest = TRUE)
score_bins = factor(score_bins, levels = rev(levels(score_bins)))
binned_df = data.frame(year_bins, score_bins)
binned_df$year = as.Date(as.character(year), format="%Y")
clean = binned_df[complete.cases(binned_df$year_bins), ]

plot1 = ggplot(clean, aes(x=year, fill=score_bins)) + 
  geom_bar(width=350) +
  scale_fill_manual(values=c("#a0ffff", "#00f0d1", "#00B24C", "#ffffaf", "#fff03C", "#ffd00f", "#f09d3C", "#eC4E2A", "#ff1010", "#ff00af")) +
  scale_x_date(labels=date_format("%Y"))  + 
  xlab("Year") + 
  ylab("Number of Games")

plot2 = ggplot(clean, aes(x=year, fill=score_bins)) + 
  geom_bar(width=350, position="fill") +
  scale_fill_manual(values=c("#a0ffff", "#00f0d1", "#00B24C", "#ffffaf", "#fff03C", "#ffd00f", "#f09d3C", "#eC4E2A", "#ff1010", "#ff00af")) +
  scale_x_date(labels=date_format("%Y"))  + 
  xlab("Year") + 
  ylab("Fraction of Games")

plot1 + plot2 + plot_layout(ncol = 2)

# Break down score brackets by group
for (s in sort(unique(score_bins))){
  select_games = which(score_bins == s)
  theme_counts = lapply(game.groups, function(g) sum(g %in% select_games))
  temp_data = data.frame(theme_group = group.names, count = unlist(theme_counts))
  temp_data$theme_group = factor(temp_data$theme_group)  # convert to factor
  temp_data$count = temp_data$count / sapply(game.groups, function(g) length(g))
  print(ggplot(temp_data, aes(x = theme_group, y = count, fill = theme_group)) + 
    geom_bar(stat="identity") +
    ggtitle(paste("Games with score bracket:", s)))
}

# Group cleaned data into usable csv
merged_data = data.frame(year, weight, best_num_players, age_rec, games$MfgPlaytime, score)
merged_data = na.omit(merged_data[year != 0 & !is.na(age_rec), ])
write.csv(merged_data, file="bgg/cleaned_data.csv", row.names=FALSE)

mechanic_dataset = cbind(mechanics, score)
write.csv(mechanic_dataset, file="bgg/mechanic_cleaned_data.csv", row.names=FALSE)


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

# Combine the vectors into a list
mech.groups <- list(resource.management, territory.control, randomness, cooperative, anti.cooperative, player.interaction, anti.interaction, building, secrets)

# Create a vector of the concatenated column names in each category
mech.group.names <- c("resource.management", "territory.control", "randomness", "cooperative", "anti.cooperative", "player.interaction", "anti.interaction", "building", "secrets")

par(mfrow=c(3,3), mar=c(0,0,0,0))

make_word_cloud = function(mechanic_titles){
  library(wordcloud)
  counts = colSums(mechanics[,mechanic_titles])
  wordcloud(mechanic_titles, counts, random.order=FALSE, colors=brewer.pal(8,"Dark2"), rot.per=0, scale=c(1,.8))
}

# Word cloud mech groups
for (i in 1:length(mech.group.names)){
  make_word_cloud(mech.groups[[i]])
}

# Hist mechanic groups
num_groups = length(mech.group.names)
mech.group.corrs = matrix(0,nrow=num_groups,ncol=1)
for (group in 1:num_groups) {
  select.games = rowSums(mechanics[mech.groups[[group]]]) > 0
  group.scores = score[select.games]
  mech.group.corrs = cor(score, select.games)
  hist(group.scores, breaks=50, freq=FALSE, xlim=c(1,10), main=mech.group.names[group])
}

# See which mechanics are post popular in popular games
sort(colSums(mechanics[score>8, ]))
# see whats the probability of seeing that many games in the top given its distribution?

# Need to look at subset
some.listed = rowSums(mechanics) > 0
most.common.mechanics = names(sort(colSums(mechanics), decreasing = TRUE)[1:10])
most.common = mechanics[, most.common.mechanics]
corrplot(cor(most.common, use="complete"))
cors = cor(mechanics)
max_cor <- max(cors[cors < 1])
which(cors == max_cor, arr.ind = TRUE)
rough.model = lm(scale(score[some.listed])~as.matrix(scale(mechanics[some.listed, ])))
# many 'significant' coefficients not sure exactly what that means in this context
# R^2 is .2964 so 29% or variation is explained, maybe pull in theme and other factors?
# Maybe its just not linear?
# Try logistic regression to above average or below average game? or is that hacky

# Check out ratings dist
hist(ratings$Rating)
hist(games$StdDev, breaks=50, freq=FALSE)
x = seq(0, 4, by=.01)
points(x, dnorm(x, mean(games$StdDev), sd(games$StdDev)), type='l')
# Could break into controversial games that have stdev > threshold
# What would threshold be? arbitrary more or less?

controversial.games = games$StdDev > 3
hist(games$AvgRating[controversial.games])
controversial.ratings = controversial.games[ratings$BGGId]
hist(ratings$Rating[controversial.ratings])

# Notes:
# Flush out EDA then full meeting for modeling
# Check out mechanics
# Research why the fall off - looks like lag in data set refer to screenshot from forumn

# Do I look into ratings distribution per game 10's vs 1's average out to normal, but are they?
# Introduce binary like good vs bad look outside of 90% of games in normal


