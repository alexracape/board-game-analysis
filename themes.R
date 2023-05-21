# Hypothesis for different themes essentially having no effect
library(corrplot)

# Prep data
games <- read.csv("bgg/games.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")
themes <- read.csv("bgg/themes.csv", sep=",", header=TRUE, fill=TRUE, row.names="BGGId")

score = games$AvgRating
year = games$YearPublished
start_year = 1977
end_year = 2017

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

theme.group.names = c("Warefare", "Historical", "Sci-fi / Fantasy", "Knowledge", "Adventure", "Nature", "Building", "Cultural")

# Find pairwise p values for null distribution: test stat is dif of means
p.vals = matrix(nrow=8, ncol=8)
alpha = .01
means = matrix(nrow=1, ncol=8)
for (i in 1:8) {
  group.games = unlist(game.groups[i])
  group.scores = score[group.games]
  m.group = mean(group.scores)
  means[i] = m.group
  sd.group = sd(group.scores)
  for (j in 1:8) {
    
    other.group.games = unlist(game.groups[j])
    other.group.scores = score[other.group.games]
    m.other = mean(other.group.scores)
    sd.other = sd(other.group.scores)

    test.stat = abs(m.group - m.other)
    null.sd = sqrt((sd.group**2)/length(group.scores) + (sd.other**2)/length(other.group.scores))
    p.vals[i, j] = pnorm(test.stat, 0, null.sd, lower.tail = FALSE) * 2
  }
  # hist(group.scores, breaks=50, xlim=c(0,10), main=i)
  select.years = year[year>start_year & year<end_year]
  hist(select.years[group.games], breaks=50, main=i)
}

# Show results in corrplot format
colnames(p.vals) = theme.group.names
rownames(p.vals) = theme.group.names
corrplot(p.vals, method = "circle", sig.level=alpha, tl.col="black")

# Check out differences in model for significantly different groups
good.cutoff = 7.62
for (i in 1:8){
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


