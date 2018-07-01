# prepare the R environment
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,            # Data munging functions
  zoo,              # Feature engineering rolling aggregates
  data.table,       # Feature engineering
  ggplot2,          # Graphics
  scales,           # Time formatted axis
  readr,            # Reading input files
  stringr,          # String functions
  reshape2,         # restructure and aggregate data 
  randomForest,     # Random forests
  corrplot,         # correlation plots
  Metrics,          # Eval metrics for ML
  vcd               # Visualizing discrete distributions
)
    
# set options for plots
options(repr.plot.width=6, repr.plot.height=6)

# Load the matches data

if(!file.exists("matches.csv")){
    tryCatch(download.file('https://github.com/neaorin/PredictTheWorldCup/raw/master/input/matches.csv'
                           ,destfile="./matches.csv",method="auto"))
}
                
if(file.exists("matches.csv")) matches_original <- read_csv("matches.csv")
    


# eliminate any duplicates that may exist in the dataset# elimi 
matches <- matches_original %>%
  distinct(.keep_all = TRUE, date, team1, team2)

# the date field is formatted as a string (e.g. 19560930) - transform that into R date
matches$date <- as.POSIXct(strptime(matches$date, "%Y%m%d"), origin="1960-01-01", tz="UTC")

# generate an id column for future use (joins etc)
matches$match_id = seq.int(nrow(matches))


matches$switch = runif(nrow(matches), min = 0, max = 1)

matches <- bind_rows(
  matches %>% dplyr::filter(switch < 0.5),
  matches %>% dplyr::filter(switch >= 0.5) %>%
    dplyr::mutate(
      x_team2 = team2,
      team2 = team1,
      team1 = x_team2,
      
      x_team2Text = team2Text,
      team2Text = team1Text,
      team1Text = x_team2Text,

      x_resText = "",
      
      x_team2Score = team2Score,
      team2Score = team1Score,
      team1Score = x_team2Score,
      
      x_team2PenScore = team2PenScore,
      team2PenScore = team1PenScore,
      team1PenScore = x_team2PenScore
    ) %>%
    dplyr::select(
      date, team1, team1Text, team2, team2Text, resText, statText, venue, IdCupSeason, CupName, team1Score, team2Score, team1PenScore, team2PenScore, match_id, switch
    )
    ) %>% 
  dplyr::arrange(date) %>%
  dplyr::select(-c(switch))

summary(matches$team1Score - matches$team2Score)

# is the game played in a neutral venue
matches$team1Home <- mapply(grepl, pattern=matches$team1Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$team2Home <- mapply(grepl, pattern=matches$team2Text, x=matches$venue, MoreArgs = list(fixed = TRUE, ignore.case = FALSE))
matches$neutralVenue <- !(matches$team1Home | matches$team2Home)

# text-matching the venue is not 100% accurate.
# some games get TRUE for both team1 and team2 (ex. Congo DR vs Congo)
# in this case, team1 is at home
matches$team2Home[(matches$team1Home == TRUE) & (matches$team2Home == TRUE)] <- FALSE

# game type: Friendly, Qualifier, Final Tournament
matches$friendly <- FALSE
matches$friendly[matches$CupName == "Friendly"] <- TRUE

matches$qualifier <- FALSE
matches$qualifier[matches$CupName %like% "qual"] <- TRUE

matches$finaltourn <- FALSE
matches$finaltourn[matches$CupName %like% "final"] <- TRUE


matches <- matches %>% dplyr::filter(friendly == FALSE)

teamperf <- bind_rows(
    (matches %>%
    dplyr::mutate(
      name = team1,
      opponentName = team2,
      homeVenue = team1Home,
      neutralVenue = neutralVenue,
      gs = team1Score,
      ga = team2Score,
      gd = gs - ga,
      w = (team1Score > team2Score),
      l = (team1Score < team2Score),
      d = (team1Score == team2Score),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
    dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, friendly, qualifier, finaltourn))
    ,
    (matches %>%
    dplyr::mutate(
      name = team2,
      opponentName = team1,
      homeVenue = team2Home,
      neutralVenue = neutralVenue,
      gs = team2Score,
      ga = team1Score,
      gd = gs - ga,
      w = (team1Score < team2Score),
      l = (team1Score > team2Score),
      d = (team1Score == team2Score),
      friendly = friendly,
      qualifier = qualifier,
      finaltourn = finaltourn
    ) %>%
      dplyr::select (match_id, date, name, opponentName, homeVenue, neutralVenue, gs, ga, gd, w, l, d, friendly, qualifier, finaltourn))
  ) %>%
  dplyr::arrange(date)


# transform old country codes into new ones.
countryCodeMappings <- matrix(c(
  "FRG","GER",
  "TCH","CZE",
  "URS","RUS",
  "SCG","SRB",
  "ZAI","COD"
  ), ncol=2, byrow = TRUE)

for (i in 1:nrow(countryCodeMappings)) {
  teamperf$name[teamperf$name == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  teamperf$opponentName[teamperf$opponentName == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  
  matches$team1[matches$team1 == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
  matches$team2[matches$team2 == countryCodeMappings[i,1]] <- countryCodeMappings[i,2]
}


# what is the occurence frequency for match scores?# what i 

scorefreq <- matches %>%
  group_by(team1Score, team2Score) %>%
  summarise(
    n = n(),
    freq = n / nrow(matches)
  ) %>%
  ungroup() %>%
  mutate(
    scoretext = paste(team1Score,"-",team2Score)
  ) %>%
  arrange(desc(freq)) 



  # how many outliers do we have?
temp <- matches %>% dplyr::filter(abs(team1Score - team2Score) > 7)
paste(nrow(temp), "matches, or", (nrow(temp)/nrow(matches)*100), "% of total.")


# get rid of all the outliers by capping the gd to [-7, +7]# get ri 
teamperf$gd[teamperf$gd < -7] <- -7
teamperf$gd[teamperf$gd > +7] <- +7

# get information about the various FIFA confederations and the teams they contain
if(!file.exists("teams.csv")){
  tryCatch(download.file('https://raw.githubusercontent.com/neaorin/PredictTheWorldCup/master/input/teams.csv'
                         ,destfile="./teams.csv",method="auto"))
}

if(file.exists("teams.csv")) teams <- read_csv("teams.csv")

# confederations and adjustment coefficients for them
confederations <- as.data.frame(matrix(c(
  "UEFA","0.99",
  "CONMEBOL","1.00",
  "CONCACAF","0.85",
  "AFC","0.85",
  "CAF","0.85",
  "OFC","0.85"
), ncol=2, byrow = TRUE, dimnames = list(NULL, c("confederation","adjust"))), stringsAsFactors = FALSE)

confederations$confederation <- as.vector(confederations$confederation)
confederations$adjust <- as.numeric(confederations$adjust)

# add a confederation coefficient for the opponent faced 
teamperf <- teamperf %>%
  dplyr::left_join(teams, by=c("opponentName" = "fifa_code")) %>%
  dplyr::left_join(confederations, by=c("confederation")) %>%
  dplyr::mutate(
    opponentConfederationCoefficient = adjust
  ) %>%
dplyr::select(match_id, date, name = name.x, opponentName, opponentConfederationCoefficient,  homeVenue, neutralVenue, gs, ga, gd, w, l, d, friendly, qualifier, finaltourn)

# set missing values to 1
teamperf$opponentConfederationCoefficient[is.na(teamperf$opponentConfederationCoefficient)] <- 1

lagfn <- function(data, width) {
  return (rollapplyr(data, width = width + 1, FUN = sum, fill = NA, partial=TRUE) - data)
}

lagfn_per <- function(data, width) {
  return (lagfn(data, width) / width)
}

team_features <- teamperf %>%
  dplyr::arrange(name, date) %>%
  dplyr::group_by(name) %>%
  dplyr::mutate(
    last10games_w_per = lagfn_per(w, 10),
    last30games_w_per = lagfn_per(w, 30),
    last50games_w_per = lagfn_per(w, 50),

    last10games_l_per = lagfn_per(l, 10),
    last30games_l_per = lagfn_per(l, 30),
    last50games_l_per = lagfn_per(l, 50),

    last10games_d_per = lagfn_per(d, 10),
    last30games_d_per = lagfn_per(d, 30),
    last50games_d_per = lagfn_per(d, 50),
            
    last10games_gd_per = lagfn_per(gd, 10),
    last30games_gd_per = lagfn_per(gd, 30),
    last50games_gd_per = lagfn_per(gd, 50),
    last10games_opp_cc_per = lagfn_per(opponentConfederationCoefficient, 10),
    last30games_opp_cc_per = lagfn_per(opponentConfederationCoefficient, 30),
    last50games_opp_cc_per = lagfn_per(opponentConfederationCoefficient, 50)

  ) %>%
  dplyr::select (
    match_id, date, name, opponentName, gs, ga,
    w, last10games_w_per, last30games_w_per, last50games_w_per,
    l, last10games_l_per, last30games_l_per, last50games_l_per,
    d, last10games_d_per, last30games_d_per, last50games_d_per,
    gd, last10games_gd_per, last30games_gd_per, last50games_gd_per,
    opponentConfederationCoefficient, last10games_opp_cc_per, last30games_opp_cc_per, last50games_opp_cc_per

          ) %>%
  dplyr::ungroup()


# fold per-team features into per-match features
match_features <- matches %>%
  left_join(team_features, by=c("match_id", "team1" = "name")) %>%
  left_join(team_features, by=c("match_id", "team2" = "name"), suffix=c(".t1",".t2")) %>%
  dplyr::select(
    date, match_id, team1, team2, team1Home, team2Home, neutralVenue, team1Score, team2Score, friendly, qualifier, finaltourn,
    last10games_w_per.t1,
    last30games_w_per.t1,
    last50games_w_per.t1,
    last10games_l_per.t1,
    last30games_l_per.t1,
    last50games_l_per.t1,
    last10games_d_per.t1,
    last30games_d_per.t1,
    last50games_d_per.t1,
    last10games_gd_per.t1, 
    last30games_gd_per.t1,
    last50games_gd_per.t1,
    last10games_opp_cc_per.t1, 
    last30games_opp_cc_per.t1, 
    last50games_opp_cc_per.t1,
    last10games_w_per.t2,
    last30games_w_per.t2,
    last50games_w_per.t2,
    last10games_l_per.t2,
    last30games_l_per.t2,
    last50games_l_per.t2,
    last10games_d_per.t2,
    last30games_d_per.t2,
    last50games_d_per.t2,
    last10games_gd_per.t2, 
    last30games_gd_per.t2,
    last50games_gd_per.t2,
    last10games_opp_cc_per.t2, 
    last30games_opp_cc_per.t2, 
    last50games_opp_cc_per.t2,
    outcome = gd.t1
  )


# drop all non-interesting columns, and those which should not be supplied for new data (like scores)
match_features <- match_features %>%
  dplyr::select(-c(match_id,team1Score,team2Score))


# create the training formula 
trainformula <- as.formula(paste('outcome',
                                 paste(names(match_features %>% dplyr::select(-c(date,team1,team2,outcome))),collapse=' + '),
                                 sep=' ~ '))
trainformula

# training and testing datasets

data.train1 <- match_features %>% dplyr::filter(date < '2009/1/1')
data.test1 <- match_features %>% dplyr::filter(date >= '2009/1/1' & date <= '2015/1/1')

nrow(data.train1)
nrow(data.test1)

# train a random forest
model.randomForest1 <- randomForest::randomForest(trainformula, data = data.train1, 
                                                  importance = TRUE, ntree = 500)

summary(model.randomForest1)


data.pred.randomForest1 <- predict(model.randomForest1, data.test1, predict.all = TRUE)

metrics.randomForest1.mae <- Metrics::mae(data.test1$outcome, data.pred.randomForest1$aggregate)
metrics.randomForest1.rmse <- Metrics::rmse(data.test1$outcome, data.pred.randomForest1$aggregate)

paste("Mean Absolute Error:", metrics.randomForest1.mae)
paste("Root Mean Square Error:",metrics.randomForest1.rmse)

abs_error <- abs(data.test1$outcome - data.pred.randomForest1$aggregate)

if(!file.exists("wc2018qualified.csv")){
    tryCatch(download.file('https://raw.githubusercontent.com/neaorin/PredictTheWorldCup/master/src/TournamentSim/wc2018qualified.csv'
                           ,destfile="./wc2018qualified.csv",method="auto"))
}
                
if(file.exists("wc2018qualified.csv")) qualified <- read_csv("wc2018qualified.csv")

# get a list of possible matches to be played at the world cup

data.topredict <- expand.grid(team1 = qualified$name, team2 = qualified$name, stringsAsFactors = FALSE) %>% filter(team1 < team2)

temp <- teamperf %>%
  semi_join(qualified, by = c("name")) %>%
  group_by(name) %>%
  summarise(
    date = max(date)
  )

temp <- team_features %>%
  semi_join(temp, by = c("name", "date"))

# calculate the features for every possbile match

data.topredict <- data.topredict %>%
  left_join(temp, by = c("team1" = "name")) %>%
  left_join(temp, by = c("team2" = "name"), suffix = c(".t1", ".t2")) %>%
  dplyr::select(
    team1, team2,
    last10games_w_per.t1,
    last30games_w_per.t1,
    last50games_w_per.t1,
    last10games_l_per.t1,
    last30games_l_per.t1,
    last50games_l_per.t1,
    last10games_d_per.t1,
    last30games_d_per.t1,
    last50games_d_per.t1,
    last10games_gd_per.t1, 
    last30games_gd_per.t1,
    last50games_gd_per.t1,
    last10games_opp_cc_per.t1, 
    last30games_opp_cc_per.t1, 
    last50games_opp_cc_per.t1,
    last10games_w_per.t2,
    last30games_w_per.t2,
    last50games_w_per.t2,
    last10games_l_per.t2,
    last30games_l_per.t2,
    last50games_l_per.t2,
    last10games_d_per.t2,
    last30games_d_per.t2,
    last50games_d_per.t2,
    last10games_gd_per.t2, 
    last30games_gd_per.t2,
    last50games_gd_per.t2,
    last10games_opp_cc_per.t2, 
    last30games_opp_cc_per.t2, 
    last50games_opp_cc_per.t2      
  ) %>%
  mutate(
    date = as.POSIXct("2018-06-14"), 
    team1Home = (team1 == "RUS"), team2Home = (team2 == "RUS"), neutralVenue = !(team1Home | team2Home), 
    friendly = FALSE, qualifier = FALSE, finaltourn = TRUE
  )

# ask the model to predict our world cup matches
data.predicted <- predict(model.randomForest1, data.topredict, predict.all = TRUE)

# calculate the standard deviation of the individual predictions of each match

data.predicted$sd = apply(data.predicted$individual, c(1), sd)

# keep only the interesting columns for running tournament simulations
data.staticpred <- data.topredict %>% 
  dplyr::select(team1, team2)

data.staticpred$outcome = data.predicted$aggregate
data.staticpred$sd = data.predicted$sd


write_csv(data.staticpred, "wc2018staticPredictions.csv")