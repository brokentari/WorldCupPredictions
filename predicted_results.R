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


# Load the results of the simulation program
             
if(file.exists("simresults.csv")) simresults <- read_csv("simresults.csv")
    
head(simresults, 20)

# plot the winners

winsperteam <- simresults %>%
  dplyr::group_by(winner) %>%
  dplyr::summarize(
    wins = n()
  ) %>%
  dplyr::arrange(desc(wins)) 

winsperteam$winner <- factor(winsperteam$winner, levels = winsperteam$winner[order(winsperteam$wins, decreasing = FALSE)])

ggplot(winsperteam, mapping = aes(x=winner, y=wins)) +
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(label=paste(wins / 100, "%")), vjust=0.3, hjust=-0.1, size=2.1) +
  ggtitle("Tournament simulation winners (10,000 iterations)")

  # calculate the sports odds 

winsperteam$odds <- lengths(simresults) / winsperteam$wins
writeLines(paste(winsperteam$winner, ": ",round(winsperteam$odds), " to 1\n"))