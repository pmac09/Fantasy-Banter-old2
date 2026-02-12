
library(fitzRoy)
library(cluster)
library(httr)
library(jsonlite)
library(factoextra)  

# import supercoach authentication variables
source('/Users/paulmcgrath/Github/Fantasy-Banter/functions/sc_functions_2025.R') 

# Connect to SC
sc <- sc_setup(cid,tkn)

# Player Data
playerList <- sc_players(sc)
playerStats <- bind_rows(lapply(playerList$playerID, function(p){s <- sc_playerStats(sc, p)}))

# Average Draft Position. 
playerADP <- lapply(playerList$playerID, function(p){
  data <- sc_download(sc$auth, sprintf(sc$url$player,p))
  adp <- data$player_stats[[1]]$adp
  adp <- ifelse(is.null(adp),NA,adp)
  return(adp)
})
names(playerADP) <- playerList$playerID
playerList$ADP <- unlist(playerADP)

## SC Stats - Player Ceiling ---------------------------------------------------

scStats <- playerStats %>%
  filter(season >= as.numeric(sc$var$season)-2) %>%
  filter(played == 1 & minutes_played > 57) %>% #Time of Ground exclusion #quantile(scStats$minutes_played, 0.90, na.rm = TRUE)/2
  
  
  

  group_by(season) %>%
  mutate(max_round = max(round)) %>%
  ungroup() %>%
  mutate(season_half = round(round/max_round,0)+1) %>%
  filter(played == 1 & minutes_played > 57) %>% #Time of Ground exclusion #quantile(scStats$minutes_played, 0.90, na.rm = TRUE)/2
  group_by(player_id, season, season_half) %>%
  summarise(
    n = n(),
    avg = mean(points, na.rm=T),
    .groups='drop'
  ) %>%
  filter(n >= 5) %>% #games minimum
  group_by(player_id) %>%
  summarise(
    maxAvg = max(avg, na.rm=T),
    consist = sd(avg)/mean(avg)
  ) 


## IMPORT DFS DATA -------------------------------------------------------------

url <- "https://dfsaustralia.com/wp-admin/admin-ajax.php"
headers <- add_headers(
  "accept" = "*/*",
  "accept-language" = "en-GB,en-US;q=0.9,en;q=0.8",
  "cache-control" = "no-cache",
  "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
  "pragma" = "no-cache",
  "priority" = "u=1, i",
  "sec-ch-ua" = "\"Google Chrome\";v=\"131\", \"Chromium\";v=\"131\", \"Not_A Brand\";v=\"24\"",
  "sec-ch-ua-mobile" = "?1",
  "sec-ch-ua-platform" = "\"Android\"",
  "sec-fetch-dest" = "empty",
  "sec-fetch-mode" = "cors",
  "sec-fetch-site" = "same-origin",
  "x-requested-with" = "XMLHttpRequest"
)

posList <- c('DEF','MID','RUC','FWD')
dfs_data <- tibble()

for (pos in posList){
  body <- list(action = "afl_supercoach_big_board_call", position = pos)
  body_encoded <- URLencode(paste(names(body), body, sep="=", collapse="&"))
  
  # Send the POST request
  response <- POST(url, headers, body = body_encoded, encode = "form")
  
  # Extract and print the response content
  content_text <- content(response, "text")
  content_list <- fromJSON(content_text)
  
  # transform to dataframe
  dfs_posData <- content_list$last %>%
    mutate(across(
      where(~ all(suppressWarnings(is.na(.) | !is.na(as.numeric(.))))),
      \(x) suppressWarnings(as.numeric(x))
    )) %>%
    mutate(dfsFilter = pos)
  
  # bind results 
  dfs_data <- bind_rows(dfs_data, dfs_posData)
}

dfs_data <-dfs_data %>%
  mutate(feedID = as.numeric(substr(playerId,5,999))) %>%
  group_by(playerId) %>%
  arrange(rankAdj) %>%
  filter(row_number() == 1)


write_csv(dfs_data, 'data.csv',na='')

## AFL FANTASY DRAFT TOOL ------------------------------------------------------

AFDT <- read.csv('./scripts/021 Draft Prep/2025/AFLFantasyDraftTool.csv') %>%
  select(ID_AFLFAN, Player, ADP, Risk, Notes) %>%
  rename(ADP_AFLfantasy = ADP)

## FINAL LIST  -----------------------------------------------------------------

playerList_cols <- c('feedID','playerName','teamAbbrev','pos','prevAvg','ADP')
dfs_cols <- c('feedID','age', 'dfsFilter', 'pricedAt', 'FPadj', 'FPfirst', 'FPsecond')
AFDT_cols <- c('ID_AFLFAN', 'Player', 'ADP_AFLfantasy', 'Risk', 'Notes')

notes <- playerList[,playerList_cols] %>%
  left_join(dfs_data[,dfs_cols], by=c('feedID')) %>%
  left_join(AFDT[,AFDT_cols], by=c('feedID'='ID_AFLFAN')) %>%
  rowwise() %>%
  mutate(best = min(ADP, ADP_AFLfantasy)) %>%
  arrange(best)
  

names(dfs_data)




# Define position limits
position_limits <- c(DEF = 5 , MID = 7, RUC = 2, FWD = 5)

x <- dfs_data %>%
  group_by(dfsFilter) %>%
  arrange(FPadj) %>%
  mutate(rank = row_number()) 

# Assign numbers
x$round_limit <- position_limits[x$dfsFilter]

y <- x %>%
  mutate(round = ceiling(rank/round_limit))





# Function to calculate replacement level and VAR safely
calculate_VAR <- function(data, position, n) {
  # Filter for current position
  pos_players <- data %>% filter(dfsFilter == position)
  
  # Find the actual Nth highest score (replacement level)
  replacement_level <- pos_players$FPadj[pos_players$rankAdj == n][1]
  
  # Calculate Value Above Replacement (VAR)
  pos_players <- pos_players %>%
    mutate(VAR = FPadj - replacement_level)
  
  return(pos_players)
}

# Apply VAR calculation to all positions and combine results
df_VAR <- bind_rows(
  calculate_VAR(df, "DEF", position_limits["DEF"]),
  calculate_VAR(df, "MID", position_limits["MID"]),
  calculate_VAR(df, "RUC", position_limits["RUC"]),
  calculate_VAR(df, "FWD", position_limits["FWD"])
)

# Rank all players based on VAR
df_ranked <- df_VAR %>% arrange(desc(VAR)) %>%
  select(playerName, positionSuperCoach, FPadj, VAR)

# View the top-ranked players
head(df_ranked)






# Age
# Download fanfooty player details
colnames(ff_players) <- c('ff_id','feed_id','first_name','last_name','team','status','number','dob','height','weight','state','recruit','career_games','goals')

age <- adp_data %>%
  mutate(feed_id = as.numeric(feed_id)) %>%
  left_join(ff_players[,c('feed_id','dob','career_games')], by='feed_id') %>%
  mutate(feed_id = as.character(feed_id)) %>%
  mutate(career_games = ifelse(is.na(career_games),0,career_games)) %>%
  mutate(age = year(Sys.Date())-year(dob)) %>%
  mutate(age_flag = case_when(career_games == 0 ~ "Y",  # First year
                              age == 30         ~ "O",  # Caution
                              age >  30         ~ "XO")) # Do not draft












# Fixture Analysis -------------------------------------------------------------
aflFixture <- sc_download(sc$auth,sc$url$aflFixture)
aflFixture <- bind_rows(lapply(aflFixture, function(f) unlist(f, use.names=TRUE)))

#reassign IDs
name_order <- sort(unique(aflFixture$team1.name)) 
name_mapping <- setNames(seq_along(name_order), name_order)  # Assign numbers
aflFixture$team1.id <- name_mapping[aflFixture$team1.name]
aflFixture$team2.id <- name_mapping[aflFixture$team2.name]

aflFixture_long <- aflFixture[,c("id","round","team1.id","team1.name","team1.abbrev","team2.id","team2.name","team2.abbrev" )]
aflFixture_long <- bind_rows(
  aflFixture_long,
  setNames(aflFixture[,c("id","round","team2.id","team2.name","team2.abbrev","team1.id","team1.name","team1.abbrev" )], names(aflFixture_long))
) %>%
  mutate(round = as.numeric(round)) %>%
  arrange(round, id) 
  
  
## Cluster predict TEAM quality
szn <- year(Sys.Date())

predictLadder <- fetch_squiggle_data("ladder", year = szn)%>%
  group_by(teamid, team) %>%
  summarise(mean_rank= mean(mean_rank)) 

set.seed(szn)
gap_stat <- clusGap(scale(predictLadder$mean_rank), FUN = kmeans, K.max = 6, B = 50)
fviz_gap_stat(gap_stat)
predictLadder_clusters <- order(-gap_stat$Tab[,3])[1]
predictLadder_kmeans <- kmeans(scale(predictLadder$mean_rank), centers = predictLadder_clusters, nstart = 25)
predictLadder$cluster <- unlist(predictLadder_kmeans$cluster)

predictLadder <- predictLadder %>%
  group_by(cluster) %>%
  mutate(cluster_score = mean(mean_rank)) %>%
  ungroup() %>%
  mutate(cluster_score = rank(cluster_score))


x <-aflFixture_long %>%
  left_join(predictLadder[,c('teamid','cluster_score')], by=c('team2.id'='teamid'))


x %>%
  filter(round>0&round<22) %>%
  group_by(team1.abbrev) %>%
  summarise(
    mean = sum(cluster_score)
  ) %>%
  arrange(desc(mean))

x %>%
  filter(round>0) %>%
  group_by(team1.abbrev) %>%
  summarise(
    mean = sum(cluster_score)
  ) %>%
  arrange(desc(mean))

x %>%
  filter(round>=22) %>%
  group_by(team1.abbrev) %>%
  summarise(
    mean = sum(cluster_score)
  ) %>%
  arrange(desc(mean))


y<- aflFixture_long %>%
  group_by(round) %>%
  summarise(
    sum=n()
  )

aflFixture_long %>%
  filter(round==13)


