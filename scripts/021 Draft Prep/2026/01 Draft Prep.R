
library(cluster)
library(httr)
library(jsonlite)
library(tidyverse)

tkn <- '084cc4e6c4c58926b971491a424dc71bbd2a3cda'
auth <- add_headers(Authorization = paste0('Bearer ', tkn))

sc_download <- function(auth, url){
  sc_data <- content(GET(url=url,config=auth))
  return(sc_data)
}
sc_setup <- function(auth){
  # Output Variable
  sc <- list()
  sc$auth <- auth
  
  # URL components
  sc$url$base    <- paste0('https://supercoach.heraldsun.com.au/', year(Sys.Date()), '/api/afl/')
  sc$url$draft   <- paste0(sc$url$base, 'draft/v1/')
  sc$url$classic <- paste0(sc$url$base, 'classic/v1/')
  
  # Generate API URLs
  sc$url$settings <- paste0(sc$url$draft,'settings')
  sc$url$me <- paste0(sc$url$draft,'me')
  
  # Call API
  sc$api$settings <- sc_download(sc$auth, sc$url$settings)
  sc$api$me <- sc_download(sc$auth, sc$url$me)
  
  # Save common variables
  sc$var$user_id <- sc$api$me$id
  
  # Generate API URLs
  sc$url$user <- paste0(sc$url$draft,'users/', sc$var$user_id, '/stats')
  
  # Call API
  sc$api$user <- sc_download(sc$auth, sc$url$user)
  
  # Save common variables
  sc$var$season        <- as.numeric(sc$api$settings$content$season)
  sc$var$league_id     <- sc$api$user$classic$leagues[[1]]$id
  sc$var$current_round <- sc$api$settings$competition$current_round
  sc$var$next_round    <- sc$api$settings$competition$next_round
  
  # Generate API URLs
  sc$url$players <- paste0(sc$url$draft,'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=')
  sc$url$player  <- paste0(sc$url$draft,'players/%s?embed=notes,odds,player_stats,player_match_stats,positions,trades')
  sc$url$league  <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/ladderAndFixtures?round=%s&scores=true')
  sc$url$team    <- paste0(sc$url$draft,'userteams/%s/statsPlayers?round=%s')
  
  # Call API
  #sc$api$league <- sc_download(sc$auth, sprintf(sc$url$league, sc$var$current_round))
  
  # Generate API URLs
  sc$url$playerStatus <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/playersStatus')
  sc$url$playerStats  <- paste0(sc$url$draft,'completeStatspack?player_id=')
  
  sc$url$aflFixture <- paste0(sc$url$draft,'real_fixture')
  
  sc$url$teamTrades       <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/teamtrades')
  sc$url$trades           <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/trades')
  sc$url$processedWaivers <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/processedWaivers')
  sc$url$draftResult      <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/recap')
  
  return(sc)
}

sc <- sc_setup(auth)
sc_players <- function(sc, rnd=NULL){
  if(is.null(rnd)) rnd <- sc$var$current_round
  
  url <- paste0(sc$url$players, rnd)
  data <- sc_download(sc$auth, url)
  names(data) <- lapply(data, function(x) as.character(x$id))
  
  playerData <- tibble(
    feedID     = as.numeric(sapply(data, function(x) x$feed_id)),
    playerID   = as.numeric(sapply(data, function(x) x$id)),
    playerName = NA,
    firstName  = sapply(data, function(x) x$first_name),
    lastName   = sapply(data, function(x) x$last_name),
    teamID     = as.numeric(sapply(data, function(x) x$team$id)),
    teamName   = sapply(data, function(x) x$team$name),
    teamAbbrev = sapply(data, function(x) x$team$abbrev),
    pos        = NA,
    pos1       = sapply(data, function(x) x$positions[[1]]$position),
    pos2       = sapply(data, function(x) ifelse(length(x$positions)>1, x$positions[[2]]$position,NA)),
    season     = as.numeric(sc$var$season),
    round      = as.numeric(sapply(data, function(x) x$player_stats[[1]]$round)),
    played     = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$games), NA, x$player_stats[[1]]$games))),
    #projPoints = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$ppts),  NA, x$player_stats[[1]]$ppts))),
    points     = as.numeric(sapply(data, function(x) ifelse(length(x$player_match_stats)==0,    NA, x$player_match_stats[[1]]$points))),
    avg        = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$avg),   NA, x$player_stats[[1]]$avg))),
    avg3       = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$avg3),  NA, x$player_stats[[1]]$avg3))),
    avg5       = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$avg5),  NA, x$player_stats[[1]]$avg5))),
    prevAvg    = as.numeric(sapply(data, function(x) ifelse(is.null(x$previous_average),        NA, x$previous_average))),
    price      = as.numeric(sapply(data, function(x) ifelse(is.null(x$player_stats[[1]]$price), NA, x$player_stats[[1]]$price)))
  ) %>%
    mutate(pos = paste0(pos1, ifelse(!is.na(pos2), paste0('/',pos2),''))) %>%
    mutate(playerName = paste0(substr(firstName,1,1),'.',lastName)) %>%
    group_by(playerName, teamAbbrev) %>%
    mutate(n = n()) %>%
    mutate(playerName = ifelse(n>1, paste0(substr(firstName,1,2),'.',lastName), playerName)) %>%
    select(-n) %>%
    ungroup()
  
  
  url <- sprintf(sc$url$league, rnd)
  data <- sc_download(sc$auth, url)
  data1 <- lapply(data$ladder, function(team){
    map_df(c("scoring", "nonscoring"), function(type) {
      scores <- team$userTeam$scores[[type]]
      tibble(
        round        = map_int(scores, "round"),
        playerID    = map_int(scores, "player_id"),
        picked       = map_chr(scores, "picked"),
        type         = type,
        position     = map_chr(scores, "position"),
        user_team_id = map_int(scores, "user_team_id")
      )
    }) %>%
      mutate(coach = team$userTeam$user$first_name) %>%
      mutate(team = team$userTeam$teamname)
  })
  teamData <- bind_rows(data1) %>%
    select(round,playerID,user_team_id,team,coach,position,picked,type)
  
  playerData1 <- playerData %>%
    left_join(teamData, by=c('round','playerID'))
  
  return(playerData1)
} 
sc_playerStats <- function(sc, playerID){
  
  url <- paste0(sc$url$playerStats, playerID)
  data <- sc_download(sc$auth, url)
  
  s <- lapply(data$playerStats, unlist)
  s <- bind_rows(s)  %>%
    mutate(across(where(~ all(suppressWarnings(!is.na(as.numeric(.))))), as.numeric))
  
  return(s)
} 

# Player Data
playerList <- sc_players(sc)
#playerStats <- bind_rows(lapply(playerList$playerID, function(p){s <- sc_playerStats(sc, p)}))

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

# scStats <- playerStats %>%
#   filter(season >= as.numeric(sc$var$season)-2) %>%
#   filter(played == 1 & minutes_played > 57) %>% #Time of Ground exclusion #quantile(scStats$minutes_played, 0.90, na.rm = TRUE)/2
#   group_by(season) %>%
#   mutate(max_round = max(round)) %>%
#   ungroup() %>%
#   mutate(season_half = round(round/max_round,0)+1) %>%
#   filter(played == 1 & minutes_played > 57) %>% #Time of Ground exclusion #quantile(scStats$minutes_played, 0.90, na.rm = TRUE)/2
#   group_by(player_id, season, season_half) %>%
#   summarise(
#     n = n(),
#     avg = mean(points, na.rm=T),
#     .groups='drop'
#   ) %>%
#   filter(n >= 5) %>% #games minimum
#   group_by(player_id) %>%
#   summarise(
#     maxAvg = max(avg, na.rm=T),
#     consist = sd(avg)/mean(avg)
#   ) 


## IMPORT DFS DATA -------------------------------------------------------------

url <- "https://dfsaustralia.com/wp-admin/admin-ajax.php"

headers <- add_headers(
  "Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8",
  "Accept" = "*/*",
  "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.6 Safari/605.1.15",
  "X-Requested-With" = "XMLHttpRequest",
  "Referer" = "https://dfsaustralia.com/supercoach-big-board/",
  "Cookie" = "f_ga=GA1.1.949841369.1750938699; _gid=GA1.2.167860346.1770973291; __stripe_mid=778b9f92-7050-40b6-a58b-13695507a2ce3bd122" # Truncated for brevity
)

posList <- c('DEF','MID','RUC','FWD')
dfs_data <- tibble()

for (pos in posList){
  body <- list(action = "afl_supercoach_big_board_call", position = pos, last_year = "0")
  body_encoded <- URLencode(paste(names(body), body, sep="=", collapse="&"))
  
  # Send the POST request
  response <- POST(url, headers, body = body_encoded, encode = "form")
  
  # Extract and print the response content
  content_text <- content(response, "text")
  content_list <- fromJSON(content_text)
  
  # transform to dataframe
  dfs_last <- content_list$last %>%
    mutate(across(where(~ all(suppressWarnings(is.na(.) | !is.na(as.numeric(.))))), \(x) suppressWarnings(as.numeric(x)))) %>%
    mutate(dfsFilter = pos) %>%
    select(playerId, teamAbbr, gms, FPadj, FPfirst, FPsecond)
  names(dfs_last) <- paste0('prev_',names(dfs_last))
  
  dfs_current <- content_list$current %>%
    select(-ownHistory) %>% 
    mutate(across(where(~ all(suppressWarnings(is.na(.) | !is.na(as.numeric(.))))), \(x) suppressWarnings(as.numeric(x)))) %>%
    mutate(dfsFilter = pos) %>%
    select(playerId, playerName, teamAbbr, positionSuperCoach, dfsFilter, statusFantasy, age, gms, FPadj, FPfirst, FPsecond) %>%
    left_join(dfs_last, by=c('playerId'='prev_playerId'))

  # bind results 
  dfs_data <- bind_rows(dfs_data, dfs_current)
}


rankings <- dfs_data %>%
  arrange(desc(FPadj)) %>%
  mutate(FPgrp = round(FPadj / 5) * 5) %>%
  mutate(potential = rowSums(across(c(FPfirst, FPsecond, prev_FPfirst, prev_FPsecond)) >= (FPgrp + 5), na.rm = TRUE)) %>%
  mutate(FPmean = rowMeans(across(c(FPfirst, FPsecond, prev_FPfirst, prev_FPsecond)), na.rm = TRUE)) %>%
  mutate(FPgrp_adj = case_when(age >= 30 ~ -5,
                               potential >=2 ~ +5,
                               TRUE ~ 0) + FPgrp) %>%
  #select(playerName,,teamAbbr,positionSuperCoach,gms,FPadj,FPfirst,FPsecond,prev_gms,prev_FPadj,prev_FPfirst,prev_FPsecond,FPgrp, FPgrp_adj,FPmean) %>%
  arrange(desc(FPgrp_adj),desc(FPgrp),desc(FPmean)) %>%
  group_by(dfsFilter) %>%
  mutate(posRank = dense_rank(row_number()))
           


# Define position limits
position_limits <- c(DEF = 5 , MID =7, RUC = 2, FWD = 5)*8
# 
# x <- dfs_data %>%
#   group_by(dfsFilter) %>%
#   arrange(FPadj) %>%
#   mutate(rank = row_number()) 
# 
# # Assign numbers
# x$round_limit <- position_limits[x$dfsFilter]
# 
# y <- x %>%
#   mutate(round = ceiling(rank/round_limit))

data <- rankings
position <- "DEF"
n <- position_limits["DEF"]

# Function to calculate replacement level and VAR safely
calculate_VAR <- function(data, position, n) {
  # Filter for current position
  pos_players <- data %>% filter(dfsFilter == position)
  
  # Find the actual Nth highest score (replacement level)
  replacement_level <- pos_players$FPgrp_adj[pos_players$posRank == n][1]
  
  # Calculate Value Above Replacement (VAR)
  pos_players <- pos_players %>%
    mutate(VAR = FPgrp_adj - replacement_level)
  
  return(pos_players)
}

# Apply VAR calculation to all positions and combine results
df_VAR <- bind_rows(
  calculate_VAR(rankings, "DEF", position_limits["DEF"]),
  calculate_VAR(rankings, "MID", position_limits["MID"]),
  calculate_VAR(rankings, "RUC", position_limits["RUC"]),
  calculate_VAR(rankings, "FWD", position_limits["FWD"])
)

# Rank all players based on VAR
df_ranked <- df_VAR %>% 
  filter(statusFantasy != 'injured') %>%
  arrange(desc(VAR)) %>%
  ungroup() %>%
  group_by(playerId) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(playerId, playerName, teamAbbr,positionSuperCoach, dfsFilter, FPgrp_adj, VAR) %>%
  rowwise() %>%
  mutate(FPtier = min(6,VAR/5)) %>%
  ungroup() %>%
  mutate(FPtier = dense_rank(-FPtier)) %>%
  filter(FPtier <= 8) %>%
  group_by(FPtier, dfsFilter) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  rename(name = playerName) %>%
  rowwise() %>%
  mutate(pos2 = substring(gsub('/','',gsub(dfsFilter, '', positionSuperCoach)),1,1)) %>%
  ungroup() %>%
  left_join((playerList[,c('feedID','playerName')] %>% mutate(feedID = paste0('CD_I',feedID))), by=c('playerId'='feedID')) %>%
  mutate(playerName = paste0(playerName,' | ',teamAbbr, ' (',pos2,')')) %>%
  mutate(playerName = trimws(gsub('\\(\\)','',playerName)))

notes <- df_ranked %>%
  select(FPtier, rank, playerName) %>%
  split( df_ranked$dfsFilter)

# Assuming df_list is your list of data frames
df_combined <- notes %>%
  reduce(full_join, by = c("FPtier", "rank")) %>%
  arrange(FPtier, rank) %>%
  select(-rank)

names(df_combined) <- c('tier', names(notes))

df_combined<- df_combined[,c('tier','DEF','MID','RUC','FWD')]

write.csv(df_combined,'~/Projects/Fantasy-Banter/scripts/021 Draft Prep/2026/draft_notes.csv',na='',row.names =F)
          
          
df_ranked %>%
  group_by(dfsFilter) %>%
  summarise(
    n=n()
  )







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


