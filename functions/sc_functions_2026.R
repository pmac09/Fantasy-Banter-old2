### SETUP --------------------------------------------------------------------
options(stringsAsFactors = FALSE)
library(tidyverse)
library(lubridate)
library(httr)
library(fireData)

# library(rjson)
# library(zoo)
# suppressMessages()
# library(googleCloudStorageR)
# library(RColorBrewer)

databaseURL <- Sys.getenv("FB_DATABASE_URL")

### BASE FUNCTIONS -------------------------------------------------------------
print_log <- function(..., verbose=TRUE){
  if(exists('printLog')) verbose <- printLog
  
  if(verbose){
    time <- format(with_tz(as_datetime(Sys.time()), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S")
    msg <- paste0(time,': ', ...)
    message(msg)
  }
}

### FIREBASE FUNCTIONS ---------------------------------------------------------
firebase_download <- function(databaseURL, databasePath, cols=NULL){
  msg <- paste0('firebase_download: ', databasePath)
  print_log(msg)
  
  data <- suppressWarnings(fireData::download(databaseURL, databasePath))
  
  if(!is.null(cols)) if(cols==TRUE) {
    databasePath <- gsub('/data/','/settings/columns/',databasePath)
    data_cols <- suppressWarnings(fireData::download(databaseURL, databasePath))
    data <- data[,data_cols]
  }
  
  return(data)
}
firebase_upload <- function(databaseURL, databasePath=NULL, data, cols=NULL){
  msg <- paste0('firebase_upload: ', databasePath)
  print_log(msg)
  
  if(is.null(databasePath)) return(NULL)
  upload_data <- put(data, databaseURL, databasePath)
  
  if(!is.null(cols)) if(cols==TRUE) {
    databasePath <- gsub('/data/','/settings/columns/',databasePath)
    upload_cols <- put(names(data), databaseURL, databasePath)
  }
  
}

### SUPERCOACH FUNCTIONS -------------------------------------------------------
sc_authenticate <- function(databaseURL){
  msg <- paste0('sc_token')
  print_log(msg)
  tkn <- firebase_download(databaseURL, '/Fantasy-Banter/settings/api/supercoach')
  
  sc_auth <- add_headers(
    Authorization = paste0('Bearer ', tkn$access_token)
  )
}
sc_download <- function(auth, url){
  msg <- paste0('sc_download: ',str_extract(url, '(?<=/afl).+'))
  print_log(msg)
  
  sc_data <- content(GET(
    url = url,
    config = auth
  ))
  
  if(!is.null(sc_data$message)){
    msg <- paste0('get_sc_data: ERROR: ',sc_data$message)
    print_log(msg)
    
    sc_data <- NULL
  }
  
  return(sc_data)
}

sc_auth <- sc_authenticate(databaseURL)
sc_data <- sc_download(sc_auth, url)

sc_setup <- function(auth){
  print_log('sc_setup')
  
  # Output Variable
  sc <- list()
  sc$auth <- sc_auth
  
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

#sc_auth <- sc_authenticate(databaseURL)
#sc <- sc_setup(sc_auth)




sc_players <- function(sc, rnd=NULL){
  if(is.null(rnd)) rnd <- sc$var$current_round
  print_log(paste0('sc_players: Round ', rnd))
  
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

sc_league <-  function(sc, rnd=NULL){
  if(is.null(rnd)) rnd <- sc$var$current_round
  print_log(paste0('sc_players: Round ', rnd))
  
  url <- sprintf(sc$url$league, rnd)
  data <- sc_download(sc$auth, url)
  
  home_data <- tibble(
    season           = rep(sc$var$season, length(data$fixture)),
    round            = as.numeric(sapply(data$fixture, function(x) x$round)),
    fixture          = as.numeric(sapply(data$fixture, function(x) x$fixture)),
    user_team_id     = as.numeric(sapply(data$fixture, function(x) x$user_team1$id)),
    team             = sapply(data$fixture, function(x) x$user_team1$teamname),
    coach            = sapply(data$fixture, function(x) x$user_team1$user$first_name),
    team_score       = as.numeric(sapply(data$fixture, function(x) ifelse(is.null(x$user_team1$stats[[1]]$points),NA,x$user_team1$stats[[1]]$points))),
    opponent_team_id = as.numeric(sapply(data$fixture, function(x) x$user_team2$id)),
    opponent_team    = sapply(data$fixture, function(x) x$user_team2$teamname),
    opponent_coach   = sapply(data$fixture, function(x) x$user_team2$user$first_name),
    opponent_score   = as.numeric(sapply(data$fixture, function(x) ifelse(is.null(x$user_team2$stats[[1]]$points),NA,x$user_team2$stats[[1]]$points)))
  )
  
  away_data <- home_data 
  names(away_data) <- names(home_data)[c(1:3,8:11,4:7)]
  
  fixture_data <- bind_rows(home_data,away_data) %>%
    mutate(team_score = ifelse(team_score==0 | sc$var$current_round < rnd,NA,team_score)) %>%
    mutate(opponent_score = ifelse(opponent_score==0 | sc$var$current_round < rnd,NA,opponent_score)) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0))
  
  if (data$ladder[[1]]$round <= sc$var$current_round){
    ladder_data <- tibble(
      user_team_id   = as.numeric(sapply(data$ladder, function(x) x$user_team_id)),
      position       = as.numeric(sapply(data$ladder, function(x) x$position)),
      points         = as.numeric(sapply(data$ladder, function(x) x$points)),
      wins           = as.numeric(sapply(data$ladder, function(x) x$wins)),
      draws          = as.numeric(sapply(data$ladder, function(x) x$draws)),
      losses         = as.numeric(sapply(data$ladder, function(x) x$losses)),
      points_for     = as.numeric(sapply(data$ladder, function(x) x$points_for)),
      points_against = as.numeric(sapply(data$ladder, function(x) x$points_against))
    ) %>%
      mutate(pcnt = round(points_for/points_against*100,1))
    
    fixture_data <- fixture_data %>%
      left_join(ladder_data, by=c('user_team_id'))
  }
  
  return(fixture_data)
  
}
sc_league_transform <- function(data, szn){
  
  home_data <- tibble(
    season           = rep(szn, length(data$fixture)),
    round            = as.numeric(sapply(data$fixture, function(x) x$round)),
    fixture          = as.numeric(sapply(data$fixture, function(x) x$fixture)),
    user_team_id     = as.numeric(sapply(data$fixture, function(x) x$user_team1$id)),
    team             = sapply(data$fixture, function(x) x$user_team1$teamname),
    coach            = sapply(data$fixture, function(x) x$user_team1$user$first_name),
    team_score       = as.numeric(sapply(data$fixture, function(x) ifelse(is.null(x$user_team1$stats[[1]]$points),NA,x$user_team1$stats[[1]]$points))),
    opponent_team_id = as.numeric(sapply(data$fixture, function(x) x$user_team2$id)),
    opponent_team    = sapply(data$fixture, function(x) x$user_team2$teamname),
    opponent_coach   = sapply(data$fixture, function(x) x$user_team2$user$first_name),
    opponent_score   = as.numeric(sapply(data$fixture, function(x) ifelse(is.null(x$user_team2$stats[[1]]$points),NA,x$user_team2$stats[[1]]$points)))
  )
  
  away_data <- home_data 
  names(away_data) <- names(home_data)[c(1:3,8:11,4:7)]
  
  fixture_data <- bind_rows(home_data,away_data) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0))
  
  ladder_data <- tibble(
    user_team_id   = as.numeric(sapply(data$ladder, function(x) x$user_team_id)),
    position       = as.numeric(sapply(data$ladder, function(x) x$position)),
    points         = as.numeric(sapply(data$ladder, function(x) x$points)),
    wins           = as.numeric(sapply(data$ladder, function(x) x$wins)),
    draws          = as.numeric(sapply(data$ladder, function(x) x$draws)),
    losses         = as.numeric(sapply(data$ladder, function(x) x$losses)),
    points_for     = as.numeric(sapply(data$ladder, function(x) x$points_for)),
    points_against = as.numeric(sapply(data$ladder, function(x) x$points_against))
  ) %>%
    mutate(pcnt = round(points_for/points_against*100,1))
  
  fixture_data <- fixture_data %>%
    left_join(ladder_data, by=c('user_team_id'))
  
  return(fixture_data)
  
}

sc_update <- function(sc, rnd=NULL){
  print_log('sc_update')
  
  szn <- sc$var$season
  rnd <- ifelse(is.null(rnd),sc$var$current_round,rnd)
  
  league_data <- fb_league()
  league_new <- sc_league(sc, rnd)
    
  league_data_new <- league_data %>%
    filter(!(season == szn & round == rnd)) %>%
    bind_rows(league_new) %>%
    arrange(season, round)
  
  
  
  firebase_upload(databaseURL, '/Fantasy-Banter/data/league', league_data_new, cols=TRUE)
  
  player_data <- fb_players()
  player_new <- sc_players(sc, rnd)
 
  player_data_new <- player_data %>%
    filter(!(season == szn & round == rnd)) %>%
    bind_rows(player_new) %>%
    arrange(season, round)
  
  firebase_upload(databaseURL, '/Fantasy-Banter/data/players', player_data_new, cols=TRUE)
  
}
sc_autoUpdate <- function(sc){
  print_log('sc_autoUpdate')
  
  szn <- sc$var$season
  rnd <- sc$var$current_round
  
  league_data <- fb_league()
  
  dataRnd <- league_data %>%
    filter(season == szn) %>%
    filter(round <= rnd) %>%
    filter(is.na(team_score)) %>%
    select(round) %>%
    distinct() %>%
    pull(round)
  
  if(length(dataRnd) > 0){
    tmp <- lapply(dataRnd, function(r) sc_update(sc = sc, rnd = r))
    rm(tmp)
  } else {
    print_log('sc_autoUpdate: Data up-to-date')
  }

}

## Fantasy Banter Functions ----------------------------------------------------

fb_league <- function(dbURL=databaseURL){
  print_log('fb_league')
  league_data <- firebase_download(dbURL, '/Fantasy-Banter/data/league', cols=TRUE)
  return(league_data)
}
fb_players <- function(dbURL=databaseURL){
  print_log('fb_players')
  player_data <- firebase_download(dbURL, '/Fantasy-Banter/data/players', cols=TRUE)
  return(player_data)
}
  
season_simulation <- function(league_data, szn, rnd) {
  print_log('sc_update')
  
  last_year <- league_data %>%
    filter(season == max(szn-1,2016)) %>%
    summarise(
      mean = mean(team_score, na.rm=T),
      sd = sd(team_score, na.rm=T)
    )
  
  szn_data <- league_data %>%
    filter(season == szn) 
  
  sim_data <- szn_data %>%
    select(season, round, fixture, coach, team_score) %>%
    mutate(team_score = ifelse(round>rnd,NA,team_score)) %>%
    cross_join(tibble(sim = c(1:100000))) 
  
  na_idx <- is.na(sim_data$team_score)
  sim_data$team_score[na_idx] <- round(rnorm(sum(na_idx), last_year$mean, last_year$sd), 0)
  
  oppo_data <- sim_data
  names(oppo_data)[grepl('coach',names(sim_data))] <- 'oppo_coach'
  names(oppo_data)[grepl('team_score',names(sim_data))] <- 'opponent_score'
  
  sim_data2 <- sim_data %>%
    left_join(oppo_data, by=c('season','round','fixture','sim'), relationship = "many-to-many") %>%
    filter(coach != oppo_coach) %>%
    mutate(differential = team_score - opponent_score) %>%
    mutate(win = ifelse(differential > 0, 1,0)) %>%
    mutate(draw = ifelse(differential == 0, 1,0)) %>%
    mutate(loss = ifelse(differential < 0, 1,0)) 
  
  sim_data3 <- sim_data2 %>%
    group_by(season, sim, coach) %>%
    summarise(
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss),
      points_total = sum(team_score),
      points_mean = mean(team_score),
      points_sd = sd(team_score),
      .groups = 'drop'
    ) %>%
    arrange(sim, desc(wins), desc(draws), desc(points_total)) %>%
    group_by(sim) %>%
    mutate(position = row_number()) %>%
    mutate(finals = ifelse(position <= 4,1,0))
  
  sim_data4 <- sim_data3 %>%
    select(season, sim, coach, position, points_mean, points_sd) %>%
    filter(position <= 4) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup()%>%
    mutate(fixture = ifelse(position %in% c(1,2), 1, 2)) %>% 
    group_by(season,sim,fixture) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  sim_data5 <- sim_data4 %>%
    filter((fixture == 1 & win == FALSE) | (fixture == 2 & win == TRUE)) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup() %>%
    group_by(season,sim) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  sim_data6 <- bind_rows(
    (sim_data4 %>% filter(fixture == 1 & win == TRUE)),
    (sim_data5 %>% filter(win == TRUE))
  ) %>%
    # sim_data4 %>% 
    # filter(win == TRUE) %>%
    arrange(season, sim) %>%
    rowwise() %>%
    mutate(team_score = round(rnorm(1, points_mean, points_sd),0)+position/10) %>%
    ungroup() %>%
    group_by(season,sim) %>%
    mutate(win = team_score==max(team_score)) %>%
    ungroup()
  
  results <- sim_data3 %>% 
    left_join(sim_data6[,c('season','sim','coach','win')], by=c('season','sim','coach')) %>%
    mutate(gfinal = ifelse(!is.na(win)==TRUE,1,0)) %>%
    mutate(champ = ifelse(win==TRUE,1,0)) %>%
    group_by(coach) %>%
    summarise(
      sims = n(),
      sim_pos = round(mean(position),2),
      sim_finals = round(sum(finals)/n(),6),
      sim_gfinal = round(sum(gfinal,na.rm=T)/n(),6),
      sim_champ = round(sum(champ,na.rm=T)/n(),6),
      sim_spoon = round(sum(ifelse(position==8,1,0))/n(),6)
    ) %>%
    arrange(sim_pos) %>%
    mutate(round = rnd) %>%
    mutate(season = szn)
  
  return(results)
  
}


## Dashboard Functions --------------------------------------------------------


spotify_theme <- function() {
  search_icon <- function(fill = "none") {
    # Icon from https://boxicons.com
    svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24"><path fill="%s" d="M10 18c1.85 0 3.54-.64 4.9-1.69l4.4 4.4 1.4-1.42-4.39-4.4A8 8 0 102 10a8 8 0 008 8.01zm0-14a6 6 0 11-.01 12.01A6 6 0 0110 4z"/></svg>', fill)
    sprintf("url('data:image/svg+xml;charset=utf-8,%s')", URLencode(svg))
  }
  
  text_color <- "hsl(0, 0%, 95%)"
  text_color_light <- "hsl(0, 0%, 70%)"
  text_color_lighter <- "hsl(0, 0%, 55%)"
  bg_color <- "hsl(0, 0%, 10%)"
  
  reactableTheme(
    color = text_color,
    backgroundColor = bg_color,
    borderColor = "hsl(0, 0%, 16%)",
    borderWidth = "1px",
    highlightColor = "rgba(255, 255, 255, 0.1)",
    cellPadding = "10px 8px",
    style = list(
      fontFamily = "Work Sans, Helvetica Neue, Helvetica, Arial, sans-serif",
      fontSize = "0.875rem",
      "a" = list(
        color = text_color,
        textDecoration = "none",
        "&:hover, &:focus" = list(
          textDecoration = "underline",
          textDecorationThickness = "1px"
        )
      ),
      ".number" = list(
        fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
      ),
      ".tag" = list(
        padding = "0.125rem 0.25rem",
        color = "hsl(0, 0%, 40%)",
        fontSize = "0.75rem",
        border = "1px solid hsl(0, 0%, 24%)",
        borderRadius = "2px",
        textTransform = "uppercase"
      )
    ),
    headerStyle = list(
      display = "flex",
      flexDirection = "column",         # Stack items vertically
      justifyContent = "flex-end",      # Align items to bottom
      color = text_color_light,
      fontWeight = 400,
      fontSize = "0.75rem",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = text_color)
    ),
    rowHighlightStyle = list(
      ".tag" = list(color = text_color, borderColor = text_color_lighter)
    ),
    # Full-width search bar with search icon
    searchInputStyle = list(
      paddingLeft = "1.9rem",
      paddingTop = "0.5rem",
      paddingBottom = "0.5rem",
      width = "100%",
      border = "none",
      backgroundColor = bg_color,
      backgroundImage = search_icon(text_color_light),
      backgroundSize = "1rem",
      backgroundPosition = "left 0.5rem center",
      backgroundRepeat = "no-repeat",
      "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
      "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
      "::placeholder" = list(color = text_color_lighter),
      "&:hover::placeholder, &:focus::placeholder" = list(color = text_color)
    ),
    paginationStyle = list(color = text_color_light),
    pageButtonHoverStyle = list(backgroundColor = "hsl(0, 0%, 20%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(0, 0%, 24%)")
  )
}
spotify_table <- function(data, 
                          columns = NULL, 
                          default_colDef = colDef(align = "center", minWidth = 90),
                          default_sorted = NULL,
                          height = "auto", full_width = TRUE, highlight = TRUE, ...) {
  reactable::reactable(
    data,
    columns = columns,
    defaultColDef = default_colDef,
    defaultSorted = default_sorted,
    highlight = highlight,
    bordered = FALSE,
    striped = FALSE,
    compact = TRUE,
    showPageSizeOptions = FALSE,
    paginationType = "simple",
    theme = spotify_theme(),
    fullWidth = full_width,
    height = height,
    ...
  )
}



fb_standings <- function(fbLeague, szn=NULL, rnd=NULL){
  
  if(is.null(szn) | is.null(rnd)){
    dflt <- fbLeague %>%
      filter(!is.na(team_score)) %>%
      select(season, round) %>%
      distinct() %>%
      arrange(desc(season), desc(round)) 
    
    szn <- dflt$season[1]
    rnd <- dflt$round[1]
  }
  
  
  x <- fbLeague %>%
    filter(season == szn) %>%
    filter(round == rnd) %>%
    mutate(points_for = round(points_for/round,0)) %>%
    mutate(points_against = round(points_against/round,0)) %>%
    mutate(points = points + points_for/10000) %>%
    select(position, team, coach, 
           points, wins, draws, losses, 
           points_for, points_against, pcnt) %>%
    arrange(position)
  
  
  c <- list(
    position = colDef(show = FALSE),
    team = colDef(
      
      cell = function(value, index) {
        tag <- span(class = "tag", x[index, "coach"])
        
        div(
          style = list(
            display = "flex",
            justifyContent = "space-between",
            alignItems = "center",
            gap = "0.5rem"
          ),
          div(
            style = list(
              overflow = "hidden",
              whiteSpace = "nowrap",
              textOverflow = "ellipsis",
              flex = "1"
            ),
            value
          ),
          div(
            style = list(flexShrink = "0"),
            tag
          )
        )
      },
      minWidth = 120,
      align = "left",
      style = list(
        position = "sticky",
        left = 0,
        background = "hsl(0, 0%, 10%)",  # match your table bg_color
        zIndex = 1
      ),
      headerStyle = list(
        position = "sticky",
        left = 0,
        zIndex = 2,
        background = "hsl(0, 0%, 10%)"
      )
    ),
    coach = colDef(show = FALSE),
    points = colDef(
      name='RESULTS',
      cell=function(value, index) {
        
        w <- if (x[index, 'wins']  < 10) paste0(" ", x[index, 'wins'] ) else as.character(x[index, 'wins'] )
        d <- x[index, 'draws']
        l <- if (x[index, 'losses'] < 10) paste0(x[index, 'losses'], " ") else as.character(x[index, 'losses'])
        
        div(
          class = "number",
          style = list(whiteSpace = "pre"),
          paste0(w, "·", d, "·", l)
        )
      }
    ),
    wins = colDef(show = FALSE),
    draws = colDef(show = FALSE),
    losses = colDef(show = FALSE),
    points_for = colDef(
      name = 'Average Score',,
      cell = function(value) {
        div(
          class = "number",
          formatC(value, format = "d", big.mark = ",")
        )
      }
    ),
    points_against = colDef(
      name = 'Average Against',
      cell = function(value) {
        div(
          class = "number",
          formatC(value, format = "d", big.mark = ",")
        )
      }
    ), 
    pcnt = colDef(
      name="%",
      cell=function(value){
        vf <- format(round(value, 1), nsmall = 1)
        vf <- if (value < 100) paste0(" ", vf) else as.character(vf)
        
        div(
          class = "number",
          style = list(whiteSpace = "pre"),
          paste0(vf)
        )
      }
    )
  )
  
  spotify_table(data = x, columns=c)
  
}



### RUN  -----------------------------------------------------------------------
# 
# scKey <- sc_key(databaseURL)  
# scAuth <- sc_authenticate(scKey$client_id, scKey$access_token)
# sc <- sc_setup(scAuth)
# 
# if(1 == 0){
# 
# league_data <- fb_league()
#   
# if(1 == 0){
# 
# 
# print_log <- function(..., verbose=TRUE){
#   if(exists('printLog')) verbose <- printLog
#   
#   if(verbose){
#     time <- format(with_tz(as_datetime(Sys.time()), "Australia/Melbourne"), format="%Y-%m-%d %H:%M:%S")
#     msg <- paste0(time,': ', ...)
#     message(msg)
#   }
# }
# 
# if_null <- function(x, replacement=NA) ifelse(!is.null(x),x,replacement)
# is.not.null <- function(x) !is.null(x)
# 
# 
# 
# gcs_save <- function(gcs_file, gcs_path){
#   msg <- paste0('gcs_save: ', gcs_path)
#   print_log(msg)
#   
#   data_upload <- gcs_upload(file=gcs_file, name=gcs_path)
#   
#   return(data_upload)
# }
# gcs_download <- function(gcs_path){
#   msg <- paste0('gcs_download: ', gcs_path)
#   print_log(msg)
#   
#   data <- suppressMessages(gcs_get_object(gcs_path))
#   
#   if(names(data)[1] == '...1'){
#     data <- data[c(-1)]
#   }
#   
#   return(data)
# }
# 
# ### PROCESSING FUNCTIONS -------------------------------------------------------
# 
# 
# get_sc_player <- function(sc, rnd=NULL){
#   msg <- ifelse(is.null(rnd), 'NULL', paste0('Round ', rnd))
#   print_log(paste0('sc_player_data: ', msg))
#   
#   # Default to current round if NULL
#   if(is.null(rnd)) rnd <- sc$var$current_round
#   
#   # Download from Supercoach API
#   url <- paste0(sc$url$players, rnd)
#   data <- sc_download(sc$auth, url)
#   
#   # Return Supercoach JSON
#   return(data)
# }
# read_sc_player <- function(scPlayerJson){
#   print_log(paste0('read_sc_player: '))
#   
#   data <- scPlayerJson
#   
#   scPlayerTbl <- tibble(
#     feed_id     = as.numeric(unlist(lapply(data, function(x) x$feed_id))),
#     first_name  = unlist(lapply(data, function(x) x$first_name)),
#     last_name   = unlist(lapply(data, function(x) x$last_name)),
#     player_name = NA,
#     id          = unlist(lapply(data, function(x) x$id)),
#     team        = unlist(lapply(data, function(x) x$team$abbrev)),
#     position    = unlist(lapply(data, function(x) paste0(lapply(x$positions, function(p) p$position), collapse = '/'))),
#     season      = as.numeric(sc$var$season),
#     round       = as.numeric(unlist(lapply(data, function(x) x$player_stats[[1]]$round))),
#     points      = as.numeric(unlist(lapply(data, function(x) if_null(x$player_stats[[1]]$points)))),
#     avg         = round(unlist(lapply(data, function(x) if_null(x$player_stats[[1]]$avg)),1)),
#     avg3        = round(unlist(lapply(data, function(x) if_null(x$player_stats[[1]]$avg3)),1)),
#     avg5        = round(unlist(lapply(data, function(x) if_null(x$player_stats[[1]]$avg5)),1)),
#   ) %>%
#     mutate(player_name = paste0(substr(first_name,1,1),'.',last_name)) %>%
#     group_by(player_name, team) %>%
#     mutate(n = n()) %>%
#     mutate(player_name = ifelse(n>1, paste0(substr(first_name,1,2),'.',last_name), player_name)) %>%
#     ungroup() %>%
#     select(-n) 
#   
# }
# 
# get_sc_league <- function(sc, rnd=NULL){
#   msg <- ifelse(is.null(rnd), 'NULL', paste0('Round ', rnd))
#   print_log(paste0('get_sc_league: ', msg))
#   
#   # Default to current round if NULL
#   if(is.null(rnd)) rnd <- sc$var$current_round
#   
#   # Download from Supercoach API
#   url <- sprintf(sc$url$league, rnd)
#   data <- sc_download(sc$auth, url)
#   
#   # Return Supercoach JSON
#   return(data)
# }
# read_sc_league_player_teams <- function(scLeagueJson){
#   print_log(paste0('read_sc_league_player_teams: '))
#   
#   league_data <- scLeagueJson
#   player_teams <- bind_rows(lapply(league_data$ladder, function(x){
#     
#     bind_rows(
#       tibble(
#         
#         team_id    = x$user_team_id,
#         team_name  = x$userTeam$teamname,
#         user_id    = x$userTeam$user_id,
#         user_name  = x$userTeam$user$first_name,
#         round      = as.numeric(unlist(lapply(x$userTeam$scores$scoring, function(x) x$round))),
#         id         = unlist(lapply(x$userTeam$scores$scoring, function(x) x$player_id)),
#         pos        = unlist(lapply(x$userTeam$scores$scoring, function(x) x$position)),
#         picked     = unlist(lapply(x$userTeam$scores$scoring, function(x) x$picked)),
#         type       = 'scoring'
#       ),
#       tibble(
#         team_id    = x$user_team_id,
#         team_name  = x$userTeam$teamname,
#         user_id    = x$userTeam$user_id,
#         user_name  = x$userTeam$user$first_name,
#         round      = as.numeric(unlist(lapply(x$userTeam$scores$nonscoring, function(x) x$round))),
#         id         = unlist(lapply(x$userTeam$scores$nonscoring, function(x) x$player_id)),
#         pos        = unlist(lapply(x$userTeam$scores$nonscoring, function(x) x$position)),
#         picked     = unlist(lapply(x$userTeam$scores$nonscoring, function(x) x$picked)),
#         type       = 'nonscoring'
#       )
#     )
#     
#   }))
#   
#   return(player_teams)
#   
# }
#   
# get_player_data <- function(){
#   print_log(paste0('get_player_data: '))
#   path <- paste0('fantasy-banter/master/player-data.csv')
#   player_data <- gcs_download(path)
#   return(player_data)
# }
# update_player_data <- function(playerData, scPlayer){
#   print_log(paste0('update_player_data: '))
#   newPlayerData <- playerData %>%
#     filter(!(season %in% scPlayer$season & round %in% scPlayer$round)) %>%
#     bind_rows(scPlayer) %>%
#     arrange(desc(season), desc(round), desc(avg))
#   
#   return(newPlayerData)
# }  
# update_player_teams <- function(playerData, scLeaguePlayerTeams, szn){
#   print_log(paste0('update_player_teams: '))
#   
#   playerTeams <- scLeaguePlayerTeams %>%
#     mutate(season = as.numeric(szn))
#   
#   newPlayerData <- playerData %>%
#     rows_update(playerTeams, by=c('season', 'round', 'id')) %>%
#     arrange(desc(season), desc(round), desc(avg))
#   
#   return(newPlayerData)
# }
# save_player_data <- function(playerData){
#   print_log(paste0('save_player_data: '))
#   path <- paste0('fantasy-banter/master/player-data.csv')
#   player_data <- gcs_save(playerData, path)
#   return(player_data)
# }
# 
# ### RUN  -----------------------------------------------------------------------
# 
#   
#   auth <- sc_auth(cid,tkn)
#   sc <- sc_setup(auth)
#   
#   rnd <- 12
#   
#   scPlayerJson <- get_sc_player(sc, rnd)
#   scPlayer <- read_sc_player(scPlayerJson)
#   
#   scLeagueJson <- get_sc_league(sc, rnd)
#   scLeaguePlayerTeams <- read_sc_league_player_teams(scLeagueJson)
#   
#   playerData <- get_player_data()
#   playerData <- update_player_data(playerData, scPlayer)
#   playerData <- update_player_teams(playerData, scLeaguePlayerTeams, sc$var$season)
#   
#   fileSave <- save_player_data(playerData)
# 
# 
# ### OLD FUNCTIONS -------------------------------------------------------
# 
# sc_setup <- function(auth){
#   print_log('sc_setup')
#   
#   # Output Variable
#   sc <- list()
#   sc$auth <- auth
#   
#   # URL components
#   sc$url$base    <- paste0('https://supercoach.heraldsun.com.au/', year(Sys.Date()), '/api/afl/')
#   sc$url$draft   <- paste0(sc$url$base, 'draft/v1/')
#   sc$url$classic <- paste0(sc$url$base, 'classic/v1/')
#   
#   # Generate API URLs
#   sc$url$settings <- paste0(sc$url$draft,'settings')
#   sc$url$me <- paste0(sc$url$draft,'me')
#   
#   # Call API
#   sc$api$settings <- sc_download(sc$auth, sc$url$settings)
#   sc$api$me <- sc_download(sc$auth, sc$url$me)
#   
#   # Save common variables
#   sc$var$user_id <- sc$api$me$id
#   
#   # Generate API URLs
#   sc$url$user <- paste0(sc$url$draft,'users/', sc$var$user_id, '/stats')
#   
#   # Call API
#   sc$api$user <- sc_download(sc$auth, sc$url$user)
#   
#   # Save common variables
#   sc$var$season        <- sc$api$settings$content$season
#   sc$var$league_id     <- sc$api$user$classic$leagues[[1]]$id
#   sc$var$current_round <- sc$api$settings$competition$current_round
#   sc$var$next_round    <- sc$api$settings$competition$next_round
#   
#   # Generate API URLs
#   
#   sc$url$players <- paste0(sc$url$draft,'players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions%2Cplayer_match_stats&round=')
#   sc$url$player  <- paste0(sc$url$draft,'players/%s?embed=notes,odds,player_stats,player_match_stats,positions,trades')
#   sc$url$league  <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/ladderAndFixtures?round=%s&scores=true')
#   sc$url$team    <- paste0(sc$url$draft,'userteams/%s/statsPlayers?round=%s')
#   
#   sc$url$playerStatus <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/playersStatus')
#   
#   sc$url$aflFixture <- paste0(sc$url$draft,'real_fixture')
#   
#   sc$url$teamTrades       <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/teamtrades')
#   sc$url$trades           <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/trades')
#   sc$url$processedWaivers <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/processedWaivers')
#   sc$url$draftResult      <- paste0(sc$url$draft,'leagues/',sc$var$league_id,'/recap')
#   
#   
#   
#   # sc$url$player       <- paste0(base,year,draft,'players/%s?embed=notes,odds,player_stats,player_match_stats,positions,trades')
#   # sc$url$statsPack    <- paste0(base,year,draft,'completeStatspack?player_id=')
#   # sc$url$league       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/ladderAndFixtures?round=',c(1:23),'&scores=true')
#   # 
#   # # Call API
#   # sc$api$league <- get_sc_data(sc$auth, sc$url$league[1])
#   # 
#   # # Save common variables
#   # sc$var$team_id <- as.numeric(sapply(sc$api$league$ladder, function(x) x$user_team_id))
#   # 
#   # # Generate API URLs
#   # sc$url$teams <- outer(paste0(base,year,draft,'userteams/',sc$var$team_id,'/statsPlayers?round='),c(0:sc$var$current_round),FUN="paste0")
#   # dim(sc$url$teams) <- NULL
#   # sc$url$teamTrades       <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/teamtrades')
#   # sc$url$trades           <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/trades')
#   # sc$url$processedWaivers <- paste0(base,year,draft,'leagues/',sc$var$league_id,'/processedWaivers')
#   # sc$url$aflFixture       <- paste0(base,year,draft,'real_fixture')
#   
#   return(sc)
# }
# 
# 
# sc_fixture <- function(sc, round=NA){
#   print_log(paste0('sc_fixture: Round ', round))
#   
#   if(is.na(round)) {
#     fixture_data <- sc_download(sc$auth, sc$url$aflFixture)
#     rounds <- unlist(lapply(fixture_data, function(x) x$round))
#     minRound <- min(rounds)
#     maxRound <- max(rounds)
#     round <- c(minRound:maxRound)
#   }
#   
#   url <- sprintf(sc$url$league, round)
#   fixture_data <- lapply(url, sc_download, auth=sc$auth)
#   
#   fixture_data2 <- bind_rows(lapply(fixture_data, function(round_data){
#     res <- bind_rows(lapply(round_data$fixtures, function(match_data){
#       
#       res <- tibble(
#         round = match_data$round,
#         fixture = match_data$fixture,
#         team_id = match_data$user_team1$id,
#         team_name = match_data$user_team1$teamname,
#         user_id = match_data$user_team1$user_id,
#         user_name = match_data$user_team1$user$first_name,
#         opponent_team_id = match_data$user_team2$id,
#         opponent_team_name = match_data$user_team2$teamname,
#         opponent_user_id = match_data$user_team2$user_id,
#         opponent_user_name = match_data$user_team2$user$first_name
#       )
#       
#       return(res)
#       
#     }))
#   }))
#   
#   fixture_data3 <- fixture_data2[c(1,2,7:10,3:6)]
#   names(fixture_data3) <- names(fixture_data2)
#   
#   fixture_data4 <- bind_rows(fixture_data2, fixture_data3) %>%
#     arrange(round, fixture) %>%
#     mutate(season = sc$var$season) %>%
#     relocate(season, .before= round)
#   
#   return(fixture_data4)
# }
# sc_results <- function(sc, round=NA){
# 
#   print_log(paste0('sc_results: Round ', round))
#   
#   if(is.na(round)) round <- sc$var$current_round
#   
#   url <- sprintf(sc$url$league, round)
#   results_data <- sc_download(sc$auth, url)
#   
#   results_data2 <- bind_rows(lapply(results_data$ladder, function(x){
#     tibble(
#       season   = as.numeric(sc$var$season),
#       round    = x$round,
#       team_id  = x$user_team_id,
#       position = x$position,
#       points   = x$points,
#       wins     = x$wins,
#       draws    = x$draws,
#       losses   = x$losses,
#       points_for = x$points_for,
#       points_against = x$points_against,
#       pcnt     = round(x$points_for/x$points_against,3),
#       team_score = x$userTeam$stats[[1]]$points
#     )
#   }))
#   
#   return(results_data2)
# }
# 
# 
# 
# 
# 
# ### DASHBOARD FUNCTIONS --------------------------------------------------------
# 
# sc_data_refresh <- function(sc){
#   print_log('sc_data_refresh')
#   
#   szn <- sc$var$season
#   rnd <- 9
#   
#   # Update Player Data
#   players_path <- 'fantasy-banter/master/player-data.csv'
#   player_data_old <- gcs_download(players_path)
#   player_data_new <- sc_player_data(sc, rnd)
#   
#   player_data_final <- player_data_old %>%
#     mutate( f = season == player_data_new$season & round == player_data_new$round) %>%
#     filter(!f) %>%
#     select(-f) %>%
#     bind_rows(player_data_new) %>%
#     arrange(desc(season), desc(round), desc(points))
#      
#   save_players <- gcs_save(player_data_final, players_path)
#   
#   
#   # Update League Data
#   league_path <- 'fantasy-banter/master/league-data.csv'
#   league_data <- gcs_download(league_path)
#   
#   league_res <- sc_results(sc, rnd) 
#   league_opp <- league_res %>%
#     select(season, round, team_id, team_score) %>%
#     rename(opponent_team_id = team_id) %>%
#     rename(opponent_score = team_score) 
#   
#   league_data2 <- league_data %>%
#     rows_update(league_res, by=c('season', 'round', 'team_id')) %>%
#     rows_update(league_opp, by=c('season', 'round', 'opponent_team_id')) %>%
#     mutate(differential = team_score - opponent_score)
#   
#   save_league <- gcs_save(league_data2, league_path)
#   
# }
# 
#   
#   auth <- sc_auth(cid,tkn)
#   sc <- sc_setup(auth)
#   
#   sc_data_refresh(sc)
#   
#   gcs_path <- paste0('fantasy-banter/master/league-data.csv')
#   league_data <- gcs_download(gcs_path)
#   
#   gcs_path <- paste0('fantasy-banter/master/player-data.csv')
#   player_data <- gcs_download(gcs_path)
#   
# 
#   x <- player_data %>%
#     mutate(points = ifelse(points==0,NA,points)) %>%
#     arrange(id, round) %>%
#     group_by(id) %>%
#     mutate(median = rollapplyr(points, 24, median, align='right', partial=T, na.rm=T)) %>%
#     mutate(median = ifelse(is.na(median),0,median))
#     
#   
#   y <- player_data %>%
#     filter(round == max(round)) %>%
#     left_join(x[,c('id','round','median')], by=c('id','round'))
#   
#   write.csv(y, 'trade.csv', na='', row.names = F)
#            
#   url <- sprintf(sc$url$draftResult)
#   data <- sc_download(sc$auth, url)
#   src_draft <- bind_rows(lapply(data, function(d){
#     tibble(
#       round     = 0,
#       team_id   = d$user_team_id,
#       player_id = d$player_id,
#       player_source = 'Draft'
#     )
#   })) 
# 
#   url <- sprintf(sc$url$teamTrades)
#   data <- sc_download(sc$auth, url)
#   src_trade <- bind_rows(lapply(data, function(d){
#     
#     players <- bind_rows(lapply(d$team_trade_players, function(p){
#       tibble(
#         player_id = p$player_id,
#         oppo_player_id = p$trade_player_id
#       )
#     }))
#     
#     trade <- tibble(
#       round    = d$round,
#       status   = d$status_id,
#       team_id  = d$trade_user_team_id,
#       oppo_team_id  = d$user_team_id
#     ) %>%
#       cross_join(players)
#     
#     trade_list <- bind_rows(trade, setNames(trade[,c(1,2,4,3,6,5)], names(trade))) %>%
#       filter(status == 7) %>%
#       select(round,team_id,player_id) %>%
#       mutate(player_source = 'Trade') %>%
#       filter(player_id != 0)
#     
#   })) 
#   
#   url <- sprintf(sc$url$trades)
#   data <- sc_download(sc$auth, url)
#   src_fa <- bind_rows(lapply(data, function(p){
#     tibble(
#       round     = p$round,
#       team_id   = p$user_id,
#       player_id = p$buy_player_id,
#       player_source = 'Free Agent'
#     ) 
#   })) 
# 
#   src_fa <- src_fa %>%
#     filter(!(round == 1 & player_id %in% src_draft$player_id))
#   
#   
#   url <- sprintf(sc$url$processedWaivers)
#   data <- sc_download(sc$auth, url)
#   src_wvr <- bind_rows(lapply(data, function(p){
#     tibble(
#       round     = p$round,
#       team_id   = p$user_team_id,
#       player_id = p$player_id,
#       player_source = 'Waiver'
#     ) 
#   }))
#     
#   src <- src_draft %>%
#     bind_rows(src_fa) %>%
#     bind_rows(src_trade) %>%
#     bind_rows(src_wvr) 
#   
#   x <- player_data %>%
#     left_join(src, by= c('round', 'id'='player_id', 'team_id')) %>%
#     arrange(team_id, id, round) %>%
#     group_by(team_id,id) %>%
#     fill(player_source, .direction = 'down') %>%
#     mutate(player_source = ifelse(is.na(team_id), NA, player_source)) %>%
#     mutate(player_source = ifelse(is.na(player_source), 'Draft', player_source)) %>%
#     filter(!is.na(team_id))
#   
#   write.csv(x, 'trade.csv')
#   
#   
#   
#   ddisplay.brewer.all()
#   display.brewer.pal(8, 'Set1')
#   x<-c('PMAC','LESTER','JMERC','RICHO','KAPPAZ','CHIEF','MELONS','GARTER')
#   rev(sort(substr(x,1,1)))
#   
#   
#   myColors <- brewer.pal(8,"Set1")
#   names(myColors) <- levels(sort(unique(league_data$user_name)))
#   colScale <- scale_colour_manual(name = "grp",values = myColors)
#   
#   sort(unique(league_data$user_name))
#   
#   library(hrbrthemes)
#   
#    league_data %>%
#      group_by(user_name) %>%
#      summarise(m=mean(team_score, na.rm=T)) %>%
#      arrange(desc(m))
#    
#    
#    colScale <- scale_colour_manual(name = "",values = myColors)
#     
#   
#   
#   data$names <- factor(data$names , levels=c("A", "D", "C", "B"))
#   
#   league_data %>%
#     filter(!is.na(team_score)) %>%
#     mutate(user_name = fct_rev(fct_reorder(user_name, team_score, .fun='median'))) %>%    
#     ggplot( aes(x=user_name, y=team_score, fill=user_name)) + 
#     geom_boxplot() +
#     #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
#     geom_point(color="black", size=0.4, alpha=0.9) +
#     theme_ipsum() +
#     theme(
#       legend.position="none",
#       plot.title = element_text(size=11)
#     ) +
#     ggtitle("A boxplot with jitter") +
#     xlab("")
#   
#   
#   
#   # hit refresh button
#   # check if refresh is required / available
#   # download file
#   # process file 
#   # save file
#   
# 
# 
# 
# 
# }
# 
# }


