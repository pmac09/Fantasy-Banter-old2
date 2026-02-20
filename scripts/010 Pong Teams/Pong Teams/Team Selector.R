library(tidyverse)
options(stringsAsFactors = FALSE)


path <- './scripts/010 Pong Teams/Pong Teams'
setwd(path)


partners <- read.csv('./pongPartner.csv')
partners2 <- partners[c("YEAR", "PARTNER", "PLAYER")]
names(partners2) <- c("YEAR", "PLAYER", "PARTNER")

history <- rbind(partners, partners2)

PLAYERS <- c('RICHO',
             'PMAC',
             'MELONS',
             'LESTER',
             'KAPPAZ',
             'MATT',
             'GARTER',
             'CHIEF')


lastTeam <- history %>%
  group_by(PLAYER, PARTNER) %>%
  summarise(LATEST = max(YEAR))



h <- history %>%
  filter(PLAYER %in% PLAYERS & PARTNER %in% PLAYERS) %>%
  group_by(PLAYER,PARTNER) %>%
  summarise(YEAR = max(YEAR)) %>%
  pivot_wider(names_from=PARTNER, values_from=YEAR)

h <- h[,c('PLAYER',h$PLAYER)]
h

h <- history %>%
  filter(PLAYER %in% PLAYERS & PARTNER %in% PLAYERS) %>%
  group_by(PLAYER,PARTNER) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from=PARTNER, values_from=n)

h <- h[,c('PLAYER',h$PLAYER)]
h



# Create empty array for unique team combinations 
teamList <- list()

while (length(teamList) < 105){# Seems like the number is 105 unique combos - not sure what the actual calc is
  
  # Create new batch of teams
  teams <- lapply(split(sample(PLAYERS),rep(1:(8/2),each=2)), sort)
  
  # Check to see if combo already exists in teamList
  if(TRUE %in% unlist(lapply(teamList, function(x){length(setdiff(teams, x)) == 0}))) {
    next
  }
  
  # Add combo to team list
  teamList[[length(teamList)+1]] <- teams

}

# Remove any instance of teams from last year
# get last years teams
lastYear <- history %>%
  filter(YEAR >= as.integer(max(lastTeam$LATEST))-1) %>%
  arrange(PLAYER)




# Returns FALSE if there is at least 1 team match up from the previous year -TRUE are the usable teams
usableTeams <- unlist(lapply(teamList, function(y){
  
  usable <- all(unlist(lapply(y, function(x){
    compare <- lastYear$PLAYER == x[1] & lastYear$PARTNER == x[2]
    all(!compare)
  })))
  
  return(usable)
}))

# Count the number of match-up double ups from previous drafts
count <- unlist(lapply(teamList, function(y){
  
  sum(unlist(lapply(y, function(x) {
    compare <- history$PLAYER == x[1] & history$PARTNER == x[2]
    count <- length(compare[compare ==TRUE])
    return(count)
  })))
  
}))

# Combine results
results <- tibble(
  notLastYear = usableTeams,
  previousMatchups = count
)

#View Results
ftable(results)

# Must have a least 4 combos to make it interesting so filter for the 4th lowest count to then filter by
#fourthLowest <- sort(count)[2]
#minPrevMatchup <- min(count)
minPrevMatchup <- sort(count[usableTeams])[2]

# return combos with no teams from last year and with at least 4 teams with 0/1 double up
eligibleTeams <- teamList[usableTeams & count <= minPrevMatchup]

# Print eligible teams
str(eligibleTeams)

# Extract teams to create matrix
tms <- lapply(eligibleTeams, function(x){
  tm <- as_tibble(t(bind_rows(x)))
  colnames(tm) <- c('PLAYER', 'PARTNER')
  return(tm)
})

# Convert to a dataframe
tms2 <- bind_rows(tms)

# Add the combo count
tms2$n <- sort(rep(seq(1,nrow(tms2)/4), 4))

#merge on opposite view
tms2 <- tms2[,c('n', 'PARTNER', 'PLAYER')]
tms3 <- tms2[,c('n','PARTNER', 'PLAYER')]
colnames(tms3) <- c('n', 'PLAYER', 'PARTNER')
tms4 <- bind_rows(tms2, tms3)

ftable(tms4[,c('PLAYER', 'PARTNER')])

# Add the dupe count
tms2$dupes <- count[usableTeams & count <= minPrevMatchup][tms2$n]

# Add the latest year the teams have been together
tms5 <- left_join(tms2, lastTeam, by=c('PLAYER', 'PARTNER')) %>%
  group_by(n, dupes) %>%
  summarise(LATEST = max(LATEST, na.rm=T)) %>%
  mutate(rand = runif(1)) %>%
  ungroup() %>%
  arrange(dupes, LATEST, rand) %>%
  mutate(order = row_number())

tms5

# Prepare final output
tms6 <- left_join(tms2[,c('n','PARTNER', 'PLAYER')], 
                  tms5[,c('n','order')], 
                  by=c('n')) %>%
  arrange(order) %>%
  select(order, PLAYER, PARTNER)

# Print
print(tms6, n=100)







#team analysis
t <- history %>% 
  filter(PLAYER > PARTNER) %>%
  mutate(team = paste0(PLAYER," & ",PARTNER))

team_history <- t %>%
  left_join(t, by=c('YEAR')) %>%
  filter(team.x != team.y) %>%
  group_by(team.x, team.y) %>%
  summarise(
    n=n(),
    max=max(YEAR)
  )



tms6 %>%
  filter(PLAYER=='PMAC' | PARTNER == 'PMAC')


print(tms6 , n=100)






