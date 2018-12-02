library(RSQLite)
library(dplyr)
library(lubridate)

conn <- dbConnect(RSQLite::SQLite(), "db")


game_data <- dbGetQuery(conn, "SELECT * FROM game_data") %>%
  filter(!is.na(opp_id)) %>%
  mutate(
    date = as.Date(date),
    loc = ifelse(loc == '', NA, loc),
    season = year(as.Date(date)) - ifelse(month(as.Date(date)) %in% c(1, 2, 3, 4, 5, 6), 1, 0),
    `fg%` = fg / fga,
    `fg2%` = fg2 / fg2a,
    `fg3%` = fg3 / fg3a,
    `ft%` = ft / fta,
    `eFG%` = (fg + 0.5 * fg3) / fga,
    `TOV%` = tov / (fga + 0.44 * fta + tov)
  )

opponent_data <- select(game_data, team_id, date, fg, fga, fta, orb, drb, tov, pts)
colnames(opponent_data) <- c("team_id", "date","opp_fg", "opp_fga", "opp_fta","opp_orb", "opp_drb", "opp_tov", "opp_pts")

game_data_with_opponent <- left_join(game_data, opponent_data, by=c("opp_id"="team_id", "date"="date")) %>%
  mutate(
    `orb%` = orb / (orb + opp_drb),
    `drb%` = drb / (drb + opp_orb),
    ftf = ft / fga,  # free throw factor
    tempo = 0.96*((fga)+(tov)+0.44*(fta)-(orb)),
    #poss = 0.5*((fga + 0.4*fta - 1.07*(orb/(orb + opp_drb))*(fga - fg) + tov)+(opp_fga + 0.4*(opp_fta) - 1.07*(opp_orb)/(opp_orb + drb))*(opp_fga - opp_fg) + opp_tov))
    eOFF = pts / tempo * 100,
    eDEF = opp_pts / tempo * 100
  )

cum_mean <- function(arr, na.rm=TRUE){
  my_means <- rep(NA, length(arr))
  for (i in 1:length(arr)) {
    my_means[[i]] <- mean(arr[1:i], na.rm=na.rm)
  }
  return(my_means)
}

pre_game_metrics <- game_data_with_opponent %>%
  group_by(team_id, season) %>%
  select(team_id, opp_id, season, date, eOFF, eDEF, `TOV%`, tempo, `orb%`, `drb%`, fga, fg3a, result) %>%
  mutate_at(vars(-team_id, -opp_id, -season, -date, -result), function(x){cum_mean(lag(x))}) %>%
  mutate(
    `3pp` = fg3a / fga,
    gp = cumsum(rep(1, n()))
  ) %>%
  select(-fga, -fg3a) %>%
  ungroup()

avg_eOFF = mean(game_data_with_opponent$eOFF)
avg_eDEF = mean(game_data_with_opponent$eDEF)

# ADJUSTMENTS ===================================================
pre_game_adjusted_metrics <- pre_game_metrics %>%
  left_join(select(pre_game_metrics, -opp_id, -season), by = c('opp_id' = 'team_id', 'date'='date'), suffix=c("", ".opp")) %>%
  group_by(team_id, season) %>%
  mutate(
    AdjOFF = eOFF + (cum_mean(eDEF.opp) - avg_eDEF), 
    AdjDEF = eDEF + (cum_mean(eOFF.opp) - avg_eOFF),
    AdjOFF.opp = eOFF.opp + (cum_mean(eDEF.opp) - avg_eDEF), 
    AdjDEF = eDEF + (cum_mean(eOFF.opp) - avg_eOFF)
  )



