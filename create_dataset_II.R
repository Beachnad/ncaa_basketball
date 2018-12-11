# LIBRARIES =====
library(dplyr)
library(lubridate)
library(RSQLite)
library(ggplot2)

# CUSTOM FUNCITONS =====
most_common_element <- function(elements){
  x <- names(sort(table(elements), decreasing = TRUE))
  return(x[1])
}

# DATA =====
conn <- dbConnect(RSQLite::SQLite(), "db")
game_data <- dbGetQuery(conn, "SELECT * FROM game_data") %>%
  mutate(date = as.Date(date)) %>%
  mutate(
    opp_stats_id = case_when(
      game_stats_id %% 2 == 0 ~ game_stats_id + 1,
      TRUE ~ game_stats_id - 1
    ),
    season = year(date) - ifelse(month(date) %in% c(11, 12), 0, 1),
    win = ifelse(result=='W', 1, 0)
  ) %>%
  group_by(season) %>%
  mutate(
    wk = ceiling(as.numeric(date - floor_date(min(date), 'week') )/ 7)
  ) %>% ungroup() %>% 
  left_join({
    select(., season, team_id, loc) %>%
      group_by(season, team_id) %>%
      summarise(home_arena = most_common_element(loc))
  }, by=c("team_id", "season")) %>%
  left_join(select(., game_stats_id, home_arena), by=c("opp_stats_id" = "game_stats_id"), suffix=c("", ".opp")) %>%
  mutate(
    home_away_neutral = case_when(
      loc == home_arena ~ 'H',
      loc == home_arena.opp ~ 'A',
      TRUE ~ 'N'
    ),
    posses = ( 0.96*((fga)+(tov)+0.44*(fta)-(orb)) )
  ) %>%
  group_by(season, team_id) %>% arrange(date) %>%
  mutate(game_num = 1:n()) %>% ungroup()



df <- game_data %>%
  select(game_stats_id, opp_stats_id, home_away_neutral,team_id, opp_id, date, season, wk, loc, orb, drb, tov, pts, fg, fga, fg3, ft, fta, posses, game_num) %>%
  left_join(select(game_data, game_stats_id, orb, drb, tov, pts, posses), by=c("opp_stats_id"="game_stats_id"), suffix=c("", ".opp")) %>%
  mutate(
    `TS%` = pts / (2*(fga + (0.44 * fta))),
    `eFG%` = (fg + 0.5 * fg3) / fga,
    posses =  (posses + posses.opp) / 2
  ) %>%
  mutate(
    `TOV%` = tov / posses,
    `TOV%_force` = tov.opp / posses,
    `TOV_adv` = (tov / posses) / ( tov.opp / posses),
    eOFF = pts / posses,
    eDEF = pts.opp / posses,
    `ORB%` = orb / (orb + drb.opp),
    `DRB%` = drb / (drb + orb.opp),
    `FT%` = ft / fta,
    wk_adj = ifelse(wk >= 19, 19, wk),
    win = pts > pts.opp
  )

df %>%
  group_by(wk_adj) %>%
  summarise(y=mean(`TOV%`)) %>%
  ggplot(aes(x=wk_adj, y=y)) + geom_point() + geom_smooth(method='lm', formula = y ~ x)

# Some linear models to account for drifts in season averages
eOFF_lm <- lm(eOFF ~ wk_adj , df)
orb_lm <- lm(`ORB%` ~ wk_adj, df)
drb_lm <- lm(`DRB%` ~ wk_adj, df)
tov_lm <- lm(`TOV%` ~ poly(wk_adj, 2), df)
ftp_lm <- lm(`FT%` ~ wk_adj, df)
eFG_lm <- lm(`eFG%` ~ wk_adj, df)
posses_lm <- lm(posses ~ wk_adj, df)

home_adv <- function(stat_name) {
  
  df$y <- df[[stat_name]]
  
  tmp <- df %>%
    group_by(season, team_id, home_away_neutral) %>%
    summarise(y_mean = mean(y, na.rm = TRUE)) %>%
    ungroup()
  
  tmp2 <- spread(tmp, home_away_neutral ,y_mean)
  
  print(t.test(x=tmp2$H, y=tmp2$A, alternative = "greater", paired = TRUE))
  ggplot(tmp, aes(x=home_away_neutral, y=y_mean)) +
    geom_boxplot() +
    labs(title=stat_name)
}

stat_name = 'ORB%'
home_adv('eOFF')
home_adv('ORB%')
home_adv('DRB%')
home_adv('TOV%')
home_adv('eFG%')
home_adv('FT%')
home_adv('posses')

h_adv <- function(stat_name){mean(df[[stat_name]][df$home_away_neutral=='H'], na.rm=T) - mean(df[[stat_name]][df$home_away_neutral=='A'], na.rm=T)}
eOFF_home_adv <- h_adv('eOFF')
tov_home_adv <- 0
orb_home_adv <- h_adv('ORB%')
drb_home_adv <- h_adv('DRB%')
ft_home_adv <- h_adv('FT%')
efg_home_adv <- h_adv('eFG%')


factors <- c('TOV%', 'eOFF', 'eDEF', 'ORB%', 'DRB%', 'FT%', 'eFG%')

ff_bball <- df %>%
  select(game_stats_id, opp_stats_id, team_id, season, game_num, wk_adj, home_away_neutral,
         `TOV%`, eOFF, eDEF, `ORB%`, `DRB%`, `FT%`, `eFG%`) %>%
  mutate(han = home_away_neutral) %>%
  group_by(season, team_id) %>%
  arrange(game_num) %>%
  mutate(
    raw_eOFF_avg = cum_mean(lag(eOFF)),
    raw_eDEF_avg = cum_mean(lag(eDEF)),
    `raw_TOV%_avg` = cum_mean(lag(`TOV%`)),
    `raw_ORB%_avg` = cum_mean(lag(`ORB%`)),
    `raw_DRB%_avg` = cum_mean(lag(`DRB%`)),
    `raw_FT%_avg` = cum_mean(lag(`FT%`)),
    `raw_eFG%_avg` = cum_mean(lag(`eFG%`))
  ) %>% ungroup() %>%
  mutate(
    nat_eOFF = predict(eOFF_lm, .),
    `nat_TOV%` = predict(tov_lm, .),
    `nat_ORB%` = predict(orb_lm, .),
    `nat_DRB%` = predict(drb_lm, .),
    `nat_FT%` = predict(ftp_lm, .),
    `nat_eFG%` = predict(eFG_lm, .),
    nat_posses = predict(posses_lm, .)
  ) %>%
  mutate(
    raw_eOFF_avg = ifelse(is.na(raw_eOFF_avg), nat_eOFF, raw_eOFF_avg),
    raw_eDEF_avg = ifelse(is.na(raw_eDEF_avg), nat_eOFF, raw_eDEF_avg),
    `raw_TOV%_avg` = ifelse(is.na(`raw_TOV%_avg`), `nat_TOV%`, `raw_TOV%_avg`),
    `raw_ORB%_avg` = ifelse(is.na(`raw_ORB%_avg`), `nat_ORB%`, `raw_ORB%_avg`),
    `raw_DRB%_avg` = ifelse(is.na(`raw_DRB%_avg`), `nat_DRB%`, `raw_DRB%_avg`),
    `raw_FT%_avg` = ifelse(is.na(`raw_FT%_avg`), `nat_FT%`, `raw_FT%_avg`),
    `raw_eFG%_avg` = ifelse(is.na(`raw_eFG%_avg`), `nat_eFG%`, `raw_eFG%_avg`)
  ) %>%
  left_join(select(
    ., game_stats_id, raw_eOFF_avg, raw_eDEF_avg, `raw_TOV%_avg`, `raw_ORB%_avg`, `raw_DRB%_avg`, `raw_FT%_avg`, `raw_eFG%_avg`
    ), by = c("opp_stats_id"="game_stats_id"), suffix=c("", ".opp")) %>%
  mutate(
    exp_eOFF = nat_eOFF + case_when(han == 'H' ~ eOFF_home_adv, han == 'A' ~ - eOFF_home_adv, TRUE ~ 0) + (raw_eDEF_avg.opp - nat_eOFF),
    exp_eDEF = nat_eOFF + case_when(han == 'H' ~ - eOFF_home_adv, han == 'A' ~ eOFF_home_adv, TRUE ~ 0) + (raw_eOFF_avg.opp - nat_eOFF),
    # exp_tov  = `nat_TOV%`,
    exp_orb = `nat_ORB%` + case_when(han == 'H' ~ orb_home_adv, han == 'A' ~ - orb_home_adv, T ~ 0) + (`raw_DRB%_avg.opp` - `nat_DRB%`),
    exp_drb = `nat_DRB%` + case_when(han=='H'~drb_home_adv,han=='A'~=drb_home_adv,T~0) + (`raw_ORB%_avg.opp` = `nat_ORB%`),
  ) %>%
  mutate(
    adj_eDEF
  )

effData <- select(df, game_stats_id, opp_stats_id, team_id, season, wk, game_num, wk_adj, eOFF, eDEF, home_away_neutral) %>%
  mutate(nat_eOFF = predict(eOFF_lm, .)) %>%
  group_by(season, team_id) %>%
  arrange(game_num) %>%
  mutate(
    avg_eOFF = cum_mean(lag(eOFF)), 
    avg_eDEF = cum_mean(lag(eDEF))
  ) %>% ungroup() %>%
  # Temporary Replacement Strategy, Should Re-Evaluate in the future
  mutate(
    avg_eOFF = ifelse(is.na(avg_eOFF), nat_eOFF, avg_eOFF), 
    avg_eDEF = ifelse(is.na(avg_eDEF), nat_eOFF, avg_eDEF)
  ) %>%
  left_join(select(., game_stats_id, avg_eOFF, avg_eDEF), by=c("opp_stats_id"="game_stats_id"), suffix=c("", ".opp")) %>%
  mutate(
    exp_eOFF = nat_eOFF + case_when(
      home_away_neutral == 'H' ~ eOFF_home_adv / 2, 
      home_away_neutral == 'A' ~ - eOFF_home_adv / 2,
      TRUE ~ 0
    ) + (avg_eDEF.opp - nat_eOFF),
    exp_eDEF = nat_eOFF + case_when(
      home_away_neutral == 'H' ~ - eOFF_home_adv / 2, 
      home_away_neutral == 'A' ~ eOFF_home_adv / 2,
      TRUE ~ 0
    ) + (avg_eOFF.opp - nat_eOFF)
  ) %>%
  mutate(
    adj_eOFF = nat_eOFF * eOFF / exp_eOFF,
    adj_eDEF = nat_eOFF * eDEF / exp_eDEF
  )

minmax <- function(x, min=0, max=1){ case_when(x<min~min, x>max~max, T~x) }

ff_adjust_iter <-function(ff_df) {
  iter_data <- ff_df %>%
    select(game_stats_id, opp_stats_id, team_id, season, game_num, 
           eOFF, eDEF, `ORB%`, `DRB%`, posses, 
           adj_eOFF, adj_eDEF, adj_orb, adj_drb, adj_tempo,
           nat_eOFF, nat_orb, nat_drb, nat_tempo,
           home_away_neutral) %>%
    mutate(han = home_away_neutral) %>%
    group_by(season, team_id) %>%
    arrange(game_num) %>%
    mutate(
      avg_adj_eOFF = cum_mean(lag(adj_eOFF)),
      avg_adj_eDEF = cum_mean(lag(adj_eDEF)),
      avg_adj_orb  = cum_mean(lag(adj_orb)),
      avg_adj_drb  = cum_mean(lag(adj_drb)),
      avg_adj_tempo= cum_mean(lag(adj_tempo))
    ) %>% ungroup() %>%
    mutate(
      avg_adj_eOFF = ifelse(is.na(avg_adj_eOFF), nat_eOFF, avg_adj_eOFF),
      avg_adj_eDEF = ifelse(is.na(avg_adj_eDEF), nat_eOFF, avg_adj_eDEF),
      avg_adj_orb  = ifelse(is.na(avg_adj_orb), nat_orb, avg_adj_orb),
      avg_adj_drb  = ifelse(is.na(avg_adj_drb), nat_drb, avg_adj_drb),
      avg_adj_tempo= ifelse(is.na(avg_adj_tempo), nat_tempo, avg_adj_tempo)
    ) %>%
    left_join(select(., game_stats_id, avg_adj_eOFF, avg_adj_eDEF, avg_adj_orb, avg_adj_drb, avg_adj_tempo), 
              by=c("opp_stats_id"="game_stats_id"), suffix=c("", ".opp")) %>%
    mutate(
      exp_eOFF = nat_eOFF + case_when(
        home_away_neutral == 'H' ~ eOFF_home_adv / 2, 
        home_away_neutral == 'A' ~ - eOFF_home_adv / 2,
        TRUE ~ 0
      ) + (avg_adj_eDEF.opp - nat_eOFF),
      exp_eDEF = nat_eOFF + case_when(
        home_away_neutral == 'H' ~ - eOFF_home_adv / 2, 
        home_away_neutral == 'A' ~ eOFF_home_adv / 2,
        TRUE ~ 0
      ) + (avg_adj_eOFF.opp - nat_eOFF),
      exp_orb = nat_orb + case_when(han=='H'~orb_home_adv,han=='A'~ -orb_home_adv,T~0) + (avg_adj_drb.opp - nat_drb),
      exp_drb = nat_drb + case_when(han=='H'~drb_home_adv,han=='A'~ -drb_home_adv,T~0) + (avg_adj_orb.opp - nat_orb),
      exp_tempo = nat_tempo + (avg_adj_tempo.opp - nat_tempo)
    ) %>%
    mutate(
      adj_eOFF  = nat_eOFF  * eOFF / exp_eOFF,
      adj_eDEF  = nat_eOFF  * eDEF / exp_eDEF,
      adj_orb   = minmax(nat_orb   * `ORB%` / exp_orb),
      adj_drb   = minmax(nat_drb   * `DRB%` / exp_drb),
      adj_tempo = nat_tempo * posses / exp_tempo
    )
  
  print(summary(
    select(iter_data, adj_eOFF, adj_eDEF, adj_orb, adj_drb, adj_tempo)
  ))
  
  return(iter_data)
}


adjData <- select(df, game_stats_id, opp_stats_id, team_id, season, wk, game_num, wk_adj, eOFF, eDEF, `ORB%`, `DRB%`, posses, home_away_neutral) %>%
  mutate(
    nat_eOFF = predict(eOFF_lm, .),
    nat_orb = predict(orb_lm, .),
    nat_drb = predict(drb_lm, .),
    nat_tempo = predict(posses_lm, .),
    han=home_away_neutral
  ) %>%
  group_by(season, team_id) %>%
  arrange(game_num) %>%
  mutate(
    avg_eOFF = cum_mean(lag(eOFF)), 
    avg_eDEF = cum_mean(lag(eDEF)),
    avg_orb = cum_mean(lag(`ORB%`)),
    avg_drb = cum_mean(lag(`DRB%`)),
    avg_tempo = cum_mean(lag(posses))
  ) %>% ungroup() %>%
  # Temporary Replacement Strategy, Should Re-Evaluate in the future
  mutate(
    avg_eOFF = ifelse(is.na(avg_eOFF), nat_eOFF, avg_eOFF), 
    avg_eDEF = ifelse(is.na(avg_eDEF), nat_eOFF, avg_eDEF),
    avg_orb  = ifelse(is.na(avg_orb),  nat_orb,  avg_orb),
    avg_drb  = ifelse(is.na(avg_drb),  nat_drb,  avg_drb),
    avg_tempo = ifelse(is.na(avg_tempo), nat_tempo, avg_tempo)
  ) %>%
  left_join(select(., game_stats_id, avg_eOFF, avg_eDEF, avg_orb, avg_drb, avg_tempo), by=c("opp_stats_id"="game_stats_id"), suffix=c("", ".opp")) %>%
  mutate(
    exp_eOFF = nat_eOFF + case_when(
      home_away_neutral == 'H' ~ eOFF_home_adv / 2, 
      home_away_neutral == 'A' ~ - eOFF_home_adv / 2,
      TRUE ~ 0
    ) + (avg_eDEF.opp - nat_eOFF),
    exp_eDEF = nat_eOFF + case_when(
      home_away_neutral == 'H' ~ - eOFF_home_adv / 2, 
      home_away_neutral == 'A' ~ eOFF_home_adv / 2,
      TRUE ~ 0
    ) + (avg_eOFF.opp - nat_eOFF),
    exp_orb = nat_orb + case_when(han=='H' ~ orb_home_adv, han=='A'~-orb_home_adv,T~0) + (avg_drb.opp - nat_drb),
    exp_drb = nat_drb + case_when(han=='H'~drb_home_adv,han=='A'~-drb_home_adv,T~0) + (avg_orb.opp - nat_orb),
    exp_tempo = nat_tempo + (avg_tempo.opp - nat_tempo)
  ) %>%
  mutate(
    adj_eOFF = nat_eOFF * eOFF / exp_eOFF,
    adj_eDEF = nat_eOFF * eDEF / exp_eDEF,
    adj_orb = minmax(nat_orb  * `ORB%` / exp_orb),
    adj_drb = minmax(nat_drb * `DRB%` / exp_drb),
    adj_tempo = nat_tempo * posses / exp_tempo
  ) %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter() %>%
  ff_adjust_iter()

adj_ff_bball <- ff_bball %>%
  select(game_stats_id, opp_stats_id, team_id, season, `TOV%`, `FT%`, `eFG%`) %>%
  left_join(select(adjData, game_stats_id, adj_eOFF, adj_eDEF, adj_eOFF, adj_orb, adj_drb, adj_tempo, han),
            by='game_stats_id') %>%
  left_join(select(df, game_stats_id, win, season), by="game_stats_id") %>%
  group_by()
  