adjusted_data <- game_data_with_opponent %>%
  ungroup() %>%
  select(team_id, opp_id, season, wk, date, eOFF, eDEF, eOFF.opp, eDEF.opp) %>%
  filter(complete.cases(.) == T) %>%
  mutate(wk = ifelse(wk >= 19, 19, wk)) %>%
  mutate_at(vars(-team_id, -season, -date, -wk), range01) %>%
  arrange(date) %>%
  group_by(season, wk) %>%
  mutate_at(vars(-date, -team_id, -season, -wk),function(x){x/mean(x)}) %>%
  group_by(team_id, season) %>%
  mutate_at(vars(-date, -team_id, -season, -wk),.funs = funs(season_avg=lagging_cum_mean)) %>%
  ungroup()


adjustment_iteration <- function(df) {
  adjusted_iter <- df %>%
    mutate(
      adj.eOFF = -(eDEF.opp_season_avg - 1),
      adj.eDEF = -(eOFF.opp_season_avg - 1),
      adj.eOFF.opp = -(eDEF_season_avg - 1),
      adj.eDEF.opp = -(eOFF_season_avg - 1)
    ) %>%
    mutate_at(vars(starts_with('adj')), function(x){ifelse(is.na(x), 0, x)}) %>%
    mutate(
      eOFF = eOFF + adj.eOFF,
      eDEF = eDEF + adj.eDEF,
      eOFF.opp = eOFF.opp + adj.eOFF.opp,
      eDEF.opp = eDEF.opp + adj.eDEF.opp
    ) %>%
    arrange(date) %>%
    mutate(
      eOFF = eOFF + (1 - mean(eOFF)),
      eDEF = eDEF + (1 - mean(eDEF)),
      eOFF.opp = eOFF.opp + (1 - mean(eOFF.opp)),
      eDEF.opp = eDEF.opp + (1 - mean(eDEF.opp))
    ) %>%
    group_by(team_id, season) %>%
    mutate_at(vars(eOFF, eDEF, eOFF.opp, eDEF.opp),.funs = funs(season_avg=lagging_cum_mean)) %>%
    ungroup()
  
  
  print(
    summary(
      select(
        adjusted_iter, 
        eOFF, eDEF, eOFF.opp, eDEF.opp,
        adj.eOFF, adj.eDEF, adj.eOFF.opp, adj.eDEF.opp 
      )
    )
  )
  
  return(adjusted_iter)
}