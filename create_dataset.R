library(RSQLite)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

conn <- dbConnect(RSQLite::SQLite(), "db")

write.csv(game_data, 'ncaa_game_data.csv')

game_data <- dbGetQuery(conn, "SELECT * FROM game_data") %>%
  filter(!is.na(opp_id)) %>%
  mutate(
    date = as.Date(date),
    loc = ifelse(loc == '', NA, loc),
    season = year(as.Date(date)) - ifelse(month(as.Date(date)) %in% c(1, 2, 3, 4, 5, 6), 1, 0),
    win = ifelse(result == 'W', 1, 0),
    `fg%` = fg / fga,
    `fg2%` = fg2 / fg2a,
    `fg3%` = fg3 / fg3a,
    `ft%` = ft / fta,
    `eFG%` = (fg + 0.5 * fg3) / fga,
    `TOV%` = tov / (fga + 0.44 * fta + tov),
    ftf = ft / fga,  # Free Throw Factor
    `%fg3` = fg3a / fga
  ) %>%
  group_by(season) %>%
  mutate(wk = as.numeric(ceiling(difftime(date, floor_date(min(date), 'week'), units='weeks')))) %>%
  ungroup()

game_data_with_opponent <- left_join(game_data, 
                                     select(game_data,-opp_id, -result, -loc, -home_away_neutral, -season), by=c("opp_id"="team_id", "date"="date"),
                                     suffix = c("", ".opp")) %>%
  # Statistics Requiring self and opponent data
  mutate(
    posses = (0.96*((fga.opp)+(tov.opp)+0.44*(fta.opp)-(orb.opp)) + 0.96*((fga)+(tov)+0.44*(fta)-(orb))) / 2,
    eOFF = pts / posses * 100,
    eOFF.opp = pts.opp / posses * 100,
    eDEF = pts.opp / posses * 100,
    eDEF.opp = pts / posses * 100,
    `orb%` = orb / (orb + drb.opp),
    `orb%.opp` = orb.opp / (orb.opp + drb),
    `drb%` = drb / (drb + orb),
    `drb%.opp` = drb.opp / (drb.opp + orb)
  )

effData <- game_data_with_opponent %>%
  select(team_id, opp_id, date, season, wk, eOFF, eDEF, posses) %>%
  group_by(season, team_id) %>%
  arrange(date) %>%
  mutate(
    avg.eOFF = cum_mean(lag(eOFF)),
    avg.eDEF = cum_mean(lag(eDEF))
  ) %>%
  ungroup() %>%
  left_join(select(., team_id, date, avg.eOFF, avg.eDEF), by =c("opp_id"="team_id", "date"="date"), suffix =c("", ".opp"))



cum_mean <- function(arr, na.rm=TRUE){
  my_means <- rep(NA, length(arr))
  for (i in 1:length(arr)) {
    my_means[[i]] <- mean(arr[1:i], na.rm=na.rm)
  }
  return(my_means)
}
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

season_averages <- game_data_with_opponent %>%
  mutate(wk = max(wk, 20)) %>%
  group_by(season, wk, team_id) %>%
  summarise_at(vars(-team_id, -season, -loc, -result, -opp_id, -home_away_neutral, -date), 
               function(x){mean(x, na.rm = T)}) %>%
  ungroup()

lagging_cum_mean <- function(x){cum_mean(lag(x))}

eff_results = c("eOFF", "eDEF", "eDEF.opp", "eOFF.opp")

adjustment_iteration <- function(df) {
  adjData <- df %>%
    mutate(
      adj.eOFF = ifelse(is.na(avg.eDEF.opp), eOFF, eOFF + (eOFF - avg.eDEF.opp) * 0.1),
      adj.eDEF = ifelse(is.na(avg.eOFF.opp), eDEF, eDEF + (eDEF - avg.eOFF.opp) * 0.1)
    ) %>%
    mutate(
      eDEF = adj.eDEF, eOFF = adj.eOFF
    ) %>%
    group_by(season, team_id) %>%
    arrange(date) %>%
    mutate(
      avg.eOFF = cum_mean(lag(adj.eOFF)),
      avg.eDEF = cum_mean(lag(adj.eDEF))
    ) %>% ungroup() %>% select(-avg.eOFF.opp, -avg.eDEF.opp) %>%
    inner_join(select(., team_id, date, avg.eOFF, avg.eDEF), by=c("opp_id"="team_id", "date"="date"), suffix=c("", ".opp"))
  
  print(summary(select(df, eOFF, eDEF)))
  print(summary(select(adjData, eOFF, eDEF)))
  return(adjData)
}

tmp <- adjustment_iteration(effData)
tmp <- adjustment_iteration(tmp)
tmp <- adjustment_iteration(tmp)
tmp <- adjustment_iteration(tmp)
tmp <- adjustment_iteration(tmp)
tmp <- adjustment_iteration(tmp)
tmp <- adjustment_iteration(tmp)

tmp <- adjustment_iteration(adjusted_data) %>%
  adjustment_iteration()

tmp2 <- adjusted_data %>%
  left_join(select(tmp, team_id, date, eOFF, eDEF), by=c("opp_id"="team_id", "date"="date"), suffix=c("", ".ADJ")

mean(tmp$eDEF.opp)

corr_data <- adjusted_data %>%
  select(-team_id, -season, -date) %>%
  filter(complete.cases(.) == TRUE) %>%
  cor(method = c("pearson")) %>%
  as.matrix() %>%
  corrplot()

library(corrplot)
corrplot::corrplot(as.matrix)













# CLUSTERING ===============
# Does not work
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
cluster.data <- season_averages %>%
  mutate(
    team_id = paste(team_id, season, sep='_'),
    rbd = (orb + drb) / 2 
    ) %>%
  select(`TOV%`, rbd, posses, `%fg3`) %>%
  mutate_all(range01)
cluster.kmeans <- kmeans(cluster.data, 5)

library(cluster)
library(HSAUR)
data(pottery)
km    <- kmeans(cluster.data, 3)
dissE <- daisy(cluster.data) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)

library(cluster)
library(fpc)

data(iris)
dat <- iris[, -5] # without known classification 
# Kmeans clustre analysis
clus <- kmeans(cluster.data, centers=5)
plotcluster(cluster.data, clus$cluster)

with(cluster.data, pairs(cluster.data, col=c(1:4)[clus$cluster])) 




library(factoextra)
library(NbClust)


# Elbow method
fviz_nbclust(cluster.data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(cluster.data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap Statistic
fviz_nbclust(cluster.data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


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



