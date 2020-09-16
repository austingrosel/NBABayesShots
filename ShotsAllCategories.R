require(httr)
library(dplyr)
library(gamlss)
library(ggplot2)

source("CustomFunction.R")

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/', #players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

seasonal_df = data.frame()
players_df = data.frame()
for (seas in c("2019-20", "2018-19", "2017-18", "2016-17", "2015-16", "2014-15", "2013-14", "2012-13")) {
  print(seas)
  
  url <- paste0("https://stats.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=", seas, "&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight=")
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  
  json_resp <- jsonlite::fromJSON(content(res, "text"))
  
  frame1 <- data.frame(json_resp[["resultSets"]][["rowSet"]])[,c(1:7, 9:10, 12:13, 15:16, 18:19, 21:22)]
  colnames(frame1) = c(json_resp$resultSets$headers$columnNames[[2]][1:7],
                       'NonRA_FGM', 'NonRA_FGA', 'Mid_FGM', 'Mid_FGA', 
                       'LC3_FGM', 'LC3_FGA', 'RC3_FGM', 'RC3_FGA',
                       'AB3_FGM', 'AB3_FGA')
  
  seasonal_df = rbind(seasonal_df, frame1 %>% mutate(season = seas))
  url <- paste0("http://data.nba.net/data/10s/prod/v1/", substr(seas, 1, 4), "/players.json")
  res <- httr::GET(url = url, httr::add_headers(.headers=headers))
  json_resp <- jsonlite::fromJSON(content(res, "text"))

  players_df = rbind(players_df, data.frame(json_resp$league$standard) %>% dplyr::select(personId, firstName, lastName, nbaDebutYear, heightFeet, heightInches, pos))
  players_df = players_df %>% distinct(personId, .keep_all = T)
}

players_df = players_df %>% mutate(height = as.numeric(as.character(heightFeet)) * 12 + as.numeric(as.character(heightInches)),
                                   basic_pos = ifelse(grepl('C', pos), "Big",
                                                      ifelse(grepl('G', pos) & height < 79 & lastName != "Iguodala", "Guard", "Wing")))

df = seasonal_df %>%
  rename(Rim_FGM=FGM, Rim_FGA=FGA) %>%
  mutate_at(vars(ends_with('FGM'), 
                 ends_with('FGA')), as.character) %>%
  mutate_at(vars(ends_with('FGM'), 
                 ends_with('FGA')), as.numeric) %>%
  left_join(., players_df %>% dplyr::select(personId, basic_pos, nbaDebutYear), by = c("PLAYER_ID"="personId")) %>%
  group_by(PLAYER_ID, PLAYER_NAME, basic_pos, nbaDebutYear) %>%
  summarise_if(is.numeric, sum) %>%
  mutate(C3_FGM = LC3_FGM + RC3_FGM, C3_FGA = LC3_FGA + RC3_FGA) %>%
  ungroup()

career <- df %>%
  filter(!is.na(basic_pos)) %>%
  mutate(basic_pos = as.factor(basic_pos))

career[is.na(career)] = 0

career_rim = build_bayesian_estimate_table(career, Rim_FGM, Rim_FGA, 'Rim_FGM', 'Rim_FGA')
career_nonra <- build_bayesian_estimate_table(career, NonRA_FGM, NonRA_FGA, 'NonRA_FGM', 'NonRA_FGA')
career_mid <- build_bayesian_estimate_table(career, Mid_FGM, Mid_FGA, 'Mid_FGM', 'Mid_FGA')
career_ab3 <- build_bayesian_estimate_table(career, AB3_FGM, AB3_FGA, 'AB3_FGM', 'AB3_FGA')
career_c3 <- build_bayesian_estimate_table(career, C3_FGM, C3_FGA, 'C3_FGM', 'C3_FGA')


### Setup:

# Get Dataframe

# Choose: Midrange, Position

setup_pos = "Guard"

rim_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = Rim_FGM/Rim_FGA), Rim_FGM, Rim_FGA)
rim_x = seq(0.3, 0.8, 0.001)
  
nonra_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = NonRA_FGM/NonRA_FGA), NonRA_FGM, NonRA_FGA)
nonra_x = seq(0.3, 0.7, 0.001)

mid_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = Mid_FGM/Mid_FGA), Mid_FGM, Mid_FGA)
mid_x = seq(0.15, 0.55, 0.001)

ab3_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = AB3_FGM/AB3_FGA), AB3_FGM, AB3_FGA)
mid_x = seq(0.2, 0.5, 0.001)

c3_prior = build_prior(df %>% filter(basic_pos == setup_pos) %>% mutate(FG_PCT = C3_FGM/C3_FGA), C3_FGM, C3_FGA)
c3_x = seq(0.2, 0.5, 0.001)

player_selected = df %>% filter(nbaDebutYear == "2016") %>% mutate(player = as.character(PLAYER_NAME)) %>% pull(player)

compare_df = df %>%
  filter(PLAYER_NAME %in% player_selected) %>%
  left_join(., career_rim %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
  rename(EstRim_FGPCT = estimate,
         EstRim_Perc = est_percentile) %>%
  left_join(., career_nonra %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
  rename(EstNonRA_FGPCT = estimate,
         EstNonRA_Perc = est_percentile) %>%
  left_join(., career_mid %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
  rename(EstMid_FGPCT = estimate,
         EstMid_Perc = est_percentile) %>%
  left_join(., career_ab3 %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
  rename(EstAB3_FGPCT = estimate,
         EstAB3_Perc = est_percentile) %>%
  left_join(., career_c3 %>% ungroup() %>% dplyr::select(PLAYER_NAME, estimate, est_percentile), by = "PLAYER_NAME") %>%
  rename(EstC3_FGPCT = estimate,
         EstC3_Perc = est_percentile) %>%
  mutate(Rim_FGPCT = Rim_FGM/Rim_FGA,
         NonRA_FGPCT = NonRA_FGM/NonRA_FGA,
         Mid_FGPCT = Mid_FGM/Mid_FGA,
         AB3_FGPCT = AB3_FGM/AB3_FGA,
         C3_FGPCT = C3_FGM/C3_FGA
  ) %>%
  mutate(EsteFG = (Rim_FGA * EstRim_FGPCT + NonRA_FGA * EstNonRA_FGPCT + 
                     Mid_FGA * EstMid_FGPCT + AB3_FGA * EstAB3_FGPCT * 1.5 + C3_FGA * EstC3_FGPCT * 1.5)/
           (Rim_FGA + NonRA_FGA + Mid_FGA + AB3_FGA + C3_FGA)) %>%
  dplyr::select(PLAYER_NAME, basic_pos,
                Rim_FGA, Rim_FGPCT, EstRim_FGPCT, EstRim_Perc,
                NonRA_FGA, NonRA_FGPCT, EstNonRA_FGPCT, EstNonRA_Perc,
                Mid_FGA, Mid_FGPCT, EstMid_FGPCT, EstMid_Perc,
                AB3_FGA, AB3_FGPCT, EstAB3_FGPCT, EstAB3_Perc,
                C3_FGA, C3_FGPCT, EstC3_FGPCT, EstC3_Perc,
                EsteFG
  )
