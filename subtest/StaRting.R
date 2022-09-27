library(ffsimulator)
library(ggplot2)
library(ggridges)
library(rvest)

run_tan_sleeper <- function(leagueID) {
  this_conn <- sleeper_connect(2022,leagueID)
  return(ff_simulate(conn = this_conn, n_seasons = 500, n_weeks = 14))
}
run_tan_mfl <- function(leagueID) {
  this_conn <- mfl_connect(2022,leagueID)
  return(ff_simulate(conn = this_conn, n_seasons = 500, n_weeks = 14))
}

autoplot(league_sim, type = "rank")
autoplot(league_sim, type = "points")
autoplot(league_sim, type = "wins")

roster_scores <- league_sim[[4]]
head <- head(roster_scores)
summary <- summarise(roster_scores,group_by(franchise_id))
view(summary)

roster_sum <- roster_scores %>% 
  group_by(player_id) %>%
  summarize(
    player = first(player_name),
    team = first(team),
    pos = first(pos),
    franchise = first(franchise_name),
    rank = first(rank),
    season = first(season),
    avg_proj = mean(projection, na.rm = T),
    avg_proj_score = mean(projected_score, na.rm = T)
  ) %>%
  arrange(desc(season)) %>% 
  arrange(desc(franchise)) %>% 
  view


autoplot(league_sim, type = "rank")
autoplot(league_sim, type = "points")
autoplot(league_sim, type = "wins")

# Devy Leagues 5
devy_dudes <- run_tan_sleeper(800555164506365952)  # 12 Team devy_dudes
league_sim <- devy_dudes
wolf_devy <- run_tan_sleeper(784850567066419200) # 12 Team wolf_devy
league_sim <- wolf_devy
wolf_alpha <- run_tan_mfl(48095) # 24 Team alpha
league_sim <- wolf_alpha
gotg <- run_tan_mfl(35014) # 48 Team gotg
league_sim <- gotg
devy_royale_c2c <- run_tan_sleeper(792080746113241088) # 12 Team C2C NFL Side devy_royale
league_sim <- devy_royale_c2c

# 12 Team Leagues 14
th_salary_cap <- run_tan_sleeper(834644883632406528)
league_sim <- th_salary_cap
roy_dt1 <- run_tan_sleeper(774628710849589248) # DT1
league_sim <- roy_dt1
roy_dt2 <- run_tan_sleeper(781936718688485376) # DT2
league_sim <- roy_dt2
pantheon <- run_tan_sleeper(813464216357945344) # pantheon
league_sim <- pantheon
joe_montana <- run_tan_sleeper(785753223159062528) # joe_montana
league_sim <- joe_montana
nothing_better <- run_tan_sleeper(784497182270140416) # nothin_better
league_sim <- nothing_better
flock <- run_tan_sleeper(795136755224395776) # flock
league_sim <- flock 
rocket <- run_tan_sleeper(784376679538798592) # rocket
league_sim <- rocket
forsha <- run_tan_sleeper(785034782676484096) # forsha
league_sim <- forsha
phil_bb1 <- run_tan_sleeper(801266676279652352) #SFBish Scoring Phil_bb1
league_sim <- phil_bb1
sl_262 <- run_tan_mfl(63397) # sl_262
league_sim <- sl_262
sl_512 <- run_tan_mfl(64669) # sl_512
league_sim <- sl_512
sl_671 <- run_tan_mfl(53494) # sl_671
league_sim <- sl_671
sl_698 <- run_tan_mfl(31749) # sl_698
league_sim <- sl_698
dt_duplicate <- run_tan_mfl(14365) # DT_Duplicate
league_sim <- dt_duplicate
int_bowl <- run_tan_sleeper(788877106963705856) # int_bowl 2TE
league_sim <- int_bowl

# 14 Team Leagues 2
galaxy_brains <- run_tan_sleeper(814701319007891456) # 2TE # galaxy_brains
league_sim <- galaxy_brains
sl_293 <- run_tan_mfl(63321) # sl_293
league_sim <- sl_293
th_14 <- run_tan_sleeper(834134821763665920)
league_sim <- th_14

# 16 Team Leagues 2
federation1 <- run_tan_sleeper(784915471026765824) #SFBish Scirubg # federation_1
league_sim <- federation1
tlateou_v <- run_tan_mfl(30759) # tlateou_v
league_sim <- tlateou_v

# 10 Team Home League 1
sche <- run_tan_sleeper(leagueID = 787853036843364352) #Sche
league_sim <- sche

# Redraft Leagues:
hogsRedraft <- run_tan_sleeper(leagueID = 867959247659491328) #Sche
league_sim <- hogsRedraft
ledgends <- run_tan_sleeper(leagueID = 859330008395669504) #Sche
league_sim <- ledgends



# Yates' Analyst League w/ JJ
playbook <- run_tan_sleeper(leagueID = 786702925895471104) #Sche
league_sim <- playbook

autoplot(league_sim, type = "rank")
 autoplot(league_sim, type = "points")
autoplot(league_sim, type = "wins")

league_sim <- roy_dt1
league_sim <- roy_dt2
league_sim <- pantheon
league_sim <- joe_montana
league_sim <- nothing_better
league_sim <- flock
league_sim <- rocket
league_sim <- forsha
league_sim <- phil_bb1
league_sim <- sl_262
league_sim <- sl_512
league_sim <- sl_671
league_sim <- sl_698
league_sim <- dt_duplicate
league_sim <- int_bowl
league_sim <- galaxy_brains
league_sim <- sl_293
league_sim <- federation1
league_sim <- tlateou_v
league_sim <- sche
league_sim <- devy_dudes
league_sim <- wolf_devy
league_sim <- wolf_alpha
league_sim <- gotg
league_sim <- devy_royale_c2c

# PaulyWall


mfl_conn <- mfl_connect(2022,30759)
sims <- ff_simulate(conn = mfl_conn, n_seasons = 50, n_weeks = 14)
autoplot(sims, type = "rank")
autoplot(sims, type = "points")
autoplot(sims, type = "wins")
# 
# j_bell_tolls <- ff_simulate(conn = mfl_conn , n_seasons = 200, n_weeks = 14)
# 
# autoplot(foureight_sim)
# 
sche_conn <- sleeper_connect(2022, 774628710849589248)
sche_sim <- ff_simulate(conn = sche_conn, n_seasons = 5, n_weeks = 14)
autoplot(sche_sim, type = "rank")
autoplot(sche_sim, type = "points")
autoplot(sche_sim, type = "wins")


#   726256237381042176


mock_conn <- sleeper_connect(2022, 648275184159170560)
mock_sim <- ff_simulate(conn = mock_conn, n_seasons = 10, n_weeks = 14)
autoplot(mock_sim, type = "rank")
autoplot(mock_sim, type = "points")
autoplot(mock_sim, type = "wins")
# mock_teams <- mock_sim$roster_scores

sfb_conn <- mfl_connect(2021,29497)
sfb_sim <- ff_simulate(conn = sfb_conn, n_seasons = 100, n_weeks = 18)
autoplot(sfb_sim, "rank")


# seasons <- 2020
# 
# pbp <- load_pbp(seasons)


# offenses <- pbp %>%
#   filter(special == 0) %>%
#   group_by(posteam) %>%
#   summarize(
#     yards_per_play = mean(yards_gained, na.rm = T),
#     team = first(posteam)
#   ) %>%
#   arrange(desc(yards_per_play)) %>%
#   mutate(
#     football_team = 1,
#     above_average = if_else(yards_per_play > mean(yards_per_play),1,0)
#   )
# 
# defenses <- pbp %>%
#   group_by(defteam) %>%
#   summarize(
#     yards_allowed_per_play = mean(yards_gained, na.rm = T),
#     team = first(posteam)
#   ) %>%
#   arrange(desc(yards_allowed_per_play)) %>%
#   mutate(
#     football_team = 1,
#     above_average = if_else(yards_allowed_per_play < mean(yards_allowed_per_play),1,0)
#   )
# 
# teams <- merge(offenses,defenses,by.x = ('posteam'), by.y = ('defteam')) %>% 
#   select(-football_team.x, -football_team.y) %>% 
#   filter(!is.na(posteam)) %>%
#   left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))



# ggplot(data = teams)+
#   # geom_point(mapping = aes(x=yards_per_play, y = yards_allowed_per_play, colour = posteam))+
#   geom_image(mapping = aes(x = yards_per_play, y = yards_allowed_per_play, image = team_logo_espn))+
#   # geom_smooth(mapping = aes(x=yards_per_play, y = yards_allowed_per_play), se = F)+
#   geom_vline(xintercept = mean(offenses$yards_per_play, na.rm = T), color = 'red', lty = 'dashed')+
#   geom_hline(yintercept = mean(defenses$yards_allowed_per_play, na.rm = T), color = 'red', lty = 'dashed')+
#   labs(title = 'Yards per play by team in 2020',
#        caption = 'Phil made this | data from nflfastR'
#        )+
#   xlab("Yards Gained Per Play")+
#   ylab("Yards Allowd Per Play")+
#   theme_bw()
# 00-0032211 lockett, "00-0033871" c davis, 00-0034272 MVS
  playerID1 <- "00-0032211"
  playerID2 <- "00-0034272"
  
  player_stats <- filter(pbp, !is.na(passer_player_id), !is.na(air_yards), posteam == "GB") %>% 
      group_by(receiver_player_id) 
      

  # playerStats <- pbp %>% 
  #   group_by(posteam)

  ggplot(data = player_stats)+
    geom_freqpoly(mapping = aes(x = air_yards, y = ..density.., color = receiver_player_name), binwidth = 5)+
    # geom_violin(mapping = aes(x = reorder(receiver_player_name,air_yards,FUN = median),y = air_yards))+
    # facet_wrap(~ receiver_player_name)+
    labs(title = 'Target Distributions 2020',
         caption = 'Phil made this | data from nflfastR'
    )

  
  # ggplot(data = player_stats)+
    
    