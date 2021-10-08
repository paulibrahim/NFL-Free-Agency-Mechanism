library(dplyr)
library(data.table)
team_city_abbr <- c("ARI", "ATL", "CAR", "CHI", "DAL", "DET", "GB", "LAR", "MIN", "NO", "NYG", "PHI", "SF", "SEA", "TB",
                    "WAS", "BAL", "BUF", "CIN", "CLE", "DEN", "HOU", "IND", "JAX", "KC", "LV", "LAC", "MIA", "NE", "NYJ",
                    "PIT", "TEN")
cap_space <- runif(32, 30, 85)
team_df <- data.frame("team"=team_city_abbr, cap_space=c(runif(32, 30, 85)), playerId=c(sort(rep(c(1:15), 32))), 
                      team_player_valuation=c(runif(480, 0.66, 30)))

player_df <- data.frame(playerId=c(sort(rep(c(1:15), 32))), player_team_valuations=c(runif(15, -10, 10)), "team"=team_city_abbr)
player_df <- player_df[order(player_df$playerId, -player_df$player_team_valuations),]

player_team_info_df <- merge(team_df, player_df, by=c("team", "playerId"))
player_team_info_df$player_utility <- player_team_info_df$team_player_valuation+player_team_info_df$player_team_valuations
player_team_info_df <- player_team_info_df[order(player_team_info_df$playerId, -player_team_info_df$player_utility),]
player_team_info_df$player_team_ranking <- rep(c(1:32), 15)
player_team_info_df <- player_team_info_df[order(player_team_info_df$team, -player_team_info_df$team_player_valuation),]
player_team_info_df$team_player_ranking <- rep(c(1:15), 32)

iterative_df <- player_team_info_df
###column names in iterative_df
#team_player_valuation denotes average annual value of team contract offer to player
#player_team_valuation denotes player type for given team
#player_utility is sum of team_player_valuation and player_team_valuation
#player_team_ranking is given player's ranking of given team
#team_player_ranking is given team's ranking of given player

match_list <- list()
i <- 1
while (nrow(iterative_df)>0) {
  iterative_df$cumulative_expense <- NULL
  iterative_df$cap_adj <- NULL
  iterative_df$rank_adj <- NULL
  iterative_df$player_rank_adj <- NULL
  iterative_df$cumsum_valuation <- NULL
  
  ###STEP 1: 
  iterative_df <- iterative_df[!iterative_df$cap_space<iterative_df$team_player_valuation,]  #omitting all instances where given team's proposed contracts for a given player exceeds their salary cap space available. 
  iterative_df <- iterative_df[!iterative_df$player_utility<0,] #omitting all instances where player's utility in prospective match is less than 0
  iterative_df <- iterative_df[!iterative_df$team_player_valuation<0,] #omitting all instances where team's utility in prospective match is less than 0
  iterative_df <- (iterative_df[order(iterative_df$team, iterative_df$team_player_ranking),])
  
  ###STEP 2: 
  team_agg_valuations <- (aggregate(team_player_valuation~team, iterative_df, sum)) #computing downward cumulative sum of the average annual value of the proposed contract offer
  team_agg_valuations$cumsum_valuation <- c(0, cumsum(team_agg_valuations$team_player_valuation)[-nrow(team_agg_valuations)])
  team_agg_valuations$team_player_valuation <- NULL
  
  ###STEP 3: 
  iterative_df <- merge(iterative_df, team_agg_valuations, by="team")
  iterative_df$cumulative_expense <- cumsum(iterative_df$team_player_valuation)-iterative_df$cumsum_valuation
  initial_assignment_df <- iterative_df[iterative_df$player_team_ranking==1 & iterative_df$cumulative_expense<= iterative_df$cap_space,] #identify cases wherein the given team's cumulative sum at the player's position on their ranking is less than their cap space available
  
  match_list[[i]] <- initial_assignment_df #matching such cases
  
  #recaclculating team available cap space
  if (nrow(initial_assignment_df>0)) {
    initial_assignment_agg <- (aggregate(team_player_valuation~team, initial_assignment_df, sum))
    initial_assignment_dupl_agg <- merge(data.frame(team=initial_assignment_df$team), initial_assignment_agg, by="team")
    names(initial_assignment_agg)[names(initial_assignment_agg) == "team_player_valuation"] <- "cap_adj"
    iterative_df <- (merge(iterative_df, initial_assignment_agg, by="team", all=T))
    iterative_df$cap_adj[is.na(iterative_df$cap_adj)==T] <- 0
    iterative_df$cap_space <- iterative_df$cap_space-iterative_df$cap_adj
    
    iterative_df <- iterative_df[!iterative_df$cap_space<iterative_df$team_player_valuation,]
    iterative_df <- iterative_df[!iterative_df$playerId %in% initial_assignment_df$playerId,]
    iterative_df <- iterative_df[order(iterative_df$playerId, iterative_df$player_team_ranking),]
    
    if (nrow(iterative_df)==0 & nrow(rbindlist(match_list))==length(unique(player_df$playerId))) {
      break
    }
    
    #recaclculating team and player rankings
    re_rank_df <- (aggregate(team~playerId, iterative_df, length))
    re_rank_df$rank_adj <- c(0, cumsum(re_rank_df$team)[-nrow(re_rank_df)])
    re_rank_df$team <- NULL
    
    iterative_df <- iterative_df[order(iterative_df$playerId, -iterative_df$player_utility),]
    iterative_df$player_team_ranking <- 1:nrow(iterative_df)
    iterative_df <- merge(iterative_df, re_rank_df, by="playerId")
    iterative_df$player_team_ranking <- iterative_df$player_team_ranking-iterative_df$rank_adj
    
    iterative_df <- (iterative_df[order(iterative_df$team, iterative_df$team_player_ranking),])
    
    player_re_rank_df <- (aggregate(playerId~team, iterative_df, length))
    player_re_rank_df$player_rank_adj <- c(0, cumsum(player_re_rank_df$playerId)[-nrow(player_re_rank_df)])
    player_re_rank_df$playerId <- NULL
    
    iterative_df$team_player_ranking <- 1:nrow(iterative_df)
    iterative_df <- merge(iterative_df, player_re_rank_df, by="team")
    iterative_df$team_player_ranking <- iterative_df$team_player_ranking-iterative_df$player_rank_adj
  }
  if (nrow(initial_assignment_df)==0) {
    wd_exp_list <- list()
    p <- 1
    ###STEP 6:
    for (player_id in c(unique(iterative_df$playerId))) {
      player_it_df <- iterative_df[iterative_df$playerId==player_id,]
      player_it_df$total_utility <- player_it_df$player_utility+player_it_df$team_player_valuation
      pot_w_domi_df <- player_it_df[player_it_df$team_player_valuation <= player_it_df$cumulative_expense,]
      wd_list <- list()
      b <- 1
      for (team_id in c(unique(pot_w_domi_df$team))) {
        team_it_df <- pot_w_domi_df[pot_w_domi_df$team==team_id,]
        other_team_it_df <- player_it_df[player_it_df$team!=team_id,]
        wd_matches <- other_team_it_df[(other_team_it_df$team_player_ranking>=team_it_df$team_player_ranking & other_team_it_df$player_team_ranking>=team_it_df$player_team_ranking),]
        eq_matches <- other_team_it_df[(other_team_it_df$team_player_ranking==team_it_df$team_player_ranking & other_team_it_df$player_team_ranking==team_it_df$player_team_ranking),]
        wd_matches <- wd_matches[!(wd_matches$team %in% eq_matches$team),]
        if (nrow(eq_matches)>0) { #Tiebreak 1
          wd_matches <- rbind(eq_matches[(eq_matches$team %in% pot_w_domi_df$team)==FALSE,],wd_matches)
          eq_matches <- eq_matches[!((eq_matches$team %in% pot_w_domi_df$team)==FALSE),]
          if (nrow(eq_matches)>0) { #Tiebreak 2
            wd_matches <- rbind(eq_matches[eq_matches$total_utility!=max(c(team_it_df$total_utility, eq_matches$total_utility)),],wd_matches)
            max_match <- rbind(eq_matches[eq_matches$total_utility==max(c(team_it_df$total_utility, eq_matches$total_utility)),],team_it_df)
            if (nrow(max_match)>0) {
              wdominant_match <- max_match[sample(nrow(max_match), 1), ]
              wd_matches <- rbind(wd_matches, max_match[!(max_match$team %in% wdominant_match$team),])
            }
          }
        }
        pot_w_domi_df <- pot_w_domi_df[!(pot_w_domi_df$team %in% wd_matches$team & pot_w_domi_df$playerId %in% wd_matches$playerId),]
        wd_list[[b]] <- wd_matches
        b <- b+1
      }
      wd_df <- rbindlist(wd_list)
      iterative_df <- iterative_df[!(iterative_df$team %in% wd_df$team & iterative_df$playerId %in% wd_df$playerId),]
      wd_exp_list[[p]] <- rbindlist(wd_list)
      p <- p+1
    }
    
    ###STEP 7:
    if (nrow(rbindlist(wd_exp_list))==0) {
      iterative_df$total_utility <- iterative_df$team_player_valuation+iterative_df$player_utility
      max_util_comb <- iterative_df[iterative_df$total_utility==max(iterative_df$total_utility),]
      if (nrow(max_util_comb)>0) {
        max_util_comb$in_cap <- max_util_comb$team_player_valuation <= max_util_comb$cumulative_expense
        if (nrow(max_util_comb[max_util_comb$in_cap==TRUE,])==1) {
          max_util_comb <- max_util_comb
        } else if (nrow(max_util_comb[max_util_comb$in_cap==TRUE,])==0) {
          for (match_id in c(1:nrow(max_util_comb))) { 
            #for combinations not under cap, calculating cap-blind instances of weak domination
            spec_match_df <- max_util_comb[match_id,]
            oth_match_df <- max_util_comb[-match_id,]
            wd_matches <- oth_match_df[(oth_match_df$team_player_ranking>=spec_match_df$team_player_ranking & oth_match_df$player_team_ranking>=spec_match_df$player_team_ranking),]
            eq_matches <- oth_match_df[(oth_match_df$team_player_ranking==spec_match_df$team_player_ranking & oth_match_df$player_team_ranking==spec_match_df$player_team_ranking),]
            wd_matches <- wd_matches[!(wd_matches$team %in% eq_matches$team),]
            max_util_comb <- max_util_comb[!(max_util_comb$team %in% wd_matches$team & max_util_comb$playerId %in% wd_matches$playerId),]
          }
          if (nrow(max_util_comb)>1) { 
            #tiebreak by minimum summed team and player ranking
            max_util_comb$summed_rankings <- max_util_comb$player_team_ranking+max_util_comb$team_player_ranking
            max_util_comb <- max_util_comb[max_util_comb$summed_rankings==min(max_util_comb$summed_rankings),]
            max_util_comb$summed_rankings <- NULL
            if (nrow(max_util_comb)>1) {
              #tiebreak by randomization
              max_util_comb <- max_util_comb[sample(nrow(max_util_comb), 1), ]
            }
          }
          
        } else if (nrow(max_util_comb[max_util_comb$in_cap==TRUE,])>1) {
          #tiebreak by minimum summed team and player ranking
          max_util_comb$summed_rankings <- max_util_comb$player_team_ranking+max_util_comb$team_player_ranking
          max_util_comb <- max_util_comb[max_util_comb$summed_rankings==min(max_util_comb$summed_rankings),]
          max_util_comb$summed_rankings <- NULL
          if (nrow(max_util_comb)>1) {
            #tiebreak by randomization
            max_util_comb <- max_util_comb[sample(nrow(max_util_comb), 1), ]
          }
        }
      }
      # max_util_comb <- max_util_comb[sample(nrow(max_util_comb), 1), ]
    }
  }
  print(iterative_df)
  i <- i+1
}
match_df <- rbindlist(match_list) #this dataframe returns the matching of players and teams



