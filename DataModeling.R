# DATA SET UP ----

setwd("~/Documents/pickoff-game-theory")

library(tidyverse)
library(lme4)

event23 <- read_csv("data/event/2023.csv")
lead23 <- read_csv("data/lead_distance/2023.csv")
game23 <- read_csv("data/game/2023.csv")
pitch23 <- read_csv("data/pitch/2023.csv") %>% select(play_id, description)
play23 <- read_csv("data/play/2023.csv")
event_map <- read_csv("batter_event.csv")
pitch_map <- read_csv("batter_pitch.csv")
poptimes <- read_csv("catcher_throwing.csv") %>% select(player_id, player_name, arm_strength, sb_attempts)
sprints <- read_csv("sprint_speed.csv") %>% select(player_id, `last_name, first_name`, sprint_speed, competitive_runs)

# Remove Duplicate Lead Distances
no_duplicate_leads <- lead23[duplicated(lead23) == FALSE,]

# Join lead distances with rest of data
baserunners <- no_duplicate_leads %>% pivot_wider(names_from = base, values_from = c(lead_distance, runner_id))
with_leads <- play23 %>% left_join(baserunners, by = "play_id")
with_leads$run1b <- as.factor(with_leads$pre_runner_1b_id)
with_leads$run2b <- as.factor(with_leads$pre_runner_2b_id)
with_leads$run3b <- as.factor(with_leads$pre_runner_3b_id)
with_leads$lead1b <- with_leads$`lead_distance_1st Base`
with_leads$lead2b <- with_leads$`lead_distance_2nd Base`
with_leads$lead3b <-with_leads$`lead_distance_3rd Base`

pitcher_batter_catcher <- event23 %>% select(game_id, event_index, batter_id, bat_side, pitcher_id, pitch_hand, fielder_2_id, inning, half_inning, post_outs, event)

# Join players involved in with data
with_pitcher_batter_catcher <- with_leads %>% left_join(pitcher_batter_catcher, by = c("game_id", "event_index")) %>% left_join(poptimes, by = c("fielder_2_id" = "player_id")) %>% left_join(sprints, by = c("runner_id_1st Base" = "player_id"))

# Replace NA values with mean for sprint speed and arm strength
mean_ss <- weighted.mean(sprints$sprint_speed, w = sprints$competitive_runs)
mean_as <- weighted.mean(poptimes$arm_strength, w = poptimes$sb_attempts)
with_pitcher_batter_catcher$sprint_speed <- coalesce(with_pitcher_batter_catcher$sprint_speed, mean_ss)
with_pitcher_batter_catcher$arm_strength <- coalesce(with_pitcher_batter_catcher$arm_strength, mean_as)

# Map events to more general descriptions
mapped_events <- with_pitcher_batter_catcher %>% left_join(event_map, by = "event") %>% left_join(pitch23, by = "play_id") %>% left_join(pitch_map, by = "description") %>% mutate(pitch_event = ifelse(batter_description == "In Play", batter_event, batter_description)) %>% mutate(pitch_event = ifelse(is.na(pitch_event), "Not Batter Event", pitch_event))

# Replace rarely-occuring players with generic id
counts <- mapped_events %>% group_by(batter_id) %>% mutate(batterCount = n()) %>% ungroup() %>% group_by(pitcher_id) %>% mutate(pitcherCount = n()) %>% ungroup() %>% group_by(fielder_2_id) %>% mutate(fielderCount = n()) %>% ungroup() %>% group_by(run1b) %>% mutate(runnerCount = n()) %>% ungroup()
rep_level <- counts %>% mutate(batter_id = ifelse(batterCount < 500, "b1", batter_id), pitcher_id = ifelse(pitcherCount < 500, "p1", pitcher_id), fielder_2_id = ifelse(fielderCount < 500, "c1", fielder_2_id), run1b = ifelse(is.na(run1b), NA, ifelse(runnerCount < 10, "r1", as.character(run1b))))


# Make variables for whether certain events occur
sb_att_var <- rep_level %>% mutate(isSBAttempt = ifelse(is_stolen_base == TRUE | is_caught_stealing == TRUE | (runner_going & post_balls == 4), 1, 0))
sb_attempts <- sb_att_var %>% filter(isSBAttempt == 1)
pickoff_var <- sb_att_var %>% mutate(isPickAttempt = ifelse(type == "pickoff", 1, 0)) 
pickoff_attempts <- pickoff_var %>% filter(isPickAttempt == 1) %>% mutate(isSuccess = ifelse(is.na(is_pickoff), 0, ifelse(is_pickoff == TRUE, 1, 0)))

# Filter only situations where 1B is occupied and 2B/3B are not
pickoff_var_1b <- pickoff_var %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
pickoff_att_1b <- pickoff_attempts %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
sb_var_1b <- sb_att_var %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b))
sb_att_1b <- sb_attempts %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b))

# Filter runners who are no threat to steal (just for modeling) # Also filter out 3-2 counts with 2 outs
sb_threats <- sb_att_1b %>% count(pre_runner_1b_id) %>% filter(n >= 3) %>% pull(pre_runner_1b_id)
pickoff_var_1b_threats <- pickoff_var_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
pickoff_att_1b_threats <- pickoff_att_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
sb_var_1b_threats <- sb_var_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
sb_att_1b_threats <- sb_att_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)


# TRANSITION PROBABILITIES ----

# Add Runners - Dis - BSO State
runners <- pickoff_var %>% mutate(R1 = ifelse(!is.na(run1b), 1, 0), R2 = ifelse(!is.na(run2b), 1, 0), R3 = ifelse(!is.na(run3b), 1, 0), Runners = paste0(R1, R2, R3)) %>% select(-R1, -R2, -R3) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))

# Sort pitches in order and find the next state after each pitch
# If runners move, set disengagements back to 0
# If walk or strikeout, set count back to 0-0
# If end of inning, find runs scored on play
sorted_states <- runners %>% mutate(half_inn_id = paste(game_id, inning, half_inning)) %>% arrange(game_id, event_index, play_index) %>% mutate(nR1 = ifelse(!is.na(post_runner_1b_id), 1, 0), nR2 = ifelse(!is.na(post_runner_2b_id), 1, 0), nR3 = ifelse(!is.na(post_runner_3b_id), 1, 0), newRunners = paste0(nR1, nR2, nR3)) %>% select(-nR1, -nR2, -nR3) %>% mutate(post_disengagements_fix = ifelse(newRunners == Runners, post_disengagements, 0)) %>% mutate(post_dis_count = paste0(post_disengagements_fix, " ", post_balls, post_strikes)) %>% mutate(post_dis_count = ifelse(lead(event_index) != event_index | post_balls > 3 | post_strikes > 2, "0 00", post_dis_count)) %>%  mutate(New_State = paste0(newRunners, " ", post_dis_count, post_outs.x)) %>% mutate(New_State = ifelse(post_outs.x == 3, paste(3, runs_on_play), New_State)) 

# Pull outs from state and next state
sorted_states$event_outs <- sorted_states$post_outs.x - sorted_states$pre_outs

#Fix walk-offs
#sorted_states <- sorted_states %>% mutate(event_outs = ifelse(substr(New_State, 1, 1) == "3" & post_outs.x != 3, 0, event_outs))

# Recount outs in an inning - only want full innings for transition probabilities
# Create states without disengagements for grouping purposes
states_final <- sorted_states %>% group_by(half_inn_id) %>% mutate(Innings_Outs = sum(event_outs)) %>% ungroup() %>% filter(pre_balls < 4, pre_disengagements < 3) %>% mutate(State_No_Dis = paste0(Runners, " ", pre_balls, pre_strikes, pre_outs)) %>% mutate(New_State_No_Dis = paste(substr(New_State, 1, 3), substr(New_State, 7, 9))) %>% mutate(State_No_Runner = paste0(pre_balls, pre_strikes, pre_outs)) %>% mutate(New_State_No_Runner = substr(New_State, 7, 9))


# Make a grid of all possible states (868 * 868)
all_possible_states_tbl <- expand.grid(R1 = c(0,1), R2 = c(0,1), R3 = c(0,1), dis = seq(0,2), balls = seq(0,3), strikes = seq(0,2), outs = seq(0,2)) %>% mutate(State = paste0(R1, R2, R3, " ", dis, " ", balls, strikes, outs))
all_possible_states <- c(all_possible_states_tbl$State, "3 0", "3 1", "3 2", "3 3")


# BIG P MATRIX ----

# General P Matrix

#Compute Transition Counts
T_matrix <- states_final %>% group_by(State, New_State) %>% summarize(Freq = n())

# Grid of all possible states with and without disengagements
state_grid <- expand.grid(State = all_possible_states, New_State = all_possible_states)
state_grid <- state_grid %>% mutate(State_No_Dis = paste0(substr(State, 1, 3), substr(State, 6, 9)))  %>% mutate(New_State_No_Dis = paste0(substr(New_State, 1, 3), substr(New_State, 6, 9))) %>% mutate(State_No_Runner = substr(State, 7, 9)) %>% mutate(New_State_No_Runner = substr(New_State, 7, 9)) 


# P^N MATRIX (No pickoff or steal attempt)
no_picks_or_steals <- states_final %>% filter(((isPickAttempt == 0 | is.na(isPickAttempt)) & (isSBAttempt == 0 | is.na(isSBAttempt))) | (substr(State, 1, 3) != "100"))

# Get probability of event given count and outs
event_probs <- no_picks_or_steals %>% count(State_No_Dis, pitch_event) %>% group_by(State_No_Dis) %>% mutate(Prob = n / sum(n))

# Get new state given state and event
state_probs <- no_picks_or_steals %>% count(State_No_Dis, pitch_event, New_State_No_Dis) %>% group_by(State_No_Dis, pitch_event) %>% mutate(Prob = n / sum(n)) 

# Merge event and state probabilities
new_state_probs <- state_probs %>% left_join(event_probs, by = c("State_No_Dis", "pitch_event")) %>% mutate(Prob = Prob.x * Prob.y) %>% group_by(State_No_Dis, New_State_No_Dis) %>% summarize(Prob = sum(Prob))

# Add disengagements back in (Assumed Disengagements do not affect pitch results when no pickoff/steal attempts)
Old_Dis <- data.frame(Old_Dis = c(0,1,2))
# If runners move or new batter, new disengagements = 0
Dis_added <- new_state_probs %>% expand_grid(., Old_Dis) %>% mutate(New_Dis = ifelse(substr(New_State_No_Dis, 1, 3) != substr(State_No_Dis, 1, 3), 0, ifelse(substr(New_State_No_Dis, 5, 6) == "00", 0, Old_Dis))) %>% mutate(State = paste0(substr(State_No_Dis, 1, 4), Old_Dis, substr(State_No_Dis, 4, 7)))  %>% mutate(New_State = paste0(substr(New_State_No_Dis, 1, 4), New_Dis, substr(New_State_No_Dis, 4, 7))) %>% mutate(New_State = ifelse(substr(New_State, 1, 1) == "3", substr(New_State, 1, 3), New_State)) %>% select(State, New_State, Prob)

# Merge state grid with transition probs
all_transitions_no_ps <- state_grid %>% left_join(Dis_added, by = c("State", "New_State")) %>% mutate(Prob = ifelse(is.na(Prob), 0 , Prob))
P_N <- all_transitions_no_ps %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == "3 0", 1, 0), Prob)) %>% group_by(State, New_State) %>% summarize(Prob = sum(Prob))


# P^SP MATRIX (Successful Pick)

# Function for what happens on succesful pickoff
# Assumes no pickoff errors or other runners moving
# Only care about runner on 1B and no runner on 2B for this project
succesful_pick <- function(State) {
  if (substr(State, 1, 2) != "10") {
    return(State)
  }
  outs <- substr(State, 9, 9) 
  if (outs == "2") {
    return("3 0")
  }
  return(paste0("00", substr(State, 3, 3), " 0 ", substr(State, 7, 8), as.numeric(outs)+1))
}

# Function for what happens on unsuccesful pickoff
# Assumes no pickoff errors or other runners moving
unsuccesful_pick <- function(State) {
  if (substr(State, 1, 2) != "10") {
    return(State)
  }
  dis <- substr(State, 5, 5) 
  if (dis == 2) {
    return(paste0("010 0 ", substr(State, 7, 9)))
  } else {
    return(paste0(substr(State, 1, 4), as.numeric(dis) + 1, substr(State, 6, 9)))
  }
  
}

# Call function on all states
all_states <- data.frame(State = all_possible_states)
T_sp <- all_states %>% mutate(New_State = sapply(State, succesful_pick), Freq = 1)

# join state grid with transition probs
all_transitions_tsp <- state_grid %>% left_join(T_sp, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))

# Create final transition prob matrix
P_SP <-  all_transitions_tsp %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total)
P_SP <- P_SP %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob)) 


# P^UP MATRIX (Failed Pick)

# Call function on all states
T_up <- all_states %>% mutate(New_State = sapply(State, unsuccesful_pick), Freq = 1)

# join state grid with transition probs
all_transitions_tup <- state_grid %>% left_join(T_up, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))

# Create final transition prob matrix
P_UP <-  all_transitions_tup %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total)
P_UP <- P_UP %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob)) 


# P^UP MATRIX (Failed Pick)

failpick <- states_final %>% filter(is_pickoff == 0, type == "pickoff")

# Get transitions on failed stolen bases from each state
T_up <- failpick %>% group_by(Runners, pre_disengagements, newRunners) %>%  summarize(Freq = n()) %>% ungroup()

# 
Dis_added <- T_up %>% expand_grid(., pre_balls = seq(0,3), pre_strikes = seq(0,2), pre_outs = seq(0,2)) %>% mutate(New_Dis = ifelse(newRunners != Runners, 0, pre_disengagements + 1)) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))  %>% mutate(New_State = paste0(newRunners, " ", New_Dis, " ", pre_balls, pre_strikes, pre_outs)) %>% select(State, New_State, Freq)

# Merge state grid with transition probs
all_transitions_tup <- state_grid %>% left_join(Dis_added, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))
P_UP <-  all_transitions_tup %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total)
P_UP <- P_UP %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob)) %>% mutate(Prob = ifelse(substr(State,1,2) != "10", ifelse(New_State == State, 1, 0), Prob))



# P^SS MATRIX (Stolen Base)

stolen_base <- states_final %>% filter(is_stolen_base == 1 | (runner_going == TRUE & post_balls == 4))

# Get transitions on stolen bases from each state
T_ss <- stolen_base %>% group_by(State_No_Dis, New_State_No_Dis) %>%  summarize(Freq = n()) %>% ungroup()

# Add disengagements back in (Assumed Disengagements do not affect results once decision to pitch is made)
Dis_added <- T_ss %>% expand_grid(., Old_Dis) %>% mutate(New_Dis = ifelse(substr(New_State_No_Dis, 1, 3) != substr(State_No_Dis, 1, 3), 0, ifelse(substr(New_State_No_Dis, 5, 6) == "00", 0, Old_Dis))) %>% mutate(State = paste0(substr(State_No_Dis, 1, 4), Old_Dis, substr(State_No_Dis, 4, 7)))  %>% mutate(New_State = paste0(substr(New_State_No_Dis, 1, 4), New_Dis, substr(New_State_No_Dis, 4, 7))) %>% mutate(New_State = ifelse(substr(New_State, 1, 1) == "3", substr(New_State, 1, 3), New_State)) %>% select(State, New_State, Freq)

# Merge state grid with transition probs
all_transitions_tss <- state_grid %>% left_join(Dis_added, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))
P_SS <-  all_transitions_tss %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total)
P_SS <- P_SS %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob)) 

# P^US MATRIX (Caught Stealing)

caught <- states_final %>% filter(is_caught_stealing == 1)

# Get transitions on failed stolen bases from each state
T_us <- caught %>% group_by(State_No_Dis, New_State_No_Dis) %>%  summarize(Freq = n()) %>% ungroup()

# Hard code in some 3 ball states that never occured in 2023
T_us[nrow(T_us)+1,] <- list("100 300", "000 311", 1)
T_us[nrow(T_us)+1,] <- list("100 301", "000 312", 1)
T_us[nrow(T_us)+1,] <- list("100 302", "3 0", 1)
T_us[nrow(T_us)+1,] <- list("100 310", "000 321", 1)

# Add disengagements back in (Assumed Disengagements do not affect results once decision to pitch is made)
Dis_added <- T_us %>% expand_grid(., Old_Dis) %>% mutate(New_Dis = ifelse(substr(New_State_No_Dis, 1, 3) != substr(State_No_Dis, 1, 3), 0, ifelse(substr(New_State_No_Dis, 5, 6) == "00", 0, Old_Dis))) %>% mutate(State = paste0(substr(State_No_Dis, 1, 4), Old_Dis, substr(State_No_Dis, 4, 7)))  %>% mutate(New_State = paste0(substr(New_State_No_Dis, 1, 4), New_Dis, substr(New_State_No_Dis, 4, 7))) %>% mutate(New_State = ifelse(substr(New_State, 1, 1) == "3", substr(New_State, 1, 3), New_State)) %>% select(State, New_State, Freq)

# Merge state grid with transition probs
all_transitions_tus <- state_grid %>% left_join(Dis_added, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))
P_US <-  all_transitions_tus %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total)
P_US <- P_US %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob)) 


# Merge all 5 together
P_N$runner_outcome <- "N"
P_SP$runner_outcome <- "SP"
P_UP$runner_outcome <- "UP"
P_SS$runner_outcome <- "SS"
P_US$runner_outcome <- "US"

prob_transition <- rbind(P_N, P_SP, P_UP, P_SS, P_US) %>% select(State, runner_outcome, New_State, Prob) %>% mutate(Prob = ifelse(substr(State, 1, 1) == "3", ifelse(New_State == "3 0", 1, 0), Prob))





# PREPARING 2022 LEAD DATA FOR COMPARISON ----
play22 <- read_csv("data/play/2022.csv")

lead22 <- read_csv("data/lead_distance/2022.csv")

# Remove Duplicate Lead Distances
no_duplicate_leads <- lead22[duplicated(lead22) == FALSE,]

baserunners_22 <- no_duplicate_leads %>% pivot_wider(names_from = base, values_from = c(lead_distance, runner_id)) 

with_leads22 <- play22 %>% left_join(baserunners_22, by = "play_id")
with_leads22$run1b <- as.factor(with_leads22$pre_runner_1b_id)
with_leads22$run2b <- as.factor(with_leads22$pre_runner_2b_id)
with_leads22$run3b <- as.factor(with_leads22$pre_runner_3b_id)
with_leads22$lead1b <- with_leads22$`lead_distance_1st Base`
with_leads22$lead2b <- with_leads22$`lead_distance_2nd Base`
with_leads22$lead3b <-with_leads22$`lead_distance_3rd Base`



second_open_22 <- baserunners_22 %>% left_join(play22, by = "play_id") %>% filter(is.na(`lead_distance_2nd Base`), is.na(`lead_distance_3rd Base`), !is.na(`lead_distance_1st Base`)) %>% select(`lead_distance_1st Base`, pre_disengagements) %>% mutate(year = "2022")

second_open_23 <- baserunners %>% left_join(play23, by = "play_id") %>% filter(is.na(`lead_distance_2nd Base`), is.na(`lead_distance_3rd Base`), !is.na(`lead_distance_1st Base`)) %>% select(`lead_distance_1st Base`, pre_disengagements) %>% mutate(year = "2023")

leads <- rbind(second_open_22, second_open_23) %>% mutate(yeardis = paste(year, pre_disengagements)) %>% mutate(yeardis = case_when(
  year == 2022 ~ "2022 - All Situations",
  year == 2023 & pre_disengagements == 0 ~ "2023 - 0 Disengagements", 
  year == 2023 & pre_disengagements == 1 ~ "2023 - 1 Disengagements", 
  year == 2023 & pre_disengagements == 2 ~ "2023 - 2 Disengagements", 
  TRUE ~ "Error"
)) %>% filter(yeardis != "Error")

mean_leads <- leads %>% group_by(yeardis) %>% summarize(meanlead = mean(`lead_distance_1st Base`))
mean_lead22 <- mean_leads[1,2]$meanlead
mean_lead23_0 <- mean_leads[2,2]$meanlead
mean_lead23_1 <- mean_leads[3,2]$meanlead
mean_lead23_2 <- mean_leads[4,2]$meanlead

# Overal 2022 vs 2023 lead distance density plot
leads_overall <- ggplot(leads) + aes(`lead_distance_1st Base`, col = yeardis, group = yeardis) + geom_density() + xlim(3,16) + theme_classic() + labs(x = "Lead Distance", y = "Density", title = "Lead Distance at 1st Base - 2022/2023", color = "Year and Prior Disengagements") + scale_colour_manual(values = c("red", "skyblue", "dodgerblue3", "darkblue")) + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 2)) + geom_vline(xintercept = mean_lead22, col = "red")  + geom_vline(xintercept = mean_lead23_0, col = "skyblue")  + geom_vline(xintercept = mean_lead23_1, col = "dodgerblue3")  + geom_vline(xintercept = mean_lead23_2, col = "darkblue")

ppi <- 300
png("figures/leads_overall.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(leads_overall)
dev.off()


pitch22 <- read_csv("data/pitch/2022.csv") %>% select(play_id, description)
event22 <- read_csv("data/event/2022.csv")
poptimes22 <- read_csv("catcher_throwing22.csv") %>% select(player_id, player_name, arm_strength, sb_attempts)
sprints22 <- read_csv("sprint_speed22.csv") %>% select(player_id, `last_name, first_name`, sprint_speed, competitive_runs)



pitcher_batter_catcher22 <- event22 %>% select(game_id, event_index, batter_id, bat_side, pitcher_id, pitch_hand, fielder_2_id, inning, half_inning, post_outs, event)

# Join players involved in with data
with_pitcher_batter_catcher22 <- with_leads22 %>% left_join(pitcher_batter_catcher22, by = c("game_id", "event_index")) %>% left_join(poptimes22, by = c("fielder_2_id" = "player_id")) %>% left_join(sprints22, by = c("runner_id_1st Base" = "player_id"))


# Replace NA values with mean for sprint speed and arm strength
mean_ss22 <- weighted.mean(sprints22$sprint_speed, w = sprints22$competitive_runs)
mean_as22 <- weighted.mean(poptimes22$arm_strength, w = poptimes22$sb_attempts, na.rm = TRUE)
with_pitcher_batter_catcher22$sprint_speed <- coalesce(with_pitcher_batter_catcher22$sprint_speed, mean_ss22)
with_pitcher_batter_catcher22$arm_strength <- coalesce(with_pitcher_batter_catcher22$arm_strength, mean_as22)

# Map events to more general descriptions
mapped_events22 <- with_pitcher_batter_catcher22 %>% left_join(event_map, by = "event") %>% left_join(pitch22, by = "play_id") %>% left_join(pitch_map, by = "description") %>% mutate(pitch_event = ifelse(batter_description == "In Play", batter_event, batter_description)) %>% mutate(pitch_event = ifelse(is.na(pitch_event), "Not Batter Event", pitch_event))

# Replace rarely-occuring players with generic id
counts22 <- mapped_events22 %>% group_by(batter_id) %>% mutate(batterCount = n()) %>% ungroup() %>% group_by(pitcher_id) %>% mutate(pitcherCount = n()) %>% ungroup() %>% group_by(fielder_2_id) %>% mutate(fielderCount = n()) %>% ungroup() %>% group_by(run1b) %>% mutate(runnerCount = n()) %>% ungroup()
rep_level22 <- counts22 %>% mutate(batter_id = ifelse(batterCount < 500, "b1", batter_id), pitcher_id = ifelse(pitcherCount < 500, "p1", pitcher_id), fielder_2_id = ifelse(fielderCount < 500, "c1", fielder_2_id), run1b = ifelse(is.na(run1b), NA, ifelse(runnerCount < 10, "r1", as.character(run1b)))) %>% ungroup()


# Make variables for whether certain events occur
sb_att_var22 <- rep_level22 %>% mutate(isSBAttempt = ifelse(is_stolen_base == TRUE | is_caught_stealing == TRUE | (runner_going & post_balls == 4), 1, 0))
sb_attempts22 <- sb_att_var22 %>% filter(isSBAttempt == 1)
pickoff_var22 <- sb_att_var22 %>% mutate(isPickAttempt = ifelse(type == "pickoff", 1, 0)) 
pickoff_attempts22 <- pickoff_var22 %>% filter(isPickAttempt == 1) %>% mutate(isSuccess = ifelse(is.na(is_pickoff), 0, ifelse(is_pickoff == TRUE, 1, 0)))

# Filter only situations where 1B is occupied and 2B is not
pickoff_var_1b22 <- pickoff_var22 %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
pickoff_att_1b22 <- pickoff_attempts22 %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
sb_var_1b22 <- sb_att_var22 %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b))
sb_att_1b22 <- sb_attempts22 %>% filter(!is.na(run1b) & is.na(run2b) & is.na(run3b))

# Filter runners who are no threat to steal (just for modeling) # Also filter out 3-2 counts with 2 outs
sb_threats22 <- sb_att_1b22 %>% count(pre_runner_1b_id) %>% filter(n >= 3) %>% pull(pre_runner_1b_id)
pickoff_var_1b_threats22 <- pickoff_var_1b22 %>% filter(pre_runner_1b_id %in% sb_threats22) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
pickoff_att_1b_threats22 <- pickoff_att_1b22 %>% filter(pre_runner_1b_id %in% sb_threats22) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
sb_var_1b_threats22 <- sb_var_1b22 %>% filter(pre_runner_1b_id %in% sb_threats22) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
sb_att_1b_threats22 <- sb_att_1b22 %>% filter(pre_runner_1b_id %in% sb_threats22) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)



all_sb_att1b <- rbind(sb_att_1b_threats, sb_att_1b_threats22)
pitchers_in_sample <- unique(rep_level$pitcher_id)
runners_in_sample <- unique(rep_level$run1b)
all_sb_att1b <- all_sb_att1b %>% mutate(pitcher_id = ifelse(pitcher_id %in% pitchers_in_sample, pitcher_id, "p1")) %>% mutate(run1b = ifelse(run1b %in% runners_in_sample & run1b %in% sb_threats, run1b, "r1")) %>% mutate(is_stolen_base = ifelse(runner_going == TRUE & post_balls == 4, TRUE, is_stolen_base))
all_pickoff_var1b <- rbind(pickoff_var_1b_threats, pickoff_var_1b_threats22) %>% filter(pre_disengagements < 3)
all_pickoff_var1b$year <- as.factor(all_pickoff_var1b$year)
all_pickoff_var1b$pre_disengagements <- as.factor(all_pickoff_var1b$pre_disengagements)



# MODELS ----


### PLAYER-SPECIFIC

# Probability of Successful Pickoff
m1 <- glmer(isSuccess ~ lead1b + (1|pitcher_id) , data = pickoff_att_1b_threats, family = binomial)
summary(m1)
# Pitcher matters a bit - runner/catcher/batter lead to singular effect

# Probability of Pickoff Attempt
m2 <- glmer(isPickAttempt ~ lead1b + pre_balls + pre_strikes + pre_outs  + (1|pitcher_id) + (1|run1b) + as.factor(year) + as.factor(year) * pre_disengagements, data = all_pickoff_var1b, family = binomial)
summary(m2)
# Pitcher and runner matters 

# Probability of Successful SB
#m3 <- glmer(is_stolen_base ~ lead1b + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength + as.factor(year), data = all_sb_att1b, family = binomial)
#summary(m3)

# Probability of SB Attempt
# Not dependent on lead distance
#m4 <- glmer(isSBAttempt ~ pre_balls + pre_strikes + pre_outs + pre_disengagements + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength, data = sb_var_1b_threats, family = binomial)
#summary(m4)

saveRDS(m1, "m1model")
saveRDS(m2, "m2model")
#saveRDS(m3, "m3model")
#saveRDS(m4, "m4model")

m1 <- readRDS("m1model")
m2 <- readRDS("m2model")
m3 <- readRDS("m3model")
m4 <- readRDS("m4model")

# 
# # Pitcher Pickoff Ratio vs Effect
# pitcher_effects <- ranef(m1)$pitcher
# pitcher_effects$pitcher_id <- rownames(pitcher_effects)
# po_success <- pickoff_attempts %>% group_by(pitcher_id) %>% summarize(PO_Ratio = sum(isSuccess) / n(), count = n())
# joined_pitchers <- pitcher_effects %>% left_join(po_success, by = "pitcher_id")
# median(joined_pitchers$count, na.rm = TRUE)
# ggplot(joined_pitchers) + aes(PO_Ratio, `(Intercept)`, color = count > 20) + geom_point()
# 
# # Pitcher Pickoff Attempts vs Effect
# pitcher_effects <- ranef(m2)$pitcher
# pitcher_effects$pitcher_id <- rownames(pitcher_effects)
# po_att <- pickoff_var %>% group_by(pitcher_id) %>% summarize(PO_Freq = sum(isPickAttempt) / n(), count = n())
# joined_pitchers <- pitcher_effects %>% left_join(po_att, by = "pitcher_id")
# median(joined_pitchers$count, na.rm = TRUE)
# ggplot(joined_pitchers) + aes(PO_Freq, `(Intercept)`, color = count > 1553) + geom_point()
# 
# # Runner SB Success vs Effect
# runner_effects <- ranef(m3)$run1b
# runner_effects$run1b <- rownames(runner_effects)
# sb_success <- sb_attempts %>% group_by(run1b) %>% summarize(SB_Pct = mean(is_stolen_base), count = n())
# joined_runners <- runner_effects %>% left_join(sb_success, by = "run1b")
# median(joined_runners$count, na.rm = TRUE)
# ggplot(joined_runners) + aes(SB_Pct, `(Intercept)`, color = count > 8) + geom_point()
# 
# # Catcher CS Success vs Effect
# catcher_effects <- ranef(m3)$fielder_2_id
# catcher_effects$fielder_2_id <- rownames(catcher_effects)
# sb_attempts$fielder_2_id <- as.factor(sb_attempts$fielder_2_id)
# sb_success <- sb_attempts %>% group_by(fielder_2_id) %>% summarize(SB_Pct = mean(is_stolen_base),  count = n())
# joined_catchers <- catcher_effects %>% left_join(sb_success, by = "fielder_2_id")
# median(joined_catchers$count, na.rm = TRUE)
# ggplot(joined_catchers) + aes(SB_Pct, `(Intercept)`, color = count > 48) + geom_point()
# 
# 
# # Runner SB Attempts vs Effect
# runner_effects <- ranef(m4)$run1b
# runner_effects$run1b <- rownames(runner_effects)
# sb_att <- sb_att_var %>% group_by(run1b) %>% summarize(SB_Freq = mean(isSBAttempt), count = n())
# joined_runners <- runner_effects %>% left_join(sb_att, by = "run1b")
# median(joined_runners$count, na.rm = TRUE)
# ggplot(joined_runners) + aes(SB_Freq, `(Intercept)`, color = count > 597) + geom_point()





# PLOTTING MODEL RESULTS ----

lead1b <- seq(0,20,by=0.1)
pre_disengagements <- as.factor(c(0,1,2))
plot_data <- expand.grid(lead1b = lead1b, pre_disengagements = pre_disengagements)
plot_data$pre_balls <- 0
plot_data$pre_strikes <- 0
plot_data$pre_outs <- 0


# GRAPH FOR M1 (successful pickoff)
pitcher_effects <- ranef(m1)$pitcher_id
pitcher_effects <- pitcher_effects %>% arrange(`(Intercept)`)
median_pitcher_m1 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m1 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pct10_pitcher_m1 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
pitcher_id <- c(pct10_pitcher_m1, median_pitcher_m1, pct90_pitcher_m1)
plot_data_m1 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m1$PickSuccess <- predict(m1, newdata = plot_data_m1, type = "response")
plot_data_m1 <- plot_data_m1 %>% mutate(pitcher_id = ifelse(pitcher_id == pct90_pitcher_m1, "90th Percentile Pitcher", ifelse(pitcher_id == median_pitcher_m1, "Median Pitcher", "10th Percentile Pitcher")))
plot_data_m1$pitcher_id <- factor(plot_data_m1$pitcher_id, levels = c("90th Percentile Pitcher", "Median Pitcher", "10th Percentile Pitcher"))


pick_succ_plot <- ggplot(plot_data_m1, aes(x = lead1b, y = PickSuccess, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Pickoff by Lead Distance", subtitle = "Against Various Pitchers", col = "Pitcher Pickoff Skill") + theme_classic() + theme(legend.position = "bottom") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 

png("figures/prob_pickoff_success.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(pick_succ_plot)
dev.off()

# GRAPH FOR M2 (pickoff attempt) - VARY PITCHER EFFECT
pitcher_effects <- ranef(m2)$pitcher_id
pitcher_effects <- pitcher_effects %>% arrange(`(Intercept)`)
median_pitcher_m2 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m2 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pct10_pitcher_m2 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
pitcher_id <- c(pct10_pitcher_m2, median_pitcher_m2, pct90_pitcher_m2)

runner_effects <- ranef(m2)$run1b
runner_effects <- runner_effects %>% arrange(`(Intercept)`) 
median_runner_m2 <- rownames(runner_effects)[0.5 * nrow(runner_effects)]
pct90_runner_m2 <- rownames(runner_effects)[0.9 * nrow(runner_effects)]
pct10_runner_m2 <- rownames(runner_effects)[0.1 * nrow(runner_effects)]
run1b <- c(pct10_runner_m2, median_runner_m2, pct90_runner_m2)


plot_data_m2 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m2$run1b <- median_runner_m2
plot_data_m2$pre_disengagements <- 0
plot_data_m2$year <- 2023
plot_data_m2$year <- factor(plot_data_m2$year, levels = levels(all_pickoff_var1b$year))
plot_data_m2$pre_disengagements <- factor(plot_data_m2$pre_disengagements, levels = levels(all_pickoff_var1b$pre_disengagements))
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")
plot_data_m2 <- plot_data_m2 %>% mutate(pitcher_id = ifelse(pitcher_id == pct90_pitcher_m2, "90th Percentile Pitcher", ifelse(pitcher_id == median_pitcher_m2, "Median Pitcher", "10th Percentile Pitcher")))
plot_data_m2$pitcher_id <- factor(plot_data_m2$pitcher_id, levels = c("90th Percentile Pitcher", "Median Pitcher", "10th Percentile Pitcher"))
#ggplot(plot_data_m2, aes(x = lead1b, y = PickAttempt, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Various Pitchers with 0 Disengagements", col = "Pitcher Pickoff Frequency") + theme_classic() + theme(legend.position = "right") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 




# GRAPH FOR M2 (pickoff attempt) - VARY RUNNER EFFECT

plot_data_m2 <- plot_data %>% cross_join(run1b, copy = TRUE) %>% rename(run1b = y)
plot_data_m2$pitcher_id <- median_pitcher_m2
plot_data_m2$pre_disengagements <- 0
plot_data_m2$year <- "2023"
plot_data_m2$year <- factor(plot_data_m2$year, levels = levels(all_pickoff_var1b$year))
plot_data_m2$pre_disengagements <- factor(plot_data_m2$pre_disengagements, levels = levels(all_pickoff_var1b$pre_disengagements))
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")
plot_data_m2 <- plot_data_m2 %>% mutate(run1b = ifelse(run1b == pct90_runner_m2, "90th Percentile Runner", ifelse(run1b == median_runner_m2, "Median Runner", "10th Percentile Runner")))
plot_data_m2$run1b <- factor(plot_data_m2$run1b, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))
#ggplot(plot_data_m2, aes(x = lead1b, y = PickAttempt, col = run1b)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Various Runners with 0 Disengagements", col = "Runner Skill") + theme_classic() + theme(legend.position = "right") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 


# GRAPH FOR M2 (pickoff attempt) - VARY DISENGAGEMENTS
plot_data_m2 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m2$run1b <- median_runner_m2
plot_data_m2$pitcher_id <- median_pitcher_m2
plot_data_m2$year <- "2023"
plot_data_m2$year <- factor(plot_data_m2$year, levels = levels(all_pickoff_var1b$year))
plot_data_m2$pre_disengagements <- factor(plot_data_m2$pre_disengagements, levels = levels(all_pickoff_var1b$pre_disengagements))
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")

leads_df <- data.frame(lead1b = seq(0,20,0.1), pitcher_id = median_pitcher_m2, run1b = median_runner_m2, year = "2022", pre_balls = 0, pre_strikes = 0, pre_outs = 0, pre_disengagements = 0)
leads_df$year <- factor(leads_df$year, levels = levels(all_pickoff_var1b$year))
leads_df$pre_disengagements <- factor(leads_df$pre_disengagements, levels = levels(all_pickoff_var1b$pre_disengagements))
leads_df$PickAttempt <- predict(m2, newdata = leads_df, type = "response")

plot_data_m2$Legend <- as.factor(plot_data_m2$pre_disengagements)
leads_df$Legend <- '2022 (0 disengagements)'

pick_att <- ggplot(plot_data_m2) + geom_line(data = plot_data_m2, aes(x = lead1b, y = PickAttempt, col = Legend)) +
  geom_line(data = leads_df, aes(x = lead1b, y = PickAttempt, col = Legend)) + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Typical Pitcher", col = "Number of Disengagements") + theme_classic() + theme(legend.position = "bottom") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) + geom_line(data = leads_df, aes(x = lead1b, y = PickAttempt), color = "red") +
  scale_colour_manual(
    values = c("skyblue", "dodgerblue3", "darkblue", "red"),
    labels = c("0", "1", "2", "2022 (0 disengagements)")
  )

png("figures/prob_pickoff_attempt.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(pick_att)
dev.off()



# GRAPH FOR M3 (sb success) 
pitcher_effects <- ranef(m3)$pitcher_id
pitcher_effects <- pitcher_effects %>% mutate(player_id = as.numeric(rownames(pitcher_effects))) %>% arrange(`(Intercept)`) 
median_pitcher_m3 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m3 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
pct10_pitcher_m3 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pitcher_id <- c(pct10_pitcher_m3, median_pitcher_m3, pct90_pitcher_m3)

runner_effects <- ranef(m3)$run1b
runner_effects <- runner_effects %>% arrange(`(Intercept)`)
sprint_speed_coef <- fixef(m3)[3]
runner_combined <- runner_effects %>% mutate(player_id = as.numeric(rownames(runner_effects))) %>% left_join(sprints, by = "player_id") %>% mutate(combined_effect = sprint_speed_coef * sprint_speed + `(Intercept)`) %>% arrange(combined_effect) %>% filter(!is.na(sprint_speed))
median_runner_m3 <- runner_combined[0.5 * nrow(runner_combined),"player_id"]
median_ss <- runner_combined[0.5 * nrow(runner_combined),"sprint_speed"]
pct90_runner_m3 <- runner_combined[0.9 * nrow(runner_combined),"player_id"]
pct90_ss <- runner_combined[0.9 * nrow(runner_combined),"sprint_speed"]
pct10_runner_m3 <- runner_combined[0.1 * nrow(runner_combined),"player_id"]
pct10_ss <- runner_combined[0.1 * nrow(runner_combined),"sprint_speed"]
run1b <- c(pct10_runner_m3, median_runner_m3, pct90_runner_m3)

r2_runner <- round(cor(runner_combined$sprint_speed, runner_combined$combined_effect)^2, 5)
runner_eff_plot <- ggplot(runner_combined) + aes(x = sprint_speed, y = combined_effect) + geom_point() + labs(x = "Sprint Speed", y = "Runner Effect", title = "Influence of Sprint Speed on Runner Effect for SB Success", subtitle = paste0("R^2 = ", r2_runner)) + theme_classic()

png("figures/runner_effect.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(runner_eff_plot)
dev.off()


catcher_effects <- ranef(m3)$fielder_2_id
catcher_effects <- catcher_effects %>% arrange(`(Intercept)`)
arm_strength_coef <- fixef(m3)[4]
catcher_combined <- catcher_effects %>% mutate(player_id = as.numeric(rownames(catcher_effects))) %>% left_join(poptimes, by = "player_id") %>% mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept)`) %>% arrange(combined_effect) %>% filter(!is.na(arm_strength))
median_catcher_m3 <- catcher_combined[0.5 * nrow(catcher_combined),"player_id"]
median_as <- catcher_combined[0.5 * nrow(catcher_combined),"arm_strength"]
pct90_catcher_m3 <-  catcher_combined[0.1 * nrow(catcher_combined),"player_id"]
pct90_as <- catcher_combined[0.1 * nrow(catcher_combined),"arm_strength"]
pct10_catcher_m3 <-  catcher_combined[0.9 * nrow(catcher_combined),"player_id"]
pct10_as <- catcher_combined[0.9 * nrow(catcher_combined),"arm_strength"]
fielder_2_id <- c(pct10_catcher_m3, median_catcher_m3, pct90_catcher_m3)


r2_catcher <- round(cor(catcher_combined$arm_strength, catcher_combined$combined_effect)^2, 5)
catcher_eff_plot <- ggplot(catcher_combined) + aes(x = arm_strength, y = combined_effect) + geom_point() + labs(x = "Arm Strength", y = "Catcher Effect", title = "Influence of Arm Strength on Catcher Effect on SB Success", subtitle = paste0("R^2 = ", r2_catcher)) + theme_classic()

png("figures/catcher_effect.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(catcher_eff_plot)
dev.off()


## BATTERY COMBINED
battery_combined <- catcher_effects %>% mutate(player_id = as.numeric(rownames(catcher_effects))) %>% left_join(poptimes, by = "player_id") %>% cross_join(pitcher_effects) %>% mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept).x` + `(Intercept).y`) %>% arrange(combined_effect)  %>% filter(!is.na(arm_strength))
median_battery_m3_catcher <- battery_combined[0.5 * nrow(battery_combined),"player_id.x"]
median_battery_m3_pitcher <- battery_combined[0.5 * nrow(battery_combined),"player_id.y"]
median_as_bat <- battery_combined[0.5 * nrow(battery_combined),"arm_strength"]
pct90_battery_m3_catcher <-  battery_combined[0.1 * nrow(battery_combined),"player_id.x"]
pct90_battery_m3_pitcher <-  battery_combined[0.1 * nrow(battery_combined),"player_id.y"]
pct90_as_bat <- battery_combined[0.1 * nrow(battery_combined),"arm_strength"]
pct10_battery_m3_catcher <-  battery_combined[0.9 * nrow(battery_combined),"player_id.x"]
pct10_battery_m3_pitcher <-  battery_combined[0.9 * nrow(battery_combined),"player_id.y"]
pct10_as_bat <- battery_combined[0.9 * nrow(battery_combined),"arm_strength"]


## VARY PITCHER EFFECT
plot_data_m3 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$pre_disengagements <- 0
plot_data_m3$year <- 2023
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(pitcher_id = ifelse(pitcher_id == pct90_pitcher_m3, "90th Percentile Pitcher", ifelse(pitcher_id == median_pitcher_m3, "Median Pitcher", "10th Percentile Pitcher")))
plot_data_m3$pitcher_id <- factor(plot_data_m3$pitcher_id, levels = c("90th Percentile Pitcher", "Median Pitcher", "10th Percentile Pitcher"))
#ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Pitchers", col = "Pitcher SB Prevention Skill") + theme_classic() + theme(legend.position = "right") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 

## VARY RUNNER EFFECT
plot_data_m3 <- plot_data %>% cross_join(run1b, copy = TRUE) %>% rename(run1b = y)
plot_data_m3$pitcher_id <- median_pitcher_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$sprint_speed <- rep(c(pct10_ss, median_ss, pct90_ss))
plot_data_m3$pre_disengagements <- 0
plot_data_m3$year <- 2023
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(run1b = ifelse(run1b == pct90_runner_m3, "90th Percentile Runner", ifelse(run1b == median_runner_m3, "Median Runner", "10th Percentile Runner")))
plot_data_m3$run1b <- factor(plot_data_m3$run1b, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))

leads_df <- data.frame(lead1b = seq(0,20,0.1), sprint_speed = median_ss, arm_strength = mean_as, pitcher_id = median_pitcher_m3, fielder_2_id = median_catcher_m3, run1b = median_runner_m3, year = 2022)
leads_df$SBSuccess <- predict(m3, newdata = leads_df, type = "response")


plot_data_m3$Legend <- as.factor(plot_data_m3$run1b)
leads_df$Legend <- '2022 (Median Runner)'

sb_succ_plot <- ggplot(plot_data_m3) + geom_line(aes(x = lead1b, y = SBSuccess, col = Legend)) +
  geom_line(data = leads_df, aes(x = lead1b, y = SBSuccess, col = Legend))  + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Runners", col = "Runner SB Skill") + theme_classic() + theme(legend.position = "bottom") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) +
  scale_colour_manual(
    values = c("skyblue", "dodgerblue3", "darkblue", "red"),
    labels = c("90th Pct Runner", "Median Runner", "10th Pct Runner", "2022 (Median Runner)")
  )
png("figures/prob_sb_success.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(sb_succ_plot)
dev.off()

# 
# ## VARY SPRINT SPEED
# pct90_ss <- quantile(sprints$sprint_speed, 0.9)
# pct10_ss <- quantile(sprints$sprint_speed, 0.1)
# sprint_speed <- c(pct90_ss, mean_ss, pct10_ss)
# 
# plot_data_m3 <- plot_data %>% cross_join(sprint_speed, copy = TRUE) %>% rename(sprint_speed = y)
# plot_data_m3$pitcher_id <- median_pitcher_m3
# plot_data_m3$fielder_2_id <- median_catcher_m3
# plot_data_m3$arm_strength <- mean_as
# plot_data_m3$run1b <- median_runner_m3
# plot_data_m3$pre_disengagements <- 0
# plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
# plot_data_m3 <- plot_data_m3 %>% mutate(sprint_speed = ifelse(sprint_speed == pct90_ss, "90th Percentile Runner", ifelse(sprint_speed == mean_ss, "Median Runner", "10th Percentile Runner")))
# plot_data_m3$sprint_speed <- factor(plot_data_m3$sprint_speed, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))
# ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = sprint_speed)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Runners", col = "Runner Sprint Speed") + theme_classic() + theme(legend.position = "right") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 
# 
# 
# ## VARY ARM STRENGTH
# pct90_as <- quantile(poptimes$arm_strength, 0.9)
# pct10_as <- quantile(poptimes$arm_strength, 0.1)
# arm_strength <- c(pct90_as, mean_as, pct10_as)
# 
# plot_data_m3 <- plot_data %>% cross_join(arm_strength, copy = TRUE) %>% rename(arm_strength = y)
# plot_data_m3$pitcher_id <- median_pitcher_m3
# plot_data_m3$fielder_2_id <- median_catcher_m3
# plot_data_m3$sprint_speed <- mean_ss
# plot_data_m3$run1b <- median_runner_m3
# plot_data_m3$pre_disengagements <- 0
# plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
# plot_data_m3 <- plot_data_m3 %>% mutate(arm_strength = ifelse(arm_strength == pct90_as, "90th Percentile Catcher", ifelse(arm_strength == mean_as, "Median Catcher", "10th Percentile Catcher")))
# plot_data_m3$arm_strength <- factor(plot_data_m3$arm_strength, levels = c("90th Percentile Catcher", "Median Catcher", "10th Percentile Catcher"))
# ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = arm_strength)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Catchers", col = "Catcher Arm Strength") + theme_classic() + theme(legend.position = "right") +   scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue")) 





## VARY BATTERY EFFECT (ARM STRENGTH PLUS RANDOM EFFECT FOR CATCHERS PLUS RANDOM EFFECT FOR PITCHERS)
fielder_2_id <- c(pct10_battery_m3_catcher, median_battery_m3_catcher, pct90_battery_m3_catcher)

plot_data_m3 <- plot_data %>% cross_join(fielder_2_id, copy = TRUE) %>% rename(fielder_2_id = y)
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$arm_strength <- rep(c(pct10_as_bat, median_as_bat, pct90_as_bat))
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$pre_disengagements <- 0
plot_data_m3$year <- 2023
plot_data_m3 <- plot_data_m3 %>% mutate(pitcher_id = ifelse(fielder_2_id == pct90_battery_m3_catcher, pct90_battery_m3_pitcher, ifelse(fielder_2_id == median_battery_m3_catcher, median_battery_m3_pitcher, pct10_battery_m3_pitcher)))
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(fielder_2_id = ifelse(fielder_2_id == pct90_battery_m3_catcher, "90th Percentile Battery", ifelse(fielder_2_id == median_battery_m3_catcher, "Median Battery", "10th Percentile Battery")))
plot_data_m3$fielder_2_id <- factor(plot_data_m3$fielder_2_id, levels = c("90th Percentile Battery", "Median Battery", "10th Percentile Battery"))
#ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = fielder_2_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Batteries", col = "Battery Skill (Catcher + Pitcher Effect)") + theme_classic() + theme(legend.position = "right") + scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue"))


# Actual Pick Prob
grouped_picks <- pickoff_var_1b %>% group_by(round(lead1b, 1)) %>% summarize(PickProb = mean(isPickAttempt), n = n())
#ggplot(grouped_picks) + aes(x = `round(lead1b, 1)`, y = PickProb) + labs(x = "Lead Distance", y = "Pickoff Attempt Probability", title = "Actual Pickoff Probability by Lead Distance") + geom_point(aes(alpha = n))



# LITTLE p MATRICES (probability of runner outcome given state and lead distance) ----

# All "possible" leads
leads <- seq(0,30, by = 0.1)

# Make grid of states and leads
state_leads_outcomes <- expand.grid(State = all_possible_states, lead1b = leads, runner_outcome = c("N", "SP", "UP", "SS", "US"))
state_leads_exp <- state_leads_outcomes %>% mutate(pre_balls = as.numeric(substr(State, 7, 7)), pre_strikes = as.numeric(substr(State, 8, 8)), pre_outs = as.numeric(substr(State, 9, 9)), pre_disengagements = as.factor(substr(State, 5, 5)), sb2B = ifelse(substr(State, 3, 3) == "0" & substr(State, 2, 2) == "0" & substr(State, 1, 1) == "1", 1, 0), sprint_speed = mean_ss, arm_strength = mean_as) %>% filter(pre_disengagements %in% c(0,1,2))



# Run models on each state/lead combo
state_leads_exp$year <- 2023
state_leads_exp$year <- as.factor(state_leads_exp$year)
state_leads_exp$pre_disengagements <- as.factor(state_leads_exp$pre_disengagements)

state_leads_exp$pickoff_prob <- predict(m2, newdata = state_leads_exp, type = "response", re.form = NA)
state_leads_exp$pick_succ <- predict(m1, newdata = state_leads_exp, type = "response", re.form = NA)

state_leads_exp$pre_disengagements <- as.numeric(state_leads_exp$pre_disengagements) - 2
state_leads_exp$sb_prob <- predict(m4, newdata = state_leads_exp, type = "response", re.form = NA)
state_leads_exp$sb_succ <- predict(m3, newdata = state_leads_exp, type = "response", re.form = NA)

state_leads_exp$pickoff_prob <- ifelse(state_leads_exp$sb2B == 0, 0, state_leads_exp$pickoff_prob)
state_leads_exp$pick_succ <- ifelse(state_leads_exp$sb2B == 0, 0, state_leads_exp$pick_succ)
state_leads_exp$sb_prob <- ifelse(state_leads_exp$sb2B == 0, 0, state_leads_exp$sb_prob)
state_leads_exp$sb_succ <- ifelse(state_leads_exp$sb2B == 0, 0, state_leads_exp$sb_succ)


# Pick correct probability based on runner outcome 
prob_runner_outcome <- state_leads_exp %>% mutate(Prob_RO = case_when(
  runner_outcome == "N" ~ (1 - pickoff_prob) * (1 - sb_prob),
  runner_outcome == "SP" ~ pickoff_prob * pick_succ,
  runner_outcome == "UP" ~ pickoff_prob * (1- pick_succ),
  runner_outcome == "SS" ~ (1 - pickoff_prob) *  sb_prob * sb_succ,
  runner_outcome == "US" ~ (1 - pickoff_prob) * sb_prob * (1- sb_succ)
)) %>% select(State, lead1b, sb2B, runner_outcome, Prob_RO)


#print(Sys.time())


# Join dataframes together 
prob_trans_adj <- prob_transition %>% filter(Prob > 0) %>% left_join(prob_runner_outcome, by = c("State", "runner_outcome"), relationship = "many-to-many") %>% mutate(Prob_product = Prob * Prob_RO) %>% group_by(State, New_State, lead1b) %>% summarize(TotalProb = sum(Prob_product)) %>% mutate(TotalProb = ifelse(substr(State, 1, 1) == "3", ifelse(New_State == "3 0", 1, 0), TotalProb))



# VALUE ITERATION STEPS ----

# Initial run expectancy table from empirical outcomes
run_counts <- states_final %>% group_by(half_inn_id) %>% mutate(runs_in_inning = sum(runs_on_play)) %>% mutate(runs_so_far = cumsum(runs_on_play) - runs_on_play) %>% mutate(runs_roi = runs_in_inning - runs_so_far)

# Run Expactancy Table - average runs scored over the rest of the inning from each state
re_table <- run_counts %>% group_by(State) %>% summarize(RE = mean(runs_roi), n = n())
all_states_df <- data.frame(State = all_possible_states)
full_re_table <- all_states_df %>% left_join(re_table, by = "State") %>% mutate(RE = ifelse(is.na(RE), 0, RE), n = ifelse(is.na(n), 0, n))

# Get Runs Scored in Each State
transitions_expanded <- expand.grid(State = all_possible_states, New_State = all_possible_states) %>% mutate(OldRunners = as.numeric(substr(State, 1, 1)) + as.numeric(substr(State, 2, 2)) + as.numeric(substr(State, 3, 3))) %>% mutate(NewRunners = as.numeric(substr(New_State, 1, 1)) + as.numeric(substr(New_State, 2, 2)) + as.numeric(substr(New_State, 3, 3))) %>% mutate(OldOuts = as.numeric(substr(State, 9, 9))) %>% mutate(NewOuts = as.numeric(substr(New_State, 9, 9))) %>% mutate(OldDis = as.numeric(substr(State, 5, 5))) %>% mutate(NewDis = as.numeric(substr(New_State, 5, 5))) %>% mutate(OldCount = substr(State, 7,8)) %>% mutate(NewCount = substr(New_State, 7,8)) %>% mutate(NewBatter = ifelse(NewCount == "00" & NewDis == 0, 1, 0)) %>% mutate(RunnerPickedOff = ifelse(OldCount == NewCount & NewOuts == OldOuts + 1 & NewRunners == OldRunners - 1, 1, 0)) %>% mutate(NewBatter = ifelse(RunnerPickedOff == 1 & (State != "101 0 000" | New_State != "100 0 001"), 0, NewBatter))

# Runs Scored on Each Transition
all_transitions <- transitions_expanded %>% mutate(RunsScored = OldRunners + OldOuts + NewBatter - NewRunners - NewOuts)  %>% mutate(RunsScored = ifelse(OldOuts > NewOuts, 0, RunsScored), RunsScored = ifelse(NewRunners - OldRunners > 1, 0, RunsScored)) %>% mutate(RunsScored = ifelse(RunsScored < 0, 0, RunsScored)) %>% mutate(RunsScored = ifelse(is.na(NewOuts), as.numeric(substr(New_State, 3, 3)), RunsScored), RunsScored = ifelse(is.na(OldOuts), 0, RunsScored))
runs_on_transition <- all_transitions %>% select(State, New_State, RunsScored) 

# Step 2

# Get Run Values of each new state you can transition to
transition_values <- prob_trans_adj %>% left_join(runs_on_transition, by = c("State", "New_State")) %>% left_join(full_re_table, by = c("State" = "State")) %>% left_join(full_re_table, by = c("New_State" = "State")) %>% mutate(New_Value = RunsScored + RE.y) %>% rename(Old_RE = RE.x, New_RE = RE.y)

# Get the expected value of each lead distance
value_of_leads <- transition_values %>% mutate(WeightedValue = TotalProb * New_Value) %>% group_by(State, lead1b) %>% summarize(RunValue = sum(WeightedValue, na.rm = TRUE))

# Step 3

# Find the lead that maximizes EV
leads_by_state <- value_of_leads %>% group_by(State) %>% filter(row_number() == which.max(RunValue))

# Only want situations where runner on 1b and no other runners on 
runon1b <- leads_by_state %>% filter(substr(State, 1, 3) == "100")
runon1b

# Group by outcome
value_by_ro <- prob_transition %>% left_join(full_re_table, by = c("New_State" = "State")) %>% mutate(wVal = Prob * RE) %>% group_by(State, runner_outcome) %>% summarize(RV = sum(wVal))

val_of_outcomes <- prob_runner_outcome %>% left_join(value_by_ro, by = c("State", "runner_outcome")) 


# BELLMAN ITERATION


# Repeat prior steps over and over until no more policy changes
change <- Inf                                                                                     
threshold <- 0.01                                                                                
old_re_table <- full_re_table
old_run_1b <- runon1b
old_run_1b$lead1b <- 10
iterations <- 0
while(change > threshold || iterations < 2) {                                                                       
  re_vals <-  transition_values %>% 
    left_join(old_re_table, by = c("New_State" = "State")) %>%
    group_by(State, lead1b) %>% 
    summarize(RE = sum(TotalProb * (RunsScored + RE)), n = mean(n)) %>% ungroup() %>%
    group_by(State) %>% filter(row_number() == which.max(RE))
  
  new_run_1b <- re_vals %>% filter(substr(State, 1, 3) == "100")
  
  change <- new_run_1b |>                                                                              
    dplyr::left_join(old_run_1b, by = "State", suffix = c("_old", "_new")) |>                      
    with(sum(abs(lead1b_new - lead1b_old))) 
  
  print(change)
  print(old_re_table[1,2])
  
  old_re_table <- re_vals %>% select(-lead1b)     
  old_run_1b <- new_run_1b
  iterations <- iterations + 1
  
}     

value_of_outcomes <- prob_transition %>% left_join(runs_on_transition, by = c("State" = "State", "New_State" = "New_State")) %>% left_join(old_re_table, by = c("New_State" = "State")) %>% group_by(State, runner_outcome) %>% mutate(New_Value = RunsScored + RE)  %>% summarize(Outcome_Value = sum(Prob * New_Value, na.rm = TRUE)) %>% pivot_wider(names_from = runner_outcome, values_from = Outcome_Value)

# VALUE ITERATION WITH VARYING SKILL ----

skill_grid <- expand.grid(run1b_m3 = run1b, fielder_2_id_m3  = fielder_2_id)
skill_grid$pitcher_id_m3 <- c(rep(pct10_battery_m3_pitcher,3), rep(median_battery_m3_pitcher,3), rep(pct90_battery_m3_pitcher,3))
skill_grid$sprint_speed_m3  <- rep(c(pct10_ss, median_ss, pct90_ss), 3)
skill_grid$arm_strength_m3  <- c(rep(pct10_as, 3), rep(median_as, 3), rep(pct90_as, 3))
skill_grid$runner_skill <- rep(c("10th Percentile Runner", "Median Runner", "90th Percentile Runner"), 3)
skill_grid$battery_skill <- c(rep("10th Percentile Battery", 3), rep("Median Battery", 3),  rep("90th Percentile Battery", 3))

skill_grid$pitcher_id_m2 <- c(rep(pct10_pitcher_m2, 3), rep(median_pitcher_m2, 3),  rep(pct90_pitcher_m2, 3))
skill_grid$run1b_m2 <- rep(c(pct10_runner_m2, median_runner_m2, pct90_runner_m2), 3)

skill_grid$pitcher_id_m1 <- c(rep(pct10_pitcher_m1,3), rep(median_pitcher_m1,3), rep(pct90_pitcher_m1, 3))



for (i in 1:nrow(skill_grid)) {
  state_leads_it <- state_leads_exp
  state_leads_it$fielder_2_id <- skill_grid[i,2]
  state_leads_it$run1b <- skill_grid[i,1]
  state_leads_it$pitcher_id <-  skill_grid[i,3]
  state_leads_it$arm_strength <- skill_grid[i,5]
  state_leads_it$sprint_speed <- skill_grid[i,4]
  state_leads_it$sb_succ <- predict(m3, newdata = state_leads_it, type = "response")
  state_leads_it$sb_prob <- predict(m4, newdata = state_leads_it, type = "response")
  
  state_leads_it$run1b <- skill_grid[i,9]
  state_leads_it$pitcher_id <-  skill_grid[i,8]
  state_leads_it$year <- as.factor(state_leads_it$year)
  state_leads_it$pre_disengagements <- state_leads_it$pre_disengagements
  state_leads_it$pre_disengagements <- as.factor(state_leads_it$pre_disengagements)  
  state_leads_it$pickoff_prob <- predict(m2, newdata = state_leads_it, type = "response")
  
  state_leads_it$pitcher_id <-  skill_grid[i,10]
  state_leads_it$pick_succ <- predict(m1, newdata = state_leads_it, type = "response")
  
  state_leads_it$pickoff_prob <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$pickoff_prob)
  state_leads_it$pick_succ <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$pick_succ)
  state_leads_it$sb_prob <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$sb_prob)
  state_leads_it$sb_succ <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$sb_succ)
  
  prob_runner_outcome_it <- state_leads_it %>% mutate(Prob_RO = case_when(
    runner_outcome == "N" ~ (1 - pickoff_prob) * (1 - sb_prob),
    runner_outcome == "SP" ~ pickoff_prob * pick_succ,
    runner_outcome == "UP" ~ pickoff_prob * (1- pick_succ),
    runner_outcome == "SS" ~ (1 - pickoff_prob) *  sb_prob * sb_succ,
    runner_outcome == "US" ~ (1 - pickoff_prob) * sb_prob * (1- sb_succ)
  )) %>% select(State, lead1b, sb2B, runner_outcome, Prob_RO)
  
  
  #print(Sys.time())
  
  
  # Join dataframes together 
  prob_trans_adj_it <- prob_transition %>% filter(Prob > 0) %>% left_join(prob_runner_outcome_it, by = c("State", "runner_outcome"), relationship = "many-to-many") %>% mutate(Prob_product = Prob * Prob_RO) %>% group_by(State, New_State, lead1b) %>% summarize(TotalProb = sum(Prob_product)) %>% mutate(TotalProb = ifelse(substr(State, 1, 1) == "3", ifelse(New_State == "3 0", 1, 0), TotalProb))
  
  
  
  # Get Run Values of each new state you can transition to
  transition_values_it <- prob_trans_adj_it %>% left_join(runs_on_transition, by = c("State", "New_State")) %>% left_join(full_re_table, by = c("State" = "State")) %>% left_join(full_re_table, by = c("New_State" = "State")) %>% mutate(New_Value = RunsScored + RE.y) %>% rename(Old_RE = RE.x, New_RE = RE.y)
  
  # Get the expected value of each lead distance
  value_of_leads_it <- transition_values_it %>% mutate(WeightedValue = TotalProb * New_Value) %>% group_by(State, lead1b) %>% summarize(RunValue = sum(WeightedValue, na.rm = TRUE))
  
  # Step 3
  
  # Find the lead that maximizes EV
  leads_by_state_it <- value_of_leads_it %>% group_by(State) %>% filter(row_number() == which.max(RunValue))
  
  # Only want situations where runner on 1b and no runners on other bases
  runon1b_it <- leads_by_state_it %>% filter(substr(State, 1, 3) == "100")
  runon1b_it
  
  # BELLMAN ITERATION
  
  
  # Repeat prior steps over and over until no more policy changes
  change <- Inf                                                                                     
  threshold <- 0.01                                                                               
  old_re_table_it <- full_re_table
  old_run_1b_it <- runon1b_it
  old_run_1b_it$lead1b <- 10
  iterations <- 0
  while(change > threshold || iterations < 2) {                                                                       
    re_vals_it <-  transition_values_it %>% 
      left_join(old_re_table_it, by = c("New_State" = "State")) %>%
      group_by(State, lead1b) %>% 
      summarize(RE = sum(TotalProb * (RunsScored + RE))) %>% ungroup() %>%
      group_by(State) %>% filter(row_number() == which.max(RE))
    
    new_run_1b_it <- re_vals_it %>% filter(substr(State, 1, 3) == "100")
    
    change <- new_run_1b_it |>                                                                              
      dplyr::left_join(old_run_1b_it, by = "State", suffix = c("_old", "_new")) |>                      
      with(sum(abs(lead1b_new - lead1b_old))) 
    
    print(change)
    
    old_re_table_it <- re_vals_it %>% select(-lead1b)     
    old_run_1b_it <- new_run_1b_it
    iterations <- iterations + 1
    
  }     
  
  print(skill_grid[i,6])
  print(skill_grid[i,7])
  print(old_run_1b_it[1,])
  skill_grid[i, 11] <- old_run_1b_it[1,]$lead1b
  skill_grid[i, 12] <- old_run_1b_it[37,]$lead1b
  skill_grid[i, 13] <- old_run_1b_it[73,]$lead1b
}


# SUMMARY PLOTS AND TABLES ----

lead_by_state <- transition_values %>% 
  left_join(old_re_table, by = c("New_State" = "State")) %>%
  group_by(State, lead1b) %>% 
  summarize(RE = sum(TotalProb * (RunsScored + RE)), n = mean(n)) %>% ungroup() %>% filter(State %in% c("100 0 000", "100 1 000", "100 2 000")) %>% mutate(State = substr(State, 5,5))

max0 <- lead_by_state %>% filter(State == 0) %>% arrange(-RE) %>% head(1) %>% pull(lead1b)
max1 <- lead_by_state %>% filter(State == 1) %>% arrange(-RE) %>% head(1) %>% pull(lead1b)
max2 <- lead_by_state %>% filter(State == 2) %>% arrange(-RE) %>% head(1) %>% pull(lead1b)

opt_lead_plot <- ggplot(lead_by_state) + aes(lead1b, y = RE, col = State) + geom_line() + labs(x = "Lead Distance", y = "Run Expectancy", col = "Disengagements", title = "Run Expectancy of Runner on 1st, 0-0 count, 0 outs", subtitle = "Based on Lead Distance and Disengagements") + theme_classic() + theme(legend.position = "bottom") + scale_colour_manual(values = c("skyblue", "dodgerblue3", "darkblue"))+ ylim(0.9,0.96) + xlim(0,20) + geom_vline(xintercept = max0, color = "skyblue") + geom_vline(xintercept = max1, color = "dodgerblue3") + geom_vline(xintercept = max2, color = "darkblue")

png("figures/finding_optimal_lead.png", width = 7 * ppi, height = 5 * ppi, res = ppi)
print(opt_lead_plot)
dev.off()


sorted <- old_run_1b %>% mutate(bases = substr(State, 1, 3), dis = substr(State, 5,5), countouts = substr(State, 7, 9)) %>% arrange(bases, countouts, dis)


table1 <- sorted %>% ungroup() %>%  filter(substr(countouts, 1,2) == "00") %>% mutate(Runners = ifelse(bases == "100", "Man on 1st", "Men on 1st and 3rd"), Outs = substr(countouts,3,3)) %>% select(Outs, dis, lead1b) %>%  pivot_wider(names_from = dis, values_from = lead1b, names_prefix = "Disengagements_")

table2 <- sorted %>% ungroup() %>%  filter(substr(countouts, 3,3) == "0", bases == "100") %>% mutate(Count = paste0(substr(countouts,1,1),"-",substr(countouts,2,2))) %>% select(Count, dis, lead1b) %>%  pivot_wider(names_from = dis, values_from = lead1b,  names_prefix = "Disengagements_")

table3 <- skill_grid %>% ungroup() %>% select(battery_skill, runner_skill, V11, V12, V13)
colnames(table3) <- c("Battery Skill", "Runner Skill", "0 Disengagements", "1 Disengagements", "2 Disengagements")

print(
  xtable::xtable(table1, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/lead_by_runners_outs.tex"
)

print(
  xtable::xtable(table2, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/lead_by_count.tex"
)

print(
  xtable::xtable(table3, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/lead_by_players.tex"
)


## How well do runners select lead distance?

pickoff_var_1b <- pickoff_var_1b %>% filter(pre_disengagements < 3)

pv1b_states_added <- pickoff_var_1b %>% mutate(R1 = ifelse(!is.na(run1b), 1, 0), R2 = ifelse(!is.na(run2b), 1, 0), R3 = ifelse(!is.na(run3b), 1, 0), Runners = paste0(R1, R2, R3)) %>% select(-R1, -R2, -R3) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))

actual_vs_recommended_leads <- pv1b_states_added %>% left_join(old_run_1b, by = "State") %>% rename(ActualLead = lead1b.x, RecLead = lead1b.y) %>% mutate(LeadDiff = ActualLead - RecLead)


mean(actual_vs_recommended_leads$LeadDiff > 0, na.rm = TRUE) # Actual lead only exceeds recommended lead 20.0% of the team

avr_table <- actual_vs_recommended_leads %>% group_by(pre_disengagements) %>% summarize(Actual = mean(ActualLead, na.rm = TRUE), Rec = mean(RecLead, na.rm = TRUE), GreaterThan = paste(round(100 * mean(LeadDiff > 0, na.rm = TRUE),2), "%"))
colnames(avr_table) <- c("Disengagements", "Actual Lead", "Recommended Lead", "Lead Exceeds Maximum")
avr_table$Disengagements <- as.factor(avr_table$Disengagements)


print(
  xtable::xtable(avr_table, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/actual_vs_rec_lead.tex"
)

dis <- actual_vs_recommended_leads %>% mutate(nextLead = lead(ActualLead), nextRec = lead(RecLead)) %>% filter(lead(pre_disengagements) == pre_disengagements + 1, lead(event_index) == event_index) %>% mutate(RecIncrease = nextRec - RecLead, ActualIncrease = nextLead - ActualLead)

#ggplot(dis) + aes(x = ActualIncrease, color = "red") + geom_density() + geom_density(aes(x = RecIncrease, color = "dodgerblue")) + theme_classic() + xlim(-2,5)

# TWO AGENT MODEL ----

change <- Inf                                                                                     
threshold <- 0.01                                                                               
old_re_table_two <- full_re_table
old_run_1b_two <- old_run_1b
iterations <- 0
while(change > threshold || iterations < 2) {  
  
  value_of_outcomes <- prob_transition %>% left_join(runs_on_transition, by = c("State" = "State", "New_State" = "New_State")) %>% left_join(old_re_table_two, by = c("New_State" = "State")) %>% group_by(State, runner_outcome) %>% mutate(New_Value = RunsScored + RE)  %>% summarize(Outcome_Value = sum(Prob * New_Value, na.rm = TRUE)) %>% pivot_wider(names_from = runner_outcome, values_from = Outcome_Value)
  
  pitcher_decision <- state_leads_exp %>% left_join(value_of_outcomes, by = "State") %>% mutate(Val_Pick_Attempt = pick_succ * SP + (1 - pick_succ) * UP, Val_NoPick = sb_prob * sb_succ * SS + sb_prob * (1 - sb_succ) * US + (1 - sb_prob) * N) %>% mutate(Val_Pick_Attempt = ifelse(sb2B == 0, 10, Val_Pick_Attempt), Val_NoPick = ifelse(sb2B == 0, N, Val_NoPick))
  
  best_lead <- pitcher_decision %>% mutate(Diff = abs(Val_Pick_Attempt - Val_NoPick)) %>% group_by(State) %>% mutate(minDiff = min(Diff)) %>% filter(Diff == minDiff) %>% dplyr::slice(1) %>% ungroup() %>%  mutate(RE = pmin(Val_Pick_Attempt, Val_NoPick))
  
  new_re_table_two <- best_lead %>% select(State, RE) 
  
  new_run_1b_two <- best_lead %>% filter(substr(State, 1, 3) == "100") %>% select(State, lead1b, RE)
  
  change <- new_run_1b_two |>                                                                              
    dplyr::left_join(old_run_1b_two, by = "State", suffix = c("_old", "_new")) |>                      
    with(sum(abs(lead1b_new - lead1b_old))) 
  
  print(change)
  print(old_re_table_two[1,2])
  
  old_re_table_two <- new_re_table_two
  old_run_1b_two <- new_run_1b_two
  iterations <- iterations + 1
  
}     

sorted_two <- old_run_1b_two %>% mutate(bases = substr(State, 1, 3), dis = substr(State, 5,5), countouts = substr(State, 7, 9)) %>% arrange(bases, countouts, dis)

table1_twoagent <- sorted_two %>% ungroup() %>%  filter(substr(countouts, 1,2) == "00") %>% mutate(Runners = ifelse(bases == "100", "Man on 1st", "Men on 1st and 3rd"), Outs = substr(countouts,3,3)) %>% select(Runners, Outs, dis, lead1b) %>%  pivot_wider(names_from = dis, values_from = lead1b, names_prefix = "Disengagements_")

table2_twoagent <- sorted_two %>% ungroup() %>%  filter(substr(countouts, 3,3) == "0", bases == "100") %>% mutate(Count = paste0(substr(countouts,1,1),"-",substr(countouts,2,2))) %>% select(Count, dis, lead1b) %>%  pivot_wider(names_from = dis, values_from = lead1b,  names_prefix = "Disengagements_")


## When should pitcher attempt a pick?
pickoff_var_1b <- pickoff_var_1b %>% filter(pre_disengagements < 3)
pickoff_var_1b$year <- as.factor(pickoff_var_1b$year)
pickoff_var_1b$pre_disengagements <- as.factor(pickoff_var_1b$pre_disengagements)

pickoff_var_1b$pickoff_prob <- predict(m2, newdata = pickoff_var_1b, type = "response", re.form = NA)
pickoff_var_1b$pick_succ <- predict(m1, newdata = pickoff_var_1b, type = "response", re.form = NA)

pickoff_var_1b$pre_disengagements <- as.numeric(pickoff_var_1b$pre_disengagements) - 1
pickoff_var_1b$sb_prob <- predict(m4, newdata = pickoff_var_1b, type = "response", re.form = NA)
pickoff_var_1b$sb_succ <- predict(m3, newdata = pickoff_var_1b, type = "response", re.form = NA)

states_added <- pickoff_var_1b %>% mutate(R1 = ifelse(!is.na(run1b), 1, 0), R2 = ifelse(!is.na(run2b), 1, 0), R3 = ifelse(!is.na(run3b), 1, 0), Runners = paste0(R1, R2, R3)) %>% select(-R1, -R2, -R3) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))

pitcher_decision_real <- states_added %>% left_join(value_of_outcomes, by = "State") %>% mutate(Val_Pick_Attempt = pick_succ * SP + (1 - pick_succ) * UP, Val_NoPick = sb_prob * sb_succ * SS + sb_prob * (1 - sb_succ) * US + (1 - sb_prob) * N) %>% mutate(PickRecommend = ifelse(Val_Pick_Attempt < Val_NoPick, 1, 0))

mean(pitcher_decision_real$PickRecommend, na.rm = TRUE) # Pitcher should attempt a pickoff 20.2% of the time!
mean(pitcher_decision_real$isPickAttempt) # Actually picks 5.5% of the time

should_pick <- pitcher_decision_real %>% filter(PickRecommend == 1)
mean(should_pick$isPickAttempt) # When pick recommended, they actually pick 8.6% of the time

no_pick <- pitcher_decision_real %>% filter(PickRecommend == 0)
mean(no_pick$isPickAttempt) # When pick not recommended, they actually pick 4.6% of the time

old_re_table_two



print(
  xtable::xtable(table1_twoagent, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/runners_outs_two_agent.tex"
)

print(
  xtable::xtable(table2_twoagent, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/count_two_agent.tex"
)

# MONOTONICITY CHECKS ----

RE_transitions <- T_matrix %>% select(State, New_State, Freq) %>% left_join(old_re_table, by = "State") %>% rename(Old_RE = RE) %>% left_join(old_re_table, by = c("New_State" = "State")) %>% rename(New_RE = RE)  %>% left_join(runs_on_transition, by = c("State", "New_State")) %>% mutate(RE_change = New_RE - Old_RE + RunsScored)

# Situations where a ball is thrown and all else is the same
all_same_but_balls <- RE_transitions %>% filter(substr(State, 1, 6) == substr(New_State, 1, 6), substr(State, 8, 9) == substr(New_State, 8, 9)) %>% mutate(Pre_Balls = as.numeric(substr(State, 7, 7)), Post_Balls = as.numeric(substr(New_State, 7, 7))) %>% filter(Post_Balls - Pre_Balls == 1)
#ggplot(all_same_but_balls) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Ball Thrown")

balls1000 <- all_same_but_balls %>% filter(Freq > 1000)
#ggplot(balls1000) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Ball Thrown - transitions with over 1000 iterations")

# Situations where a strike is thrown and all else is the same
all_same_but_strikes <- RE_transitions %>% filter(substr(State, 1, 7) == substr(New_State, 1, 7), substr(State, 9, 9) == substr(New_State, 9, 9)) %>% mutate(Pre_Strikes = as.numeric(substr(State, 8, 8)), Post_Strikes = as.numeric(substr(New_State, 8, 8))) %>% filter(Post_Strikes - Pre_Strikes == 1)
#ggplot(all_same_but_strikes) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Strike Thrown")

strikes1000 <- all_same_but_strikes %>% filter(Freq > 1000)
#ggplot(strikes1000) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Strike Thrown - transitions with over 1000 iterations")


# Situations where a disengagement occurs and all else is the same
all_same_but_dis <- RE_transitions %>% filter(substr(State, 1, 4) == substr(New_State, 1, 4), substr(State, 6, 9) == substr(New_State, 6, 9)) %>% mutate(Pre_Dis = as.numeric(substr(State, 5, 5)), Post_Dis = as.numeric(substr(New_State, 5, 5))) %>% filter(Post_Dis - Pre_Dis == 1)
#ggplot(all_same_but_dis) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Disengagement")

dis100 <- all_same_but_dis %>% filter(Freq > 100)
#ggplot(dis100) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Disengagement - transitions with over 100 iterations")


# Situations where an out is made and all else is the same
all_same_but_outs <- RE_transitions %>% filter(substr(State, 1, 8) == substr(New_State, 1, 8), substr(State, 8, 8) == substr(New_State, 8, 8)) %>% mutate(Pre_Outs = as.numeric(substr(State, 9, 9)), Post_Outs = as.numeric(substr(New_State, 9, 9))) %>% filter(Post_Outs - Pre_Outs == 1)
#ggplot(all_same_but_outs) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Out")



write_csv(old_run_1b, "res.csv")


write_csv(sorted, "res.csv")

