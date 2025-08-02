# DATA SET UP ----

library(tidyverse)
library(lme4)

event_2022 <- read_csv("input/data/event/2022.csv")
event_2023 <- read_csv("input/data/event/2023.csv")

lead_2022 <- read_csv("input/data/lead_distance/2022.csv")
lead_2023 <- read_csv("input/data/lead_distance/2023.csv")

pitch_2022 <- read_csv("input/data/pitch/2022.csv") |>
  select(play_id, description)
pitch_2023 <- read_csv("input/data/pitch/2023.csv") |>
  select(play_id, description)

play_2022 <- read_csv("input/data/play/2022.csv")
play_2023 <- read_csv("input/data/play/2023.csv")

event_map <- read_csv("input/data/batter_event.csv")
pitch_map <- read_csv("input/data/batter_pitch.csv")

arm_strength_2023 <- read_csv("input/data/arm_strength/2023.csv") |>
  select(player_id, player_name, arm_strength, sb_attempts)
arm_strength_2022 <- read_csv("input/data/arm_strength/2022.csv") |>
  select(player_id, player_name, arm_strength, sb_attempts)

sprint_speed_2023 <- read_csv("input/data/sprint_speed/2023.csv") |>
  select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)
sprint_speed_2022 <- read_csv("input/data/sprint_speed/2022.csv") |>
  select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)


# Remove Duplicate Lead Distances
no_duplicate_leads <- lead_2023[duplicated(lead_2023) == FALSE,]

# Join lead distances with rest of data
baserunners <- no_duplicate_leads %>% pivot_wider(names_from = base, values_from = c(lead_distance, runner_id))
with_leads <- play_2023 %>% left_join(baserunners, by = "play_id")
with_leads$run1b <- as.factor(with_leads$pre_runner_1b_id)
with_leads$run2b <- as.factor(with_leads$pre_runner_2b_id)
with_leads$run3b <- as.factor(with_leads$pre_runner_3b_id)
with_leads$lead1b <- with_leads$`lead_distance_1st Base`
with_leads$lead2b <- with_leads$`lead_distance_2nd Base`
with_leads$lead3b <-with_leads$`lead_distance_3rd Base`

pitcher_batter_catcher <- event_2023 %>% select(game_id, event_index, batter_id, bat_side, pitcher_id, pitch_hand, fielder_2_id, inning, half_inning, post_outs, event)

# Join players involved in with data
with_pitcher_batter_catcher <- with_leads %>% left_join(pitcher_batter_catcher, by = c("game_id", "event_index")) %>% left_join(arm_strength_2023, by = c("fielder_2_id" = "player_id")) %>% left_join(sprint_speed_2023, by = c("runner_id_1st Base" = "player_id"))

# Replace NA values with mean for sprint speed and arm strength
mean_ss <- weighted.mean(sprint_speed_2023$sprint_speed, w = sprint_speed_2023$competitive_runs)
mean_as <- weighted.mean(arm_strength_2023$arm_strength, w = arm_strength_2023$sb_attempts)
with_pitcher_batter_catcher$sprint_speed <- coalesce(with_pitcher_batter_catcher$sprint_speed, mean_ss)
with_pitcher_batter_catcher$arm_strength <- coalesce(with_pitcher_batter_catcher$arm_strength, mean_as)

# Map events to more general descriptions
mapped_events <- with_pitcher_batter_catcher %>% left_join(event_map, by = "event") %>% left_join(pitch_2023, by = "play_id") %>% left_join(pitch_map, by = "description") %>% mutate(pitch_event = ifelse(batter_description == "In Play", batter_event, batter_description)) %>% mutate(pitch_event = ifelse(is.na(pitch_event), "Not Batter Event", pitch_event))

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
no_picks_or_steals <- states_final %>% filter(((isPickAttempt == 0 | is.na(isPickAttempt)) & (isSBAttempt == 0 | is.na(isSBAttempt))) | (substr(State, 1, 3) == "999"))

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

# Remove Duplicate Lead Distances
no_duplicate_leads <- lead_2022[duplicated(lead_2022) == FALSE,]

baserunners_22 <- no_duplicate_leads %>% pivot_wider(names_from = base, values_from = c(lead_distance, runner_id)) 

with_leads22 <- play_2022 %>% left_join(baserunners_22, by = "play_id")
with_leads22$run1b <- as.factor(with_leads22$pre_runner_1b_id)
with_leads22$run2b <- as.factor(with_leads22$pre_runner_2b_id)
with_leads22$run3b <- as.factor(with_leads22$pre_runner_3b_id)
with_leads22$lead1b <- with_leads22$`lead_distance_1st Base`
with_leads22$lead2b <- with_leads22$`lead_distance_2nd Base`
with_leads22$lead3b <-with_leads22$`lead_distance_3rd Base`




pitcher_batter_catcher22 <- event_2022 %>% select(game_id, event_index, batter_id, bat_side, pitcher_id, pitch_hand, fielder_2_id, inning, half_inning, post_outs, event)

# Join players involved in with data
with_pitcher_batter_catcher22 <- with_leads22 %>% left_join(pitcher_batter_catcher22, by = c("game_id", "event_index")) %>% left_join(arm_strength_2022, by = c("fielder_2_id" = "player_id")) %>% left_join(sprint_speed_2022, by = c("runner_id_1st Base" = "player_id"))


# Replace NA values with mean for sprint speed and arm strength
mean_ss22 <- weighted.mean(sprint_speed_2022$sprint_speed, w = sprint_speed_2022$competitive_runs)
mean_as22 <- weighted.mean(arm_strength_2022$arm_strength, w = arm_strength_2022$sb_attempts, na.rm = TRUE)
with_pitcher_batter_catcher22$sprint_speed <- coalesce(with_pitcher_batter_catcher22$sprint_speed, mean_ss22)
with_pitcher_batter_catcher22$arm_strength <- coalesce(with_pitcher_batter_catcher22$arm_strength, mean_as22)

# Map events to more general descriptions
mapped_events22 <- with_pitcher_batter_catcher22 %>% left_join(event_map, by = "event") %>% left_join(pitch_2022, by = "play_id") %>% left_join(pitch_map, by = "description") %>% mutate(pitch_event = ifelse(batter_description == "In Play", batter_event, batter_description)) %>% mutate(pitch_event = ifelse(is.na(pitch_event), "Not Batter Event", pitch_event))

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
#fit_po_success <- glmer(isSuccess ~ lead1b + (1|pitcher_id) , data = pickoff_att_1b_threats, family = binomial)
#summary(fit_po_success)
# Pitcher matters a bit - runner/catcher/batter lead to singular effect

# Probability of Pickoff Attempt
#fit_po_attempt <- glmer(isPickAttempt ~ lead1b + pre_balls + pre_strikes + pre_outs  + (1|pitcher_id) + as.factor(year) + as.factor(year) * pre_disengagements, data = all_pickoff_var1b, family = binomial)
#summary(fit_po_attempt)
# Pitcher and runner matters 

# Probability of Successful SB
#fit_sb_success <- glmer(is_stolen_base ~ lead1b + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength + as.factor(year), data = all_sb_att1b, family = binomial)
#summary(fit_sb_success)

# Probability of SB Attempt
# Not dependent on lead distance
#m4 <- glmer(isSBAttempt ~ pre_balls + pre_strikes + pre_outs + pre_disengagements + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength, data = sb_var_1b_threats, family = binomial)
#summary(m4)

#saveRDS(fit_po_success, "models/fit_po_success.rds")
#saveRDS(fit_po_attempt, "models/fit_po_attempt.rds")
#saveRDS(fit_sb_success, "models/fit_sb_success.rds")
#saveRDS(m4, "m4model")

fit_po_success <- readRDS("models/fit_po_success.rds")
fit_po_attempt <- readRDS("models/fit_po_attempt.rds")
fit_sb_success <- readRDS("models/fit_sb_success.rds")
m4 <- readRDS("models/m4.rds")

# 
# # Pitcher Pickoff Ratio vs Effect
# pitcher_effects <- ranef(fit_po_success)$pitcher
# pitcher_effects$pitcher_id <- rownames(pitcher_effects)
# po_success <- pickoff_attempts %>% group_by(pitcher_id) %>% summarize(PO_Ratio = sum(isSuccess) / n(), count = n())
# joined_pitchers <- pitcher_effects %>% left_join(po_success, by = "pitcher_id")
# median(joined_pitchers$count, na.rm = TRUE)
# ggplot(joined_pitchers) + aes(PO_Ratio, `(Intercept)`, color = count > 20) + geom_point()
# 
# # Pitcher Pickoff Attempts vs Effect
# pitcher_effects <- ranef(fit_po_attempt)$pitcher
# pitcher_effects$pitcher_id <- rownames(pitcher_effects)
# po_att <- pickoff_var %>% group_by(pitcher_id) %>% summarize(PO_Freq = sum(isPickAttempt) / n(), count = n())
# joined_pitchers <- pitcher_effects %>% left_join(po_att, by = "pitcher_id")
# median(joined_pitchers$count, na.rm = TRUE)
# ggplot(joined_pitchers) + aes(PO_Freq, `(Intercept)`, color = count > 1553) + geom_point()
# 
# # Runner SB Success vs Effect
# runner_effects <- ranef(fit_sb_success)$run1b
# runner_effects$run1b <- rownames(runner_effects)
# sb_success <- sb_attempts %>% group_by(run1b) %>% summarize(SB_Pct = mean(is_stolen_base), count = n())
# joined_runners <- runner_effects %>% left_join(sb_success, by = "run1b")
# median(joined_runners$count, na.rm = TRUE)
# ggplot(joined_runners) + aes(SB_Pct, `(Intercept)`, color = count > 8) + geom_point()
# 
# # Catcher CS Success vs Effect
# catcher_effects <- ranef(fit_sb_success)$fielder_2_id
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





# EXTRACT PLAYER EFFECTS FROM MODELS ----

# M1 pitcher effects
pitcher_effects <- ranef(fit_po_success)$pitcher_id %>%
  arrange(`(Intercept)`)
median_pitcher_m1 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m1 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pct10_pitcher_m1 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]

# M2 pitcher effects
pitcher_effects <- ranef(fit_po_attempt)$pitcher_id %>%
  arrange(`(Intercept)`)
pct10_pitcher_m2 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
median_pitcher_m2 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m2 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]

# M3 pitcher effects
pitcher_effects <- ranef(fit_sb_success)$pitcher_id %>%
  tibble::rownames_to_column(var = "player_id") %>%
  arrange(`(Intercept)`) 

# M3 runner effects
runner_effects <- ranef(fit_sb_success)$run1b %>%
  arrange(`(Intercept)`)
sprint_speed_coef <- fixef(fit_sb_success)[3]
runner_combined <- runner_effects %>%
  mutate(player_id = as.numeric(rownames(runner_effects))) %>%
  left_join(sprint_speed_2023, by = "player_id") %>%
  mutate(combined_effect = sprint_speed_coef * sprint_speed + `(Intercept)`) %>%
  arrange(combined_effect) %>%
  filter(!is.na(sprint_speed))
pct10_runner_m3 <- runner_combined[0.1 * nrow(runner_combined),"player_id"]
median_runner_m3 <- runner_combined[0.5 * nrow(runner_combined),"player_id"]
pct90_runner_m3 <- runner_combined[0.9 * nrow(runner_combined),"player_id"]
run1b_m3 <- c(pct10_runner_m3, median_runner_m3, pct90_runner_m3)
pct10_ss_m3 <- runner_combined[0.1 * nrow(runner_combined),"sprint_speed"]
median_ss_m3 <- runner_combined[0.5 * nrow(runner_combined),"sprint_speed"]
pct90_ss_m3 <- runner_combined[0.9 * nrow(runner_combined),"sprint_speed"]

# M3 catcher effects
catcher_effects <- ranef(fit_sb_success)$fielder_2_id %>%
  arrange(`(Intercept)`)
arm_strength_coef <- fixef(fit_sb_success)[4]
catcher_combined <- catcher_effects %>%
  mutate(player_id = as.numeric(rownames(catcher_effects))) %>%
  left_join(arm_strength_2023, by = "player_id") %>%
  mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept)`) %>%
  arrange(combined_effect) %>%
  filter(!is.na(arm_strength))
pct10_catcher_m3 <-  catcher_combined[0.9 * nrow(catcher_combined),"player_id"]
median_catcher_m3 <- catcher_combined[0.5 * nrow(catcher_combined),"player_id"]
pct90_catcher_m3 <-  catcher_combined[0.1 * nrow(catcher_combined),"player_id"]
fielder_2_id_m3 <- c(pct10_catcher_m3, median_catcher_m3, pct90_catcher_m3)
pct10_as_m3 <- catcher_combined[0.9 * nrow(catcher_combined),"arm_strength"]
median_as_m3 <- catcher_combined[0.5 * nrow(catcher_combined),"arm_strength"]
pct90_as_m3 <- catcher_combined[0.1 * nrow(catcher_combined),"arm_strength"]

# M3 combined battery effects
battery_combined <- catcher_effects %>%
  mutate(player_id = as.numeric(rownames(catcher_effects))) %>%
  left_join(arm_strength_2023, by = "player_id") %>%
  cross_join(pitcher_effects) %>%
  mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept).x` + `(Intercept).y`) %>%
  arrange(combined_effect) %>%
  filter(!is.na(arm_strength))
pct10_battery_m3_pitcher <- battery_combined[0.9 * nrow(battery_combined),"player_id.y"]
median_battery_m3_pitcher <- battery_combined[0.5 * nrow(battery_combined),"player_id.y"]
pct90_battery_m3_pitcher <- battery_combined[0.1 * nrow(battery_combined),"player_id.y"]

## M4 PITCHER EFFECTS
pitcher_effects <- ranef(m4)$pitcher_id
pitcher_effects <- pitcher_effects %>% mutate(player_id = as.numeric(rownames(pitcher_effects))) %>% arrange(`(Intercept)`) 
median_pitcher_m4 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m4 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
pct10_pitcher_m4 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pitcher_id <- c(pct10_pitcher_m4, median_pitcher_m4, pct90_pitcher_m4)

## M4 RUNNER EFFECTS
runner_effects <- ranef(m4)$run1b
runner_effects <- runner_effects %>% arrange(`(Intercept)`)
sprint_speed_coef <- fixef(m4)[6]
runner_combined <- runner_effects %>% mutate(player_id = as.numeric(rownames(runner_effects))) %>% left_join(sprint_speed_2023, by = "player_id") %>% mutate(combined_effect = sprint_speed_coef * sprint_speed + `(Intercept)`) %>% arrange(combined_effect) %>% filter(!is.na(sprint_speed))
median_runner_m4 <- runner_combined[0.5 * nrow(runner_combined),"player_id"]
median_ss_m4 <- runner_combined[0.5 * nrow(runner_combined),"sprint_speed"]
pct90_runner_m4 <- runner_combined[0.9 * nrow(runner_combined),"player_id"]
pct90_ss_m4 <- runner_combined[0.9 * nrow(runner_combined),"sprint_speed"]
pct10_runner_m4 <- runner_combined[0.1 * nrow(runner_combined),"player_id"]
pct10_ss_m4 <- runner_combined[0.1 * nrow(runner_combined),"sprint_speed"]
run1b_m4 <- c(pct10_runner_m4, median_runner_m4, pct90_runner_m4)

## M4 CATCHER EFFECT
catcher_effects <- ranef(m4)$fielder_2_id
catcher_effects <- catcher_effects %>% arrange(`(Intercept)`)
arm_strength_coef <- fixef(m4)[7]
catcher_combined <- catcher_effects %>% mutate(player_id = as.numeric(rownames(catcher_effects))) %>% left_join(arm_strength_2023, by = "player_id") %>% mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept)`) %>% arrange(combined_effect) %>% filter(!is.na(arm_strength))
median_catcher_m4 <- catcher_combined[0.5 * nrow(catcher_combined),"player_id"]
median_as_m4 <- catcher_combined[0.5 * nrow(catcher_combined),"arm_strength"]
pct90_catcher_m4 <-  catcher_combined[0.1 * nrow(catcher_combined),"player_id"]
pct90_as_m4 <- catcher_combined[0.1 * nrow(catcher_combined),"arm_strength"]
pct10_catcher_m4 <-  catcher_combined[0.9 * nrow(catcher_combined),"player_id"]
pct10_as_m4 <- catcher_combined[0.9 * nrow(catcher_combined),"arm_strength"]
fielder_2_id_m4 <- c(pct10_catcher_m4, median_catcher_m4, pct90_catcher_m4)

## M4 BATTERY COMBINED
battery_combined <- catcher_effects %>% mutate(player_id = as.numeric(rownames(catcher_effects))) %>% left_join(arm_strength_2023, by = "player_id") %>% cross_join(pitcher_effects) %>% mutate(combined_effect = arm_strength_coef * arm_strength + `(Intercept).x` + `(Intercept).y`) %>% arrange(combined_effect)  %>% filter(!is.na(arm_strength))
median_battery_m4_catcher <- battery_combined[0.5 * nrow(battery_combined),"player_id.x"]
median_battery_m4_pitcher <- battery_combined[0.5 * nrow(battery_combined),"player_id.y"]
median_as_bat_m4 <- battery_combined[0.5 * nrow(battery_combined),"arm_strength"]
pct90_battery_m4_catcher <-  battery_combined[0.1 * nrow(battery_combined),"player_id.x"]
pct90_battery_m4_pitcher <-  battery_combined[0.1 * nrow(battery_combined),"player_id.y"]
pct90_as_bat_m4 <- battery_combined[0.1 * nrow(battery_combined),"arm_strength"]
pct10_battery_m4_catcher <-  battery_combined[0.9 * nrow(battery_combined),"player_id.x"]
pct10_battery_m4_pitcher <-  battery_combined[0.9 * nrow(battery_combined),"player_id.y"]
pct10_as_bat_m4 <- battery_combined[0.9 * nrow(battery_combined),"arm_strength"]



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

state_leads_exp$pickoff_prob <- predict(fit_po_attempt, newdata = state_leads_exp, type = "response", re.form = NA)
state_leads_exp$pick_succ <- predict(fit_po_success, newdata = state_leads_exp, type = "response", re.form = NA)

state_leads_exp$pre_disengagements <- as.numeric(state_leads_exp$pre_disengagements) - 2
state_leads_exp$sb_prob <- predict(m4, newdata = state_leads_exp, type = "response", re.form = NA)
state_leads_exp$sb_succ <- predict(fit_sb_success, newdata = state_leads_exp, type = "response", re.form = NA)

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
prob_trans_adj <- prob_transition %>% filter(Prob > 0) %>% left_join(prob_runner_outcome, by = c("State", "runner_outcome"), relationship = "many-to-many") %>% mutate(Prob_product = Prob * Prob_RO) %>% mutate(TotalProb = ifelse(substr(State, 1, 1) == "3", ifelse(New_State == "3 0", 1, 0), Prob_product))



# VALUE ITERATION STEPS ----

# Initial run expectancy table from empirical outcomes
run_counts <- states_final %>% group_by(half_inn_id) %>% mutate(runs_in_inning = sum(runs_on_play)) %>% mutate(runs_so_far = cumsum(runs_on_play) - runs_on_play) %>% mutate(runs_roi = runs_in_inning - runs_so_far)

# Run Expactancy Table - average runs scored over the rest of the inning from each state
re_table <- run_counts %>% group_by(State) %>% summarize(RE = mean(runs_roi), n = n())
all_states_df <- data.frame(State = all_possible_states)
full_re_table <- all_states_df %>% left_join(re_table, by = "State") %>% mutate(RE = ifelse(is.na(RE), 0, RE), n = ifelse(is.na(n), 0, n))


runner_outcomes <- c("N", "SP", "UP", "SS", "US")
# Get Runs Scored in Each State
transitions_expanded <- expand.grid(State = all_possible_states, New_State = all_possible_states, runner_outcome = runner_outcomes) %>% mutate(OldRunners = as.numeric(substr(State, 1, 1)) + as.numeric(substr(State, 2, 2)) + as.numeric(substr(State, 3, 3))) %>% mutate(NewRunners = as.numeric(substr(New_State, 1, 1)) + as.numeric(substr(New_State, 2, 2)) + as.numeric(substr(New_State, 3, 3))) %>% mutate(OldOuts = as.numeric(substr(State, 9, 9))) %>% mutate(NewOuts = as.numeric(substr(New_State, 9, 9))) %>% mutate(OldDis = as.numeric(substr(State, 5, 5))) %>% mutate(NewDis = as.numeric(substr(New_State, 5, 5))) %>% mutate(OldCount = substr(State, 7,8)) %>% mutate(NewCount = substr(New_State, 7,8)) %>% mutate(NewBatter = ifelse(NewCount == "00" & runner_outcome %in% c("N", "SS", "US"), 1, 0)) 

# Runs Scored on Each Transition
all_transitions <- transitions_expanded %>% mutate(RunsScored = OldRunners + OldOuts + NewBatter - NewRunners - NewOuts)  %>% mutate(RunsScored = ifelse(OldOuts > NewOuts, 0, RunsScored), RunsScored = ifelse(NewRunners - OldRunners > 1, 0, RunsScored)) %>% mutate(RunsScored = ifelse(RunsScored < 0, 0, RunsScored)) %>% mutate(RunsScored = ifelse(is.na(NewOuts), as.numeric(substr(New_State, 3, 3)), RunsScored), RunsScored = ifelse(is.na(OldOuts), 0, RunsScored))
runs_on_transition <- all_transitions %>% select(State, New_State, runner_outcome, RunsScored) 

# Step 2

# Get Run Values of each new state you can transition to
transition_values <- prob_trans_adj %>% left_join(runs_on_transition, by = c("State", "New_State", "runner_outcome")) %>% left_join(full_re_table, by = c("State" = "State")) %>% left_join(full_re_table, by = c("New_State" = "State")) %>% mutate(New_Value = RunsScored + RE.y) %>% rename(Old_RE = RE.x, New_RE = RE.y)

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
    summarize(RE = sum(TotalProb * (RunsScored + RE)), n = mean(n)) %>% 
    filter(row_number() == which.max(RE))
  
  new_run_1b <- re_vals %>% filter(substr(State, 1, 3) == "100")
  
  change <- new_run_1b |>                                                                              
    dplyr::left_join(old_run_1b, by = "State", suffix = c("_old", "_new")) |>                      
    with(sum(abs(lead1b_new - lead1b_old))) 
  
  
  
  old_re_table <- re_vals %>% select(-lead1b)     
  old_run_1b <- new_run_1b
  iterations <- iterations + 1
  print(change)
  print(old_re_table[1,2])
}     

value_of_outcomes <- prob_transition %>% left_join(runs_on_transition, by = c("State", "New_State", "runner_outcome")) %>% left_join(old_re_table, by = c("New_State" = "State")) %>% group_by(State, runner_outcome) %>% mutate(New_Value = RunsScored + RE)  %>% summarize(Outcome_Value = sum(Prob * New_Value, na.rm = TRUE)) %>% pivot_wider(names_from = runner_outcome, values_from = Outcome_Value)



# VALUE ITERATION WITH VARYING SKILL ----

skill_grid <- expand.grid(run1b_m3 = run1b_m3, fielder_2_id_m3  = fielder_2_id_m3)
skill_grid$pitcher_id_m3 <- c(rep(pct10_battery_m3_pitcher,3), rep(median_battery_m3_pitcher,3), rep(pct90_battery_m3_pitcher,3))
skill_grid$sprint_speed_m3  <- rep(c(pct10_ss_m3, median_ss_m3, pct90_ss_m3), 3)
skill_grid$arm_strength_m3  <- c(rep(pct10_as_m3, 3), rep(median_as_m3, 3), rep(pct90_as_m3, 3))
skill_grid$runner_skill <- rep(c("10th Percentile Runner", "Median Runner", "90th Percentile Runner"), 3)
skill_grid$battery_skill <- c(rep("10th Percentile Battery", 3), rep("Median Battery", 3),  rep("90th Percentile Battery", 3))

skill_grid$run1b_m4  <- rep(c(pct10_runner_m4, median_runner_m4, pct90_runner_m4), 3)
skill_grid$fielder_2_id_m4  <- c(rep(pct10_catcher_m4, 3), rep(median_catcher_m4, 3), rep(pct90_battery_m4_catcher, 3))
skill_grid$pitcher_id_m4 <- c(rep(pct10_battery_m4_pitcher,3), rep(median_battery_m4_pitcher,3), rep(pct90_battery_m4_pitcher,3))
skill_grid$sprint_speed_m4  <- rep(c(pct10_ss_m4, median_ss_m4, pct90_ss_m4), 3)
skill_grid$arm_strength_m4  <- c(rep(pct10_as_m4, 3), rep(median_as_m4, 3), rep(pct90_as_m4, 3))

skill_grid$pitcher_id_m2 <- c(rep(pct10_pitcher_m2, 3), rep(median_pitcher_m2, 3),  rep(pct90_pitcher_m2, 3))
#skill_grid$run1b_m2 <- rep(c(median_runner_m2, median_runner_m2, median_runner_m2), 3)

skill_grid$pitcher_id_m1 <- c(rep(pct10_pitcher_m1,3), rep(median_pitcher_m1,3), rep(pct90_pitcher_m1, 3))

# 
# for (i in 1:nrow(skill_grid)) {
#   state_leads_it <- state_leads_exp
#   state_leads_it$fielder_2_id <- skill_grid[i,2]
#   state_leads_it$run1b <- skill_grid[i,1]
#   state_leads_it$pitcher_id <-  skill_grid[i,3]
#   state_leads_it$arm_strength <- skill_grid[i,5]
#   state_leads_it$sprint_speed <- skill_grid[i,4]
#   state_leads_it$sb_succ <- predict(fit_sb_success, newdata = state_leads_it, type = "response")
#   
#   
#   state_leads_it$fielder_2_id <- skill_grid[i,9]
#   state_leads_it$run1b <- skill_grid[i,8]
#   state_leads_it$pitcher_id <-  skill_grid[i,10]
#   state_leads_it$arm_strength <- skill_grid[i,12]
#   state_leads_it$sprint_speed <- skill_grid[i,11]
#   state_leads_it$sb_prob <- predict(m4, newdata = state_leads_it, type = "response")
#   
#   state_leads_it$run1b <- skill_grid[i,14]
#   state_leads_it$pitcher_id <-  skill_grid[i,13]
#   state_leads_it$year <- as.factor(state_leads_it$year)
#   state_leads_it$pre_disengagements <- state_leads_it$pre_disengagements
#   state_leads_it$pre_disengagements <- as.factor(state_leads_it$pre_disengagements)  
#   state_leads_it$pickoff_prob <- predict(fit_po_attempt, newdata = state_leads_it, type = "response")
#   
#   state_leads_it$pitcher_id <-  skill_grid[i,15]
#   state_leads_it$pick_succ <- predict(fit_po_success, newdata = state_leads_it, type = "response")
#   
#   state_leads_it$pickoff_prob <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$pickoff_prob)
#   state_leads_it$pick_succ <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$pick_succ)
#   state_leads_it$sb_prob <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$sb_prob)
#   state_leads_it$sb_succ <- ifelse(state_leads_it$sb2B == 0, 0, state_leads_it$sb_succ)
#   
#   base10 <- state_leads_it %>% filter(State == "100 0 000", lead1b == 10, runner_outcome == "N")
#   print(skill_grid[i,6])
#   print(skill_grid[i,7])
#   print(base10)
#   
#   
# }


for (i in 1:nrow(skill_grid)) {
  state_leads_it <- state_leads_exp
  state_leads_it$fielder_2_id <- skill_grid[i,2]
  state_leads_it$run1b <- skill_grid[i,1]
  state_leads_it$pitcher_id <-  skill_grid[i,3]
  state_leads_it$arm_strength <- skill_grid[i,5]
  state_leads_it$sprint_speed <- skill_grid[i,4]
  state_leads_it$sb_succ <- predict(fit_sb_success, newdata = state_leads_it, type = "response")
  
  
  state_leads_it$fielder_2_id <- skill_grid[i,9]
  state_leads_it$run1b <- skill_grid[i,8]
  state_leads_it$pitcher_id <-  skill_grid[i,10]
  state_leads_it$arm_strength <- skill_grid[i,12]
  state_leads_it$sprint_speed <- skill_grid[i,11]
  state_leads_it$sb_prob <- predict(m4, newdata = state_leads_it, type = "response")
  
  #state_leads_it$run1b <- skill_grid[i,14]
  state_leads_it$pitcher_id <-  skill_grid[i,13]
  state_leads_it$year <- as.factor(state_leads_it$year)
  state_leads_it$pre_disengagements <- state_leads_it$pre_disengagements
  state_leads_it$pre_disengagements <- as.factor(state_leads_it$pre_disengagements)  
  state_leads_it$pickoff_prob <- predict(fit_po_attempt, newdata = state_leads_it, type = "response")
  
  state_leads_it$pitcher_id <-  skill_grid[i,14]
  state_leads_it$pick_succ <- predict(fit_po_success, newdata = state_leads_it, type = "response")
  
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
  prob_trans_adj_it <- prob_transition %>% filter(Prob > 0) %>% left_join(prob_runner_outcome_it, by = c("State", "runner_outcome"), relationship = "many-to-many") %>% mutate(Prob_product = Prob * Prob_RO) %>% mutate(TotalProb = ifelse(substr(State, 1, 1) == "3", ifelse(New_State == "3 0", 1, 0), Prob_product))
  
  
  
  # Get Run Values of each new state you can transition to
  transition_values_it <- prob_trans_adj_it %>% left_join(runs_on_transition, by = c("State", "New_State", "runner_outcome")) %>% left_join(full_re_table, by = c("State" = "State")) %>% left_join(full_re_table, by = c("New_State" = "State")) %>% mutate(New_Value = RunsScored + RE.y) %>% rename(Old_RE = RE.x, New_RE = RE.y)
  
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
  skill_grid[i, 15] <- old_run_1b_it[1,]$lead1b
  skill_grid[i, 16] <- old_run_1b_it[37,]$lead1b
  skill_grid[i, 17] <- old_run_1b_it[73,]$lead1b
}


# SUMMARY TABLES ----


sorted <- old_run_1b %>% mutate(bases = substr(State, 1, 3), dis = substr(State, 5,5), countouts = substr(State, 7, 9)) %>% arrange(bases, countouts, dis)


table1 <- sorted %>% ungroup() %>%  filter(substr(countouts, 1,2) == "00") %>% mutate(Runners = ifelse(bases == "100", "Man on 1st", "Men on 1st and 3rd"), Outs = substr(countouts,3,3)) %>% select(Outs, dis, lead1b) %>%  pivot_wider(names_from = dis, values_from = lead1b, names_prefix = "Disengagements_")

print(
  xtable::xtable(table1, digits = 1),
  hline.after = NULL,
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  file = "figures/lead_by_runners_outs.tex"
)

## How well do runners select lead distance?

pickoff_var_1b <- pickoff_var_1b %>% filter(pre_disengagements < 3)

pv1b_states_added <- pickoff_var_1b %>% mutate(R1 = ifelse(!is.na(run1b), 1, 0), R2 = ifelse(!is.na(run2b), 1, 0), R3 = ifelse(!is.na(run3b), 1, 0), Runners = paste0(R1, R2, R3)) %>% select(-R1, -R2, -R3) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))

actual_vs_recommended_leads <- pv1b_states_added %>% left_join(old_run_1b, by = "State") %>% rename(ActualLead = lead1b.x, RecLead = lead1b.y) %>% mutate(LeadDiff = ActualLead - RecLead)
mean(actual_vs_recommended_leads$LeadDiff > 0, na.rm = TRUE) # Actual lead only exceeds recommended lead 20.0% of the team

dis <- actual_vs_recommended_leads %>% mutate(nextLead = lead(ActualLead), nextRec = lead(RecLead)) %>% filter(lead(pre_disengagements) == pre_disengagements + 1, lead(event_index) == event_index) %>% mutate(RecIncrease = nextRec - RecLead, ActualIncrease = nextLead - ActualLead)

#ggplot(dis) + aes(x = ActualIncrease, color = "red") + geom_density() + geom_density(aes(x = RecIncrease, color = "dodgerblue")) + theme_classic() + xlim(-2,5)

# TWO AGENT MODEL ----

change <- Inf                                                                                     
threshold <- 0.01                                                                               
old_re_table_two <- full_re_table
old_run_1b_two <- old_run_1b
iterations <- 0
while(change > threshold || iterations < 2) {  
  
  value_of_outcomes_two <- prob_transition %>% left_join(runs_on_transition, by = c("State", "New_State", "runner_outcome")) %>% filter(Prob > 0) %>% left_join(old_re_table_two, by = c("New_State" = "State")) %>% mutate(New_Value = RunsScored + RE) %>% group_by(State, runner_outcome)  %>% summarize(Outcome_Value = sum(Prob * New_Value, na.rm = TRUE)) %>% pivot_wider(names_from = runner_outcome, values_from = Outcome_Value)
  
  pitcher_decision <- state_leads_exp %>% left_join(value_of_outcomes_two, by = "State") %>% mutate(Val_Pick_Attempt = pick_succ * SP + (1 - pick_succ) * UP, Val_NoPick = sb_prob * sb_succ * SS + sb_prob * (1 - sb_succ) * US + (1 - sb_prob) * N) %>% mutate(Val_Pick_Attempt = ifelse(sb2B == 0, 10, Val_Pick_Attempt), Val_NoPick = ifelse(sb2B == 0, N, Val_NoPick))
  
  best_lead <- pitcher_decision %>% mutate(Diff = abs(Val_Pick_Attempt - Val_NoPick)) %>% group_by(State) %>% mutate(minDiff = min(Diff)) %>% filter(Diff == minDiff) %>% dplyr::slice(1) %>% ungroup() %>%  mutate(RE = pmin(Val_Pick_Attempt, Val_NoPick))
  
  new_re_table_two <- best_lead %>% select(State, RE) 
  
  new_run_1b_two <- best_lead %>% filter(substr(State, 1, 3) == "100") %>% select(State, lead1b, RE)
  
  change <- new_run_1b_two |>                                                                              
    dplyr::left_join(old_run_1b_two, by = "State", suffix = c("_old", "_new")) |>                      
    with(sum(abs(lead1b_new - lead1b_old))) 
 
  
  old_re_table_two <- new_re_table_two
  old_run_1b_two <- new_run_1b_two
  iterations <- iterations + 1
  
  print(change)
  print(old_re_table_two[1,2])
}     




## When should pitcher attempt a pick?
pickoff_var_1b <- pickoff_var_1b %>% filter(pre_disengagements < 3)
pickoff_var_1b$year <- as.factor(pickoff_var_1b$year)
pickoff_var_1b$pre_disengagements <- as.factor(pickoff_var_1b$pre_disengagements)

pickoff_var_1b$pickoff_prob <- predict(fit_po_attempt, newdata = pickoff_var_1b, type = "response", re.form = NA)
pickoff_var_1b$pick_succ <- predict(fit_po_success, newdata = pickoff_var_1b, type = "response", re.form = NA)

pickoff_var_1b$pre_disengagements <- as.numeric(pickoff_var_1b$pre_disengagements) - 1
pickoff_var_1b$sb_prob <- predict(m4, newdata = pickoff_var_1b, type = "response", re.form = NA)
pickoff_var_1b$sb_succ <- predict(fit_sb_success, newdata = pickoff_var_1b, type = "response", re.form = NA)

states_added <- pickoff_var_1b %>% mutate(R1 = ifelse(!is.na(run1b), 1, 0), R2 = ifelse(!is.na(run2b), 1, 0), R3 = ifelse(!is.na(run3b), 1, 0), Runners = paste0(R1, R2, R3)) %>% select(-R1, -R2, -R3) %>% mutate(State = paste0(Runners, " ", pre_disengagements, " ", pre_balls, pre_strikes, pre_outs))

pitcher_decision_real <- states_added %>% left_join(value_of_outcomes, by = "State") %>% mutate(Val_Pick_Attempt = pick_succ * SP + (1 - pick_succ) * UP, Val_NoPick = sb_prob * sb_succ * SS + sb_prob * (1 - sb_succ) * US + (1 - sb_prob) * N) %>% mutate(PickRecommend = ifelse(Val_Pick_Attempt < Val_NoPick, 1, 0))

mean(pitcher_decision_real$PickRecommend, na.rm = TRUE) # Pitcher should attempt a pickoff 8.3% of the time!
mean(pitcher_decision_real$isPickAttempt) # Actually picks 5.6% of the time

should_pick <- pitcher_decision_real %>% filter(PickRecommend == 1)
mean(should_pick$isPickAttempt) # When pick recommended, they actually pick 10.8% of the time

no_pick <- pitcher_decision_real %>% filter(PickRecommend == 0)
mean(no_pick$isPickAttempt) # When pick not recommended, they actually pick 5.0% of the time

old_re_table_two




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

