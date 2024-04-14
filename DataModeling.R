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
rep_level <- counts %>% mutate(batter_id = ifelse(batterCount < 500, "b1", batter_id), pitcher_id = ifelse(pitcherCount < 500, "p1", pitcher_id), fielder_2_id = ifelse(fielderCount < 500, "c1", fielder_2_id), run1b = ifelse(runnerCount < 10, "r1", run1b)) 

# Make variables for whether certain events occur
sb_att_var <- rep_level %>% mutate(isSBAttempt = ifelse(is_stolen_base == TRUE | is_caught_stealing == TRUE, 1, 0))
sb_attempts <- sb_att_var %>% filter(isSBAttempt == 1)
pickoff_var <- sb_att_var %>% mutate(isPickAttempt = ifelse(type == "pickoff", 1, 0)) 
pickoff_attempts <- pickoff_var %>% filter(isPickAttempt == 1) %>% mutate(isSuccess = ifelse(is.na(is_pickoff), 0, ifelse(is_pickoff == TRUE, 1, 0)))

# Filter only situations where 1B is occupied and 2B is not
pickoff_var_1b <- pickoff_var %>% filter(!is.na(run1b) & is.na(run2b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
pickoff_att_1b <- pickoff_attempts %>% filter(!is.na(run1b) & is.na(run2b)) %>% filter(isSBAttempt == 0, is_defensive_indiff == FALSE, is.na(runner_going))
sb_var_1b <- sb_att_var %>% filter(!is.na(run1b) & is.na(run2b))
sb_att_1b <- sb_attempts %>% filter(!is.na(run1b) & is.na(run2b))

# Filter runners who are no threat to steal (just for modeling) # Also filter out 3-2 counts with 2 outs
sb_threats <- sb_att_1b %>% count(pre_runner_1b_id) %>% filter(n >= 3) %>% pull(pre_runner_1b_id)
pickoff_var_1b_threats <- pickoff_var_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)
sb_var_1b_threats <- sb_var_1b %>% filter(pre_runner_1b_id %in% sb_threats) %>% filter(pre_balls < 3 | pre_strikes < 2 | pre_outs < 2)

### Update Transition Probs

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
states_final <- sorted_states %>% group_by(half_inn_id) %>% mutate(Innings_Outs = sum(event_outs)) %>% ungroup() %>% filter(Innings_Outs == 3) %>% filter(pre_balls < 4) %>% mutate(State_No_Dis = paste0(Runners, " ", pre_balls, pre_strikes, pre_outs)) %>% mutate(New_State_No_Dis = paste(substr(New_State, 1, 3), substr(New_State, 7, 9))) %>% mutate(State_No_Runner = paste0(pre_balls, pre_strikes, pre_outs)) %>% mutate(New_State_No_Runner = substr(New_State, 7, 9))


# Make a grid of all possible states (868 * 868)
all_possible_states_tbl <- expand.grid(R1 = c(0,1), R2 = c(0,1), R3 = c(0,1), dis = seq(0,2), balls = seq(0,3), strikes = seq(0,2), outs = seq(0,2)) %>% mutate(State = paste0(R1, R2, R3, " ", dis, " ", balls, strikes, outs))
all_possible_states <- c(all_possible_states_tbl$State, "3 0", "3 1", "3 2", "3 3")


## BIG P MATRIX

# General P Matrix

#Compute Transition Counts
T_matrix <- states_final %>% group_by(State, New_State) %>% summarize(Freq = n())

# Grid of all possible states with and without disengagements
state_grid <- expand.grid(State = all_possible_states, New_State = all_possible_states)
state_grid <- state_grid %>% mutate(State_No_Dis = paste0(substr(State, 1, 3), substr(State, 6, 9)))  %>% mutate(New_State_No_Dis = paste0(substr(New_State, 1, 3), substr(New_State, 6, 9))) %>% mutate(State_No_Runner = substr(State, 7, 9)) %>% mutate(New_State_No_Runner = substr(New_State, 7, 9)) 

# Merge grid with transition probabilities
all_transitions <- state_grid %>% left_join(T_matrix, by = c("State", "New_State")) %>% mutate(Freq = ifelse(is.na(Freq), 0 , Freq))

dim(all_transitions)

#Compute probability matrix from transitions
P_matrix <- all_transitions %>% group_by(State) %>% mutate(Total = sum(Freq)) %>% ungroup() %>% mutate(Prob = Freq / Total) %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == State, 1, 0), Prob))


# P^N MATRIX (No pickoff or steal attempt)
no_picks_or_steals <- states_final %>% filter(isPickAttempt == 0, isSBAttempt == 0)

# Get probability of event given count and outs
event_probs <- no_picks_or_steals %>% count(State_No_Runner, pitch_event) %>% group_by(State_No_Runner) %>% mutate(Prob = n / sum(n))

# Get new state given state and event
state_probs <- no_picks_or_steals %>% count(State_No_Dis, pitch_event, New_State_No_Dis) %>% group_by(State_No_Dis, pitch_event) %>% mutate(Prob = n / sum(n)) %>% mutate(State_No_Runner = substr(State_No_Dis, 5, 7))

# Merge event and state probabilities
new_state_probs <- state_probs %>% left_join(event_probs, by = c("State_No_Runner", "pitch_event")) %>% mutate(Prob = Prob.x * Prob.y) %>% group_by(State_No_Dis, New_State_No_Dis) %>% summarize(Prob = sum(Prob))

# Add disengagements back in (Assumed Disengagements do not affect pitch results when no pickoff/steal attempts)
Old_Dis <- data.frame(Old_Dis = c(0,1,2))
# If runners move or new batter, new disengagements = 0
Dis_added <- new_state_probs %>% expand_grid(., Old_Dis) %>% mutate(New_Dis = ifelse(substr(New_State_No_Dis, 1, 3) != substr(State_No_Dis, 1, 3), 0, ifelse(substr(New_State_No_Dis, 5, 6) == "00", 0, Old_Dis))) %>% mutate(State = paste0(substr(State_No_Dis, 1, 4), Old_Dis, substr(State_No_Dis, 4, 7)))  %>% mutate(New_State = paste0(substr(New_State_No_Dis, 1, 4), New_Dis, substr(New_State_No_Dis, 4, 7))) %>% mutate(New_State = ifelse(substr(New_State, 1, 1) == "3", substr(New_State, 1, 3), New_State)) %>% select(State, New_State, Prob)

# Merge state grid with transition probs
all_transitions_no_ps <- state_grid %>% left_join(Dis_added, by = c("State", "New_State")) %>% mutate(Prob = ifelse(is.na(Prob), 0 , Prob))
P_N <- all_transitions_no_ps %>% mutate(Prob = ifelse(is.na(Prob), ifelse(New_State == "3 0", 1, 0), Prob)) %>% group_by(State, New_State, State_No_Dis, New_State_No_Dis) %>% summarize(Prob = sum(Prob))


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


# P^SS MATRIX (Stolen Base)

stolen_base <- states_final %>% filter(is_stolen_base == 1)

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



### LITTLE p MATRICES (probability of runner outcome given state and lead distance)

# All "possible" leads
leads <- seq(0,30, by = 0.1)

# Make grid of states and leads
state_leads_outcomes <- expand.grid(State = all_possible_states, lead1b = leads, runner_outcome = c("N", "SP", "UP", "SS", "US"))
state_leads_exp <- state_leads_outcomes %>% mutate(pre_balls = as.numeric(substr(State, 7, 7)), pre_strikes = as.numeric(substr(State, 8, 8)), pre_outs = as.numeric(substr(State, 9, 9)), pre_disengagements = as.numeric(substr(State, 5, 5)), sb2B = ifelse(substr(State, 2, 2) == "0" & substr(State, 1, 1) == "1", 1, 0), sprint_speed = mean_ss, arm_strength = mean_as)


### MODELS

### PLAYER-SPECIFIC

# Probability of Successful Pickoff
m1 <- glmer(isSuccess ~ lead1b + (1|pitcher_id) , data = pickoff_att_1b, family = binomial)
summary(m1)
# Pitcher matters a bit - runner/catcher/batter lead to singular effect

# Probability of Pickoff Attempt
m2 <- glmer(isPickAttempt ~ lead1b + pre_balls + pre_strikes + pre_outs + as.factor(pre_disengagements) + (1|pitcher_id) + (1|run1b), data = pickoff_var_1b, family = binomial)
summary(m2)
# Pitcher and runner matters 
print(Sys.time())

# Probability of Successful SB
m3 <- glmer(is_stolen_base ~ lead1b + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength, data = sb_att_1b, family = binomial)
summary(m3)
print(Sys.time())

# Probability of SB Attempt
# Not dependent on lead distance
m4 <- glmer(isSBAttempt ~ pre_balls + pre_strikes + pre_outs + pre_disengagements + (1|pitcher_id) + (1|fielder_2_id) + (1|run1b) + sprint_speed + arm_strength, data = sb_var_1b, family = binomial)
summary(m4)

print(Sys.time())


saveRDS(m2, "m2model")
saveRDS(m4, "m4model")


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


#### PLAYER-NEUTRAL MODELS

# # Probability of Successful Pickoff
# m1n <- glm(isSuccess ~ lead1b, data = pickoff_att_1b, family = binomial)
# summary(m1n)
# 
# # Probability of Pickoff Attempt
# m2n <- glm(isPickAttempt ~ lead1b + pre_balls + pre_strikes + pre_outs + pre_disengagements, data = pickoff_var_1b_threats, family = binomial)
# summary(m2n)
# 
# # Probability of Successful SB
# m3n <- glm(is_stolen_base ~ lead1b, data = sb_att_1b, family = binomial)
# summary(m3n)
# 
# # Probability of SB Attempt
# # Not dependent on lead distance
# m4n <- glm(isSBAttempt ~ pre_balls + pre_strikes + pre_outs, data = sb_var_1b_threats, family = binomial)
# summary(m4n)

# Plotting model results
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
ggplot(plot_data_m1, aes(x = lead1b, y = PickSuccess, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Pickoff by Lead Distance", subtitle = "Against Various Pitchers", col = "Pitcher Pickoff Skill") + theme_classic() 



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
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")
plot_data_m2 <- plot_data_m2 %>% mutate(pitcher_id = ifelse(pitcher_id == pct90_pitcher_m2, "90th Percentile Pitcher", ifelse(pitcher_id == median_pitcher_m2, "Median Pitcher", "10th Percentile Pitcher")))
plot_data_m2$pitcher_id <- factor(plot_data_m2$pitcher_id, levels = c("90th Percentile Pitcher", "Median Pitcher", "10th Percentile Pitcher"))
ggplot(plot_data_m2, aes(x = lead1b, y = PickAttempt, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Various Pitchers with 0 Disengagements", col = "Pitcher Pickoff Frequency") + theme_classic() 

# GRAPH FOR M2 (pickoff attempt) - VARY DISENGAGEMENTS
plot_data_m2 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m2$run1b <- median_runner_m2
plot_data_m2$pitcher_id <- median_pitcher_m2
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")
ggplot(plot_data_m2, aes(x = lead1b, y = PickAttempt, col = pre_disengagements)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Typical Pitcher", col = "Number of Disengagements") + theme_classic() 


# GRAPH FOR M2 (pickoff attempt) - VARY RUNNER EFFECT

plot_data_m2 <- plot_data %>% cross_join(run1b, copy = TRUE) %>% rename(run1b = y)
plot_data_m2$pitcher_id <- median_pitcher_m2
plot_data_m2$pre_disengagements <- 0
plot_data_m2$PickAttempt <- predict(m2, newdata = plot_data_m2, type = "response")
plot_data_m2 <- plot_data_m2 %>% mutate(run1b = ifelse(run1b == pct90_runner_m2, "90th Percentile Runner", ifelse(run1b == median_runner_m2, "Median Runner", "10th Percentile Runner")))
plot_data_m2$run1b <- factor(plot_data_m2$run1b, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))
ggplot(plot_data_m2, aes(x = lead1b, y = PickAttempt, col = run1b)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Pickoff Attempt by Lead Distance", subtitle = "Against Various Runners with 0 Disengagements", col = "Runner Skill") + theme_classic() 









# GRAPH FOR M3 (sb success) 
pitcher_effects <- ranef(m3)$pitcher_id
pitcher_effects <- pitcher_effects %>% arrange(`(Intercept)`)
median_pitcher_m3 <- rownames(pitcher_effects)[0.5 * nrow(pitcher_effects)]
pct90_pitcher_m3 <- rownames(pitcher_effects)[0.1 * nrow(pitcher_effects)]
pct10_pitcher_m3 <- rownames(pitcher_effects)[0.9 * nrow(pitcher_effects)]
pitcher_id <- c(pct10_pitcher_m3, median_pitcher_m3, pct90_pitcher_m3)

runner_effects <- ranef(m3)$run1b
runner_effects <- runner_effects %>% arrange(`(Intercept)`)
median_runner_m3 <- rownames(runner_effects)[0.5 * nrow(runner_effects)]
pct90_runner_m3 <- rownames(runner_effects)[0.9 * nrow(runner_effects)]
pct10_runner_m3 <- rownames(runner_effects)[0.1 * nrow(runner_effects)]
run1b <- c(pct10_runner_m3, median_runner_m3, pct90_runner_m3)

catcher_effects <- ranef(m3)$fielder_2_id
catcher_effects <- catcher_effects %>% arrange(`(Intercept)`)
median_catcher_m3 <- rownames(catcher_effects)[0.5 * nrow(catcher_effects)]
pct90_catcher_m3 <- rownames(catcher_effects)[0.9 * nrow(catcher_effects)]
pct10_catcher_m3 <- rownames(catcher_effects)[0.1 * nrow(catcher_effects)]
fielder_2_id <- c(pct10_catcher_m3, median_catcher_m3, pct90_catcher_m3)

## VARY PITCHER EFFECT
plot_data_m3 <- plot_data %>% cross_join(pitcher_id, copy = TRUE) %>% rename(pitcher_id = y)
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$pre_disengagements <- 0
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(pitcher_id = ifelse(pitcher_id == pct90_pitcher_m3, "90th Percentile Pitcher", ifelse(pitcher_id == median_pitcher_m3, "Median Pitcher", "10th Percentile Pitcher")))
plot_data_m3$pitcher_id <- factor(plot_data_m3$pitcher_id, levels = c("90th Percentile Pitcher", "Median Pitcher", "10th Percentile Pitcher"))
ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = pitcher_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Pitchers", col = "Pitcher SB Prevention Skill") + theme_classic() 

## VARY RUNNER EFFECT
plot_data_m3 <- plot_data %>% cross_join(run1b, copy = TRUE) %>% rename(run1b = y)
plot_data_m3$pitcher_id <- median_pitcher_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$pre_disengagements <- 0
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(run1b = ifelse(run1b == pct90_runner_m3, "90th Percentile Runner", ifelse(run1b == median_runner_m3, "Median Runner", "10th Percentile Runner")))
plot_data_m3$run1b <- factor(plot_data_m3$run1b, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))
ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = run1b)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Runners", col = "Runner SB Skill (Beyond Sprint Speed)") + theme_classic() 




## VARY SPRINT SPEED
pct90_ss <- quantile(sprints$sprint_speed, 0.9)
pct10_ss <- quantile(sprints$sprint_speed, 0.1)
sprint_speed <- c(pct90_ss, mean_ss, pct10_ss)

plot_data_m3 <- plot_data %>% cross_join(sprint_speed, copy = TRUE) %>% rename(sprint_speed = y)
plot_data_m3$pitcher_id <- median_pitcher_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$pre_disengagements <- 0
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(sprint_speed = ifelse(sprint_speed == pct90_ss, "90th Percentile Runner", ifelse(sprint_speed == mean_ss, "Median Runner", "10th Percentile Runner")))
plot_data_m3$sprint_speed <- factor(plot_data_m3$sprint_speed, levels = c("90th Percentile Runner", "Median Runner", "10th Percentile Runner"))
ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = sprint_speed)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Runners", col = "Runner Sprint Speed") + theme_classic() 


## VARY ARM STRENGTH
pct90_as <- quantile(poptimes$arm_strength, 0.9)
pct10_as <- quantile(poptimes$arm_strength, 0.1)
arm_strength <- c(pct90_as, mean_as, pct10_as)

plot_data_m3 <- plot_data %>% cross_join(arm_strength, copy = TRUE) %>% rename(arm_strength = y)
plot_data_m3$pitcher_id <- median_pitcher_m3
plot_data_m3$fielder_2_id <- median_catcher_m3
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$pre_disengagements <- 0
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(arm_strength = ifelse(arm_strength == pct90_as, "90th Percentile Catcher", ifelse(arm_strength == mean_as, "Median Catcher", "10th Percentile Catcher")))
plot_data_m3$arm_strength <- factor(plot_data_m3$arm_strength, levels = c("90th Percentile Catcher", "Median Catcher", "10th Percentile Catcher"))
ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = arm_strength)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Catchers", col = "Catcher Arm Strength") + theme_classic() 


## VARY CATCHER EFFECT
plot_data_m3 <- plot_data %>% cross_join(fielder_2_id, copy = TRUE) %>% rename(fielder_2_id = y)
plot_data_m3$pitcher_id <- median_pitcher_m3
plot_data_m3$run1b <- median_runner_m3
plot_data_m3$arm_strength <- mean_as
plot_data_m3$sprint_speed <- mean_ss
plot_data_m3$pre_disengagements <- 0
plot_data_m3$SBSuccess <- predict(m3, newdata = plot_data_m3, type = "response")
plot_data_m3 <- plot_data_m3 %>% mutate(fielder_2_id = ifelse(fielder_2_id == pct90_catcher_m3, "90th Percentile Catcher", ifelse(fielder_2_id == median_catcher_m3, "Median Catcher", "10th Percentile Catcher")))
plot_data_m3$fielder_2_id <- factor(plot_data_m3$fielder_2_id, levels = c("90th Percentile Catcher", "Median Catcher", "10th Percentile Catcher"))
ggplot(plot_data_m3, aes(x = lead1b, y = SBSuccess, col = fielder_2_id)) + geom_line() + labs(x = "Lead Distance", y = "Probability", title = "Probability of Successful Stolen Base by Lead Distance", subtitle = "Against Various Runners", col = "Catcher Skill (Beyond Arm Strength)") + theme_classic() 



#plot_data$SBAttempt <- predict(m4, newdata = plot_data, type = "response")


grouped_picks <- pickoff_var_1b %>% group_by(round(lead1b, 1)) %>% summarize(PickProb = mean(isPickAttempt), n = n())
ggplot(grouped_picks) + aes(x = `round(lead1b, 1)`, y = PickProb) + labs(x = "Lead Distance", y = "Pickoff Attempt Probability", title = "Actual Pickoff Probability by Lead Distance") + geom_point(aes(alpha = n))


# Run models on each state/lead combo
state_leads_exp$pickoff_prob <- predict(m2, newdata = state_leads_exp, type = "response", re.form = NA)
state_leads_exp$pick_succ <- predict(m1, newdata = state_leads_exp, type = "response", re.form = NA)

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


# VALUE ITERATION STEPS

# Initial run expectancy table from empirical outcomes
run_counts <- states_final %>% group_by(half_inn_id) %>% mutate(runs_in_inning = sum(runs_on_play)) %>% mutate(runs_so_far = cumsum(runs_on_play) - runs_on_play) %>% mutate(runs_roi = runs_in_inning - runs_so_far)

# Run Expactancy Table - average runs scored over the rest of the inning from each state
re_table <- run_counts %>% group_by(State) %>% summarize(RE = mean(runs_roi), n = n())
all_states_df <- data.frame(State = all_possible_states)
full_re_table <- all_states_df %>% left_join(re_table, by = "State") %>% mutate(RE = ifelse(is.na(RE), 0, RE), n = ifelse(is.na(n), 0, n))

# Get Runs Scored in Each State
transitions_expanded <- expand.grid(State = all_possible_states, New_State = all_possible_states) %>% mutate(OldRunners = as.numeric(substr(State, 1, 1)) + as.numeric(substr(State, 2, 2)) + as.numeric(substr(State, 3, 3))) %>% mutate(NewRunners = as.numeric(substr(New_State, 1, 1)) + as.numeric(substr(New_State, 2, 2)) + as.numeric(substr(New_State, 3, 3))) %>% mutate(OldOuts = as.numeric(substr(State, 9, 9))) %>% mutate(NewOuts = as.numeric(substr(New_State, 9, 9))) %>% mutate(OldDis = as.numeric(substr(State, 5, 5))) %>% mutate(NewDis = as.numeric(substr(New_State, 5, 5))) %>% mutate(OldCount = substr(State, 7,8)) %>% mutate(NewCount = substr(New_State, 7,8)) %>% mutate(NewBatter = ifelse(NewCount == "00" & NewDis == 0, 1, 0)) %>% mutate(RunnerPickedOff = ifelse(OldCount == NewCount & NewOuts == OldOuts + 1 & NewRunners == OldRunners - 1, 1, 0)) %>% mutate(NewBatter = ifelse(RunnerPickedOff == 1, 0, NewBatter))

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

# Only want situations where runner on 1b and no runner on 2b
runon1b <- leads_by_state %>% filter(substr(State, 1, 2) == "10")
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
    summarize(RE = sum(TotalProb * (RunsScored + RE))) %>% ungroup() %>%
    group_by(State) %>% filter(row_number() == which.max(RE))
  
  new_run_1b <- re_vals %>% filter(substr(State, 1, 2) == "10")
  
  change <- new_run_1b |>                                                                              
    dplyr::left_join(old_run_1b, by = "State", suffix = c("_old", "_new")) |>                      
    with(sum(abs(lead1b_new - lead1b_old))) 
  
  print(change)
              
  old_re_table <- re_vals %>% select(-lead1b)     
  old_run_1b <- new_run_1b
  iterations <- iterations + 1
  
}      



# MONOTONICITY CHECKS
RE_transitions <- T_matrix %>% select(State, New_State, Freq) %>% left_join(old_re_table, by = "State") %>% rename(Old_RE = RE) %>% left_join(old_re_table, by = c("New_State" = "State")) %>% rename(New_RE = RE)  %>% left_join(runs_on_transition, by = c("State", "New_State")) %>% mutate(RE_change = New_RE - Old_RE + RunsScored)

# Situations where a ball is thrown and all else is the same
all_same_but_balls <- RE_transitions %>% filter(substr(State, 1, 6) == substr(New_State, 1, 6), substr(State, 8, 9) == substr(New_State, 8, 9)) %>% mutate(Pre_Balls = as.numeric(substr(State, 7, 7)), Post_Balls = as.numeric(substr(New_State, 7, 7))) %>% filter(Post_Balls - Pre_Balls == 1)
ggplot(all_same_but_balls) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Ball Thrown")

balls1000 <- all_same_but_balls %>% filter(Freq > 1000)
ggplot(balls1000) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Ball Thrown - transitions with over 1000 iterations")

# Situations where a strike is thrown and all else is the same
all_same_but_strikes <- RE_transitions %>% filter(substr(State, 1, 7) == substr(New_State, 1, 7), substr(State, 9, 9) == substr(New_State, 9, 9)) %>% mutate(Pre_Strikes = as.numeric(substr(State, 8, 8)), Post_Strikes = as.numeric(substr(New_State, 8, 8))) %>% filter(Post_Strikes - Pre_Strikes == 1)
ggplot(all_same_but_strikes) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Strike Thrown")

strikes1000 <- all_same_but_strikes %>% filter(Freq > 1000)
ggplot(strikes1000) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Strike Thrown - transitions with over 1000 iterations")


# Situations where a disengagement occurs and all else is the same
all_same_but_dis <- RE_transitions %>% filter(substr(State, 1, 4) == substr(New_State, 1, 4), substr(State, 6, 9) == substr(New_State, 6, 9)) %>% mutate(Pre_Dis = as.numeric(substr(State, 5, 5)), Post_Dis = as.numeric(substr(New_State, 5, 5))) %>% filter(Post_Dis - Pre_Dis == 1)
ggplot(all_same_but_dis) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Disengagement")

dis100 <- all_same_but_dis %>% filter(Freq > 100)
ggplot(dis100) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Disengagement - transitions with over 100 iterations")


# Situations where an out is made and all else is the same
all_same_but_outs <- RE_transitions %>% filter(substr(State, 1, 8) == substr(New_State, 1, 8), substr(State, 8, 8) == substr(New_State, 8, 8)) %>% mutate(Pre_Outs = as.numeric(substr(State, 9, 9)), Post_Outs = as.numeric(substr(New_State, 9, 9))) %>% filter(Post_Outs - Pre_Outs == 1)
ggplot(all_same_but_outs) + aes(Freq, RE_change) + geom_point() + geom_hline(yintercept = 0) + labs(title = "Change in RE from Out")



write_csv(old_run_1b, "res.csv")

sorted <- old_run_1b %>% mutate(bases = substr(State, 1, 3), dis = substr(State, 5,5), countouts = substr(State, 7, 9)) %>% arrange(bases, countouts, dis)

write_csv(sorted, "res.csv")

