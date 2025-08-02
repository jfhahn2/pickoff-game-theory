
fig_make <- TRUE
fig_mode <- "light"


# Read data ----

arm_strength <- data.table::fread("input/data/arm_strength/2023.csv") |>
  dplyr::select(player_id, player_name, arm_strength, sb_attempts)
sprint_speed <- data.table::fread("input/data/sprint_speed/2023.csv") |>
  dplyr::select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)


# Plot data summary ----

second_open_22 <- baserunners_22 |>
  dplyr::left_join(play_2022, by = "play_id") |>
  dplyr::filter(
    is.na(`lead_distance_2nd Base`),
    is.na(`lead_distance_3rd Base`),
    !is.na(`lead_distance_1st Base`)
  ) |>
  dplyr::select(`lead_distance_1st Base`, pre_disengagements) |>
  dplyr::mutate(year = "2022")

second_open_23 <- baserunners |>
  dplyr::left_join(play_2023, by = "play_id") |>
  dplyr::filter(
    is.na(`lead_distance_2nd Base`),
    is.na(`lead_distance_3rd Base`),
    !is.na(`lead_distance_1st Base`)
  ) |>
  dplyr::select(`lead_distance_1st Base`, pre_disengagements) |>
  dplyr::mutate(year = "2023")

lead_distance_1b <- rbind(second_open_22, second_open_23) |>
  dplyr::mutate(
    year_dis = case_when(
      year == 2022 ~ "2022 - All Situations",
      year == 2023 & pre_disengagements == 0 ~ "2023 - 0 Disengagements",
      year == 2023 & pre_disengagements == 1 ~ "2023 - 1 Disengagements",
      year == 2023 & pre_disengagements == 2 ~ "2023 - 2 Disengagements"
    )
  ) |>
  dplyr::filter(!is.na(year_dis))

breaks <- seq(from = 4, to = 16, by = 2)

if (fig_make) {

  sputil::open_device(glue::glue("output/figures/leads_overall_{fig_mode}.pdf"), height = 4, width = 7)
  plot <- lead_distance_1b |>
    ggplot2::ggplot(ggplot2::aes(`lead_distance_1st Base`, col = year_dis, linetype = year_dis)) +
    ggplot2::stat_density(geom = "line", position = "identity") +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_color_manual(
      name = "Scenario",
      values = c(sputil::color("gray", fig_mode), rep(sputil::color("blue", fig_mode), 3))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario",
      values = c("solid", "solid", "dashed", "dotted")
    ) +
    ggplot2::labs(x = "Lead Distance", y = "Density") +
    ggplot2::coord_cartesian(xlim = c(3, 16)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.2, 0.8))
  print(plot)
  dev.off()
}


# Plot probability model summaries ----

fit_po_attempt <- readRDS("models/fit_po_attempt.rds")
fit_po_success <- readRDS("models/fit_po_success.rds")
fit_sb_success <- readRDS("models/fit_sb_success.rds")

covariate_baseline <- tibble::tibble(
  pre_balls = 0,
  pre_strikes = 0,
  pre_outs = 0,
  arm_strength = median(arm_strength$arm_strength)
)
lead_distance_grid <- tibble::tibble(lead1b = seq(from = 0, to = 20, by = 0.1))

if (fig_make) {

  sputil::open_device(glue::glue("output/figures/prob_po_attempt_{fig_mode}.pdf"), height = 4, width = 4)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(tibble::tibble(subset = c("2022_0", "2023_0", "2023_1", "2023_2"))) |>
    dplyr::mutate(
      year = substring(subset, 1, 4),
      pre_disengagements = substring(subset, 6),
      legend = ifelse(
        test = year == 2022,
        yes = as.character(year),
        no = glue::glue("{year}; {pre_disengagements} Disengagements")
      )
    )
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_po_attempt = predict(
        object = fit_po_attempt,
        newdata = covariate_grid,
        type = "response",
        re.form = NA)
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead1b, y = prob_po_attempt, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.2, 0.4, 0.6),
      labels = glue::glue("{c(0, 20, 40, 60)}%")
    ) +
    ggplot2::scale_color_manual(
      name = "Scenario",
      values = c(sputil::color("gray", fig_mode), rep(sputil::color("blue", fig_mode), 3))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario",
      values = c("solid", "solid", "dashed", "dotted")
    ) +
    ggplot2::labs(title = "Pickoff Attempt", x = "Lead Distance", y = "Probability") +
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0, 0.7)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.4, 0.7))
  print(plot)
  dev.off()
}


pitcher_grid <- ranef(fit_po_success)$pitcher_id |>
  dplyr::arrange(`(Intercept)`) |>
  dplyr::slice(round(dplyr::n() * c(0.9, 0.5, 0.1))) |>
  tibble::rownames_to_column("pitcher_id") |>
  dplyr::select(pitcher_id) |>
  dplyr::mutate(
    legend = factor(
      x = paste(c("90th", "50th", "10th"), "Percentile"),
      levels = paste(c("90th", "50th", "10th"), "Percentile")
    )
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/prob_po_success_{fig_mode}.pdf"), height = 4, width = 4)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(pitcher_grid) |>
    dplyr::mutate(
      year = "2023",
      pre_disengagements = "0"
    )
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_po_attempt = predict(fit_po_success, newdata = covariate_grid, type = "response")
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead1b, y = prob_po_attempt, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8),
      labels = glue::glue("{c(0, 20, 40, 60, 80)}%")
    ) +
    ggplot2::scale_color_manual(
      name = "Pitcher Pickoff Skill",
      values = rep(sputil::color("blue", fig_mode), 3)
    ) +
    ggplot2::scale_linetype_manual(
      name = "Pitcher Pickoff Skill",
      values = c("dotted", "solid", "dashed")
    ) +
    ggplot2::labs(title = "Pickoff Success", x = "Lead Distance", y = "Probability") +
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0, 0.7)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.3, 0.7))
  print(plot)
  dev.off()
}

sb_success_effect_runner <- ranef(fit_sb_success)$run1b |>
  tibble::rownames_to_column("player_id") |>
  dplyr::filter(player_id != "r1") |>   # remove "replacement-level" runner
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(sprint_speed, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` + fixef(fit_sb_success)["sprint_speed"] * sprint_speed |>
      scale(scale = FALSE)
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/effect_runner_{fig_mode}.pdf"), height = 4, width = 4)
  plot <- sb_success_effect_runner |>
    ggplot2::ggplot(ggplot2::aes(x = sprint_speed, y = effect)) +
    ggplot2::geom_point(color = sputil::color("blue", fig_mode), alpha = 0.5) +
    ggplot2::labs(
      title = "Runner Effects",
      x = "Sprint Speed",
      y = "Effect on SB Success (log-odds)"
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.6, 0.6)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

sb_success_effect_catcher <- ranef(fit_sb_success)$fielder_2_id |>
  tibble::rownames_to_column("player_id") |>
  dplyr::filter(player_id != "c1") |>   # remove "replacement-level" catcher
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(arm_strength, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` + fixef(fit_sb_success)["arm_strength"] * arm_strength |>
      scale(scale = FALSE)
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/effect_catcher_{fig_mode}.pdf"), height = 4, width = 4)
  plot <- sb_success_effect_catcher |>
    ggplot2::ggplot(ggplot2::aes(x = arm_strength, y = effect)) +
    ggplot2::geom_point(color = sputil::color("blue", fig_mode), alpha = 0.5) +
    ggplot2::labs(
      title = "Catcher Effects",
      x = "Arm Strength",
      y = "Effect on SB Success (log-odds)"
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.6, 0.6)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

runner_grid <- sb_success_effect_runner |>
  dplyr::arrange(effect) |>
  dplyr::slice(round(dplyr::n() * c(0.9, 0.5, 0.1, 0.5))) |>
  dplyr::mutate(run1b = as.character(player_id)) |>
  dplyr::select(run1b, sprint_speed) |>
  dplyr::mutate(
    year = c(rep(2023, 3), 2022),
    legend = factor(
      x = paste0(year, "; ", c(90, 50, 10, 50), "th Pct Runner"),
      levels = paste0(year, "; ", c(90, 50, 10, 50), "th Pct Runner")
    )
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/prob_sb_success_{fig_mode}.pdf"), height = 4, width = 7)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(runner_grid)
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_sb_success = predict(
        object = fit_sb_success,
        newdata = covariate_grid,
        type = "response",
        re.form = ~ (1 | run1b)
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead1b, y = prob_sb_success, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0.65, to = 0.85, by = 0.05),
      labels = glue::glue("{seq(from = 65, to = 85, by = 5)}%")
    ) +
    ggplot2::scale_color_manual(
      name = "Scenario",
      values = c(rep(sputil::color("blue", fig_mode), 3), sputil::color("gray"))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario",
      values = c("dotted", "solid", "dashed", "solid")
    ) +
    ggplot2::labs(x = "Lead Distance", y = "Probability") +
    ggplot2::coord_cartesian(xlim = c(3, 16)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.85, 0.25))
  print(plot)
  dev.off()
}


# Plot MDP example ----

lead_value <- transition_values |>
  dplyr::left_join(old_re_table, by = c("New_State" = "State")) |>
  dplyr::group_by(State, lead1b) |>
  dplyr::summarize(RE = sum(TotalProb * (RunsScored + RE)), n = mean(n), .groups = "drop") |>
  dplyr::filter(State %in% c("100 0 000", "100 1 000", "100 2 000")) |>
  dplyr::mutate(disengagements = substring(State, 5, 5))

lead_value_max <- lead_value |>
  dplyr::group_by(disengagements) |>
  dplyr::arrange(-RE) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

if (fig_make) {
  sputil::open_device(
    file = glue::glue("output/figures/finding_optimal_lead_{fig_mode}.pdf"),
    height = 4,
    width = 7
  )
  plot <- lead_value |>
    dplyr::mutate(disengagements = factor(disengagements, levels = 2:0)) |>   # re-order for legend
    ggplot2::ggplot(ggplot2::aes(lead1b, y = RE, linetype = disengagements)) +
    ggplot2::geom_line(color = sputil::color("blue", fig_mode)) +
    ggplot2::labs(x = "Lead Distance", y = "Expected Runs to End of Inning") +
    ggplot2::scale_linetype_manual(
      name = "Disengagements",
      values = c("dotted", "dashed", "solid")
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 20), ylim = c(0.9, 0.96)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.2, 0.25))
  print(plot)
  dev.off()
}


# Write results tables ----

sorted |>
  dplyr::ungroup() |>
  dplyr::filter(substr(countouts, 3, 3) == "0", bases == "100") |>
  dplyr::mutate(count = paste0(substr(countouts, 1, 1), "-", substr(countouts, 2, 2))) |>
  dplyr::select(count, dis, lead1b) |>
  tidyr::pivot_wider(names_from = dis, values_from = lead1b, names_prefix = "disengagements_") |>
  sputil::write_latex_table(
    file = "output/tables/lead_by_count.tex",
    prefix_rows = " & \\multicolumn{3}{c}{Disengagements}",
    colnames = c("Count", "0", "1", "2"),
    align = "c|rrr",
    digits = 1
  )

skill_grid |>
  dplyr::ungroup() |>
  # Hopefully this nastiness can be cleaned up upstream in the future
  dplyr::mutate(
    battery_skill = dplyr::case_when(
      battery_skill == "10th Percentile Battery" ~ "10th",
      battery_skill == "Median Battery" ~ "50th",
      battery_skill == "90th Percentile Battery" ~ "90th"
    ),
    runner_skill = dplyr::case_when(
      runner_skill == "10th Percentile Runner" ~ "10th",
      runner_skill == "Median Runner" ~ "50th",
      runner_skill == "90th Percentile Runner" ~ "90th"
    )
  ) |>
  dplyr::select(battery_skill, runner_skill, V15, V16, V17) |>
  sputil::write_latex_table(
    file = "output/tables/lead_by_players.tex",
    prefix_rows = "\\multicolumn{2}{c|}{Skill Percentile} & \\multicolumn{3}{c}{Disengagements}",
    colnames = c("Battery", "Runner", "0", "1", "2"),
    align = "cc|rrr",
    digits = 1,
    hline.after = c(0, 3, 6)
  )

sorted_two <- old_run_1b_two |>
  dplyr::mutate(
    bases = substr(State, 1, 3),
    dis = substr(State, 5,5), countouts = substr(State, 7, 9)
  ) |>
  dplyr::arrange(bases, countouts, dis)

sorted_two |>
  dplyr::ungroup() |>
  dplyr::filter(substr(countouts, 3, 3) == "0", bases == "100") |>
  dplyr::mutate(Count = paste0(substr(countouts, 1, 1), "-", substring(countouts, 2, 2))) |>
  dplyr::select(Count, dis, lead1b) |>
  pivot_wider(names_from = dis, values_from = lead1b, names_prefix = "Disengagements_") |>
  sputil::write_latex_table(
    file = "output/tables/count_two_agent.tex",
    prefix_rows = " & \\multicolumn{3}{c}{Disengagements}",
    colnames = c("Count", "0", "1", "2"),
    align = "l|rrr",
    digits = 1
  )

actual_vs_recommended_leads |>
  dplyr::group_by(pre_disengagements) |>
  dplyr::summarize(
    Actual = mean(ActualLead, na.rm = TRUE),
    Rec = mean(RecLead, na.rm = TRUE),
    Exceeds = mean(LeadDiff > 0, na.rm = TRUE)
  ) |>
  dplyr::mutate(Exceeds = glue::glue("{sprintf('%.1f', 100 * Exceeds)}\\%")) |>
  sputil::write_latex_table(
    file = "output/tables/actual_vs_rec_lead.tex",
    prefix_rows = " & \\multicolumn{2}{c|}{Average Lead} & Actual Exceeds",
    colnames = c("Disengagements", "Actual", "Recommended", "Recommendation"),
    align = "c|cc|c",
    digits = c(NA, 0, 1, 1, NA)
  )

