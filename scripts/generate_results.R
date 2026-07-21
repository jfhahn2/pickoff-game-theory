
# This script will work without the raw lead distance data, but you need to first run
# scripts/estimate_models.R and set `include_bootstrap_se <- FALSE` below. This will cause the
# bootstrap standard errors to be written as zero because they cannot be calculated.
include_bootstrap_se <- TRUE
fig_make <- TRUE
fig_mode <- "light"

if (!dir.exists("output/figures")) {
  dir.create("output/figures", recursive = TRUE)
}

if (!dir.exists("output/tables")) {
  dir.create("output/tables", recursive = TRUE)
}


# Read data ----

arm_strength <- data.table::fread("input/data/arm_strength/2023.csv") |>
  dplyr::select(player_id, player_name, arm_strength, sb_attempts)
sprint_speed <- data.table::fread("input/data/sprint_speed/2023.csv") |>
  dplyr::select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)

data_glmm <- data.table::fread("output/data/data_glmm.csv")

policy_mrp <- data.table::fread("output/policy_mrp.csv")
policy_mdp <- data.table::fread("output/policy_mdp.csv")
policy_zsg <- data.table::fread("output/policy_zsg.csv")
policy_mdp_skill <- data.table::fread("output/policy_mdp_skill.csv")



# Read bootstrap results ----

if (include_bootstrap_se) {

  policy_mrp_boot <- NULL
  policy_mdp_boot <- NULL
  policy_zsg_boot <- NULL
  policy_mdp_skill_boot <- NULL
  
  result <- list()
  for (file in list.files("output/bootstrap")) {
    bag <- as.integer(gsub("([0-9]+).*$", "\\1", file))
    result[[bag]] <- readRDS(file.path("output", "bootstrap", file))
  
    policy_mrp_boot <- result[[bag]]$policy_mrp |>
      dplyr::mutate(bag = bag, .before = 1) |>
      dplyr::bind_rows(policy_mrp_boot)
  
    policy_mdp_boot <- result[[bag]]$policy_mdp |>
      dplyr::mutate(bag = bag, .before = 1) |>
      dplyr::bind_rows(policy_mdp_boot)
  
    policy_zsg_boot <- result[[bag]]$policy_zsg |>
      dplyr::mutate(bag = bag, .before = 1) |>
      dplyr::bind_rows(policy_zsg_boot)
  
    policy_mdp_skill_boot <- result[[bag]]$policy_mdp_skill |>
      dplyr::mutate(bag = bag, .before = 1) |>
      dplyr::bind_rows(policy_mdp_skill_boot)
  }

} else {
  # If we don't have the bootstrapped results, we're just going to hack it so that the rest of the
  # code runs correctly and results in bootstrap errors equal to zero.
  policy_mrp_boot <- tidyr::expand_grid(policy_mrp, bag = 0:1)
  policy_mdp_boot <- tidyr::expand_grid(policy_mdp, bag = 0:1)
  policy_zsg_boot <- tidyr::expand_grid(policy_zsg, bag = 0:1)
  policy_mdp_skill_boot <- tidyr::expand_grid(policy_mdp_skill, bag = 0:1)
}



# Plot data summary ----

breaks <- seq(from = 4, to = 16, by = 2)

if (fig_make & !all(is.na(data_glmm$lead_distance))) {

  sputil::open_device(glue::glue("output/figures/leads_overall_{fig_mode}.pdf"), height = 4, width = 8)
  plot <- data_glmm |>
    dplyr::mutate(
      year_dis = dplyr::case_when(
        year == 2022 ~ "2022 - All Situations",
        year == 2023 & pre_disengagements == 0 ~ "2023 - 0 Disengagements",
        year == 2023 & pre_disengagements == 1 ~ "2023 - 1 Disengagements",
        year == 2023 & pre_disengagements == 2 ~ "2023 - 2 Disengagements"
      )
    ) |>
    dplyr::mutate(
      mean = sprintf("%.1f", mean(lead_distance)),
      label = glue::glue("{year_dis} ({mean} ft)"),
      .by = year_dis
    ) |>
    ggplot2::ggplot(ggplot2::aes(lead_distance, col = label, linetype = label)) +
    ggplot2::stat_density(geom = "line", position = "identity") +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_color_manual(
      name = "Scenario (mean)",
      values = c(sputil::color("orange", fig_mode), rep(sputil::color("blue", fig_mode), 3))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario (mean)",
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

fit_runner_outcome <- readRDS("output/models/fit_runner_outcome.rds")

extract_model_fit <- function(model) {

  if (class(model) == "glmmTMB") {
    summary <- summary(model)

  } else if (class(model) == "glmmTMBreduced") {
    summary <- model$summary
  }

  ranef <- tibble::tibble(
    type = "random",
    variable = names(summary$varcor$cond),
    `Std. Error` = sapply(summary$varcor$cond, function(x) {attr(x, "stddev")})
  )
  summary$coefficients$cond |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::mutate(type = "fixed") |>
    dplyr::bind_rows(ranef) |>
    dplyr::mutate(
      estimate = paste(
        ifelse(type == "random", "", glue::glue("${sprintf('%.2f', Estimate)}$")),
        glue::glue("{{\\color{{gray}} $\\pm{sprintf('%.2f', `Std. Error`)}$}}")
      )
    ) |>
    dplyr::select(type, variable, estimate)
}

table_pickoff_attempt <- extract_model_fit(fit_runner_outcome$pickoff_attempt) |>
  dplyr::rename(pickoff_attempt = estimate)
table_pickoff_success <- extract_model_fit(fit_runner_outcome$pickoff_success) |>
  dplyr::rename(pickoff_success = estimate)
table_runner_going <- extract_model_fit(fit_runner_outcome$runner_going) |>
  dplyr::rename(runner_going = estimate)
table_going_interrupt <- extract_model_fit(fit_runner_outcome$going_interrupt) |>
  dplyr::rename(going_interrupt = estimate)
table_stolen_base <- extract_model_fit(fit_runner_outcome$stolen_base) |>
  dplyr::rename(stolen_base = estimate)

table_pickoff_attempt |>
  dplyr::full_join(table_pickoff_success, by = c("type", "variable")) |>
  dplyr::full_join(table_runner_going, by = c("type", "variable")) |>
  dplyr::full_join(table_going_interrupt, by = c("type", "variable")) |>
  dplyr::full_join(table_stolen_base, by = c("type", "variable")) |>
  dplyr::arrange(type) |>
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "(Intercept)" ~ "{\\it Intercept}",
      variable == "year2023" ~ "Year (2023 vs. 2022)",
      variable == "lead_distance_centered" ~ "Lead Distance (ft)",
      variable == "pre_balls1" ~ "Balls (1 vs. 0)",
      variable == "pre_balls2" ~ "Balls (2 vs. 0)",
      variable == "pre_balls3" ~ "Balls (3 vs. 0)",
      variable == "pre_strikes1" ~ "Strikes (1 vs. 0)",
      variable == "pre_strikes2" ~ "Strikes (2 vs. 0)",
      variable == "pre_outs1" ~ "Outs (1 vs. 0)",
      variable == "pre_outs2" ~ "Outs (2 vs. 0)",
      variable == "pre_disengagements1" ~ "Disengagements (1 vs. 0)",
      variable == "pre_disengagements2" ~ "Disengagements (2 vs. 0)",
      variable == "sprint_speed_centered" ~ "Runner Sprint Speed (ft/s)",
      variable == "arm_strength_centered" ~ "Catcher Arm Strength (mi/h)",
      variable == "pitcher_id" ~ "Pitcher",
      variable == "catcher_id" ~ "Catcher",
      variable == "runner_id" ~ "Runner",
    )
  ) |>
  tibble::add_row(variable = "{\\it Fixed Effects}", .after = 1) |>
  tibble::add_row(variable = "{\\it Random Effects}", .after = 15) |>
  dplyr::select(variable, pickoff_attempt, pickoff_success, runner_going, going_interrupt, stolen_base) |>
  sputil::write_latex_table(
    file = "output/tables/model_summary.tex",
    colnames = c("Variable", "Pickoff Attempt", "Pickoff Success", "Runner Going", "Batter Interruption", "Stolen Base"),
    prefix_rows = c("&\\multicolumn{5}{c}{Estimated Effect on Log-Odds}"),
    align = "l|rrrrr",
    hline.after = c(0, 1, 15)
  )


# Plot runner outcome probabilities as functions of lead distance ----

covariate_baseline <- tibble::tibble(
  pre_outs = factor(0, levels = 0:2),
  pre_balls = factor(0, levels = 0:3),
  pre_strikes = factor(0, levels = 0:2),
  arm_strength_centered = 0
)
lead_distance_grid <- tibble::tibble(
  lead_distance = seq(from = 0, to = 20, by = 0.1),
  lead_distance_centered = lead_distance - 10
)

if (fig_make) {

  sputil::open_device(glue::glue("output/figures/prob_pickoff_attempt_{fig_mode}.pdf"), height = 4, width = 4)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(tibble::tibble(subset = c("2022_0", "2023_0", "2023_1", "2023_2"))) |>
    dplyr::mutate(
      year = factor(substring(subset, 1, 4), levels = c(2022, 2023)),
      pre_disengagements = factor(substring(subset, 6), levels = 0:2),
      legend = ifelse(
        test = year == 2022,
        yes = as.character(year),
        no = glue::glue("{year}; {pre_disengagements} Disengagements")
      )
    )
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_pickoff_attempt = predict_runner_outcome_component(
        object = fit_runner_outcome$pickoff_attempt,
        newdata = covariate_grid,
        include_ranef = FALSE
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_pickoff_attempt, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.2, 0.4, 0.6),
      labels = glue::glue("{c(0, 20, 40, 60)}%")
    ) +
    ggplot2::scale_color_manual(
      name = "Scenario",
      values = c(sputil::color("orange", fig_mode), rep(sputil::color("blue", fig_mode), 3))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario",
      values = c("solid", "solid", "dashed", "dotted")
    ) +
    ggplot2::labs(title = "Pickoff Attempt", x = "Lead Distance", y = "Probability") +
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0, 0.6)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.4, 0.7))
  print(plot)
  dev.off()
}


pitcher_grid <- pickoffgame::extract_ranef(fit_runner_outcome$pickoff_success)$pitcher_id |>
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
  sputil::open_device(
    file = glue::glue("output/figures/prob_pickoff_success_{fig_mode}.pdf"),
    height = 4,
    width = 4
  )
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(pitcher_grid) |>
    dplyr::mutate(
      year = factor(2023, levels = 2022:2023),
      pre_disengagements = factor(0, levels = 0:2)
    )
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_pickoff_attempt = predict_runner_outcome_component(
        object = fit_runner_outcome$pickoff_success,
        newdata = covariate_grid,
        include_ranef = TRUE
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_pickoff_attempt, col = legend, linetype = legend)
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
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0, 0.6)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.3, 0.7))
  print(plot)
  dev.off()
}

stolen_base_effect_runner <- pickoffgame::extract_ranef(fit_runner_outcome$stolen_base)$runner_id |>
  tibble::rownames_to_column("player_id") |>
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(sprint_speed, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` +
      (
        pickoffgame::extract_fixef(fit_runner_outcome$stolen_base)["sprint_speed_centered"] *
        (sprint_speed - 27)
      )
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/effect_runner_{fig_mode}.pdf"), height = 4, width = 4)
  plot <- stolen_base_effect_runner |>
    ggplot2::ggplot(ggplot2::aes(x = sprint_speed, y = effect)) +
    ggplot2::geom_point(color = sputil::color("blue", fig_mode), alpha = 0.5) +
    ggplot2::labs(
      title = "Runner Effects",
      x = "Sprint Speed",
      y = "Effect on Log-Odds of Stolen Base"
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.8, 0.8)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

stolen_base_effect_catcher <- pickoffgame::extract_ranef(fit_runner_outcome$stolen_base)$catcher_id |>
  tibble::rownames_to_column("player_id") |>
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(arm_strength, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` +
      (
        pickoffgame::extract_fixef(fit_runner_outcome$stolen_base)["arm_strength_centered"] *
        (arm_strength - 80)
      )
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/effect_catcher_{fig_mode}.pdf"), height = 4, width = 4)
  plot <- stolen_base_effect_catcher |>
    ggplot2::ggplot(ggplot2::aes(x = arm_strength, y = effect)) +
    ggplot2::geom_point(color = sputil::color("blue", fig_mode), alpha = 0.5) +
    ggplot2::labs(
      title = "Catcher Effects",
      x = "Arm Strength",
      y = "Effect on Log-Odds of Stolen Base"
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.8, 0.8)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

# What percentage of variation in catcher effect is explained by arm strength?
with(stolen_base_effect_catcher, cor(arm_strength, effect)^2)


runner_grid <- stolen_base_effect_runner |>
  dplyr::arrange(effect) |>
  dplyr::slice(round(dplyr::n() * c(0.9, 0.5, 0.1, 0.5))) |>
  dplyr::mutate(runner_id = as.character(player_id), sprint_speed_centered = sprint_speed - 27) |>
  dplyr::select(runner_id, sprint_speed_centered) |>
  dplyr::mutate(
    year = factor(c(rep(2023, 3), 2022), levels = 2022:2023),
    legend = factor(
      x = paste0(year, "; ", c(90, 50, 10, 50), "th Pct Runner"),
      levels = paste0(year, "; ", c(90, 50, 10, 50), "th Pct Runner")
    ),
    pitcher_id = "0",
    catcher_id = "0"
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/prob_stolen_base_{fig_mode}.pdf"), height = 4, width = 8)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(runner_grid)
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_stolen_base = predict_runner_outcome_component(
        object = fit_runner_outcome$stolen_base,
        newdata = covariate_grid,
        include_ranef = TRUE,
        allow_new_levels = TRUE
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_stolen_base, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0.3, to = 0.9, by = 0.1),
      labels = glue::glue("{seq(from = 30, to = 90, by = 10)}%")
    ) +
    ggplot2::scale_color_manual(
      name = "Scenario",
      values = c(rep(sputil::color("blue", fig_mode), 3), sputil::color("orange", fig_mode))
    ) +
    ggplot2::scale_linetype_manual(
      name = "Scenario",
      values = c("dotted", "solid", "dashed", "solid")
    ) +
    ggplot2::labs(x = "Lead Distance", y = "Probability") +
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0.4, 0.9)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.85, 0.25))
  print(plot)
  dev.off()
}

# What percentage of variation in runner effect is explained by sprint speed?
with(stolen_base_effect_runner, cor(sprint_speed, effect)^2)


# Write results tables ----

policy_zsg_se <- tibble::as_tibble(policy_zsg) |>
  dplyr::left_join(policy_zsg_boot, by = "state", suffix = c("", "_boot")) |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    bases = state$bases,
    outs = state$outs,
    balls = state$balls,
    strikes = state$strikes,
    disengagements = state$disengagements,
    first = state$first
  ) |>
  dplyr::filter(bases == "100", !(first == 0 & balls == 0 & strikes == 0 & disengagements == 0)) |>
  dplyr::group_by(outs, balls, strikes, disengagements) |>
  dplyr::summarize(
    policy_runner = mean(policy_runner),
    se = sd(policy_runner_boot),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    count = glue::glue("{balls}-{strikes}"),
    policy = glue::glue("{sprintf('%.1f', policy_runner)} {{\\color{{gray}} $\\pm$ {sprintf('%.1f', se)}}}")
  ) |>
  dplyr::arrange(strikes, balls, outs, disengagements) |>
  dplyr::select(outs, count, disengagements, policy)

policy_zsg_se |>
  dplyr::filter(outs == 0) |>
  dplyr::select(count, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  sputil::write_latex_table(
    file = "output/tables/policy_by_count_zsg.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Optimal Lead by Prior Disengagements}",
    colnames = c("Count", "0", "1", "2"),
    align = "c|ccc",
    hline.after = c(0, 4, 8, 12)
  )

policy_zsg_se |>
  dplyr::filter(count == "0-0") |>
  dplyr::select(outs, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  sputil::write_latex_table(
    file = "output/tables/policy_by_outs_zsg.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Optimal Lead by Prior Disengagements}",
    colnames = c("Outs", "0", "1", "2"),
    align = "c|ccc"
  )


behavior_vs_equilibrium <- data_glmm |>
  dplyr::filter(year == 2023) |>
  dplyr::left_join(policy_zsg, by = c("pre_state" = "state")) |>
  dplyr::mutate(lead_exceeds_recommendation = lead_distance > policy_runner)

# How does runner behavior in 2023 compare with the equilibrium?
behavior_vs_equilibrium |>
  dplyr::group_by(pre_disengagements) |>
  dplyr::summarize(n = dplyr::n(), mean(lead_exceeds_recommendation), .groups = "drop")
  
# How does pitcher behavior in 2023 compare with the equilibrium?
behavior_vs_equilibrium |>
  dplyr::group_by(pre_disengagements, lead_exceeds_recommendation) |>
  dplyr::summarize(n = dplyr::n(), mean(is_pickoff_attempt), .groups = "drop")



policy_mdp_se <- tibble::as_tibble(policy_mdp) |>
  dplyr::left_join(policy_mdp_boot, by = "state", suffix = c("", "_boot")) |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    bases = state$bases,
    outs = state$outs,
    balls = state$balls,
    strikes = state$strikes,
    disengagements = state$disengagements
  ) |>
  dplyr::filter(bases == "100", !(first == 0 & balls == 0 & strikes == 0 & disengagements == 0)) |>
  dplyr::group_by(outs, balls, strikes, disengagements) |>
  dplyr::summarize(
    policy_runner = mean(policy_runner),
    se = sd(policy_runner_boot),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    count = glue::glue("{balls}-{strikes}"),
    policy = glue::glue("{sprintf('%.1f', policy_runner)} {{\\color{{gray}}$\\pm$ {sprintf('%.1f', se)}}}")
  ) |>
  dplyr::arrange(strikes, balls, outs, disengagements) |>
  dplyr::select(outs, count, disengagements, policy)

policy_mdp_se |>
  dplyr::filter(outs == 0) |>
  dplyr::select(count, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  sputil::write_latex_table(
    file = "output/tables/policy_by_count_mdp.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Optimal Lead by Prior Disengagements}",
    colnames = c("Count", "0", "1", "2"),
    align = "c|ccc",
    hline.after = c(0, 4, 8, 12)
  )


lead_increase_long <- tibble::as_tibble(policy_mdp) |>
  dplyr::left_join(policy_mdp_boot, by = "state", suffix = c("", "_boot")) |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    pre_bases = state$bases,
    pre_outs = state$outs,
    pre_balls = state$balls,
    pre_strikes = state$strikes,
    pre_disengagements = state$disengagements
  ) |>
  dplyr::filter(
    pre_bases == 100,
    !(first == 0 & pre_balls == 0 & pre_strikes == 0 & pre_disengagements == 0)
  ) |>
  dplyr::select(
    bag, pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements, policy_runner, policy_runner_boot
  ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("policy_runner")) |>
  dplyr::mutate(name = paste0(name, "_", pre_disengagements)) |>
  dplyr::select(bag, pre_bases, pre_outs, pre_balls, pre_strikes, name, value) |>
  tidyr::pivot_wider() |>
  dplyr::mutate(
    increase_runner_1 = policy_runner_1 - policy_runner_0,
    increase_runner_2 = policy_runner_2 - policy_runner_1,
    increase_runner_1_boot = policy_runner_boot_1 - policy_runner_boot_0,
    increase_runner_2_boot = policy_runner_boot_2 - policy_runner_boot_1
  ) |>
  dplyr::select(bag, pre_bases, pre_outs, pre_balls, pre_strikes, dplyr::starts_with("increase"))

lead_increase_by_state <- lead_increase_long |>
  dplyr::group_by(pre_bases, pre_outs, pre_balls, pre_strikes) |>
  dplyr::summarize(
    increase_runner_1 = mean(increase_runner_1),
    increase_runner_2 = mean(increase_runner_2),
    se_1 = sd(increase_runner_1_boot),
    se_2 = sd(increase_runner_2_boot),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    increase_1 = glue::glue(
      "{sprintf('%.1f', increase_runner_1)} {{\\color{{gray}}$\\pm$ {sprintf('%.1f', se_1)}}}"
    ),
    increase_1 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, NA, increase_1),
    increase_2 = glue::glue(
      "{sprintf('%.1f', increase_runner_2)} {{\\color{{gray}}$\\pm$ {sprintf('%.1f', se_2)}}}"
    ),
    increase_2 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, NA, increase_2)
  )

lead_increase_by_state |>
  dplyr::select(pre_outs, pre_balls, pre_strikes, increase_1, increase_2) |>
  # Group same-strike counts together visually
  dplyr::arrange(pre_strikes, pre_balls, pre_outs) |>
  dplyr::mutate(count = glue::glue("{pre_balls}-{pre_strikes}")) |>
  tidyr::pivot_longer(cols = c(increase_1, increase_2)) |>
  dplyr::arrange(name) |>
  dplyr::mutate(name = paste(name, pre_outs, sep = "_")) |>
  dplyr::select(count, name, value) |>
  tidyr::pivot_wider() |>
  sputil::write_latex_table(
    file = "output/tables/policy_increase_mdp.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Increase after 1$^\\text{st}$ Disengagement} & \\multicolumn{3}{c}{Increase after 2$^\\text{nd}$ Disengagement}",
    colnames = c("Count", "0 Outs", "1 Out", "2 Outs", "0 Outs", "1 Out", "2 Outs"),
    align = "c|ccc|ccc",
    hline.after = c(0, 4, 8, 12)
  )


# What is the average recommended increase in lead distance, weighted by frequency of
# (pre_outs, pre_balls, pre_strikes, pre_disengagements) in which unsuccessful pickoffs occur?

state_frequency <- data_glmm |>
  dplyr::filter(is_pickoff_attempt, !is_pickoff_success) |>
  dplyr::count(pre_outs, pre_balls, pre_strikes, pre_disengagements)

lead_increase_long |>
  tidyr::pivot_longer(cols = dplyr::starts_with("increase_runner")) |>
  dplyr::mutate(
    pre_disengagements = as.integer(substring(name, 17, 17)) - 1,
    name = paste0("increase_runner", substring(name, 18))
  ) |>
  tidyr::pivot_wider() |>
  dplyr::left_join(
    y = state_frequency,
    by = c("pre_outs", "pre_balls", "pre_strikes", "pre_disengagements")
  ) |>
  dplyr::filter(pre_outs < 2 | pre_balls < 3 | pre_strikes < 2) |>  # no SB allowed in these states
  dplyr::group_by(bag, pre_disengagements) |>
  dplyr::summarize(
    increase_runner = weighted.mean(increase_runner, w = n),
    increase_runner_boot = weighted.mean(increase_runner_boot, w = n),
    .groups = "drop"
  ) |>
  dplyr::group_by(pre_disengagements) |>
  dplyr::summarize(
    increase_runner = mean(increase_runner),
    se = sd(increase_runner_boot),
    .groups = "drop"
  )




policy_mdp_skill_se <- tibble::as_tibble(policy_mdp_skill) |>
  dplyr::left_join(
    y = policy_mdp_skill_boot,
    by = c("state", "pct_runner", "pct_battery"),
    suffix = c("", "_boot")
  ) |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    bases = state$bases,
    outs = state$outs,
    balls = state$balls,
    strikes = state$strikes,
    disengagements = state$disengagements
  ) |>
  dplyr::filter(
    bases == "100",
    !(first == 0 & balls == 0 & strikes == 0 & disengagements == 0),
    outs == 0,
    balls == 0,
    strikes == 0
  ) |>
  dplyr::group_by(pct_runner, pct_battery, disengagements) |>
  dplyr::summarize(
    policy_runner = mean(policy_runner),
    se = sd(policy_runner_boot),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    policy = glue::glue("{sprintf('%.1f', policy_runner)} {{\\color{{gray}}$\\pm$ {sprintf('%.1f', se)}}}")
  ) |>
  dplyr::select(pct_runner, pct_battery, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  dplyr::arrange(-pct_runner, pct_battery)

increase_mdp_skill_se <- tibble::as_tibble(policy_mdp_skill) |>
  dplyr::left_join(
    y = policy_mdp_skill_boot,
    by = c("state", "pct_runner", "pct_battery"),
    suffix = c("", "_boot")
  ) |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    pre_bases = state$bases,
    pre_outs = state$outs,
    pre_balls = state$balls,
    pre_strikes = state$strikes,
    pre_disengagements = state$disengagements
  ) |>
  dplyr::filter(
    pre_bases == 100,
    !(first == 0 & pre_balls == 0 & pre_strikes == 0 & pre_disengagements == 0),
    pre_outs < 2 | pre_balls < 3 | pre_strikes < 2    # no SB allowed in these states
  ) |>
  dplyr::select(
    pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements,
    bag, pct_runner, pct_battery, policy_runner, policy_runner_boot
  ) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("policy_runner")) |>
  dplyr::mutate(name = paste0(name, "_", pre_disengagements)) |>
  dplyr::select(bag, pre_bases, pre_outs, pre_balls, pre_strikes, pct_runner, pct_battery, name, value) |>
  tidyr::pivot_wider() |>
  dplyr::mutate(
    increase_runner_1 = policy_runner_1 - policy_runner_0,
    increase_runner_2 = policy_runner_2 - policy_runner_1,
    increase_runner_1_boot = policy_runner_boot_1 - policy_runner_boot_0,
    increase_runner_2_boot = policy_runner_boot_2 - policy_runner_boot_1
  ) |>
  dplyr::select(bag, pre_bases, pre_outs, pre_balls, pre_strikes, pct_runner, pct_battery, dplyr::starts_with("increase")) |>
  tidyr::pivot_longer(cols = dplyr::starts_with("increase_runner")) |>
  dplyr::mutate(
    pre_disengagements = as.integer(substring(name, 17, 17)) - 1,
    name = paste0("increase_runner", substring(name, 18))
  ) |>
  tidyr::pivot_wider() |>
  dplyr::left_join(
    y = state_frequency,
    by = c("pre_outs", "pre_balls", "pre_strikes", "pre_disengagements")
  ) |>
  dplyr::filter(pre_outs < 2 | pre_balls < 3 | pre_strikes < 2) |>  # no SB allowed in these states
  dplyr::group_by(bag, pct_runner, pct_battery, pre_disengagements) |>
  dplyr::summarize(
    increase_runner = weighted.mean(increase_runner, w = n),
    increase_runner_boot = weighted.mean(increase_runner_boot, w = n),
    .groups = "drop"
  ) |>
  dplyr::group_by(pct_runner, pct_battery, pre_disengagements) |>
  dplyr::summarize(
    increase_runner = mean(increase_runner),
    se = sd(increase_runner_boot),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    increase = glue::glue(
      "{sprintf('%.1f', increase_runner)} {{\\color{{gray}}$\\pm$ {sprintf('%.1f', se)}}}"
    )
  ) |>
  dplyr::select(pct_runner, pct_battery, pre_disengagements, increase) |>
  tidyr::pivot_wider(names_from = pre_disengagements, values_from = increase)

policy_mdp_skill_se |>
  dplyr::left_join(increase_mdp_skill_se, by = c("pct_runner", "pct_battery")) |>
  dplyr::mutate(
    pct_runner = glue::glue("{100 * pct_runner}$^\\text{{th}}$"),
    pct_battery = glue::glue("{100 * pct_battery}$^\\text{{th}}$")
  ) |>
  sputil::write_latex_table(
    file = "output/tables/policy_increase_by_skill_mdp.tex",
    prefix_rows = "\\multicolumn{2}{c|}{Skill Percentile} & \\multicolumn{3}{c|}{Lead by Disengagements (0 outs, 0-0 count)} & \\multicolumn{2}{c}{Mean Increase (all outs/counts)}",
    colnames = c(
      "Runner", "Battery", "0", "1", "2", "after 1$^\\text{{st}}$ Dis.", "after 2$^\\text{{nd}}$ Dis."
    ),
    align = "cc|ccc|cc",
    hline.after = c(0, 3, 6)
  )


example <- tibble::tibble(
  year = factor(2023, levels = 2022:2023),
  pre_outs = factor(0, levels = 0:2),
  pre_balls = factor(3, levels = 0:3),
  pre_strikes = factor(2, levels = 0:2),
  pre_disengagements = factor(2, levels = 0:2),
  lead_distance_centered = 14.4 - 10,
  sprint_speed_centered = 0,
  arm_strength_centered = 0,
  pitcher_id = "-1",
  runner_id = "-1",
  catcher_id = "-1"
)

# Explain why a 14.4-foot lead with a 3-2 count and 2 prior disengagements is not ridiculous
predict_runner_outcome_component(
  object = fit_runner_outcome$pickoff_success,
  newdata = example,
  include_ranef = TRUE,
  allow_new_levels = TRUE
)
predict_runner_outcome_component(
  object = fit_runner_outcome$stolen_base,
  newdata = example,
  include_ranef = TRUE,
  allow_new_levels = TRUE
)



policy_mdp_boot |>
  dplyr::rename(value_mdp = value) |>
  dplyr::left_join(dplyr::rename(policy_mrp_boot, value_mrp = value), by = c("bag", "state")) |>
  dplyr::left_join(dplyr::rename(policy_zsg_boot, value_zsg = value), by = c("bag", "state")) |>
  dplyr::filter(state == "000_0_00_0_1") |>
  dplyr::summarize(
    mean_value_mdp = mean(value_mdp),
    mean_value_mrp = mean(value_mrp),
    mean_value_zsg = mean(value_zsg),
    mean_diff = 9 * 162 * mean(value_mdp - value_mrp),
    sd_diff = 9 * 162 * sd(value_mdp - value_mrp),
    .groups = "drop"
  )

