
fig_make <- TRUE
fig_mode <- "light"


# Read data ----

arm_strength <- data.table::fread("input/data/arm_strength/2023.csv") |>
  dplyr::select(player_id, player_name, arm_strength, sb_attempts)
sprint_speed <- data.table::fread("input/data/sprint_speed/2023.csv") |>
  dplyr::select(player_id, player_name = `last_name, first_name`, sprint_speed, competitive_runs)

data_glmer <- data.table::fread("output/data/data_glmer.csv")

policy_mdp <- data.table::fread("output/policy_mdp.csv")
policy_zsg <- data.table::fread("output/policy_zsg.csv")
policy_mdp_skill <- data.table::fread("output/policy_mdp_skill.csv")



# Read bootstrap results ----

policy_zsg_boot <- NULL
policy_mdp_boot <- NULL
policy_mdp_skill_boot <- NULL

result <- list()
for (file in list.files("output/bootstrap")) {
  bag <- as.integer(gsub("([0-9]+).*$", "\\1", file))
  result[[bag]] <- readRDS(file.path("output", "bootstrap", file))

  policy_zsg_boot <- result[[bag]]$policy_zsg |>
    dplyr::mutate(bag = bag, .before = 1) |>
    dplyr::bind_rows(policy_zsg_boot)

  policy_mdp_boot <- result[[bag]]$policy_mdp |>
    dplyr::mutate(bag = bag, .before = 1) |>
    dplyr::bind_rows(policy_mdp_boot)

  policy_mdp_skill_boot <- result[[bag]]$policy_mdp_skill |>
    dplyr::mutate(bag = bag, .before = 1) |>
    dplyr::bind_rows(policy_mdp_skill_boot)
}


# Plot data summary ----

breaks <- seq(from = 4, to = 16, by = 2)

if (fig_make) {

  sputil::open_device(glue::glue("output/figures/leads_overall_{fig_mode}.pdf"), height = 4, width = 7)
  plot <- data_glmer |>
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
  ranef <- tibble::tibble(
    type = "random",
    variable = names(glmmTMB::VarCorr(model)$cond),
    `Std. Error` = sapply(glmmTMB::VarCorr(model)$cond, function(x) {attr(x, "stddev")})
  )
  summary(model)$coefficients$cond |>
    tibble::as_tibble(rownames = "variable") |>
    dplyr::mutate(type = "fixed") |>
    dplyr::bind_rows(ranef) |>
    dplyr::mutate(
      estimate = paste(
        ifelse(type == "random", "", glue::glue("${sprintf('%.3f', Estimate)}$")),
        glue::glue("$\\pm{sprintf('%.3f', `Std. Error`)}$")
      )
    ) |>
    dplyr::select(type, variable, estimate)
}

table_po_attempt <- extract_model_fit(fit_runner_outcome$po_attempt) |>
  dplyr::rename(po_attempt = estimate)
table_po_success <- extract_model_fit(fit_runner_outcome$po_success) |>
  dplyr::rename(po_success = estimate)
table_sb_attempt <- extract_model_fit(fit_runner_outcome$sb_attempt) |>
  dplyr::rename(sb_attempt = estimate)
table_sb_success <- extract_model_fit(fit_runner_outcome$sb_success) |>
  dplyr::rename(sb_success = estimate)

table_po_attempt |>
  dplyr::full_join(table_po_success, by = c("type", "variable")) |>
  dplyr::full_join(table_sb_attempt, by = c("type", "variable")) |>
  dplyr::full_join(table_sb_success, by = c("type", "variable")) |>
  dplyr::arrange(type) |>
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "(Intercept)" ~ "{\\it Intercept}",
      variable == "lead1b" ~ "Lead Distance (ft)",
      variable == "pre_balls" ~ "Balls",
      variable == "pre_strikes" ~ "Strikes",
      variable == "pre_outs" ~ "Outs",
      variable == "year2023" ~ "Year (2023 vs. 2022)",
      variable == "pre_disengagements1" ~ "Disengagements (1 vs. 0)",
      variable == "pre_disengagements2" ~ "Disengagements (2 vs. 0)",
      variable == "sprint_speed" ~ "Runner Sprint Speed (ft/s)",
      variable == "arm_strength" ~ "Catcher Arm Strength (mi/h)",
      variable == "pitcher_id" ~ "Pitcher",
      variable == "fielder_2_id" ~ "Catcher",
      variable == "run1b" ~ "Runner",
    )
  ) |>
  tibble::add_row(variable = "{\\it Fixed Effects}", .after = 1) |>
  tibble::add_row(variable = "{\\it Random Effects}", .after = 11) |>
  dplyr::select(variable, po_attempt, po_success, sb_attempt, sb_success) |>
  sputil::write_latex_table(
    file = "output/tables/model_summary.tex",
    colnames = c("Variable", "PO Attempt", "PO Success", "SB Attempt", "SB Success"),
    prefix_rows = c("&\\multicolumn{4}{c}{Estimated Effect on Log-Odds}"),
    align = "l|rrrr",
    hline.after = c(0, 1, 11)
  )


# Plot runner outcome probabilities as functions of lead distance ----

covariate_baseline <- tibble::tibble(
  pre_outs = 0,
  pre_balls = 0,
  pre_strikes = 0,
  arm_strength_centered = 0
)
lead_distance_grid <- tibble::tibble(
  lead_distance = seq(from = 0, to = 20, by = 0.1),
  lead_distance_centered = lead_distance - 10
)

if (fig_make) {

  sputil::open_device(glue::glue("output/figures/prob_po_attempt_{fig_mode}.pdf"), height = 4, width = 4)
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
      prob_po_attempt = predict(
        object = fit_runner_outcome$po_attempt,
        newdata = covariate_grid,
        type = "response",
        re.form = NA
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_po_attempt, col = legend, linetype = legend)
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
    ggplot2::coord_cartesian(xlim = c(3, 16), ylim = c(0, 0.7)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.4, 0.7))
  print(plot)
  dev.off()
}


pitcher_grid <- glmmTMB::ranef(fit_runner_outcome$po_success)$cond$pitcher_id |>
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
    file = glue::glue("output/figures/prob_po_success_{fig_mode}.pdf"),
    height = 4,
    width = 4
  )
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(pitcher_grid) |>
    dplyr::mutate(
      year = "2023",
      pre_disengagements = "0"
    )
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_po_attempt = predict(
        object = fit_runner_outcome$po_success,
        newdata = covariate_grid,
        type = "response"
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_po_attempt, col = legend, linetype = legend)
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

sb_success_effect_runner <- glmmTMB::ranef(fit_runner_outcome$sb_success)$cond$runner_id |>
  tibble::rownames_to_column("player_id") |>
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(sprint_speed, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` +
      (
        glmmTMB::fixef(fit_runner_outcome$sb_success)$cond["sprint_speed_centered"] *
        (sprint_speed - mean(sprint_speed))
      )
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
    ggplot2::coord_cartesian(ylim = c(-0.8, 0.8)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

sb_success_effect_catcher <- glmmTMB::ranef(fit_runner_outcome$sb_success)$cond$catcher_id |>
  tibble::rownames_to_column("player_id") |>
  dplyr::mutate(player_id = as.integer(player_id)) |>
  dplyr::inner_join(arm_strength, by = "player_id") |>
  dplyr::mutate(
    effect = `(Intercept)` +
      (
        glmmTMB::fixef(fit_runner_outcome$sb_success)$cond["arm_strength_centered"] *
        (arm_strength - mean(arm_strength))
      )
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
    ggplot2::coord_cartesian(ylim = c(-0.8, 0.8)) +
    sputil::theme_sleek(mode = fig_mode)
  print(plot)
  dev.off()
}

runner_grid <- sb_success_effect_runner |>
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
    pitcher_id = 0,
    catcher_id = 0
  )

if (fig_make) {
  sputil::open_device(glue::glue("output/figures/prob_sb_success_{fig_mode}.pdf"), height = 4, width = 7)
  covariate_grid <- covariate_baseline |>
    dplyr::cross_join(lead_distance_grid) |>
    dplyr::cross_join(runner_grid)
  plot <- covariate_grid |>
    dplyr::mutate(
      prob_sb_success = predict(
        object = fit_runner_outcome$sb_success,
        newdata = covariate_grid,
        type = "response",
        allow.new.levels = TRUE
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(x = lead_distance, y = prob_sb_success, col = legend, linetype = legend)
    ) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = breaks) +
    ggplot2::scale_y_continuous(
      breaks = seq(from = 0.2, to = 1, by = 0.2),
      labels = glue::glue("{seq(from = 20, to = 100, by = 20)}%")
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
    ggplot2::coord_cartesian(xlim = c(3, 16)) +
    sputil::theme_sleek(mode = fig_mode) +
    ggplot2::theme(legend.position.inside = c(0.85, 0.25))
  print(plot)
  dev.off()
}


# Write results tables ----

policy_zsg_se <- policy_zsg_boot |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    bases = state$bases,
    outs = state$outs,
    balls = state$balls,
    strikes = state$strikes,
    disengagements = state$disengagements
  ) |>
  dplyr::filter(bases == "100", !(first == 0 & balls == 0 & strikes == 0)) |>
  dplyr::group_by(outs, balls, strikes, disengagements) |>
  dplyr::summarize(mean = mean(policy_runner), sd = sd(policy_runner), .groups = "drop") |>
  dplyr::mutate(
    count = glue::glue("{balls}-{strikes}"),
    policy = glue::glue("{sprintf('%.1f', mean)}' $\\pm$ {sprintf('%.1f', sd)}'")
  ) |>
  dplyr::arrange(strikes, balls, outs, disengagements) |>
  dplyr::select(outs, count, disengagements, policy)

policy_zsg_se |>
  dplyr::filter(outs == 0) |>
  dplyr::select(count, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  sputil::write_latex_table(
    file = "output/tables/policy_by_count_zsg.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Prior Disengagements}",
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
    prefix_rows = "& \\multicolumn{3}{c}{Prior Disengagements}",
    colnames = c("Outs", "0", "1", "2"),
    align = "c|ccc"
  )


policy_mdp_se <- policy_mdp_boot |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    bases = state$bases,
    outs = state$outs,
    balls = state$balls,
    strikes = state$strikes,
    disengagements = state$disengagements
  ) |>
  dplyr::filter(bases == "100", !(first == 0 & balls == 0 & strikes == 0)) |>
  dplyr::group_by(outs, balls, strikes, disengagements) |>
  dplyr::summarize(mean = mean(policy_runner), sd = sd(policy_runner), .groups = "drop") |>
  dplyr::mutate(
    count = glue::glue("{balls}-{strikes}"),
    policy = glue::glue("{sprintf('%.1f', mean)}' $\\pm$ {sprintf('%.1f', sd)}'")
  ) |>
  dplyr::arrange(strikes, balls, outs, disengagements) |>
  dplyr::select(outs, count, disengagements, policy)

policy_mdp_se |>
  dplyr::filter(outs == 0) |>
  dplyr::select(count, disengagements, policy) |>
  tidyr::pivot_wider(names_from = disengagements, values_from = policy) |>
  sputil::write_latex_table(
    file = "output/tables/policy_by_count_mdp.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Prior Disengagements}",
    colnames = c("Count", "0", "1", "2"),
    align = "c|ccc",
    hline.after = c(0, 4, 8, 12)
  )


lead_increase_long <- policy_mdp_boot |>
  dplyr::mutate(
    state = pickoffgame::deconstruct_state(state),
    first = state$first,
    pre_bases = state$bases,
    pre_outs = state$outs,
    pre_balls = state$balls,
    pre_strikes = state$strikes,
    pre_disengagements = state$disengagements
  ) |>
  dplyr::select(
    bag, first, pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements, policy_runner
  ) |>
  tidyr::pivot_wider(names_from = pre_disengagements, values_from = policy_runner) |>
  dplyr::mutate(`0` = 12 * (`1` - `0`), `1` = 12 * (`2` - `1`)) |>    # report inches
  dplyr::group_by(first, pre_bases, pre_outs, pre_balls, pre_strikes) |>
  dplyr::summarize(
    dplyr::across(.cols = c(`0`, `1`), .fns = c(mean = mean, sd = sd)),
    .groups = "drop"
  ) |>
  dplyr::filter(pre_bases == 100, !(first == 0 & pre_balls == 0 & pre_strikes == 0)) |>
  dplyr::mutate(
    increase_1 = glue::glue(
      "{sprintf('%.1f', `0_mean`)}'' $\\pm$ {sprintf('%.1f', `0_sd`)}''"
    ),
    increase_1 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, NA, increase_1),
    increase_2 = glue::glue(
      "{sprintf('%.1f', `1_mean`)}'' $\\pm$ {sprintf('%.1f', `1_sd`)}''"
    ),
    increase_2 = ifelse(pre_balls == 3 & pre_strikes == 2 & pre_outs == 2, NA, increase_2)
  )

lead_increase_long |>
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
    prefix_rows = "& \\multicolumn{3}{c}{After 1$^\\text{st}$ Disengagement} & \\multicolumn{3}{c}{After 2$^\\text{nd}$ Disengagement}",
    colnames = c("Count", "0 Outs", "1 Out", "2 Outs", "0 Outs", "1 Out", "2 Outs"),
    align = "c|ccc|ccc",
    hline.after = c(0, 4, 8, 12)
  )


# What is the average recommended increase in lead distance, weighted by frequency of
# (pre_outs, pre_balls, pre_strikes, pre_disengagements) in which unsuccessful pickoffs occur?

state_frequency <- data_glmer |>
  dplyr::filter(is_po_attempt, !is_po_success) |>
  dplyr::count(pre_outs, pre_balls, pre_strikes, pre_disengagements)

lead_increase_by_state <- lead_increase_long |>
  dplyr::select(pre_outs, pre_balls, pre_strikes, `0_mean`, `1_mean`) |>
  tidyr::pivot_longer(cols = dplyr::contains("_mean"), values_to = "lead_distance_increase") |>
  dplyr::mutate(pre_disengagements = as.integer(substring(name, 1, 1))) |>
  dplyr::left_join(
    y = state_frequency,
    by = c("pre_outs", "pre_balls", "pre_strikes", "pre_disengagements")
  ) |>
  dplyr::filter(pre_outs < 2 | pre_balls < 3 | pre_strikes < 2) |>  # no SB allowed in these states
  dplyr::summarize(lead_distance_increase = weighted.mean(lead_distance_increase, w = n))


policy_mdp_skill_boot |>
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
    !(first == 0 & pre_balls == 0 & pre_strikes == 0),
    pre_outs < 2 | pre_balls < 3 | pre_strikes < 2    # no SB allowed in these states
  ) |>
  dplyr::select(
    first, pre_bases, pre_outs, pre_balls, pre_strikes, pre_disengagements,
    bag, pct_runner, pct_battery, policy_runner
  ) |>
  tidyr::pivot_wider(names_from = pre_disengagements, values_from = policy_runner) |>
  dplyr::mutate(`0` = 12 * (`1` - `0`), `1` = 12 * (`2` - `1`)) |>    # report inches
  tidyr::pivot_longer(
    cols = c(`0`, `1`),
    names_to = "pre_disengagements",
    values_to = "increase"
  ) |>
  dplyr::mutate(pre_disengagements = as.integer(pre_disengagements)) |>
  dplyr::left_join(
    y = state_frequency,
    by = c("pre_outs", "pre_balls", "pre_strikes", "pre_disengagements")
  ) |>
  dplyr::group_by(bag, pct_runner, pct_battery) |>
  dplyr::summarize(increase = weighted.mean(increase, w = n), .groups = "drop") |>
  dplyr::group_by(pct_runner, pct_battery) |>
  dplyr::summarize(increase_mean = mean(increase), increase_sd = sd(increase), .groups = "drop") |>
  dplyr::mutate(
    pct_runner = glue::glue("{100 * pct_runner}$^\\text{{th}}$"),
    pct_battery = glue::glue("{100 * pct_battery}$^\\text{{th}}$"),
    increase = glue::glue("{sprintf('%.1f', increase_mean)}'' $\\pm$ {sprintf('%.1f', increase_sd)}''")
  ) |>
  dplyr::select(pct_runner, pct_battery, increase) |>
  tidyr::pivot_wider(names_from = pct_battery, values_from = increase) |>
  sputil::write_latex_table(
    file = "output/tables/policy_increase_by_skill_mdp.tex",
    prefix_rows = "& \\multicolumn{3}{c}{Battery Percentile}",
    colnames = c("Runner Percentile", "10$\\text{th}$", "50$\\text{th}$", "90$\\text{th}$"),
    align = "c|ccc",
    hline.after = 0
  )
