# blair_cooper_coppock_humphreys_2019/maintained/figure_1_experimental_designs.R
# Output: output/figure_1_experimental_designs.csv,
#         output/figure_1_experimental_designs.pdf,
#         output/figure_1_experimental_designs.png,
#         output/figure_1_experimental_designs_diagnosands.csv
# Depends on: helpers.R
# Description: Figure 1 of the article and the diagnosis behind it: bias, RMSE
#   and power for a three-arm design and for a two by two factorial design, as
#   the size of the interaction effect varies.

source(here::here("maintained", "helpers.R"))

# multi_arm_designer()'s estimator assembles its three difference in means into a
# data frame with rbind.data.frame(). The declared package, which QCA loads,
# replaces base::rbind.data.frame() with a version that errors on any list
# argument that is not a data frame, and the replacement survives for the rest of
# the session. Nothing here loads QCA, and the appendix table that does is run
# after this script, but the dependence would otherwise be invisible, so it is
# asserted rather than left to the order of run_all.R.
stopifnot(!isNamespaceLoaded("declared"))

set.seed(published_seed)

three_arm_design <- multi_arm_designer(N = 500, m_arms = 3, outcome_means = c(0, 0.2, 0.2))

interactions <- seq(-0.2, 0.2, length.out = 9)

means <- map(interactions, \(x) c(0, 0.2, 0.2, 0.4 + x))

two_by_two_designs <- expand_design(
  designer = two_by_two_designer,
  prefix = "factorial",
  N = 500,
  weight_A = 0,
  weight_B = 0,
  outcome_means = means
)

diagnosis_three_arm <- diagnose_design(
  three_arm_design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

diagnosis_two_by_two <- diagnose_design(
  two_by_two_designs,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

diagnosands_all <- bind_rows(
  get_diagnosands(diagnosis_three_arm) |> as_tibble() |> mutate(design = "three_arm"),
  get_diagnosands(diagnosis_two_by_two) |> as_tibble()
)

write_csv(diagnosands_all, out_path("figure_1_experimental_designs_diagnosands.csv"))

# The three-arm design does not involve an interaction, so its diagnosands are
# constant across the nine interaction sizes and are repeated to draw a flat line.
three_arm_results <- get_diagnosands(diagnosis_three_arm) |>
  as_tibble() |>
  filter(inquiry == "ate_Y_2_1") |>
  slice(rep(1, length(interactions))) |>
  mutate(interaction = interactions)

two_by_two_results <- get_diagnosands(diagnosis_two_by_two) |>
  as_tibble() |>
  filter(inquiry %in% c("interaction", "ate_A")) |>
  left_join(
    tibble(design = paste0("factorial_", seq_along(interactions)), interaction = interactions),
    by = "design"
  )

gg_df <- bind_rows(three_arm_results, two_by_two_results) |>
  pivot_longer(c(bias, rmse, power), names_to = "diagnosand", values_to = "value") |>
  mutate(
    diagnosand = factor(diagnosand, levels = c("bias", "rmse", "power"),
                        labels = c("Bias", "RMSE", "Power")),
    design_type = factor(
      estimator,
      levels = c("DIM (Z_2 - Z_1)", "No_Interaction", "Interaction"),
      labels = c("Three-arm Design",
                 "2x2 Factorial Design (Main Effect)",
                 "2x2 Factorial Design (Interaction Effect)")
    )
  ) |>
  select(design, design_type, inquiry, estimator, interaction, diagnosand, value)

write_csv(gg_df, out_path("figure_1_experimental_designs.csv"))

labels_df <- gg_df |>
  filter(diagnosand == "Power") |>
  group_by(design_type) |>
  slice_max(interaction, n = 1) |>
  ungroup()

g <- ggplot(gg_df, aes(x = interaction, y = value, group = design_type,
                       colour = design_type, linetype = design_type, shape = design_type)) +
  geom_hline(yintercept = 0, linewidth = 0) +
  geom_point(size = 3) +
  stat_smooth(se = FALSE) +
  geom_text(
    data = labels_df, aes(label = design_type), hjust = 1, vjust = -1.2, size = 3,
    show.legend = FALSE
  ) +
  scale_colour_grey() +
  facet_wrap(~diagnosand, scales = "free") +
  labs(x = "Interaction Effect Size", y = NULL) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "#eeeeee"),
    strip.background = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12)
  )

ggsave(out_path("figure_1_experimental_designs.pdf"), plot = g, width = 9, height = 3.4)
ggsave(out_path("figure_1_experimental_designs.png"), plot = g, width = 9, height = 3.4, dpi = 300)

print(gg_df |> filter(diagnosand == "Power") |> select(design_type, interaction, value))
