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

# The deposit's three-arm designer, DesignLibrary::multi_arm_designer(), assembles
# its estimates with rbind.data.frame(). The declared package, which QCA loads,
# replaces base::rbind.data.frame() with a version that errors on any list argument
# that is not a data frame, and the replacement survives for the rest of the
# session. The declaration below assembles them with bind_rows() instead, but the
# figure still runs before the QCA appendix table and asserts it, so that nothing
# else in the diagnosis meets the replacement unnoticed.
stopifnot(!isNamespaceLoaded("declared"))

set.seed(published_seed)

# The two designs are declared here rather than taken from DesignLibrary's
# multi_arm_designer() and two_by_two_designer(). The DesignLibrary on CRAN
# (0.1.10) still builds them with declare_potential_outcomes(), which DeclareDesign
# 2.0.0 made defunct, and the 2.0.0 rewrite that ports them is not yet released.
# The declarations are DesignLibrary 2.0.0's multiarm_trial and two_by_two at the
# arguments the deposit passes, and at the same seed they give a diagnosands
# file byte-identical to the designers'. The three-arm design is passed through
# redesign() with its own values so that the diagnosands carry the same parameter
# columns the designer's make_design() recorded.
three_arm_design <- local({
  N <- 500
  m_arms <- 3L
  outcome_means <- c(0, 0.2, 0.2)
  outcome_sds <- c(0, 0, 0)
  conditions <- 1:3
  sd_i <- 1
  design <-
    declare_model(N = N, u = rnorm(N) * sd_i) +
    declare_inquiry(
      handler = function(data, m_arms, outcome_means) {
        pairs <- combn(m_arms, 2)
        data.frame(
          inquiry = paste0("ate_Y_", pairs[2, ], "_", pairs[1, ]),
          estimand = outcome_means[pairs[2, ]] - outcome_means[pairs[1, ]]
        )
      },
      m_arms = m_arms,
      outcome_means = outcome_means
    ) +
    declare_assignment(Z = complete_ra(N, conditions = conditions)) +
    declare_measurement(Y = outcome_means[Z] + u + rnorm(N, 0, outcome_sds[Z])) +
    declare_estimator(
      handler = function(data, m_arms, conditions) {
        pairs <- combn(m_arms, 2)
        map(seq_len(ncol(pairs)), \(i) {
          k <- pairs[1, i]
          j <- pairs[2, i]
          cbind(
            estimator = paste0("DIM (Z_", j, " - Z_", k, ")"),
            tidy(difference_in_means(Y ~ Z, data = data,
                                     condition1 = conditions[k], condition2 = conditions[j])),
            inquiry = paste0("ate_Y_", j, "_", k)
          )
        }) |>
          bind_rows()
      },
      m_arms = m_arms,
      conditions = conditions
    )
  redesign(design, N = N, m_arms = m_arms, outcome_means = list(outcome_means),
           outcome_sds = list(outcome_sds), conditions = list(conditions), sd_i = sd_i)
})

# A two by two factorial: A assigned with probability one half, then B with
# probability one half within each arm of A, so each of the four cells has
# probability 1/4. Cell means are in the order AB = 00, 01, 10, 11, and the
# weights of zero make each main effect the effect at the other factor's zero.
two_by_two_design <-
  declare_parameters(N = 500, weight_A = 0, weight_B = 0, outcome_means = c(0, 0, 0, 0)) +
  declare_model(
    N = N,
    u = rnorm(N, sd = 1),
    potential_outcomes(
      Y ~ outcome_means[1 + B + 2 * A] + u + rnorm(N, sd = 0),
      conditions = list(A = c(0, 1), B = c(0, 1))
    )
  ) +
  declare_inquiry(
    ate_A = weight_B * mean(Y_A_1_B_1 - Y_A_0_B_1) + (1 - weight_B) * mean(Y_A_1_B_0 - Y_A_0_B_0),
    ate_B = weight_A * mean(Y_A_1_B_1 - Y_A_1_B_0) + (1 - weight_A) * mean(Y_A_0_B_1 - Y_A_0_B_0),
    interaction = mean((Y_A_1_B_1 - Y_A_1_B_0) - (Y_A_0_B_1 - Y_A_0_B_0))
  ) +
  declare_assignment(A = complete_ra(N, prob = 0.5), B = block_ra(blocks = A, prob = 0.5)) +
  declare_measurement(Y = reveal_outcomes(Y ~ A + B)) +
  declare_estimator(Y ~ A + B, .method = lm_robust, term = c("A", "B"),
                    inquiry = c("ate_A", "ate_B"), label = "No_Interaction") +
  declare_estimator(Y ~ A + B + A:B, .method = lm_robust, term = "A:B",
                    inquiry = "interaction", label = "Interaction")

interactions <- seq(-0.2, 0.2, length.out = 9)

means <- map(interactions, \(x) c(0, 0.2, 0.2, 0.4 + x))

two_by_two_designs <- redesign(two_by_two_design, outcome_means = means)

diagnosis_three_arm <- diagnose_design(
  three_arm_design,
  sims = published_sims,
  bootstrap_sims = published_bootstrap_sims
)

# The nine factorial designs are diagnosed one at a time. DeclareDesign 2.0.0
# bootstraps the diagnosands of a multi-design diagnosis in one join, and at 5000
# simulations and 5000 bootstrap replicates over nine designs that join exhausts
# 48 Gb of memory. Each design alone peaks near 23 Gb and returns the same quantities.
diagnosands_two_by_two <- imap(
  two_by_two_designs,
  \(design, design_name) {
    diagnose_design(
      design,
      sims = published_sims,
      bootstrap_sims = published_bootstrap_sims
    ) |>
      get_diagnosands() |>
      as_tibble() |>
      mutate(design = design_name)
  }
) |>
  list_rbind()

diagnosands_all <- bind_rows(
  get_diagnosands(diagnosis_three_arm) |> as_tibble() |> mutate(design = "three_arm"),
  diagnosands_two_by_two
)

write_csv(diagnosands_all, out_path("figure_1_experimental_designs_diagnosands.csv"))

# The three-arm design does not involve an interaction, so its diagnosands are
# constant across the nine interaction sizes and are repeated to draw a flat line.
three_arm_results <- get_diagnosands(diagnosis_three_arm) |>
  as_tibble() |>
  filter(inquiry == "ate_Y_2_1") |>
  slice(rep(1, length(interactions))) |>
  mutate(interaction = interactions)

two_by_two_results <- diagnosands_two_by_two |>
  filter(inquiry %in% c("interaction", "ate_A")) |>
  left_join(
    tibble(design = names(two_by_two_designs), interaction = interactions),
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
