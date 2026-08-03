# blair_cooper_coppock_humphreys_2019/maintained/figure_2_bs_sampling_distributions.R
# Output: output/figure_2_bs_sampling_distributions.pdf,
#         output/figure_2_bs_sampling_distributions.png
# Depends on: helpers.R, table_a19_bs_diagnosis.R output
# Description: Figure 2 of the article, the sampling distributions of the two
#   Bjorkman and Svensson estimators. The values plotted are read back from the
#   CSV that table_a19_bs_diagnosis.R writes, so the figure and the appendix
#   section 4.5 table come from one diagnosis rather than two.

source(here::here("maintained", "helpers.R"))

gg_df <- read_csv(out_path("figure_2_bs_sampling_distributions.csv"), show_col_types = FALSE)

summary_df <- gg_df |>
  pivot_longer(c(estimate, estimand), names_to = "key", values_to = "value") |>
  group_by(inquiry, key) |>
  summarize(est = mean(value, na.rm = TRUE), .groups = "drop") |>
  mutate(label = paste0("Mean ", key))

g <- ggplot(gg_df, aes(estimate)) +
  geom_histogram(bins = 50, alpha = .5) +
  geom_vline(xintercept = 0, colour = "black", linewidth = .3) +
  geom_vline(data = summary_df, aes(xintercept = est, linetype = label), linewidth = 0.8) +
  geom_text(
    data = summary_df, aes(x = est, y = Inf, label = label),
    angle = 90, hjust = 1.1, vjust = -0.4, size = 3
  ) +
  scale_linetype_manual(values = c("dotted", "dashed")) +
  facet_wrap(~inquiry, scales = "free_x") +
  labs(x = "Sampling Distributions of the Two Estimators", y = "Frequency") +
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

ggsave(out_path("figure_2_bs_sampling_distributions.pdf"), plot = g, width = 9, height = 3)
ggsave(out_path("figure_2_bs_sampling_distributions.png"), plot = g, width = 9, height = 3, dpi = 300)

print(summary_df)
