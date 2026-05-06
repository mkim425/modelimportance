# This script generates the computational feasibility plots for LOMO and LASOMO algorithms
# based on the runtime results stored in a CSV file.
# The plots were generated from the runtime results, by fitting a smooth curve or line
# to the observations (points).

library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbreak)
library(patchwork)
# load the results from the CSV file for further analysis
runtime_df <- read.csv("inst/for-vignettes/runtime_results.csv")

runtime_lomo <- runtime_df |>
  select(n_models, n_tasks, elapsed_lomo, plan) |>
  rename(elapsed = elapsed_lomo) |>
  mutate(algorithm = "lomo")

runtime_lasomo <- runtime_df |>
  select(n_models, n_tasks, elapsed_lasomo, plan) |>
  rename(elapsed = elapsed_lasomo) |>
  mutate(algorithm = "lasomo")

theme_p1 <- theme(
  legend.position = "bottom",
  legend.text = element_text(size = 14),
  legend.title = element_text(size = 15),
  legend.key.size = unit(1, "cm"),
  strip_text = element_text(size = 15),
  text = element_text(size = 14)
)

theme_p2 <- theme(
  panel.spacing = unit(1, "lines"),
  plot.margin = margin(10, 10, 10, 10),
  panel.clip = "off",
  strip_text = element_text(size = 15),
  text = element_text(size = 14)
)

# Plots
p_lomo <- runtime_lomo |>
  filter(n_models <= 10, n_tasks %in% c(10, 20, 50, 100)) |>
  ggplot(aes(x = n_models, y = elapsed, color = plan, shape = plan)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(aes(linetype = plan), method = "lm", se = FALSE, linewidth = 1) +
  facet_wrap(
    ~n_tasks,
    labeller = labeller(
      n_tasks = function(x) {
        ifelse(as.numeric(x) == 1, paste(x, "task"), paste(x, "tasks"))
      }
    )
  ) +
  scale_color_manual(
    values = c(
      "sequential" = "coral",
      "multisession" = "steelblue"
    )
  ) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = " ",
    x = "Number of Models",
    y = "Elapsed Time (seconds)"
  ) +
  theme_p1

p_lasomo <- runtime_lasomo |>
  filter(n_models <= 10, n_tasks %in% c(10, 20, 50, 100)) |>
  ggplot(aes(x = n_models, y = elapsed, color = plan, shape = plan)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(aes(linetype = plan), se = FALSE, linewidth = 1) +
  facet_wrap(
    ~n_tasks,
    labeller = labeller(
      n_tasks = function(x) {
        ifelse(as.numeric(x) == 1, paste(x, "task"), paste(x, "tasks"))
      }
    )
  ) +
  scale_color_manual(
    values = c(
      "sequential" = "coral",
      "multisession" = "steelblue"
    )
  ) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "",
    x = "Number of Models",
    y = "Elapsed Time (seconds)"
  ) +
  theme_p1

# Plots for multisession plan only with more combinations of models and tasks
p_multi_lomo <- runtime_lomo |>
  filter(plan == "multisession", n_tasks %in% c(5, 20, 50, 100)) |>
  ggplot(aes(x = n_models, y = elapsed)) +
  geom_point(color = "steelblue", alpha = 0.8) +
  geom_smooth(se = FALSE, linewidth = 1.2, color = "steelblue") +
  facet_wrap(
    ~n_tasks,
    labeller = labeller(n_tasks = function(x) paste(x, "tasks"))
  ) +
  labs(
    title = "", # "Elapsed Time for LOMO Algorithm by Number of Models and Tasks (Multisession Plan)",
    x = "Number of Models",
    y = "Elapsed Time (seconds)"
  ) +
  theme_p2

p_multi_lasomo <- runtime_lasomo |>
  filter(!is.na(elapsed), n_models <= 14) |>
  filter(plan == "multisession", n_tasks == 5) |>
  ggplot(aes(x = n_models, y = elapsed)) +
  geom_point(color = "steelblue", size = 1.85, alpha = 0.8) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_text(
    aes(label = round(elapsed, 2)),
    size = 3.75,
    vjust = -0.5,
    hjust = 1
  ) +
  facet_wrap(
    ~n_tasks,
    labeller = labeller(n_tasks = function(x) paste(x, "tasks"))
  ) +
  labs(
    title = "", # "Elapsed Time for LASOMO Algorithm by Number of Models and Tasks (Multisession Plan)",
    x = "Number of Models",
    y = "Elapsed Time (seconds)"
  ) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  theme_p2


plan_comparison <- ggarrange(
  p_lomo,
  p_lasomo,
  ncol = 2,
  legend = "bottom",
  common.legend = TRUE,
  labels = c("(A) LOMO", "(B) LASOMO")
)

ggsave(
  "inst/for-vignettes/runtime_comparison-fitted.png",
  plot = plan_comparison,
  width = 8,
  height = 6,
  dpi = 300
)

p_multiplan <- ggarrange(
  p_multi_lomo,
  p_multi_lasomo,
  ncol = 2,
  legend = "bottom",
  common.legend = TRUE,
  labels = c("(A) LOMO", "(B) LASOMO")
)

ggsave(
  "inst/for-vignettes/runtime_parallel-fitted.png",
  plot = p_multiplan,
  width = 8,
  height = 6,
  dpi = 300
)
