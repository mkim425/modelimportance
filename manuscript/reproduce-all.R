# Standalone replication script for the JSS manuscript (package vignette).
#
# This script sequentially sources all replication scripts required to
# reproduce the figures, tables, and analyses presented in the manuscript.

# install.packages("modelimportance")
library(modelimportance)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)

##### Table 1 #####
forecast_data_example |>
        dplyr::filter(
                .data$reference_date == "2022-12-17",
                .data$horizon != 0
        ) |>
        head(10)

##### Figure 1 #####
forecast_data_example |>
        filter(
                reference_date == "2022-12-17",
                horizon != 0,
                output_type_id %in% c(0.05, 0.5, 0.95)
        ) |>
        pivot_wider(names_from = output_type_id, values_from = value) |>
        rename(lower = "0.05", upper = "0.95", value = "0.5") |>
        ggplot(aes(x = target_end_date)) +
        facet_grid(~model_id) +
        geom_point(aes(y = value, color = "medians"), size = 2) +
        geom_line(aes(y = value, color = "medians"), linewidth = 1) +
        geom_ribbon(
                aes(
                        ymin = lower, ymax = upper,
                        fill = "#3388FF"
                ),
                alpha = 0.5
        ) +
        geom_point(
                data = target_data_example |> filter(target_end_date <= "2022-12-17"),
                aes(y = observation, group = 1, color = "obs")
        ) +
        geom_line(
                data = target_data_example |> filter(target_end_date <= "2022-12-17"),
                aes(y = observation, group = 1, color = "obs")
        ) +
        geom_point(
                data = target_data_example |> filter(target_end_date > "2022-12-17"),
                aes(y = observation, group = 1, color = "truth"),
                shape = 1, alpha = 1
        ) +
        geom_line(
                data = target_data_example |> filter(target_end_date > "2022-12-17"),
                aes(y = observation, group = 1, color = "truth"),
                alpha = 0.75
        ) +
        # coord_cartesian(ylim = c(0, 500)) +
        scale_x_date(breaks = target_data_example$target_end_date, date_labels = "%Y-%m-%d") +
        labs(
                y = "Weekly Hospitalization",
                x = "Date"
        ) +
        scale_color_manual(
                name = "",
                values = c(
                        "medians" = "DodgerBlue",
                        "obs" = "Black",
                        "truth" = "Black"
                ),
                labels = c(
                        "Forecast (Predictive median)",
                        "Observed data before forecasting",
                        "Eventually observed value"
                )
        ) +
        scale_fill_manual("",
                          values = "#3388FF",
                          labels = "90% Prediction interval"
        ) +
        theme(
                axis.title.x = element_text(size = 8),
                axis.title.y = element_text(size = 8),
                axis.text.x = element_text(size = 6, angle = 90),
                axis.text.y = element_text(size = 8),
                strip.text = element_text(size = 8),
                legend.title = element_blank(),
                legend.text = element_text(size = 8),
                legend.position = "bottom",
                legend.direction = "vertical",
                legend.box = "horizontal"
        )

##### Figure 3 #####
data.frame(n = 2:10) |>
        mutate(
                w_eq = 1 / (2^(n - 1) - 1),
                w_perm_min = 1 / ((n - 1) * choose(n - 1, floor((n - 1) / 2))),
                w_perm_max = 1 / (n - 1)
        ) |>
        ggplot(aes(x = n)) +
        geom_line(aes(y = w_eq, color = "eq"), size = 0.75, linetype = "longdash") +
        geom_point(aes(y = w_eq, color = "eq"), size = 2, shape = 15) +
        geom_line(aes(y = w_perm_min, color = "perm_min"), size = 0.75) +
        geom_point(aes(y = w_perm_min, color = "perm_min"), size = 2, shape = 16) +
        geom_line(aes(y = w_perm_max, color = "perm_max"), size = 0.75) +
        geom_point(aes(y = w_perm_max, color = "perm_max"), size = 2, shape = 17) +
        scale_color_manual(
                name = "Weights",
                values = c(
                        "eq" = "#F8766D",
                        "perm_min" = "#619CFF",
                        "perm_max" = "#00BA38"
                ),
                labels = c(
                        eq = expression(w^{
                                eq
                        }),
                        perm_min = expression(w^{
                                perm - min
                        }),
                        perm_max = expression(w^{
                                perm - max
                        })
                )
        ) +
        scale_x_continuous(breaks = 2:10) +
        labs(
                x = "Number of models (n)",
                y = "Weight assigned to a subset",
                color = "Weighting Scheme"
        ) +
        theme(
                axis.title.x = element_text(size = 9),
                axis.title.y = element_text(size = 9),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8),
                legend.title = element_text(size = 9),
                legend.position = "bottom",
                legend.text = element_text(size = 9),
                legend.spacing.x = unit(0.5, "mm"),
                legend.key.width = unit(1, "cm")
        )

##### Example data and Figure 4 #####
# Source: get-vignette-example-data.R

forecast_to_remove <- data.frame(
        model_id = c("MOBS-GLEAM_FLUH", "PSI-DICE"),
        location = c("25", "48"),
        target_end_date = as.Date(c("2022-11-26", "2022-12-10"))
)

# Filter out the specified forecasts from the original data
forecast_data <- forecast_data_raw |>
        anti_join(
                forecast_to_remove,
                by = c("model_id", "location", "target_end_date")
        )

target_data <- target_data_raw |>
        dplyr::filter(
                target_end_date %in% unique(forecast_data$target_end_date),
                location %in% unique(forecast_data$location),
                target == "wk inc flu hosp"
        )

forecast_data |>
        knitr::kable(format = "latex", booktabs = TRUE) |>
        kable_styling(latex_options = c("scale_down", "hold_position"))

print(target_data, width = Inf)

target_data |>
        mutate(model_id = "Observed") |>
        rename(value = oracle_value) |>
        rbind(forecast_data |> select("target_end_date", "target", "location", "value", "model_id")) |>
        ggplot(aes(x = target_end_date)) +
        geom_point(aes(y = value, color = model_id, shape = model_id), size = 2) +
        geom_point(
                data = target_data,
                aes(y = oracle_value, color = "Observed", shape = "Observed"),
                size = 3
        ) +
        facet_wrap(~location,
                   scales = "free_y",
                   labeller = labeller(location = function(x) paste0("Location: ", x))
        ) +
        scale_x_date(
                breaks = target_data$target_end_date,
                date_labels = "%Y-%m-%d",
                expand = expansion(add = c(5, 5))
        ) +
        scale_color_manual(
                name = "model_id",
                values = c(
                        "Flusight-baseline" = "#F8766D",
                        "MOBS-GLEAM_FLUH" = "#00BA38",
                        "PSI-DICE" = "#619CFF",
                        "Observed" = "black"
                ),
                limits = c(
                        "Flusight-baseline",
                        "MOBS-GLEAM_FLUH",
                        "PSI-DICE",
                        "Observed"
                )
        ) +
        scale_shape_manual(
                name = "model_id",
                values = c(
                        "Flusight-baseline" = 16,
                        "MOBS-GLEAM_FLUH" = 17,
                        "PSI-DICE" = 15,
                        "Observed" = 18
                ),
                limits = c(
                        "Flusight-baseline",
                        "MOBS-GLEAM_FLUH",
                        "PSI-DICE",
                        "Observed"
                )
        ) +
        labs(
                y = "Weekly Hospitalization",
                x = "Date"
        ) +
        theme(
                axis.title.x = element_text(size = 9),
                axis.title.y = element_text(size = 9),
                axis.text.x = element_text(size = 9),
                axis.text.y = element_text(size = 9),
                strip.text = element_text(size = 9),
                legend.title = element_blank(),
                legend.text = element_text(size = 8),
                legend.position = "bottom",
                legend.box = "horizontal",
                legend.spacing.x = unit(0.25, "mm"),
                legend.key.width = unit(0.4, "cm")
        )

##### Results in Section 6.2 Evaluation using LOMO algorithm #####
scores_lomo <- model_importance(
        forecast_data = forecast_data,
        oracle_output_data = target_data,
        ensemble_fun = "simple_ensemble",
        importance_algorithm = "lomo"
)

print(scores_lomo |>
              mutate(importance = round(importance, 2)) |>
              rename(ref_date = reference_date, h=horizon, loc=location,
                     t_end_date=target_end_date, o_type=output_type, imp=importance)
)

summary(scores_lomo)
s <- summary(scores_lomo)
s$all_tasks
s$model_summary
s$task_winners

ggplot(scores_lomo, aes(x = model_id, y = importance, fill = model_id)) +
        geom_col() +
        coord_flip() +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.25) +
        facet_grid(cols = vars(target, horizon, location, target_end_date),
                   scales = "free_x") +
        labs(
                x = "Model ID", y = "Importance Score",
                title = "Model Importance by Task"
        ) +
        scale_x_discrete(labels = function(x) gsub("[-_]", "-\n", x)) +
        theme(
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                panel.spacing.x = unit(0.5, "lines"),
                legend.position = "none"
        )

aggregate(scores_lomo, by = "model_id", na_action = "drop", fun = mean)
aggregate(scores_lomo, by = "model_id", na_action = "worst", fun = mean)
aggregate(scores_lomo, by = "model_id", na_action = "average", fun = mean)


### Results in Section 6.3 Evaluation using LASOMO algorithm
scores_lasomo_eq <- suppressMessages(model_importance(
        forecast_data = forecast_data,
        oracle_output_data = target_data,
        ensemble_fun = "simple_ensemble",
        importance_algorithm = "lasomo",
        subset_wt = "equal"
))
aggregate(scores_lasomo_eq, by = "model_id", na_action = "drop", fun = mean)

scores_lasomo_perm <- suppressMessages(model_importance(
        forecast_data = forecast_data,
        oracle_output_data = target_data,
        ensemble_fun = "simple_ensemble",
        importance_algorithm = "lasomo",
        subset_wt = "perm_based"
))
aggregate(scores_lasomo_perm, by = "model_id", na_action = "drop", fun = mean)

##### Figures in Section 7. Computational complexity #####
# Source: computational-complexity-plots.R
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbreak)
library(patchwork)

runtime_df <- read.csv("runtime_results.csv")
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

##### Figure 4 #####
plan_comparison <- ggarrange(
        p_lomo,
        p_lasomo,
        ncol = 2,
        legend = "bottom",
        common.legend = TRUE,
        labels = c("(A) LOMO", "(B) LASOMO")
)
##### Figure 5 #####
p_multiplan <- ggarrange(
        p_multi_lomo,
        p_multi_lasomo,
        ncol = 2,
        legend = "bottom",
        common.legend = TRUE,
        labels = c("(A) LOMO", "(B) LASOMO")
)







