rm(list = ls())

library(tidyverse)

df_parallel <- read.csv("data/simulation_result_parallel.csv")
df_nonparallel <- read.csv("data/simulation_result_nonparallel.csv")

df_parallel %>% 
  ggplot() +
  geom_density(aes(x = coef, fill = pretrend), alpha = 0.3) +
  geom_vline(xintercept = mean(filter(df_parallel, pretrend==TRUE)$coef), col = "blue") +
  geom_vline(xintercept = mean(df_parallel$coef), col = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Parallel trend", x = "Coefficient", y = "Frequency", fill = "Significant Pretrend") +
  theme_minimal() +
  theme(legend.position = "bottom")

df_nonparallel %>%
  ggplot() +
  geom_density(aes(x = coef, fill = pretrend), alpha = 0.3) +
  geom_vline(xintercept = mean(filter(df_nonparallel, pretrend==TRUE)$coef), col = "blue") +
  geom_vline(xintercept = mean(df_nonparallel$coef), col = "black") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Non-parallel trend", x = "Coefficient", y = "Frequency", fill = "Significant Pretrend") +
  theme_minimal() +
  theme(legend.position = "bottom")

df_nonparallel %>%
  ggplot() +
  geom_histogram(aes(x = diagCoef), bins = 30, col = "black", fill = "white") +
  geom_vline(xintercept = mean(df_nonparallel$diagCoef), color = "red") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = "Non-parallel trend", x = "Coefficient", y = "Frequency") +
  theme_minimal()

threshold <- 1

df_nonparallel %>% 
  mutate(
    row_number = row_number(),
    ymin = coef - threshold*se,
    ymax = coef + threshold*se
  ) %>% 
  filter(row_number <= 100) %>%
  ggplot() +
  geom_errorbar(aes(x = row_number, ymin = ymin, ymax = ymax)) +
  geom_point(aes(x = row_number, y = coef, col = pretrend)) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = paste("Coef under Non-parallel Trend power =", mean(df_nonparallel$pretrend)),
    subtitle = paste("Mean of Coef: Overall =", round(mean(df_nonparallel$coef), 3), ", Pretrend =", round(mean(filter(df_nonparallel, pretrend==TRUE)$coef), 3)),
    x = "Simulation", y = "Coefficient", col = "Significant Pretrend"
  ) +
  theme_minimal()

df_nonparallel %>%
  filter(between(id, 1, 100)) %>%
  select(id, pretrend, yMeanDiffN1, yMeanDiff0, yMeanDiff1) %>% 
  pivot_longer(cols = -c(id, pretrend), names_to = "time", values_to = "yMeanDiff") %>%
  mutate(
    time = case_when(
      time == "yMeanDiffN1" ~ -1,
      time == "yMeanDiff0" ~ 0,
      time == "yMeanDiff1" ~ 1
    )
  ) %>%
  ggplot() +
  geom_line(aes(x = time, y = yMeanDiff, group = id, col = pretrend), alpha = 0.3) +
  geom_point(aes(x = time, y = yMeanDiff, col = pretrend)) +
  labs(
    title = "Non-parallel trend",
    x = "Time", y = "Mean Difference", col = "Significant Pretrend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


rbind(
  df_nonparallel %>%
    select(pretrend, yMeanDiffN1, yMeanDiff0, yMeanDiff1) %>% 
    pivot_longer(cols = -pretrend, names_to = "time", values_to = "yMeanDiff") %>%
    mutate(
      time = case_when(
        time == "yMeanDiffN1" ~ -1,
        time == "yMeanDiff0" ~ 0,
        time == "yMeanDiff1" ~ 1
      )
    ) %>% 
    filter(pretrend == TRUE) %>%
    group_by(time) %>%
    summarise(
      yMeanDiff = mean(yMeanDiff),
      pretrend = "TRUE"
    ),
  df_nonparallel %>%
    select(pretrend, yMeanDiffN1, yMeanDiff0, yMeanDiff1) %>% 
    pivot_longer(cols = -pretrend, names_to = "time", values_to = "yMeanDiff") %>%
    mutate(
      time = case_when(
        time == "yMeanDiffN1" ~ -1,
        time == "yMeanDiff0" ~ 0,
        time == "yMeanDiff1" ~ 1
      )
    ) %>% 
    group_by(time) %>%
    summarise(
      yMeanDiff = mean(yMeanDiff),
      pretrend = "Overall"
    )
  ) %>% 
  ggplot() +
  geom_line(aes(x = time, y = yMeanDiff, group = pretrend, col = pretrend), alpha = 0.3) +
  geom_point(aes(x = time, y = yMeanDiff, col = pretrend)) +
  labs(
    title = "Non-parallel trend",
    x = "Time", y = "Mean Difference", col = "Significant Pretrend"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
