rm(list = ls())

library(tidyverse)
library(fixest)
set.seed(14)

N <- 100

treatment_ratio <- 0.5

beta <- 0
gamma <- 0.5
min_time <- -1
max_time <- 1
first_treat <- 1

threshold <- 1

gen_data <- function(N, treatment_ratio, beta, gamma){
  
  df <- tibble(
    id = rep(1:N, each = max_time - min_time + 1),
    time = rep(min_time:max_time, N),
    isTreated = rep(rbinom(N, 1, treatment_ratio), each = max_time - min_time + 1),
    firstTreat = rep(if_else(isTreated == 1, first_treat, 10000)),
    idFE = rep(rnorm(N), each = max_time - min_time + 1),
    timeFE = rep(rnorm(max_time - min_time + 1), N),
    e = rnorm(N*(max_time - min_time + 1))
  ) %>% 
    mutate(
      D = ifelse(time > 0 & isTreated == 1, 1, 0),
      y = idFE + timeFE + beta*D + gamma*time*isTreated + e
    )
  
  return(df)
}

M <- 1000

list_coef <- numeric(M)
list_se <- numeric(M)
list_diag_coef <- numeric(M)
list_diag_se <- numeric(M)
list_ymeandiff_n1 <- numeric(M)
list_ymeandiff_0 <- numeric(M)
list_ymeandiff_1 <- numeric(M)

for(i in 1:M){
  df <- gen_data(N, treatment_ratio, beta, gamma = 0)
  
  result <- feols(y ~ sunab(firstTreat, time) | id + time, data = df, vcov = "cluster")
  list_coef[i] <- result$coeftable[[2]]
  list_se[i] <- result$se[[2]]
  list_diag_coef[i] <- result$coeftable[[1]]
  list_diag_se[i] <- result$se[[1]]
  
  # result0 <- feols(
  #   y ~ D | id + time,
  #   data = df %>% 
  #     filter(time <= 0) %>% 
  #     mutate(D = ifelse(time == 0 & isTreated == 1, 1, 0)),
  #   vcov = "cluster"
  # )
  # result1 <- feols(
  #   y ~ D | id + time,
  #   data = df %>% filter(time >= 0),
  #   vcov = "cluster"
  # )
  # list_coef[i] <- result1$coeftable[[1]]
  # list_se[i] <- result1$se[[1]]
  # list_diag_coef[i] <- result0$coeftable[[1]]
  # list_diag_se[i] <- result0$se[[1]]
  
  list_ymeandiff_n1[i] <- mean(filter(df, time == -1, isTreated == 1)$y) - mean(filter(df, time == -1, isTreated == 0)$y)
  list_ymeandiff_0[i] <- mean(filter(df, time == 0, isTreated == 1)$y) - mean(filter(df, time == 0, isTreated == 0)$y)
  list_ymeandiff_1[i] <- mean(filter(df, time == 1, isTreated == 1)$y) - mean(filter(df, time == 1, isTreated == 0)$y)
}

result_parallel <- tibble(
  id = 1:M,
  coef = list_coef,
  se = list_se,
  diagCoef = list_diag_coef,
  diagSe = list_diag_se,
  yMeanDiffN1 = list_ymeandiff_n1,
  yMeanDiff0 = list_ymeandiff_0,
  yMeanDiff1 = list_ymeandiff_1
) %>% 
  mutate(
    t = coef/se,
    pvalue = 2*(1 - pt(abs(t), df = N - 2)),
    diagPvalue = 2*(1 - pt(abs(diagCoef/diagSe), df = N - 2)),
    pretrend = diagPvalue > 2*pnorm(-abs(threshold))
  ) %>% 
  select(id, coef, se, t, pvalue, diagCoef, diagSe, diagPvalue, pretrend, yMeanDiffN1, yMeanDiff0, yMeanDiff1)

for(i in 1:M){
  df <- gen_data(N, treatment_ratio, beta, gamma = gamma)
  result <- feols(y ~ sunab(firstTreat, time) | id + time, data = df, vcov = "cluster")
  list_coef[i] <- result$coeftable[[2]]
  list_se[i] <- result$se[[2]]
  list_diag_coef[i] <- result$coeftable[[1]]
  list_diag_se[i] <- result$se[[1]]
  list_ymeandiff_n1[i] <- mean(filter(df, time == -1, isTreated == 1)$y) - mean(filter(df, time == -1, isTreated == 0)$y)
  list_ymeandiff_0[i] <- mean(filter(df, time == 0, isTreated == 1)$y) - mean(filter(df, time == 0, isTreated == 0)$y)
  list_ymeandiff_1[i] <- mean(filter(df, time == 1, isTreated == 1)$y) - mean(filter(df, time == 1, isTreated == 0)$y)
}

result_nonparallel <- tibble(
  id = 1:M,
  coef = list_coef,
  se = list_se,
  diagCoef = list_diag_coef,
  diagSe = list_diag_se,
  yMeanDiffN1 = list_ymeandiff_n1,
  yMeanDiff0 = list_ymeandiff_0,
  yMeanDiff1 = list_ymeandiff_1
) %>% 
  mutate(
    t = coef/se,
    pvalue = 2*(1 - pt(abs(t), df = N - 2)),
    diagPvalue = 2*(1 - pt(abs(diagCoef/diagSe), df = N - 2)),
    pretrend = diagPvalue > 2*pnorm(-abs(threshold))
  ) %>% 
  select(id, coef, se, t, pvalue, diagCoef, diagSe, diagPvalue, pretrend, yMeanDiffN1, yMeanDiff0, yMeanDiff1)

write.csv(result_parallel, "data/simulation_result_parallel.csv", row.names = FALSE)
write.csv(result_nonparallel, "data/simulation_result_nonparallel.csv", row.names = FALSE)


