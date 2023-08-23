
## 4780 hw 5 fall 2023
# Bootstrapping

mpg <- read.csv("./data/mpg.csv")

## Tidymodel way
################
library(tidymodels)
library(tidyverse)
library(glue)

df_fit <- linear_reg() %>%
  set_engine("lm") %>%
  fit(y ~ x1, data = mpg)
intercept <- df_fit$fit$coefficients[1]
slope <- df_fit$fit$coefficients[2]

set.seed(2023)

df_boot_samples_200 <- mpg |>
  specify(y ~ x1) |>
  generate(reps = 200, type = "bootstrap")

p_df_boot_samples_100 <- ggplot(df_boot_samples_200,
                                aes(x = x1, y = y, group = replicate)) +
  geom_line(stat = "smooth", method = "lm", se = FALSE, alpha = 0.05) +
  labs(
    x = "Displacement",
    y = "MPG",
    title = glue::glue("Bootstrap samples 1 - 200")
  ) +
  scale_x_continuous(labels = label_number())
p_df_boot_samples_100 +
  geom_abline(intercept = intercept, slope = slope, color = "red", size = 2)


df_boot_samples_200_fit <- df_boot_samples_200 |>
  fit()

lower <- df_boot_samples_200_fit %>%
  ungroup() %>%
  filter(term == "x1") %>%
  summarise(quantile(estimate, 0.025)) %>%
  pull()

upper <- df_boot_samples_200_fit %>%
  ungroup() %>%
  filter(term == "x1") %>%
  summarise(quantile(estimate, 0.975)) %>%
  pull()

df_boot_samples_200_hist <- ggplot(df_boot_samples_200_fit %>% filter(term == "x1"),
                                   aes(x = estimate)) +
  geom_histogram(binwidth = 0.005, color = "white") +
  geom_vline(xintercept = slope, color = "red", size = 1) +
  labs(x = "Slope", y = "Count",
       title = "Slopes of 100 bootstrap samples")

df_boot_samples_200_hist +
  geom_vline(xintercept = lower, color = "#66CDAA", size = 2, linetype = "dashed") +
  geom_vline(xintercept = upper, color = "#66CDAA", size = 2, linetype = "dashed")


observed_fit <- mpg |>
  specify(y ~ x1) |>
  fit()

df_boot_samples_200_fit |> get_ci(point_estimate = observed_fit, type = "percentile")

df_boot_samples_200_fit |> get_ci(point_estimate = observed_fit, type = "se") ## t quantiles

df_boot_samples_200_fit |> get_ci(point_estimate = observed_fit, type = "bias-corrected")


## CAR way
################
library(car)
lm_fit <- lm(y ~ x1, data = mpg)
set.seed(2023)
car_boot <- car::Boot(lm_fit, R = 200)
car::brief(car_boot$t)
summary(car_boot)



# plot function is used to plot
# the data type with "n" is used
# to remove the plotted data
plot(1, type = "n", xlab = "", ylab = "",
     xlim = range(mpg$x1),
     ylim = range(mpg$y))
for(i in 1:200) {
  abline(a = car_boot$t[i, 1], b = car_boot$t[i, 2], lwd = 0.5)
}
abline(lm_fit, lwd = 4, col = 2)

hist(car_boot$t[, 2])


hist_bca <- hist(car_boot, ci = "bca")
hist_norm <- hist(car_boot, ci = "norm")
hist_perc <- hist(car_boot, ci = "perc")

Confint(lm_fit, vcov. = vcov(car_boot)) ## se method using t quantiles
confint(car_boot, type = "bca")
confint(car_boot, type = "norm")
confint(car_boot, type = "perc")
confint(car_boot, type = "basic")


## ISL way boot::boot()
################
library(boot)
boot.fn <- function(data, idx) {
  coef(lm(y ~ x1, data = data, subset = idx))
}
set.seed(2023)
bstrap <- boot::boot(data = mpg, statistic = boot.fn, R = 200)

bstrap$t0
bstrap$t

plot(1, type = "n", xlab = "", ylab = "",
     xlim = range(mpg$x1),
     ylim = range(mpg$y))
for(i in 1:200) {
  abline(a = bstrap$t[i, 1], b = bstrap$t[i, 2], lwd = 0.1)
}
abline(lm_fit, lwd = 4, col = 2)
hist(bstrap$t[, 2])

confint(bstrap, type = 'bca')
confint(bstrap, type = 'basic')
confint(bstrap, type = 'norm') # using bootstrap SE
confint(bstrap, type = 'perc') # using percentiles


boot.ci(bstrap)


