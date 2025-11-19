# Simulation helpers for the missing-data presentation

simulate_birth_data <- function(n = 1000,
                                beta_x = 0.5,
                                beta_e = 1,
                                sd_error = 2,
                                mcar_rate = 0.5,
                                mar_logit_shift = 1.2,
                                mar_depend = c("E", "X")) {
  mar_depend <- match.arg(mar_depend)
  # E: education (0 = lower, 1 = higher)
  E <- rbinom(n, 1, 0.5)

  # X: nutrition score, higher with higher education
  X <- 1.5 * E + rnorm(n)

  # Y: birthweight, affected by both nutrition and education
  Y <- 2 + beta_x * X + beta_e * E + rnorm(n, sd = sd_error)

  # MCAR missingness in X: constant probability
  p_miss_mcar <- rep(mcar_rate, n)
  miss_mcar   <- rbinom(n, 1, p_miss_mcar)
  X_mcar      <- ifelse(miss_mcar == 1, NA_real_, X)

  # MAR missingness in X
  # Default: low education → more missing (mar_depend = "E")
  # Optionally: missingness depends on X itself (mar_depend = "X")
  mar_var <- if (mar_depend == "E") {
    1 - E
  } else {
    -as.numeric(scale(X))  # lower X → higher missingness
  }
  alpha <- if (mar_logit_shift == 0) {
    qlogis(mcar_rate)
  } else {
    uniroot(
      f = function(a) mean(plogis(a + mar_logit_shift * mar_var)) - mcar_rate,
      interval = c(-15, 15)
    )$root
  }
  p_miss_mar   <- plogis(alpha + mar_logit_shift * mar_var)
  miss_mar     <- rbinom(n, 1, p_miss_mar)
  X_mar        <- ifelse(miss_mar == 1, NA_real_, X)

  tibble(
    E, X, Y,
    X_mcar, miss_mcar,
    X_mar,  miss_mar
  )
}

analyze_mar_strategies_once <- function(n = 1000, mcar_rate = 0.6) {
  dat <- simulate_birth_data(n = n, mcar_rate = mcar_rate)
  full_fit <- lm(Y ~ X + E, data = dat)

  beta_true <- coef(full_fit)[["X"]]

  cc <- dat |> filter(!is.na(X_mar))

  fit_cc_naive <- lm(Y ~ X_mar, data = cc)
  fit_cc_adj   <- lm(Y ~ X_mar + E, data = cc)

  mean_x <- mean(cc$X_mar, na.rm = TRUE)
  dat_mean <- dat |>
    mutate(
      X_imp_mean = if_else(is.na(X_mar), mean_x, X_mar)
    )
  fit_mean <- lm(Y ~ X_imp_mean + E, data = dat_mean)

  dat_mean_ind <- dat |>
    mutate(
      X_miss_ind = as.integer(is.na(X_mar)),
      X_imp_mean = if_else(is.na(X_mar), mean(cc$X_mar, na.rm = TRUE), X_mar)
    )
  fit_mean_ind <- lm(Y ~ X_imp_mean + X_miss_ind + E, data = dat_mean_ind)

  reg_x <- lm(X ~ E, data = dat, subset = !is.na(X_mar))
  dat_reg <- dat |>
    mutate(
      X_imp_reg = if_else(
        is.na(X_mar),
        predict(reg_x, newdata = dat),
        X_mar
      )
    )
  fit_reg <- lm(Y ~ X_imp_reg + E, data = dat_reg)

  summary_tbl <- tibble(
    method = c(
      "Full data (Y ~ X + E)",
      "MAR CC: naive (Y ~ X)",
      "MAR CC: adjust E",
      "Mean imputation",
      "Mean + missing indicator",
      "Regression imputation (E only)"
    ),
    estimate = c(
      coef(full_fit)[["X"]],
      coef(fit_cc_naive)[["X_mar"]],
      coef(fit_cc_adj)[["X_mar"]],
      coef(fit_mean)[["X_imp_mean"]],
      coef(fit_mean_ind)[["X_imp_mean"]],
      coef(fit_reg)[["X_imp_reg"]]
    ),
    se = c(
      coef(summary(full_fit))[["X", "Std. Error"]],
      coef(summary(fit_cc_naive))[["X_mar", "Std. Error"]],
      coef(summary(fit_cc_adj))[["X_mar", "Std. Error"]],
      coef(summary(fit_mean))[["X_imp_mean", "Std. Error"]],
      coef(summary(fit_mean_ind))[["X_imp_mean", "Std. Error"]],
      coef(summary(fit_reg))[["X_imp_reg", "Std. Error"]]
    ),
    beta_true = beta_true
  )

  list(
    dat_full     = dat,
    cc           = cc,
    dat_mean     = dat_mean,
    dat_mean_ind = dat_mean_ind,
    dat_reg      = dat_reg,
    full_fit     = full_fit,
    fit_cc_naive = fit_cc_naive,
    fit_cc_adj   = fit_cc_adj,
    fit_mean     = fit_mean,
    fit_mean_ind = fit_mean_ind,
    fit_reg      = fit_reg,
    summary      = summary_tbl
  )
}

# Partial-plot helpers used to compare truth vs. each strategy
make_partial_df <- function(dat, x_var) {
  # dat must contain Y, E, and the x_var column
  x <- dat[[x_var]]
  keep <- !is.na(x) & !is.na(dat$Y) & !is.na(dat$E)
  x <- x[keep]
  y <- dat$Y[keep]
  e <- dat$E[keep]

  y_resid <- resid(lm(y ~ e))
  x_resid <- resid(lm(x ~ e))

  tibble(
    x_resid = x_resid,
    y_resid = y_resid
  )
}

plot_partial_truth_vs <- function(mar_obj, x_method, label_method) {
  df_truth <- make_partial_df(mar_obj$dat_full, "X") |>
    mutate(source = "Truth (full data)")

  dat_method <- mar_obj$dat_full
  dat_method[[x_method]] <- dplyr::coalesce(
    dat_method[[x_method]],
    dat_method[[x_method]]
  )
  df_method <- make_partial_df(dat_method, x_method) |>
    mutate(source = label_method)

  df_both <- bind_rows(df_truth, df_method)

  ggplot(df_both, aes(x = x_resid, y = y_resid, color = source)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Residualized nutrition (X | E)",
      y = "Residualized birthweight (Y | E)",
      color = NULL
    ) +
    theme_minimal(base_size = 14)
}
