# Generate multiple MAR scenarios at varying missing levels and strengths
generate_mar_scenarios <- function(n = 1000,
                                   levels = c(0.1, 0.3, 0.5),
                                   beta_x = beta_x_true,
                                   beta_e = beta_e_true,
                                   sd_error = sd_error_true) {
  # Two scenarios per missing level: one E-driven (shift=1), one X-driven (shift=5)
  params <- tibble::tibble(
    mcar_rate = rep(levels, each = 2),
    mar_logit_shift = rep(c(1, 5), times = length(levels)),
    mar_depend = rep(c("E", "X"), times = length(levels))
  )

  scenarios <- params |>
    mutate(
      label = paste0(
        sprintf("MAR p=%.0f%%", mcar_rate * 100),
        ", shift=", mar_logit_shift,
        ", depend=", mar_depend
      ),
      data = purrr::pmap(
        list(mcar_rate, mar_logit_shift, mar_depend),
        ~ simulate_birth_data(
          n         = n,
          beta_x    = beta_x,
          beta_e    = beta_e,
          sd_error  = sd_error,
          mcar_rate = ..1,
          mar_logit_shift = ..2,
          mar_depend = ..3
        )
      )
    )

  scenarios
}

summarize_mar_scenarios <- function(scenarios) {
  full_row <- tibble(
    label = "Full data",
    N = nrow(scenarios$data[[1]]),
    mean_Y = mean(scenarios$data[[1]]$Y),
    sd_Y = sd(scenarios$data[[1]]$Y),
    mean_X = mean(scenarios$data[[1]]$X),
    sd_X = sd(scenarios$data[[1]]$X),
    mean_E = mean(scenarios$data[[1]]$E),
    sd_E = sd(scenarios$data[[1]]$E)
  )

  mar_rows <- scenarios |>
    mutate(
      data_obs = purrr::map(data, ~ dplyr::filter(.x, !is.na(X_mar))),
      N = purrr::map_int(data_obs, nrow),
      mean_Y = purrr::map_dbl(data_obs, ~ mean(.x$Y)),
      sd_Y   = purrr::map_dbl(data_obs, ~ sd(.x$Y)),
      mean_X = purrr::map_dbl(data_obs, ~ mean(.x$X_mar)),
      sd_X   = purrr::map_dbl(data_obs, ~ sd(.x$X_mar)),
      mean_E = purrr::map_dbl(data_obs, ~ mean(.x$E)),
      sd_E   = purrr::map_dbl(data_obs, ~ sd(.x$E))
    ) |>
    select(label, N, mean_Y, sd_Y, mean_X, sd_X, mean_E, sd_E)

  bind_rows(full_row, mar_rows)
}
