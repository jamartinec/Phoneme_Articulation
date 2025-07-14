# Tristan Mahr code (wisclabmisc) which contains helpers for 
# running brms R package.

#' Set default arguments for brms model fitting
#' @param ... arguments to [brms::brm()] to use as default values.
#' @param .backend,.threads,.chains,.cores,.iter,.silent,.file_refit,.refresh,.control
#'   These arguments set *default* default values. Overwrite these defaults by
#'   using the argument without the `.` prefix.
#' @return A function for generating a list of arguments to [brms::brm()].
#' @export
#'
#' @details
#' One can set the default value for `adapt_delta` directly
#' (`adapt_delta = .98`). This value will be propagated to
#' `control$adapt_delta`.
#'
#' @examples
#' brm_args <- brms_args_create()
#'
#' # using package-provided defaults
#' brm_args()
#'
#' # overwriting a default value
#' brm_args(iter = 500)
#'
#' # adapt_delta is handled specially
#' brm_args(adapt_delta = .95)
#' # adapt_delta is handled specially
#' brm_args(adapt_delta = .95)
#'
#' # We can overwrite the package-provided defaults
#' other_brm_args <- brms_args_create(iter = 4000, backend = "rstan")
#' other_brm_args()
#'
#' # And overwrite them too
#' other_brm_args(backend = "cmdstanr")

export("brms_args_create") 
brms_args_create <- function(
    ...,
    #.backend = "cmdstanr",
    .backend = "rstan",
    .threads = 2,
    .chains = 4,
    .cores = 4,
    .iter = 2000,
    .silent = 0,
    .file_refit = "on_change",
    .refresh = 25,
    .control = list()
) {
  # the .names prevent `file` from partial matching `file_refit`
  defaults <- list(
    backend = .backend,
    threads = .threads,
    chains = .chains,
    cores = .cores,
    iter = .iter,
    silent = .silent,
    file_refit = .file_refit,
    refresh = .refresh,
    control = .control
  )
  outer_dots <- list(...) |>
    handle_control_args()
  
  # Let the user set adapt_delta outside of a `control` argument
  
  defaults <- utils::modifyList(defaults, outer_dots)
  
  function(...) {
    inner_dots <- list(...) |> handle_control_args()
    l <- utils::modifyList(defaults, inner_dots)
    structure(l, class = c("brm_args", "list"))
  }
}

#' @export
print.brm_args <- function(x, ...) {
  utils::str(x, ...)
}

handle_control_args <- function(args) {
  if (!is.null(args$adapt_delta)) {
    if(is.null(args$control)) {
      args$control <- list()
    }
    args$control$adapt_delta <- args$adapt_delta
    args$adapt_delta <- NULL
  }
  args
}

# #' Compute the finite population SD for random effect levels
# #'
# #' @param model a model fitted by brms
# #' @return a data.frame with one row per grouping variable per coefficient per
# #' posterior draw with the SD of the random effect estimates for grouping
# #' variable
# #' @noRd
# fsd_ranef <- function(model) {
#   ls <- brms::ranef(model, summary = FALSE)
#   l <- as.list(seq_along(ls))
#
#   for (i in seq_along(ls)) {
#     l[[i]] <- ls[[i]] |>
#       apply(3, as.data.frame) |>
#       bind_rows(.id = "ranef") |>
#       mutate(
#         group = names(ls)[i],
#         .draw = seq_along(.data$ranef)
#       ) |>
#       tidyr::pivot_longer(
#         cols = -one_of(c("ranef", "group", ".draw")),
#         names_to = "level"
#       )
#   }
#
#   results <- bind_rows(l) |>
#     group_by(ranef, group, .draw) |>
#     summarise(
#       n_levels = n_distinct(.data$level),
#       finite_sd = sd(.data$value),
#       .groups = "drop"
#     ) |>
#     mutate(
#       label = ifelse(ranef == "Intercept", .data$group, paste0(.data$group, " . ", .data$ranef)),
#       label = sprintf("%s (%s)", .data$label, .data$n_levels)
#     )
#
#   if (model$family$family == "gaussian") {
#     draws <- stats::residuals(model, summary = FALSE)
#     resid <- tibble::tibble(
#       ranef = "Residual",
#       group = "Residual",
#       .draw = seq(1, nrow(draws)),
#       n_levels = ncol(draws),
#       finite_sd = apply(draws, 1, sd),
#       label = sprintf("Residual (%s)", ncol(draws))
#     )
#     results <- rbind(results, resid)
#   }
#
#   results
# }
export("add_validation_criterion")
# reloo = TRUE (Indicate whether reloo should be applied on problematic observations)
# reloo.brmsfit: Compute exact cross-validation for problematic observations for
# which approximate leave-one-out cross-validation may return incorrect results.
# recompile: Logical, indicating whether the Stan model should be recompiled.
# if NULL recompile_model tries to figure out internally if recompilation is 
# necessary. FALSE will cause recompile_model to always return the brmsfit object
# unchanged. This may be necessary if you are running reloo on another machine
#than the one used to fit the model.
#' Add validation criteria (e.g., LOO/WAIC) to a brms model
#'
#' @param x A brmsfit object (fitted model).
#' @param ... Additional arguments passed to brms::add_criterion().
#' @param val_list Character vector of criteria to add (default: "loo").
#'                 Can include "loo", "waic", "kfold".
#' @param use_reloo Logical. Use exact LOO (`reloo=TRUE`) for problematic observations? (default: FALSE)
#'
#' @return A brmsfit object with updated criteria.
#'
#' @examples
#' model <- add_validation_criterion(model, val_list = c("loo", "waic"), use_reloo = TRUE)
add_validation_criterion <- function(
    x,
    ...,
    val_list = c("loo"),
    use_reloo = FALSE
) {
  # Check that x is a brmsfit
  if (!inherits(x, "brmsfit")) {
    stop("`x` must be a brmsfit object.")
  }
  
  # Validate val_list contents
  allowed_criteria <- c("loo", "waic", "kfold")
  invalid_criteria <- setdiff(val_list, allowed_criteria)
  if (length(invalid_criteria) > 0) {
    stop(
      sprintf("Invalid criteria specified: %s. Allowed: %s",
              paste(invalid_criteria, collapse = ", "),
              paste(allowed_criteria, collapse = ", "))
    )
  }
  
  # Call add_criterion with or without reloo
  if (use_reloo) {
    brms::add_criterion(
      x,
      criterion = val_list,
      reloo = TRUE,
      recompile = FALSE,
      ...
    )
  } else {
    brms::add_criterion(
      x,
      criterion = val_list,
      ...
    )
  }
}
