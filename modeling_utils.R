library(recipes)
library(rlang)
library(cli)
library(dials)
library(tibble)

### EXAMPLE ###
# library(dplyr)
# 
# # 1. Create sample dataset
# df <- tibble(
#   time = 1:5,
#   Temperature = c(100, 102, 105, 101, 108),
#   Humidity = c(50, 52, 48, 55, 60)
# )
# 
# # 2. Build recipe with multiple smoothing values
# rec <- recipe(~ ., data = df) %>%
#   step_exp_smooth(Temperature, Humidity, alpha = 0.01, prefix = "smooth_99_") %>%
#   step_exp_smooth(Temperature, alpha = 0.05, prefix = "smooth_95_")
# 
# # 3. Inspect before prep using tidy()
# tidy(rec, number = 1)
# 
# # 4. Train and transform data
# prepped_rec <- prep(rec, training = df)
# processed_df <- bake(prepped_rec, new_data = df)
# 
# # 5. Inspect final result
# print(processed_df)


# ------------------------------------------------------------------------------
# 1. Parameter Definition (for dials/tuning)
# ------------------------------------------------------------------------------

#' Parameter for exponential smoothing alpha (0 to 1)
exp_alpha <- function(range = c(0.001, 0.999), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(exp_alpha = "Smoothing Parameter (Alpha)"),
    finalize = NULL
  )
}

# ------------------------------------------------------------------------------
# 2. Step Constructors
# ------------------------------------------------------------------------------

#' User-facing step function
step_exp_smooth <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  alpha = 0.05,
  columns = NULL,
  prefix = "exp_smooth_",
  skip = FALSE,
  id = rand_id("exp_smooth")
) {
  add_step(
    recipe,
    step_exp_smooth_new(
      terms = enquos(...),
      trained = trained,
      role = role,
      alpha = alpha,
      columns = columns,
      prefix = prefix,
      skip = skip,
      id = id
    )
  )
}

#' Internal step constructor
step_exp_smooth_new <- function(terms, role, trained, alpha, columns, prefix, skip, id) {
  step(
    subclass = "exp_smooth",
    terms = terms,
    role = role,
    trained = trained,
    alpha = alpha,
    columns = columns,
    prefix = prefix,
    skip = skip,
    id = id
  )
}

# ------------------------------------------------------------------------------
# 3. Core S3 Methods (prep, bake, print)
# ------------------------------------------------------------------------------

#' prep() method
prep.step_exp_smooth <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))
  
  if (is.numeric(x$alpha) && (x$alpha <= 0 || x$alpha >= 1)) {
    cli::cli_abort("{.arg alpha} must be strictly between 0 and 1.")
  }

  step_exp_smooth_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    alpha = x$alpha,
    columns = col_names,
    prefix = x$prefix,
    skip = x$skip,
    id = x$id
  )
}

#' Helper function for recursive exponential smoothing
apply_exp_smoothing <- function(vec, alpha) {
  n <- length(vec)
  if (n == 0) return(vec)
  
  out <- numeric(n)
  out[1] <- vec[1]
  
  for (t in seq_len(n)[-1]) {
    out[t] <- (1 - alpha) * out[t - 1] + alpha * vec[t]
  }
  
  out
}

#' bake() method
bake.step_exp_smooth <- function(object, new_data, ...) {
  col_names <- object$columns

  for (col in col_names) {
    new_col_name <- paste0(object$prefix, col)
    new_data[[new_col_name]] <- apply_exp_smoothing(new_data[[col]], object$alpha)
  }

  new_data
}

#' print() method
print.step_exp_smooth <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Exponential smoothing (alpha = ", format(x$alpha), ") on ")
  print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

# ------------------------------------------------------------------------------
# 4. Integration S3 Methods (tunable, tidy)
# ------------------------------------------------------------------------------

#' S3 method registering `alpha` for tidymodels tuning
tunable.step_exp_smooth <- function(x, ...) {
  tibble::tibble(
    name = "alpha",
    call_info = list(
      list(
        pkg = "dials",
        fun = "exp_alpha"
      )
    ),
    source = "recipe",
    component = "step_exp_smooth",
    component_id = x$id
  )
}

#' S3 method for tidying step_exp_smooth
tidy.step_exp_smooth <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(
      terms = x$columns,
      alpha = x$alpha
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble::tibble(
      terms = term_names,
      alpha = x$alpha
    )
  }
  
  res$id <- x$id
  res
}