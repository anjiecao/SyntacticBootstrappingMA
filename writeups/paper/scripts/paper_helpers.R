#  paper helpers

# taken from the ME paper
get_MA_params <- function(age_moderated, df) {

  this_data <- df
  n = nrow(this_data)

  if (age_moderated){
    model <- rma.mv(d_calc ~ log(mean_age), V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant,
                    method = "REML",
                    data = this_data)

    this_age_estimate <- model$b[2]
    this_age_estimate.cil <- model$ci.lb[2]
    this_age_estimate.cih <- model$ci.ub[2]
    this_age_z <- model$zval[2]
    this_age_p <- model$pval[2]

  } else {
    model <- rma.mv(d_calc, V = d_var_calc,
                    random = ~ 1 | short_cite/same_infant,
                    method = "REML",
                    data = this_data)

    this_age_estimate <- NA
    this_age_estimate.cil <- NA
    this_age_estimate.cih <- NA
    this_age_z <- NA
    this_age_p <- NA

  }

  params <- data.frame(age_moderated = age_moderated,
                       n = n,
                       estimate = model$b[1],
                       estimate.cil = model$ci.lb[1],
                       estimate.cih = model$ci.ub[1],
                       z = model$zval[1],
                       p = model$pval[1],
                       age_estimate = this_age_estimate,
                       age_estimate.cil = this_age_estimate.cil,
                       age_estimate.cih = this_age_estimate.cih,
                       age_z = this_age_z,
                       age_p = this_age_p,
                       Q = model$QE,
                       Qp = model$QEp)
}
