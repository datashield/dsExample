#'
#' @title Returns the levels of a factor vector with a fun message
#' @param x a factor vector
#' @param fun_message a fun message
#' @return a list, the factor levels present in the vector
#' @importFrom dsBase checkPermissivePrivacyControlLevel
#' @author Tim Cadman
#' @export
funLevelsDS <- function(x, fun_message){
  dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'banana')) ## Check privacy mode setting
  data <- eval(parse(text=x), envir = parent.frame()) ## Load object withing function
  levels_out <- levels(data) ## Get levels
  .checkLevelsDisclosureRisk(data, levels_out) ## Check disclosure issues

  return(
    paste0(
      fun_message, ": ", paste(levels_out, collapse = ", ") ## Paste message to levels
    )
  )
}

#' Check Disclosure Risk for Levels
#'
#' This function checks whether the levels of a variable can be safely returned
#' based on the disclosure risk settings.
#'
#' @param input A vector of values from which levels are derived.
#' @param levels_out A vector containing the levels to be checked.
#'
#' @return No return value; throws an error if disclosure risk is too high.
#' @keywords internal
#' @importFrom dsBase listDisclosureSettingsDS
.checkLevelsDisclosureRisk <- function(input, levels_out) {

  levels_density <- .getDensitySetting()
  threshold <- .calculateThreshold(input, levels_density)
  .throwErrorIfRisk(input, levels_out, threshold)

}

#' Get Disclosure Density Setting
#'
#' Retrieves the `nfilter.levels.density` setting from DataSHIELD's disclosure settings.
#'
#' @return A numeric value representing the density setting.
#' @keywords internal
#' @importFrom dsBase listDisclosureSettingsDS
.getDensitySetting <- function() {
  thr <- listDisclosureSettingsDS()
  return(as.numeric(thr$nfilter.levels.density))
}

#' Calculate Disclosure Risk Threshold
#'
#' This function calculates the threshold for the number of allowed levels
#' based on the input length and density setting.
#'
#' @param input A vector of values.
#' @param levels_density A numeric value representing the density setting.
#'
#' @return A numeric threshold for the maximum number of allowed levels.
#' @keywords internal
.calculateThreshold <- function(input, levels_density) {
  input_length <- length(input)
  return(input_length * levels_density)
}

#' Throw an Error if Disclosure Risk is Too High
#'
#' Internal function that throws an error if the number of levels exceeds the
#' threshold set by disclosure risk settings.
#'
#' @param input A vector of values.
#' @param levels_out A vector of levels being checked.
#' @param threshold A numeric value representing the maximum allowed levels.
#'
#' @return No return value; throws an error if the disclosure risk is exceeded.
#' @keywords internal
#' @importFrom cli cli_abort
.throwErrorIfRisk <- function(input, levels_out, threshold) {

  if (threshold < length(levels_out)) {
    cli::cli_abort(
      c(
        "x" = "The levels cannot be returned due to a disclosure risk",
        "i" = "The length of the variable is {length(input)} and the number of levels is {length(levels_out)}",
        "i" = "Based on current disclosure settings the maximum number of levels that can be returned is {threshold}",
        call = NULL
      )
    )
  }

}


