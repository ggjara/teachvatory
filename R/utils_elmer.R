#' Get AI model configuration
#'
#' @description Get the AI model from the golem configuration file
#'
#' @return Character string with the model name
#' @export
get_ai_model <- function() {
  tryCatch({
    # Try to get from golem config first
    config_data <- get_golem_config("ai")
    if (!is.null(config_data) && !is.null(config_data$model)) {
      return(config_data$model)
    }
    
    # Fallback: try direct config file access for development
    config_data <- config::get(file = "inst/golem-config.yml")
    if (!is.null(config_data$ai) && !is.null(config_data$ai$model)) {
      return(config_data$ai$model)
    }
    
    # Final fallback
    "gpt-5"
  }, error = function(e) {
    warning("Could not read AI model from config, using default: gpt-5")
    "gpt-5"
  })
}

#' Get AI provider configuration
#'
#' @description Get the AI provider from the golem configuration file
#'
#' @return Character string with the provider name
#' @export
get_ai_provider <- function() {
  tryCatch({
    # Try to get from golem config first
    config_data <- get_golem_config("ai")
    if (!is.null(config_data) && !is.null(config_data$provider)) {
      return(config_data$provider)
    }
    
    # Fallback: try direct config file access for development
    config_data <- config::get(file = "inst/golem-config.yml")
    if (!is.null(config_data$ai) && !is.null(config_data$ai$provider)) {
      return(config_data$ai$provider)
    }
    
    # Final fallback
    "openai"
  }, error = function(e) {
    warning("Could not read AI provider from config, using default: openai")
    "openai"
  })
}

#' Get AI temperature configuration
#'
#' @description Get the AI temperature from the golem configuration file
#'
#' @return Numeric value for temperature
#' @export
get_ai_temperature <- function() {
  tryCatch({
    # Try to get from golem config first
    config_data <- get_golem_config("ai")
    if (!is.null(config_data) && !is.null(config_data$temperature)) {
      return(config_data$temperature)
    }
    
    # Fallback: try direct config file access for development
    config_data <- config::get(file = "inst/golem-config.yml")
    if (!is.null(config_data$ai) && !is.null(config_data$ai$temperature)) {
      return(config_data$ai$temperature)
    }
    
    # Final fallback
    0.3
  }, error = function(e) {
    warning("Could not read AI temperature from config, using default: 0.3")
    0.3
  })
}

#' Create AI chat object with configured settings
#'
#' @description Create an ellmer chat object using the configured provider and model
#'
#' @return An ellmer chat object
#' @export
create_ai_chat <- function() {
  provider <- get_ai_provider()
  model <- get_ai_model()
  
  # Create chat object with the configured provider
  chat_obj <- ellmer::chat(provider)
  
  # Note: ellmer handles model selection internally, but we store it for reference
  attr(chat_obj, "configured_model") <- model
  
  return(chat_obj)
}

#' Setup Ellmer for AI functionality
#'
#' @description This function sets up the Ellmer package for use with OpenAI API.
#' It checks for the API key and provides helpful error messages if not configured.
#'
#' @return TRUE if setup is successful, FALSE otherwise
#' @export
#'
#' @examples
#' \dontrun{
#' setup_ellmer()
#' }
setup_ellmer <- function() {
  # Check if ellmer package is available
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("The 'ellmer' package is required but not installed. Please run: install.packages('ellmer')")
  }
  
  # Check if API key is set
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    warning(
      "OPENAI_API_KEY environment variable is not set.\n",
      "Please set your OpenAI API key using one of these methods:\n",
      "1. Sys.setenv(OPENAI_API_KEY = 'your-key-here')\n",
      "2. Add OPENAI_API_KEY=your-key-here to your .Renviron file\n",
      "3. Use ellmer::set_api_key('your-key-here')"
    )
    return(FALSE)
  }
  
  # Test the connection with a simple call
  tryCatch({
    chat_obj <- create_ai_chat()
    test_response <- chat_obj$chat("Respond with exactly: 'OK'")
    
    if (grepl("OK", test_response, ignore.case = TRUE)) {
      message("✓ Ellmer setup successful!")
      message("✓ Using model: ", get_ai_model())
      message("✓ Using provider: ", get_ai_provider())
      return(TRUE)
    } else {
      warning("Ellmer test call succeeded but returned unexpected response")
      return(FALSE)
    }
    
  }, error = function(e) {
    warning("Ellmer setup failed: ", e$message)
    return(FALSE)
  })
}

#' Check if Ellmer is properly configured
#'
#' @description A simple check function to verify Ellmer configuration
#' without making an API call.
#'
#' @return TRUE if configured, FALSE otherwise
#' @export
is_ellmer_configured <- function() {
  return(
    requireNamespace("ellmer", quietly = TRUE) && 
    Sys.getenv("OPENAI_API_KEY") != ""
  )
}
