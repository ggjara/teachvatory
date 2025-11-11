# Setup script for Ellmer package
# 
# The Ellmer package is a modern R interface for language models.
# Before using the application, you need to:
# 
# 1. Install the Ellmer package if not already installed:
# install.packages("ellmer")
#
# 2. Set up your OpenAI API key. You can do this in several ways:
#
# Option A: Set as environment variable (recommended)
# Sys.setenv(OPENAI_API_KEY = "your-api-key-here")
#
# Option B: Add to your .Renviron file
# usethis::edit_r_environ()
# Then add: OPENAI_API_KEY=your-api-key-here
#
# Option C: Set it programmatically at the start of your session
# ellmer::set_api_key("your-api-key-here")
#
# 3. Test the setup:
test_ellmer_setup <- function() {
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    stop("Ellmer package is not installed. Please run: install.packages('ellmer')")
  }
  
  # Try a simple chat completion
  tryCatch({
    # Note: This simple test uses basic ellmer without config since setup_elmer.R 
    # is standalone. For actual app usage, create_ai_chat() will use configured model.
    chat_obj <- ellmer::chat("openai")
    result <- chat_obj$chat("Say 'Hello, Ellmer is working!' in exactly those words.")
    
    cat("✓ Ellmer is set up correctly!\n")
    cat("Test response:", result, "\n")
    cat("Note: App will use configured model from golem-config.yml\n")
    return(TRUE)
  }, error = function(e) {
    cat("✗ Ellmer setup failed:\n")
    cat(e$message, "\n")
    cat("\nPlease check:\n")
    cat("1. Your API key is set correctly\n")
    cat("2. You have internet connection\n")
    cat("3. Your OpenAI account has sufficient credits\n")
    return(FALSE)
  })
}

# Uncomment the line below to test your setup:
# test_ellmer_setup()
