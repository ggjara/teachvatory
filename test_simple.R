# Simple test to check roster and quiz join functionality
# Replace the parameters below with your actual data

library(dplyr)
library(stringr)

# REPLACE THESE WITH YOUR ACTUAL VALUES:
# =====================================
COURSE_NAME  <- "ExecEd UE"
ROSTER_FILE  <- "ExecEd UE - July 2025 (Hybrid) - Roster"
ROSTER_SHEET <- "roster"
QUIZ_FILE    <- "Exec Ed UE - July 2025 (Hybrid) - Quizzes Master Sheet"
QUIZ_SHEET   <- "S2 - Jamaica Process"

# Load required functions
source("R/fct_driveaccess.R")
source("R/mod_quiz_fct_logic.R")
source("R/globalvars.R")

# Function to filter columns by meaningful names (exclude those with numbers)
filter_meaningful_columns <- function(column_names) {
  # The function keeps column names that do NOT match the pattern "...<NUMBER>" at the end.
  # grepl("\\.\\.\\.[0-9]+$", column_names) returns TRUE for columns to remove.
  # The "!" negates this, so we keep everything that doesn't match.
  return(column_names[!grepl("\\.\\.\\.[0-9]+$", column_names)])
}

# Step 1: Get data
courses <- get_courses(PATH_DASHBOARD)
course_directory <- get_course_directory(courses, COURSE_NAME)

# Step 2: Load roster data
cat("\n=== ROSTER DATA ===\n")
roster_basic    <- get_roster(course_directory, ROSTER_FILE, ROSTER_SHEET, include_all_columns = FALSE)
roster_full_raw <- get_roster(course_directory, ROSTER_FILE, ROSTER_SHEET, include_all_columns = TRUE)

# Filter the full roster to only include meaningful columns
meaningful_cols <- filter_meaningful_columns(colnames(roster_full_raw))
meaningful_cols

# Create filtered roster_full with only meaningful columns
# Keep the basic required columns plus meaningful additional ones
required_cols <- c("standardized_name", "teachly")
all_meaningful_cols <- unique(c(required_cols, meaningful_cols))
# Only select columns that actually exist
existing_meaningful_cols <- all_meaningful_cols[all_meaningful_cols %in% colnames(roster_full_raw)]

roster_full <- roster_full_raw %>%
  select(all_of(existing_meaningful_cols))

cat("Filtered roster_full columns:\n")
roster_full

# Step 3: Get filter columns based on the filtered meaningful columns
# Remove the required columns from meaningful_cols to get only additional columns
additional_cols <- meaningful_cols[!meaningful_cols %in% required_cols]

# Create named list for filter columns (like get_roster_filter_columns does)
filter_cols <- list()
if (length(additional_cols) > 0) {
  names(additional_cols) <- stringr::str_to_title(stringr::str_replace_all(additional_cols, "_", " "))
  filter_cols <- as.list(additional_cols)
}

cat("\nAvailable filter columns:\n")
if (length(filter_cols) > 0) {
  print(filter_cols)
} else {
  cat("No additional filter columns found\n")
}

# Step 4: Load actual quiz data
cat("\n=== QUIZ DATA ===\n")

# Load actual quiz data using your parameters
quiz_metadata <- googlesheets4::gs4_get(course_directory %>% filter(name == QUIZ_FILE) %>% head(1))
actual_quiz   <- googlesheets4::read_sheet(quiz_metadata, sheet = QUIZ_SHEET)

cat("Quiz data loaded. Columns:\n")
colnames(actual_quiz)
cat("Quiz rows:", nrow(actual_quiz), "\n")

# Step 5: Test joins
cat("\n=== TESTING JOINS ===\n")
id_col <- get_idcolname(actual_quiz)
cat("ID column:", id_col, "\n")

# Test standard join
standard_result <- join_quiz_roster(actual_quiz, roster_basic, id_col)
cat("Standard join columns:", paste(colnames(standard_result), collapse = ", "), "\n")
cat("Standard join rows:", nrow(standard_result), "\n")

# Test extended join if filter columns exist
if (length(filter_cols) > 0) {
  test_cols <- unlist(filter_cols)
  cat("Actual column names:", paste(test_cols, collapse = ", "), "\n")

  extended_result <- join_quiz_roster_extended(actual_quiz, roster_full, id_col, test_cols)
  cat("Extended join columns:", paste(colnames(extended_result), collapse = ", "), "\n")
  cat("Extended join rows:", nrow(extended_result), "\n")

  # Check which additional columns made it through
  success_cols <- test_cols[test_cols %in% colnames(extended_result)]
  cat("Successfully joined additional columns:", paste(success_cols, collapse = ", "), "\n")

  # Also check for display names (in case columns were renamed)
  display_names <- names(filter_cols)
  success_display_names <- display_names[display_names %in% colnames(extended_result)]
  
  total_success <- length(success_cols) + length(success_display_names)

  if (total_success > 0) {
    cat("SUCCESS: Additional columns are being joined correctly!\n")
    if (length(success_cols) > 0) {
      cat("Joined with original names:", paste(success_cols, collapse = ", "), "\n")
    }
    if (length(success_display_names) > 0) {
      cat("Joined with display names:", paste(success_display_names, collapse = ", "), "\n")
    }
    
    cat("Sample of extended result (first 3 rows, key columns):\n")
    # Show some key columns from the result
    key_cols <- c(id_col, "teachly")
    available_key_cols <- key_cols[key_cols %in% colnames(extended_result)]
    
    # Add up to 3 additional columns to the sample
    sample_additional <- c(success_cols, success_display_names)
    sample_additional <- sample_additional[seq_len(min(3, length(sample_additional)))]
    
    sample_cols <- c(available_key_cols, sample_additional)
    n_rows <- min(3, nrow(extended_result))
    if (n_rows > 0) {
      print(extended_result[seq_len(n_rows), sample_cols, drop = FALSE])
    }
  } else {
    cat("ISSUE: No additional columns made it through the join\n")
    cat("Expected column names:", paste(test_cols, collapse = ", "), "\n")
    cat("Expected display names:", paste(names(filter_cols), collapse = ", "), "\n")
    cat("Actual result columns:", paste(colnames(extended_result), collapse = ", "), "\n")
    cat("Checking roster_full for these columns:\n")
    for(col in test_cols) {
      exists <- col %in% colnames(roster_full)
      cat(paste0("  ", col, ": ", ifelse(exists, "EXISTS", "MISSING"), "\n"))
    }
  }
} else {
  cat("No filter columns available for extended join test\n")
}

cat("\n=== TEST COMPLETE ===\n")
