# Here you should put not-sensitive variables used in the
# app's logic

# Path of One Dashboard to Rule them All
PATH_DASHBOARD <- "https://drive.google.com/drive/u/1/folders/1HFEBLVmrWReWn8DNwoTS7nrS6XZpAcdQ"

# Regexp of questions in quizzes to take out of the question select inputs.
QUESTIONS_TO_TAKE_OUT <- c(
  "teachly",
  "email",
  "your name",
  "if your name is not listed above",
  "timestamp",
  "imported"
)

# Regexp of name field in quiz
NAME_FIELD_QUIZ <- "Your Name"

# Regexp of name field in quiz
NAME_ALTERNATIVE_FIELD_QUIZ <- "If your name is not listed"

# Names to remove from quizzes name
NAMES_TO_REMOVE_FROM_QUIZ <- c("Student, Test", "Levy, Dan", "Dan Levy", "Dan, Levy", "Student Test", "Test, Student")


# Initial Date
#INITIAL_DATE_DEFAULT = as.Date("2023-01-01") # fixed date
current_year <- as.integer(format(Sys.Date(), "%Y"))
INITIAL_DATE_DEFAULT <- as.Date(paste(current_year, "01-01", sep = "-"))

# Color palette
COLORS <- c("#9E2A2B", "#429EA6", "#383D3B", "#F4F4F9", "#FCDE9C")

COLOR_RED <- COLORS[1]
COLOR_GREEN <- COLORS[2]
COLOR_DEFAULT <- COLORS[3]
COLOR_WHITE <- COLORS[4]
COLOR_YELLOW <- COLORS[5]


