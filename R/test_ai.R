library(openai)
Sys.setenv(
  OPENAI_API_KEY = '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'
)

client <- OpenAI()
completion <- client$chat$completions$create(
  model = "gpt-4-0125-preview",
  messages = list(list("role" = "user", "content" = "What's up?"))
)

completion$choices[[1]]$message$content
