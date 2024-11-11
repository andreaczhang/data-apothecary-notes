# bot in rstudio 

# install.packages(c('devtools', 'openai'))
# install_github("isinaltinkaya/gptchatteR")
# library(devtools)
# # load gptchatteR package
# library(gptchatteR)
# chatter.auth('YOUR KEY')
# # initiate the chat
# chatter.create()
# response <- chatter.chat('what is the result of presidential election of USA')
# gptchatteR


install.packages('gptstudio')

usethis::edit_r_environ()
# edit, add the API key
# restart 
# can check the API key 
Sys.getenv()
Sys.getenv('OPENAI_API_KEY')

library(gptstudio)
chat("What is the weather like today?")


# elmer ----
install.packages('pak')
install.packages('httr2')
pak::pak("tidyverse/elmer")

library(elmer)
chat <- chat_openai(
  model = "gpt-4o-mini",
  system_prompt = "You are a friendly but terse assistant.",
  echo = TRUE
)
live_console(chat)

chat$chat("What preceding languages most influenced R?")





