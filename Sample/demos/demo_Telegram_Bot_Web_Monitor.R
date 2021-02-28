library(telegram.bot)
library(httr)

my_token <- readLines("/mnt/R/.telegram_token.conf") # Never put sensitive data directly in the script
start <- function(bot, update) {
  bot$sendMessage(
    chat_id = update$message$chat$id,
    text = sprintf("Hello %s!", update$message$from$first_name)
  )
}
updater <- Updater(my_token) + CommandHandler("start", start)

# Initialize bot
bot <- Bot(token = my_token)

# Get bot inf
print(bot$getMe())
Sys.sleep(10) # Give time to send a message to the bot
# Get updates
updates <- bot$getUpdates()
# Retrieve your chat id
# Note: you should text the bot BEFORE calling `getUpdates`
chat_id <- updates[[1L]]$from_chat_id()


repeat {
   # Get updated check
   to_be_validated <- GET("https://www.kaizen-r.com/2021/02/before-ml/")
   # Send message
   bot$sendMessage(chat_id,
                text = paste("https://www.kaizen-r.com/2021/02/before-ml/","\nResponse Code: ",to_be_validated$status_code)
   )
   Sys.sleep(600) # Wait for 10 minutes
}
