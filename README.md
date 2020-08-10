# Haskell Echo Bot

This is a bot for Telegram and VK which repeats the messages that you send to it.

It also understands two commands:
- `/help` shows you the description of the bot.
- `/repeat` allows you to configure the number of repeated messages that the bot will answer you with.


## Building and running

In order to build the bot from source, you need to already have the [Stack](https://www.haskellstack.org) build tool installed.

You also need to register new bots in [Telegram](https://core.telegram.org/bots/api) and [VK](https://vk.com/dev/bots_longpoll) and get the tokens.

Then, do the following:
- clone the repository: `git clone https://github.com/gleb-akhmerov/haskell-echo-bot.git`
- build: `stack build`
- edit the config (TODO: describe how to create the config and how to specify tokens, repeats, etc.)
- run the bot: `stack run`


## Development

Project structure:
- [src/Bot.hs](src/Bot.hs) contains the main logic of the bot.
- [src/Telegram.hs](src/Telegram.hs) is the code which integrates the logic with the Telegram API.
- [app/Main.hs](app/Main.hs) is the source of the executable. Here you will find the `main` function, which ties everything together. Run `stack run` to execute it.
- [test](test) contains the tests, which you can run with `stack test`.
