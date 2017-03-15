# Blokus

This repo has two parts: a server for officiating the Blokus board game, and a client for human IO. The client is very rough. The purpose is to provide a competitive environment where you can
1. Play blokus against your friends online
2. Program your own AI to play blokus against
3. Run competitive programming competitions to see who has the best AI coding skills (this one's my favorite).

# Run

On OSX with homebrew, clone this repo and run
* brew install activator
* activator run
* http://localhost:9000/

# Play

1. Write your AI code.
2. Update the config to the host's IP address prior to play.
3. Configure the parameters including the ruleset and the number of rounds of play.
4. Use the right arrow to fetch a newer board state and a left arrow to fetch an older board state. (Pull-model only for the moment.)
