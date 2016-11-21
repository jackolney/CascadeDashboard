# MasterData script for handling the 'MasterData' structure

# 'MasterData' needs to be accessible to entire server.R and ONE session
# MasterData <<- is not the solution, as this makes the variable global
# Just needs to be defined somewhere... reactiveValue?

master <- reactiveValues()

# now we just assign things to master$data and pile on the '$'... I think
