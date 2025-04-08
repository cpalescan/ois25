fish <- 2000
predators <- 50
weeks <- 52
fish_tracker <- c(2000)
predators_tracker <- c(50)

fish_die <- function(fish){
  poisoned_fish <- (fish * 10)/100
  cat(round(poisoned_fish), " fish died\n")
  return(round(fish - poisoned_fish))
}

enough_fish <- function(fish, predators){
  if(fish >= predators*5){
    cat("There were enough fish: \n -", fish, "fish\n -", 
        predators*5, " fish needed\n")
    return(TRUE)}
  else{
    print("Not enough fish")
    cat(predators*5, "fish needed\n", fish, " fish available\n")
    return(FALSE)}
}

predators_eat <-function(fish, predators){
  fish <- fish - predators*5
  cat("predators eat ",predators*5, "fish\n")
  return(fish)
}

predators_die <- function(predators){
  remaining_predators <- predators - round((predators*5)/100)
  cat(round((predators*5)/100), " predators died.\nRemaining predators: ", remaining_predators)
  return(remaining_predators)
}

predators_proliferate <- function(predators){
  cat(round(predators/5), "new predators.\n")
  return(predators + round(predators/5))
}

is_r_week <- function(week){
  if(week%%4 == 0){
    print("---PROLIFERATION---")
    return(TRUE)}
  else{return(FALSE)}
}

fish_proliferate <- function(fish){
  new_fish <- fish + round((fish*20)/100)
  cat(round((fish*20)/100), " fish were born.\nNew fish population: ", new_fish,"\n")
  return(new_fish)
}


cycle_counter <- 1
total_cycles <- c(1:weeks)

for(week in total_cycles){
  cat("week number: ", cycle_counter, "\n")
  cat("Predators: ", predators, "\n")
  cat("Fish: ", fish, "\n")
  fish <- fish_die(fish)
  if(enough_fish(fish, predators)){
    fish <- (predators_eat(fish, predators))}

  else{predators <- predators_die(predators)
  print("Some predators died")}
  
# ----- fine strana situazione
  
  if(is_r_week(cycle_counter)){
    fish <- fish_proliferate(fish)
    if(enough_fish(fish, predators)){predators <- predators_proliferate(predators)}
    }
  
  fish_tracker[cycle_counter] <- fish
  predators_tracker[cycle_counter] <- predators
  cycle_counter <- cycle_counter +1
  
  cat("-------End of the week-------\n\n")
  
}

length(fish_tracker)
cycle_counter

plot(c(1:(cycle_counter-1)), fish_tracker, xlab="Weeks", ylab="Colony Size",
     type="b", col="blue")
lines(c(1:(cycle_counter-1)), predators_tracker, col="red", type="l")
legend(10,1500,legend=c("Fish", "Predators"), col=c("blue", "red"), pch=16)
