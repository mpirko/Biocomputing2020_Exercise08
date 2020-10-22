#Open table of UWvsMSU data by importing it from terminal
UWvMSU=read.table("/Users/melissapirko/Downloads/data-shell/Biocomputing2020_Exercise08/UWvMSU_1-22-13.txt", header=TRUE, sep = "\t")
#count number of times UW and MSU scores
numberUW=UWvMSU[UWvMSU[,2]=="UW", ]
numberMSU=UWvMSU[UWvMSU[,2]=="MSU", ]

#Removes the team data with it being segmented
numberUW$team <- NULL
numberMSU$team <- NULL

#Create a new column with the cumulative score after scoring
numberUW[,"cum_score"] <- cumsum(numberUW$score)
numberMSU[,"cum_score"] <- cumsum(numberMSU$score)

#Removes original score data
numberUW$score <- NULL
numberMSU$score <- NULL

#Plots the data
plot(numberUW[,1],numberUW[,2],type='l', xlab="time(min)", ylab="score",col="red")
lines(numberMSU[,1],numberMSU[,2],col="green")
legend("bottomright", inset=.05, title= "Team",
  c("UW","MSU"), fill=terrain.colors(3), horiz=TRUE)


#Guess my number game
print("I'm thinking of a number 1-100:")
numbergame <- function(x) {
  #set the population size from 1-100
  x<-sample(1:100, size=1)
  #counts down by 1
  counter <- 1
  guess<-readline("Guess a number!")
  #Allows up to 7 guesses
  while(counter < 8) {
    #If the guess is less than the number, says to guess higher and type it
    #again, counter goes down by 1
    if (guess < x) {
      print(paste0("Higher"))
      m <- readline("Type number again:")
      guess <- as.numeric(m)
      counter <- counter + 1
      #If the guess is higher than the number, says to guess lower and type it
      #again, counter goes down by 1
    } else if (guess > x) {
      print(paste0("Lower"))
      m <- readline("Type number again:")
      guess <- as.numeric(m)
      counter <- counter + 1
      #If the guess is correct, stops the countdown and says "Correct!"
    } else if (guess == x) {
      print("Correct!")
      opt <- options(show.error.messages=FALSE)
      on.exit(options(opt))
      stop()
    }
  }
  #If there are no more attempts left after 7 guesses, reveals the right answer
  print(paste0("You've run out of attempts! Correct answer was: ", x))
}
