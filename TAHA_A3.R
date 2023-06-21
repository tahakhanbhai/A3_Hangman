#HANGMAN GAME

#Creating a function to check credibility of character inputted by user
#User input can't have multiple characters, non-letter characters, numbers, or already have been guessed before.
check_user_input <- function(user_guess, guessed_letters) {
  if (nchar(user_guess) != 1 || user_guess %in% guessed_letters || !(grepl("^[a-zA-Z]$", user_guess))) {
    print("Invalid input: Please input a single letter that has not been guessed before.")
    return(FALSE)
  }
  return(TRUE)
}

#Creating a function for the visual display given to users so that they know which letters they have guessed in the word, and in which order.
visual_display <- function(mystery_word, user_guess) {
  word.length <- length(strsplit(mystery_word, "")[[1]])
  word_display <- rep('_', word.length)

  for (i in 1:word.length){
    if (mystery_word[i] == user_guess) {
      word_display[i] <- user_guess
    }
  }
  return(paste(visual, collapse = ""))
}

  #    word_display <- paste(word_display, letter, sep = "")
 #   } else {
#      word_display <- paste(word_display, "_", sep = "")
#    }
 # }
  #cat(word_display, "\n")
  #return(word_display)
#}


#Prepare a dictionary of words to choose from and save it in a txt file (one column) and save it in the project directory
#Read the word list from your program.
#Choose a random element from the list. Hint: You may want to check sample() and sample.int()  functions. There are many different ways of doing this. You are welcome to experiment.
play_game <- function() {

wordlist <- readLines("Hangman_Words.txt")
mystery_word <- sample(wordlist, 1, prob = NULL)
mistake <- 0
guessed_letters <- c()
#Inform the user on the length of the secret word. Hint: You may test nchar()

#Inform the user about the number of wrong guesses/tries allowed. You decide on the rule here and implement it. Please comment your code appropriately.
print("Welcome to Hangman!")
print("The objective of the game is to guess the mystery word, one letter at a time")
print("You are allowed 5 wrong guesses before the game is over. You can also guess the mystery word in full after every turn")
print("Good luck!")
print(paste0("The length of the mystery word is: ", nchar(mystery_word), " letters long."))



#Ask for user input. The user is expected to enter one character only, check for this.
repeat {
  user_guess <- toupper(readline(prompt = "Please enter a single letter: "))

  if (check_user_input(user_guess, guessed_letters) == FALSE) {
    next
  } else {
    guessed_letters <- c(guessed_letters, user_guess)

    if (user_guess %in% strsplit(mystery_word, "")[[1]]) {
      print(paste0(user_guess, " is in the mystery word"))
    } else {
      print(paste0(user_guess, " is not in the mystery word"))
      mistake <- mistake + 1
      print(paste0("You have ", 5-mistake, " mistakes left."))
    }
    if (mistake >= 5) {
      print("No more remaining guesses. Game Over :(")
      print(paste0("The mystery word was: ", mystery_word))
      break
    }
  }
  #FUNCTION THAT UPDATES THE DISPLAY OF THE WORD

    choice <- toupper(readline(prompt = "Would you like to guess the mystery word? (Y/N): "))
    if (choice == "N") {
      next
    } else {
    guess_the_word <- toupper(readline(prompt = "Enter guess: ")) }
    if (guess_the_word == mystery_word) {
      print("Congratulations! You guessed the mystery word.")
      break
    } else {
      print(paste0("Sorry ", guess_the_word, " is not the mystery word."))
      print(paste0("You have ", 5-mistake, " mistakes left."))
    }
  }
}
#Check to see if the user input is in the secret word.
#If yes, notify user and ask for next letter
#If not, notify user.
#If user has not exhausted the available tries, ask for the next letter.
#If tries are exhausted, notify user that they’ve lost. Reveal secret and exit.
#Make sure that your code never goes into an infinite loop, and always manages to exit gracefully (ie without throwing an error)
#Always notify the user about the correct letters/wrong letters, remaining tries.



#Additional functionality you may want to consider:
#
#  Provide the user with a visual clue of how they are progressing. E.g. if the secret word is “correct” and the user input is ‘c’, you may want to provide them with something like this:
#  C_ _ _ _ C _

#You may want to give the user the option to guess the whole word when they want, instead of forcing the one-letter-at-a-time approach.
