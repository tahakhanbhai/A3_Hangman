#HANGMAN GAME

#Creating a function to check credibility of character inputted by user
#User input can't have multiple characters, non-letter characters, numbers, or already have been guessed before.
check_user_input <- function(user_guess, guessed_letters) {
  if (nchar(user_guess) != 1 || user_guess %in% guessed_letters || !(grepl("^[a-zA-Z]$", user_guess))) {
    print("Invalid input: Please input a single letter or guess the entire word.")
    return(FALSE)
  }
  return(TRUE)
}

#Creating a function for the visual display given to users so that they know which letters they have guessed in the word, and in which order.
visual_display <- function(mystery_word, guessed_letters) {
  word_display <- ""

  for (letter in mystery_word) {
    if (letter %in% guessed_letters) {
      word_display <- paste(word_display, letter, sep = "")
    } else {
      word_display <- paste(word_display, "_", sep = "")
    }
  }
  cat("Current state: ", paste(strsplit(word_display, "")[[1]], collapse = " "), "\n")
}



#Prepare a dictionary of words to choose from and save it in a txt file (one column) and save it in the project directory
#Read the word list from your program.
#Choose a random element from the list. Hint: You may want to check sample() and sample.int()  functions. There are many different ways of doing this. You are welcome to experiment.
wordlist <- read.csv("Hangman_Words.txt")
mystery_word <- sample(wordlist, 1)
#Inform the user on the length of the secret word. Hint: You may test nchar()

#Inform the user about the number of wrong guesses/tries allowed. You decide on the rule here and implement it. Please comment your code appropriately.
print("Welcome to Hangman!")
print(paste0("The length of the secret word is: ", nchar(mystery_word), " letters long."))
print("You are allowed 5 wrong guesses before the game is over. Good luck!")

#Ask for user input. The user is expected to enter one character only, check for this.

user_guess <- readline(prompt = " Please enter a single letter: ")
#Optional; check if the character is a letter
#Optional: make sure that both lower and upper case letters are acceptable
#heck to see if the user input is in the secret word.
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