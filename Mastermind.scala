import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Mastermind {
	// Input manager
	val input = new Scanner(System.in)

	// Max # of guesses a player can make
	val maxTurns = 10

	// # of pins and markers for a code
	val numPinsPlayable = 4

	// All player guesses
	val guesses = Array.ofDim[Char](maxTurns, numPinsPlayable)

	// All markers for player guesses
	val markers = Array.ofDim[Char](maxTurns, numPinsPlayable)

	// Pin choices
	val colorChoices = Array('B', 'G', 'O', 'P', 'R', 'Y')

	// Computer pins selection
	val computerCode = new Array[Char](numPinsPlayable)

	// Turn #
	var turn = 0

	// Tracks game end state
	var gameOver = false

	// Tracks win state
	var win = false

	def main(args: Array[String]): Unit = {
		writeInstructions
		println
		promptGameStart
		println("------------")
		println("GAME START")
		println("------------")
		println
		run
	}

	/**
	*	writeInstructions
	*
	*	Prints the game instructions to the terminal.
	*/
	def writeInstructions(): Unit = {
		println("-------------------------------------------")
		println("Welcome to Mastermind!\n")
		println("Developed by Sachandhan Ganesh")
		println("-------------------------------------------")
		println("\n")
		println("How to play:")
		println("------------")
		println("It's just you versus the computer.\nMastermind is known as the codebreaker's game, and as you read/play you will see why!\n")
		println("There are six colored pins.")
		println("The blue pin is represented by `B`")
		println("The green pin is represented by `G`")
		println("The orange pin is represented by `O`")
		println("The purple pin is represented by `P`")
		println("The red pin is represented by `R`")
		println("The yellow pin is represented by `Y`")
		println
		println("The computer will choose a permutation of " + numPinsPlayable + " colored pins.")
		println
		println("You will have " + maxTurns + " turns to find the computer's code.")
		println("Every turn, you will choose a permutation of " + numPinsPlayable + " pins to match against the computer's choice.")
		println("You win if you can guess the code in " + maxTurns + " turns or less!")
		println
		println("In order to help you out, you will have " + numPinsPlayable + " hint markers.")
		println("After every turn, you will be given the " + numPinsPlayable + " markers, which help indicate if your choices are correct.")
		println("The white marker is represented by `-`. It means you have a correct pin, but it is in the wrong position.")
		println("The black marker is represented by `o`. It means you have the correct pin, in the correct position.")
		println("The empty marker is represented by `x`. It means you have an incorrect pin.")
		println
		println("OBJECTIVE: Guess the permutation that the computer chose in " + maxTurns + " turns or less.")
		println("------------")
		println
	}

	/**
	*	promptGameStart
	*
	*	Prompts user to either start game or quit playing.
	*/
	def promptGameStart(): Unit = {
		var response: String = ""
		do {
			print("Start the game? (`n` to quit, `y` to continue)\t> ")
			response = input.nextLine
			println("\n\n")
		} while (!response.equals("") && !(response.equals("y") || response.equals("n") || response.equals("q")))

		if (response.equals("n") || response.equals("q")) {
			System.exit(1)
		}
	}

	/**
	*	run
	*
	*	Main game loop that handles all game operations and logic.
	*/
	def run(): Unit = {
		computerTurn(true)
		while (gameOver == false && turn < maxTurns) {
			println("Turn #" + (turn + 1))
			println("-------------\n")
			playerTurn
			computerTurn()
			println
			printBoard
			turn += 1
			println("\n\n")
		}

		if (win) {
			println("Congratulations! You won!")
		} else {
			println("You lost. Better luck next time!")
			println("The computer's code was " + computerCode.mkString)
		}
		println("\n\n")
	}

	/**
	*	computerTurn
	*
	*	Processes the computer's turn; selects pins or provides hint markers.
	*/
	def computerTurn(isFirstTurn: Boolean = false): Unit = {
		if (isFirstTurn) {
			selectComputerPins
			return
		}

		// Determine hint markers for latest guess
		markers(turn) = checkGuess
	}

	/**
	*	playerTurn
	*
	*	Processes the player's turn; prompts player for code guess and persists data.
	*/
	def playerTurn(): Unit = {
		// Determine player guess
		guesses(turn) = promptGuess
	}

	/**
	*	printBoard
	*
	*	Displays the game board after player has guessed.
	*/
	def printBoard(): Unit = {
		println("\nBoard:\n")

		for (i <- turn to 0 by -1) {
			println(guesses(i).mkString + ",\t\t" + markers(i).mkString)
		}
	}

	/**
	*	selectComputerPins
	*
	*	Selects the computer's pins and alerts the player when completed.
	*/
	def selectComputerPins(): Unit = {
		for (i <- 0 until computerCode.length) {
			computerCode(i) = colorChoices((math.random * colorChoices.length).asInstanceOf[Int])
		}

		println("Computer has selected its pins.\n")
	}

	/**
	*	promptGuess
	*
	*	Prompts user to input their guess of the computer's code.
	*
	*	return: (Array[Char]) player's valid guess
	*/
	def promptGuess(): Array[Char] = {
		var guess = ""

		do {
			println("Select from: " + "[" + colorChoices.mkString(", ") + "]")
			print("Enter a valid guess (e.g. " + colorChoices.dropRight(colorChoices.length - numPinsPlayable).mkString + ")\t> ")
			guess = stripGuess(input.nextLine)
		} while (!guessIsValid(guess))

		return guess.toCharArray
	}

	/**
	*	guessIsValid
	*
	*	Determines whether a given guess is of valid pins.
	*
	*	param: (String) player's guess
	*	return: (Boolean) whether player's guess is valid
	*/
	def guessIsValid(guess: String = null): Boolean = {
		if (guess == null || guess.length != numPinsPlayable) {
			return false
		}

		return true
	}

	/**
	*	stripGuess
	*
	*	Collects all valid pins from a guess.
	*
	*	param: (String) player's guess
	*	return: (Array[Char]) all valid pins player's guess
	*/
	def stripGuess(guess: String = null): String = guess.toUpperCase.toCharArray.filter(colorChoices.contains(_)).mkString

	/**
	*	checkGuess
	*
	*	Derives markers associated with a player's guess and determines win or loss per guess.
	*
	*	return: (Array[Char]) markers associated with a guess
	*/
	def checkGuess(): Array[Char] = {
		if (guesses(turn).deep == computerCode.deep) {
			gameOver = true
			win = true
		}

		return configureHints
	}

	/**
	*	configureHints
	*
	*	Determines hints associated with a player's guess.
	*
	*	return: (Array[Char]) markers associated with a guess
	*/
	def configureHints(): Array[Char] = {
		val hints = new ArrayBuffer[Char]
		val matched = new ArrayBuffer[Int]

		for (i <- 0 until guesses(turn).length) {
			if (guesses(turn)(i) == computerCode(i)) {
				hints += 'o'
				matched += i
			}
		}

		for (i <- 0 until guesses(turn).length) {
			for (k <- 0 until computerCode.length) {
				if (!matched.contains(i) && guesses(turn)(i) == computerCode(k)) {
						hints += '-'
						matched += i
				}
			}
		}

		val remaining = numPinsPlayable - hints.length
		for (i <- 0 until remaining) {
			hints += 'x'
		}

		return hints.toArray
	}
}
