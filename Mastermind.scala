import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Mastermind {
	// Input manager
	val input = new Scanner(System.in)
	// Max # of guesses a player can make
	val maxTurns = 10

	// All player guesses
	val guesses = Array.ofDim[Char](maxTurns, 4)
	// All markers for player guesses
	val markers = Array.ofDim[Char](maxTurns, 4)

	// Pin choices
	val colorChoices = Array('R', 'G', 'B', 'O', 'P', 'Y')
	// Computer pins selection
	val computerCode = new Array[Char](4)

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
		println("The red pin is represented by `R`")
		println("The green pin is represented by `G`")
		println("The blue pin is represented by `B`")
		println("The orange pin is represented by `O`")
		println("The purple pin is represented by `P`")
		println("The yellow pin is represented by `Y`")
		println
		println("The computer will choose a permutation of four colored pins.")
		println
		println("You will have " + maxTurns + " turns to find the computer's code.")
		println("Every turn, you will choose a permutation of four pins to match against the computer's choice.")
		println("You win if you can guess the code in " + maxTurns + " turns or less!")
		println
		println("In order to help you out, you will have four hint markers.")
		println("After every turn, you will be given the four markers, which help indicate if your choices are correct.")
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
		if (response.equals("n") || response.equals("q")) System.exit(1)
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

	def computerTurn(isFirstTurn: Boolean = false): Unit = {
		if (isFirstTurn) {
			selectComputerPins
			return
		}

		// Determine hint markers for latest guess
		markers(turn) = checkGuess
	}

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
			var display = ""
			for (pin <- guesses(i))
				display += pin
			display += ",\t\t"
			for (marker <- markers(i))
				display += marker
			println(display)
		}
	}

	/**
	*	selectComputerPins
	*
	*	Selects the computer's pins and alerts the player when completed.
	*/
	def selectComputerPins(): Unit = {
		for (i <- 0 until computerCode.length)
			computerCode(i) = colorChoices((math.random * colorChoices.length).asInstanceOf[Int])
		println("Computer has selected its pins.\n")
		println(computerCode.mkString)
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
			println("Select from: [`R`, `G`, `B`, `O`, `P`, `Y`]")
			print("Enter a valid guess (e.g. `ROPY`)\t> ")
			guess = input.nextLine
		} while (!guessIsValid(guess))
		return stripGuess(guess)
	}

	/**
	*	guessIsValid
	*
	*	Determines whether a given guess is of 4 valid pins.
	*
	*	param: (String) player's guess
	*	return: (Boolean) whether player's guess is valid
	*/
	def guessIsValid(guess: String = null): Boolean = {
		if (guess == null) return false
		val trimmedGuess: Array[Char] = stripGuess(guess)
		if (trimmedGuess.length != 4) return false
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
	def stripGuess(guess: String = null): Array[Char] = return guess.toUpperCase.toCharArray.filter(colorChoices.contains(_))

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
		return determineMarkers
	}

	/**
	*	determineMarkers
	*
	*	Determines markers associated with a player's guess.
	*
	*	return: (Array[Char]) markers associated with a guess
	*/
	def determineMarkers(): Array[Char] = {
		val markers = new Array[Char](guesses(turn).length)
		val matches = new ArrayBuffer[Int]
		for (i <- 0 until guesses(turn).length)
			if (guesses(turn)(i) == computerCode(i)) {
				markers(i) = 'X'
				matches += i
			}
		for (i <- (0 until guesses(turn).length).filterNot(matches.contains(_))) {
			for (k <- 0 until computerCode.length) {
				if (guesses(turn)(i) == computerCode(k)) {
					markers(i) = 'W'
					matches += i
				}
			}
		}
		return markersSort(markers)
	}

	/**
	*	determineMarker
	*
	*	Determines the appropriate marker for a pin in a player's guess.
	*
	*	param: (Int) index of considered pin
	*	return: (Char) marker for a pin
	*/
	def determineMarker(i: Int, matched: Array[Int]): Char = {
		if (guesses(turn)(i) == computerCode(i))
			return 'X'
		for (k <- 0 until computerCode.length)
			if (guesses(turn)(i) == computerCode(k) && !matched.contains(k)) {
				return 'W'
			}
		return 'E'
	}

	/**
	*	markersSort
	*
	*	Sorts the markers determined by accuracy.
	*
	*	param: (ArrayBuffer[Char]) unranked set of markers
	*	return: (Array[Char]) markers ranked from most accurate to least
	*/
	def markersSort(markers: Array[Char]): Array[Char] = {
		val buffer = new ArrayBuffer[Char]
		for (i <- 0 until markers.length) {
			if (markers(i) == 'X')
				buffer.insert(0, 'X')
			else if (markers(i) == 'W')
				buffer.insert(buffer.lastIndexOf('X') + 1, 'W')
			else
				buffer += 'E'
		}
		if (buffer.length > guesses(turn).length)
			buffer.trimEnd(buffer.length - guesses(turn).length)
		else if (buffer.length < guesses(turn).length)
			for (i <- 0 until (guesses(turn).length - buffer.length))
				buffer += 'E'
		return buffer.toArray
	}
}
