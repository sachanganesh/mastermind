import java.util.Scanner
import scala.collection.mutable.ArrayBuffer

object Mastermind {
	val input = new Scanner(System.in)
	val maxTurns = 10

	val guesses = Array.ofDim[Char](8, 4)
	val markers = Array.ofDim[Char](8, 4)
	val colorChoices = Array('R', 'G', 'B', 'O', 'P', 'Y')
	val computerCode = new Array[Char](4)

	var turn = 0
	var gameOver = false
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
		println("The white marker is represented by `W`. It means you have a correct pin, but it is in the wrong position.")
		println("The black marker is represented by `X`. It means you have the correct pin, in the correct position.")
		println("The empty marker is represented by `E`. It means you have an incorrect pin.")
		println
		println("OBJECTIVE: Guess the permutation that the computer chose in 8 turns or less.")
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
		chooseCode
		while (win == false || turn >= maxTurns) {
			turn += 1
			println("Turn #" + turn)
			println("-------------\n")
			var guess = promptGuess
			for (i <- 0 until guesses(turn - 1).length)
				guesses(turn - 1)(i) = guess(i)
			var markersForGuess = checkGuess
			for (i <- 0 until markers(turn - 1).length)
				markers(turn - 1)(i) = markersForGuess(i)
			println
			printBoard
			println("\n\n")
		}
		if (win)
			println("Congratulations! You won!")
		else
			println("You lost. Better luck next time!")
		println("\n\n")
	}

	/**
	*	printBoard
	*
	*	Displays the game board after player has guessed.
	*/
	def printBoard(): Unit = {
		println("\nBoard:\n")
		for (i <- turn - 1 to 0 by -1) {
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
	*	chooseCode
	*
	*	Selects the computer's pins and alerts the player when completed.
	*/
	def chooseCode(): Unit = {
		for (i <- 0 until computerCode.length)
			computerCode(i) = colorChoices((math.random * colorChoices.length).asInstanceOf[Int])
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
	*	Collects all valid pins in a guess.
	*
	*	param: (String) player's guess
	*	return: (Array[Char]) all valid pins player's guess
	*/
	def stripGuess(guess: String = null): Array[Char] = guess.toUpperCase.toCharArray.filter(colorChoices.contains(_))

	/**
	*	checkGuess
	*
	*	Derives markers associated with a player's guess and determines win or loss per guess.
	*
	*	return: (Array[Char]) markers associated with a guess
	*/
	def checkGuess(): Array[Char] = {
		if (guesses(turn - 1).deep == computerCode.deep) {
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
		val markers = new ArrayBuffer[Char]
		for (i <- 0 until guesses(turn - 1).length)
			markers += determineMarker(i)
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
	def determineMarker(i: Int): Char = {
		println(computerCode.mkString(", "))
		println("!!!! - " + guesses(turn - 1)(i))
		if (guesses(turn - 1)(i) == computerCode(i)) 
			return 'X'
		for (k <- 0 until computerCode.length)
			if (guesses(turn - 1)(i) == computerCode(k))
				return 'W'
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
	def markersSort(markers: ArrayBuffer[Char]): Array[Char] = {
		val buffer = new ArrayBuffer[Char]
		for (i <- 0 until markers.length) {
			if (markers(i) == 'X')
				buffer.insert(0, 'X')
			else if (markers(i) == 'W')
				buffer.insert(buffer.lastIndexOf('X') + 1, 'W')
			else
				buffer += 'E'
		}
		return buffer.toArray
	}
}
