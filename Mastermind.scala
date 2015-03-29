import java.util.Scanner;

object Mastermind {
	val input = new Scanner(System.in)

	def main(args: Array[String]): Unit = {
		writeInstructions
		println
		promptGameStart
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
		println("You will have 8 turns to find the computer's code.")
		println("Every turn, you will choose a permutation of four pins to match against the computer's choice.")
		println("You win if you can guess the code in 8 turns or less!")
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
			println("What would you like to do?")
			println("Enter `y` to start the game.")
			println("Enter `n` or `q` to quit the game.")
			print("Start the game?\t> ")
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

	}
}
