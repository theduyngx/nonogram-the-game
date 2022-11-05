# NONOGRAM - The Game

## About
**NONOGRAM - The Game** is a Haskell interactive puzzle game revolving around the popular Japanese puzzle
nonograms. Players can input any puzzle they like, and the program will make sure that the puzzle is truly
solvable before players can proceed any further.

About nonogram, the rules are simple. For each row / column, there are 'hints', which we will refer to as row
hint and column hint. These hints will specify the layout the the row / column that players must follow.
For instance, let's take a 10 x 8 puzzle, meaning any given row should be of size 10. Here is an example of the
initial state of said row (initial state being fully unmarked):

``_ _ _ _ _ _ _ _ _ _``  (hint: `2 1 3`)

The row's hint reads ``2 1 3``, and this is the format to be followed: "*from left to right, somehow mark 2 `X`
adjacent to each other, followed by a single `X` that's separated by 1 or more unmarked spaces `_`, followed
by 3 adjacent marks separated by 1 or more unmarked spaces `_`*". Here are some possible combinations:

``X X _ X _ X X X _ _``  (hint: `2 1 3`)

``_ X X _ X _ X X X _``  (hint: `2 1 3`)

``_ X X _ _ X _ X X X``  (hint: `2 1 3`)

In more concise terms, we're trying to create some combinations with given hints such that the format matches with
the hint (where each marked group must be separated by at least 1 unmarked space). The same goes for columns as well
(but instead of from left to right like column hints, we now consider from top to bottom).
Fully solved puzzle will be given in Usage segment below.

## Usage
### Running the program
To interact with the program, run ``main`` function in module ``Main``:
```
ghci> main
```
or run the entire stack on the correct directory:
```
/directory/> stack run
```
to which the program will show the home screen, with avaiable help to instruct players about the available commands.
Players can do this by using the command '`help`' any time.

### Starting the game
By entering '`start`', the user will have started the game. At which point the game will ask for more specific inputs.
These inputs are: `Row hints` and `Column hints`, and as stated above, these are self-explanatorily the hints for each
row and column. The format of the hints is a list of integer sublists:

``[[1,2],[2,3],[1],[1,1,1]]``
(*An example of the format. If this is a column hint, then it means the first column must follow the format `1 2`, and
so on. This also implies that the column size of the puzzle is `4`, which is the number of sublists in the column hint.*)

Here's what the program will look like while running:
```
Enter the hints for row and column:
    -- Row hint:
[[1,2],[1],[2],[2,2],[3]]
    -- Column hint:
[[1,2],[2,3],[1],[1,1,1],[3]]
```

After then, the program will check if the puzzle has a solution or not. If not, it will prompt a message and ask for a
different input. If the puzzle does have a solution, the player will be proceeded to start solving it.

### Solving the puzzle
The game follows the rule of a nonogram. You can mark X to any position within the board, and also remove them (if
position is actually marked). These commands are, respectively, `add x y` and `delete x y` where `x` and `y` are x-y
positions of the point you want to add/remove. You can, however, abbreviate the commands by simply typing `a 1 3` or
`d 2 5`. Other commands cannot be abbreviated, and they should all appear in the help list. Though it should be noted
that solutions can be revealed if asked even before completing the puzzle. This is achieved by simply entering `reveal`
command at any given moment during the game.

Once you've managed to solve the puzzle, the program will prompt a congratulation message and ask whether you'd like to
continue playing or not. Continue playing means inputting another puzzle into the program.

Here is an example of a fully completed nonogram:
```
     Initial                            Completed
|_|_|_|_|_|_|_|_| 3                 |_|X|X|X|_|_|_|_| 3
|_|_|_|_|_|_|_|_| 2 1               |X|X|_|X|_|_|_|_| 2 1
|_|_|_|_|_|_|_|_| 3 2               |_|X|X|X|_|_|X|X| 3 2
|_|_|_|_|_|_|_|_| 2 2               |_|_|X|X|_|_|X|X| 2 2
|_|_|_|_|_|_|_|_| 6                 |_|_|X|X|X|X|X|X| 6
|_|_|_|_|_|_|_|_| 1 5     ---->     |X|_|X|X|X|X|X|_| 1 5
|_|_|_|_|_|_|_|_| 6                 |X|X|X|X|X|X|_|_| 6
|_|_|_|_|_|_|_|_| 1                 |_|_|_|_|X|_|_|_| 1
|_|_|_|_|_|_|_|_| 2                 |_|_|_|X|X|_|_|_| 2
 1 3 1 7 5 3 4 3                     1 3 1 7 5 3 4 3
 2 1 5 1                             2 1 5 1 
```

Enjoy the game!
