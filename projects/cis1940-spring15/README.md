# README

Workspace for:

https://www.seas.upenn.edu/~cis1940/spring15/lectures.html

## Chap02

### Exercise 1

In this exercise you will implement a function that returns the number of exact matches between the secret code and the codebreaker’s guess.

```haskell
exactMatches :: Code -> Code -> Int
```

Example:

```haskell
exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] == 0
```

Example:

```haskell
exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] == 2
```

### Exercise 2

Now you will write a function that returns the number of total matches between the secret code and the guess.
This is a little bit more complicated than finding exact matches because we have to take in to account duplicate colors.
For example, the sequences [Red, Red, Blue, Blue] and [Red, Red, Green, Green] only have 2 matches even though both of the Red pegs in the secret match both of the Red pegs in the guess.
For this reason, we can’t just scan the first list and count the occurences of each element in the second list.
Instead, we will count the number of times that each peg appears in both lists and sum the minimum values for each color.
In the example above, Red and Blue both occur twice and all of the other colors never appear in the first list and Red and Green occur twice in the second list.
The number of matches is therefore 2 since Red occurs twice in both lists and all the other colors are not in both of the lists.
Before you can count the number of matches, you should implement the helper function:

```haskell
countColors :: Code -> [Int]
```

This function takes in a Code and returns a list containing the numbers of times that each color in the list colors appears in the Code.
The counts should appear in the same order that they occur in the list colors.
As a sanity check, output should always have length 6 and the sum of all the entries should be equal to length Code.

Example:

```haskell
countColors [Red, Blue, Yellow, Purple] == [1, 0, 1, 1, 0, 1]
```

Example:

```haskell
countColors [Green, Blue, Green, Orange] == [0, 2, 1, 0, 1, 0]
```

Now you are ready to implement the main function:

```haskell
matches :: Code -> Code -> Int
```

Example:

```haskell
matches [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == 3
```

### Exercise 3

A Move is a new datatype that is constructed with a Code and two Ints.
The first Int is the number of exact matches that the Code has with the secret and the second Int is the number of nonexact matches 1.

Implement the function:

```haskell
getMove :: Code -> Code -> Move 
```

The first Code is the secret, the second Code is guess, and the output is the resulting Move.

Example:

```haskell
getMove [Red, Blue, Yellow, Orange] [Red, Orange, Orange, Blue] == Move [Red, Orange, Orange, Blue] 1 2
```

### Exercise 4

We will now define a concept that will be important in playing the Mastermind game.
This is the concept of consistency; we say that a Code is consistent with a Move if the Code could have been the secret that generated that move.
In other words, if the guess inside the Move has the same number of exact and non-exact matches with the provided Code as it did with the actual secret, then the Code is consistent with the Move.

Define the function:

```haskell
isConsistent :: Move -> Code -> Bool
```

Example:

```haskell
isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Yellow, Purple] == True
```

Example:

```haskell
isConsistent (Move [Red, Red, Blue, Green] 1 1) [Red, Blue, Red, Purple] == False
```

### Exercise 5

Now that we have the concept of consistency, we can filter a list of Codes to only contain those that are consistent with a given Move.
This will be useful to us since our game solver will start with a list of all possible codes and gradually filter the list based on each new move until there is only one code left.
Implement the function:

```haskell
filterCodes :: Move -> [Code] -> [Code]
```

### Exercise 6

As mentioned in Exercise 5, the final algorithm will start with a list of all possible codes and filter out the inconsistent ones.
In order to do this, we first need to be able to generate a list of all the codes, ie all length n combinations of the 6 colors.
In general, Mastermind games use codes of length 4, however in theory the code could be any length.
We have not yet made any assumptions about the lengths of the codes, so why start now?
Your function should take in a length2 and return all Codes of that length:
2 Haskell’s type system is strong enough to encode the length of a list in its type, however it uses an advanced feature of Haskell called Generalized Algebraic Datatypes which is beyond the scope of this homework.
Using this feature, we could write allCodes without giving the length as an input.

```haskell
allCodes :: Int -> [Code]
```

Hint:

This exercise is a bit tricky.
Try using a helper function that takes in all the codes of length n − 1 and uses it to produce all codes of length n.
You may find the concatMap function helpful.


#### work

filterCodes
    (Move [Blue, Red, Green, Yellow] 0 2)
    [
          [Red, Blue, Green, Yellow],         0 4
          [Blue, Green, Yellow, Red],         0 4
          [Red, Purple, Green, Orange],       0 2
          [Yellow, Yellow, Green, Blue],      0 4
          [Red, Red, Green, Yellow]           0 4
    ]

