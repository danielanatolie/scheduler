

# scheduler

Inspired by common industry problems that deal with using a limited resource (for example: hotel, flight booking, and room scheduling), our Haskell service provides various information about interval data such as conflict detection and resource use minimisation.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

```
ghci
```

### Installing

Before running main, please ensure you have a .txt file that is ready to be processed. Each row of the test file should follow the following format
```
START_TMIE, END_TIME
```
Each .txt file represents a 24 hour day in which the times are restricted to natural numbers between 0 and 24. 
After your file is ready, run: 
```
ghci
:l main
main
```
Your data analysis result will be computed and stored in: output.txt

Here is an example input and output:

INPUT:
```
1,2
3,5
5,11
11,15
13,17
```

OUTPUT:
```
Hello! 
Here are your results: 
Original Data: [(1,2),(3,5),(5,11),(11,15),(13,17)]
Free Time: 7 hours between 17 and 24
Conflicts: A conflict occurs between (11,15) and (13,17).
Merged Interval Schedule: [(1,2),(3,17)]
Thank you for using Scheduler! 
```

## Running the tests

The tests folder contain test cases for each of the files, use ghci to run them individually.
