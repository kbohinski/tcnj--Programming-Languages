Kevin Bohinski & Evan Melquist
10/9/16
Updated 10/20/16
CSC 435 Programming Languages

Project 2: Part 1
Parser Generator

README File
  File Structure:
    The tree of the project is below.
    | input.txt
    | main.cpp
    | P2-part1.pdf
    | readme.txt
    \-scanner
        | driver.cc
        | scan.cc
        | scan.h
        | Makefile
    
    Our source is available in main.cpp, the scanner is within the scanner folder.

  Instructions to Run:
    The program can be compiled via g++, and accepts a input file via cin. The simplest way to do this is via bash redirection. g++ will create an executable in the same folder, to run please use file redirection in conjunction with the executable.
    
    To compile and run, at the root of the project, run the following:
    g++ -std=c++11 main.cpp -o a.exe && .\a.exe < input.txt

  Conventions:
    We attempted to follow the conventions found in the C++ language documentation, and the C++ standard library.

  Input/Output:
    Our application requires input.
        A sample input file can be found at input.txt.
	Please refer to the format of this file.
	Please use file redirection for simplicity when running our program.  An example is below:
	.\a.exe < input.txt
    Our application prints to stdout & stderr.

  Limitations & Bugs:
    Limitations:
		We are unsure if error handling is fully complete.
		File should have been split into many for organization.
		Keeping everything in the main method is highly undesirable.
    Bugs:
		No bugs that we know of.

  Completed Functionality:
    These programs are completed and functional as per the spec (ignoring question of correctness regarding error handling).

  Justification of Abstractions, Data Structures, Algorithms, and Design Decisions:
    Our application reads in the file and saves the terminals into a map, as well as the non terminals. We use maps over vectors or standard arrays due to the O(1) lookup time, as well as simplify lookup. We then form the productions in the data structure of map<int, vector<vector<int>>> in order to maintain the empty sets. With the productions numbered rather than named, we are then able to generate the first and follow sets, which use the algorithms discussed during lecture. The first and follow sets are stored in map<int, set<int>> data structures to prevent duplicates.
    With the first, and follow sets generated we can then generate the parse table which is stored as a two dimensional array of ints. We use this over vectors as we have a constant size. 
    From here we use both a parse stack and a parse queue to consume the input. We follow the algorithm in the book.
    Our applications use the std library.
