David Vassallo & Kevin Bohinski
9/19/16
CSC 435 Programming Languages

Project 1 - "Five Weekends"
Description:
Print all years between 2000 -> 2100 that do not have any month with 5 weekends.

README File
  Instructions to Run:
    Ada:
      gcc -c five_weekends.adb
      gnatbind five_weekends
      gnatlink five_weekends
      gnatmake five_weekends.adb
      ./five_weekends.exe
    Clojure:
      Please install leiningen then run:
      lein run
    C#:
      Using Mono:
        mcs 5WeekendCalculator.cs
        ./5WeekendCalculator.exe
      Using Microsoft.NET:
        C:/Windows/Microsoft.NET/Framework64/v3.5/csc.exe /target:exe /out:5WeekendCalculator.exe 5WeekendCalculator.cs
        ./5WeekendCalculator.exe
    Ruby:
      ruby weekend_calculator.rb

  Conventions:
    We attempted to follow the conventions found in the respective language documentation.

  Input/Output:
    None of our applications require input.
    All of our applications print to stdout.

  Limitations & Bugs:
    There are no bugs or limitations that we know of.

  Completed Functionality:
    These programs are completed and functional as per the spec.

  Justification of Abstractions, Data Structures, Algorithms, and Design Decisions:
    Our applications use the languages standard calendaring libraries to iterate through the months with 31 days from 2000 to 2100. If the first day is a Friday or the last day is a Sunday, that month has five weekends. At this point the year is ignored as it has five weekends. With the use of this iteration we are able to print to stdout the years that do not have five weekends. The Data Structures used are arrays or dynamic arrays depending on the language, some solutions forgo this is and make use of breaks in the loops.

  Comparison of Languages:
    We found C# to be easy for us to pick up as it is similar to Java. We found Ruby to be more difficult as it had unfamiliar syntax. Clojure was the most difficult as we were unfamiliar with the functional paradigm. Ada was fairly simple, due to a website that compared its syntax to C.
    Clojure was the slowest, due to the time required to spawn the JVM. Between the other languages there was no noticeable speed difference as this was a fairly small assignment. Differences would be more pronounced in larger code bases. Theoretically the compiled languages should be faster than the interpreted languages, and the statically typed languages should be faster than dynamic typed languages.
    Aside from syntax the experiences were enjoyable with the exception of Clojure. It was frustrating to adapt to Clojure's paradigms.
