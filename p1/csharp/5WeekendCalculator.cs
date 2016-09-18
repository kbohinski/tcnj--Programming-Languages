using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using System.Globalization;

namespace _5WeekendCalculator {
    class Program {
        static void Main(string[] args) {
            DateTime CurrentDate = new DateTime(2000, 1, 1); 
            ArrayList InvalidYears = new ArrayList(); 
            ArrayList ValidYears = new ArrayList();

            //While loop that goes through each month with 31 days

            while (CurrentDate.Year < 2101) { 
                if (CurrentDate.DayOfWeek == DayOfWeek.Friday) { 
                    InvalidYears.Add(CurrentDate.Year); //
                    CurrentDate = NextYear(CurrentDate);
                }
                else {
                    CurrentDate = NextMonth(CurrentDate);
                }
            }

            //Goes through all years and adds all valid years to an ArrayList

            for (int counter = 2000; counter <= 2100; counter++) {
                if (InvalidYears.Contains(counter)) {
                    continue;
                }
                else {
                    ValidYears.Add(counter);
                }
            }

            //Outputs all years in the ValidYears ArrayList

            foreach (int Year in ValidYears) {
                Console.Out.WriteLine(Year);
            }
        }

        /*
         * Jumps to next year
         * <param name = input>DateTime object to be incremented by year</param>
         */ 

        static DateTime NextYear(DateTime input) {
            if (input.Month == 1) input = input.AddMonths(12);
            else if (input.Month == 3) input = input.AddMonths(10);
            else if (input.Month == 5) input = input.AddMonths(8);
            else if (input.Month == 7) input = input.AddMonths(6);
            else if (input.Month == 8) input = input.AddMonths(5);
            else if (input.Month == 10) input = input.AddMonths(3);
            else if (input.Month == 12) input = input.AddMonths(1);
            return (input);
        }

        /*
         * Jumps to next month containing 31 days
         * <param name = input>DateTime object that will jump to next month with 31 days</param>
         */

        static DateTime NextMonth(DateTime input) {
            if (input.Month == 1) input = input.AddMonths(2);
            else if (input.Month == 3) input = input.AddMonths(2);
            else if (input.Month == 5) input = input.AddMonths(2);
            else if (input.Month == 7) input = input.AddMonths(1);
            else if (input.Month == 8) input = input.AddMonths(2);
            else if (input.Month == 10) input = input.AddMonths(2);
            else if (input.Month == 12) input = input.AddMonths(1);
            return input;
        }
    }
}
