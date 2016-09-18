-- Kevin Bohinski & David Vassallo
-- 9/19/16
-- CSC 435 Programming Languages

-- Project 1
-- Print all years between 2000 -> 2100 that do not have any month with 5 weekends.

-- five_weekends.adb

-- Imports for calendar and printing to console.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar.Formatting; use Ada.Calendar;
use Ada.Calendar.Formatting;

-- Create procedure per spec.
procedure five_weekends is
start_year : Integer := 2000;
end_year : Integer := 2100;
curr_year : Integer := start_year;
months_in_year : Integer := 12;
curr_month : Integer := 1;
begin
  -- Iterate through all years.
  while curr_year <= end_year loop
    curr_month := 1;
    
    -- Iterate through this year's months.
    while curr_month <= months_in_year loop
      begin
        -- If the month's 31st day is a Sunday, then break as this year has a month with 5 weekends.
        if Day_Of_Week (Formatting.Time_of (curr_year, curr_month, 31)) = Sunday then
          exit;
        end if;
      exception
        -- Catch error for months without 31 days.
        when Time_Error =>
	  null;
      end;

      curr_month := curr_month + 1;
    end loop;

    -- If the loop did not break, print year to console.
    if curr_month = 12 then
      Put_Line (Integer'Image (curr_year));
    end if;

    curr_year := curr_year + 1;
  end loop;
end five_weekends;
