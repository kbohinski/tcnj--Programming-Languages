require 'date'

class Weekend_Calculator

=begin
  class method for jumping current date to next year
  @input #date to be incremented
=end

  def self.next_year(input)
    if input.month == 1
      input = input >> 12
    elsif input.month == 3
      input = input >> 10
    elsif input.month == 5
      input = input >> 8
    elsif input.month == 7
      input = input >> 6
    elsif input.month == 8
      input = input >> 5
    elsif input.month == 10
      input = input >> 3
    elsif input.month == 12
      input = input >> 1
    end
    return input
  end

=begin
    class method for jumping current date to next month with 31 days
    @input #date to be incremented
=end

  def self.next_month(input)
    if input.month == 1
      input = input >> 2
    elsif input.month == 3
      input = input >> 2
    elsif input.month == 5
      input = input >> 2
    elsif input.month == 7
      input = input >> 1
    elsif input.month == 8
      input = input >> 2
    elsif input.month == 10
      input = input >> 2
    elsif input.month == 12
      input = input >> 1
    end
    return input
  end

  current_date = Date.new(2000, 1, 1)
  valid_years = Array.new
  invalid_years = Array.new

=begin
  Loop that goes through each year and finds all month with 31 days that begin on a Friday
  If month has 5 weekends it's added to invalid_years array
=end

  while current_date.year.to_int <= 2100 do
    if current_date.friday?
      invalid_years.push(current_date.year.to_int)
      current_date = next_year(current_date)
    else
      current_date = next_month(current_date)
    end
  end

  counter = 2000

=begin
  Loop that adds all years not in the invalid_years array to the valid_years array
=end

  while counter <= 2100 do
    if invalid_years.include?(counter)
      counter += 1
    else
      valid_years.push(counter)
      counter += 1
    end
  end

  puts valid_years

end