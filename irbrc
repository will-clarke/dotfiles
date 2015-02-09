# If pry is available, lets kick that off...
begin
  Pry.start || exit
rescue NameError
  # just using irb - no pry
end
