Column definition
nest    unique identification of the nest
year    year when the nest was active
first_egg   date of the first egg (yyyy-mm-dd)
start_expo  date and time when the nest was found (yyyy-mm-dd hh:mm:ss)
last_ok     date and time (yyyy-mm-dd hh:mm:ss) of the last visit when the nest was active (for the predated nests with logger is the same as the "end_expo" and in an update this shall be the case also for hatched nests with loggers)
last_control    date and time (yyyy-mm-dd hh:mm:ss) of the last (final) visit of the nest (visit when fate was determined)
end_expo    end datetime (yyyy-mm-dd hh:mm:ss) of the nest; for predated nests precise end based on logger data, else a midpoint between last_ok and last_control, for hatch nests the estimated hatch date based on laying or flotation or midpoint between last_ok and last_control (whichever is smaller) 
fate        was the nest predated (0) or did it hatch (1), was deserted (2) or unknown fate (5); 2 and 5 survived for the time of exposure so for some models could be use as 1
logger_fate     is the fate based on the logger data
temperature     surface temperature when a logger nest was predated


According to my understanding in this dataset the predated nests shall have last_ok = last_control = last_expo. Or is it intended that last_ok and last_control are always true visits and last_expo is either calculated or derived from the loggers? If so, this shall then be clear.
Last, but not least, we are missing the Logger table (see our DB_draft)
