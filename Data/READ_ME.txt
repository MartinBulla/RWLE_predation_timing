Column definition
nest    unique identification of the nest
year    year when the nest was active
first_egg   estimated date of the first egg (yyyy-mm-dd)
start_expo  date and time when the nest was found (yyyy-mm-dd hh:mm:ss)
last_ok     date and time (yyyy-mm-dd hh:mm:ss) of the last physical visit when the nest was active, or the last time when the nest was active based on logger recordings.
last_control    date and time (yyyy-mm-dd hh:mm:ss) of the last (final) visit of the nest (visit when fate was determined)
end_expo    end datetime (yyyy-mm-dd hh:mm:ss) of the nest; for predated nests precise end based on logger data, else a midpoint between last_ok and last_control, for hatch nests the estimated hatch date based on laying or flotation or midpoint between last_ok and last_control (whichever is smaller) 
fate        was the nest predated (0) or did it hatch (1), was deserted (2) or unknown fate (5); 2 and 5 survived for the time of exposure so for some models could be use as 1
logger_fate     is the fate (predation) based on the logger data
end_type    was the end_expo determined base on the logger recording (logger), during last visit (visit), based on the expected date of hatching (hde),
            as the half between the last_ok and last_control (hals_rule), or by last_ok plus one day for the nests depredated after expected date of hatching (i.e. when 
	    neither the half-rule nor the hde approach was applicable).
temperature     surface temperature when a logger nest was predated
lat   the latitude of the nest
lon   the longitude of the nest

According to my understanding in this dataset the predated nests shall have last_ok = last_control = end_expo. 
- last control is recently the last physical control! Thus, there are some (a few) nests where last control was not writen into db, but we have data from logger
- in such nests last_ok (derived from logger) is later, than last_control... 


Or is it intended that last_ok and last_control are always true visits and last_expo is either calculated or derived from the loggers? If so, this shall then be clear.
Last, but not least, we are missing the Logger table (see our DB_draft)
there is file "logger_data.RData" - now a bit updated
nest    unique identification of the nest
datetime_start start datetime (yyyy-mm-dd hh:mm:ss) when the logger was placed to the nest
datetime_end end datetime (yyyy-mm-dd hh:mm:ss) when the logger was either collected from the nest, the memory of the logger was filled, 
             nest was depredated/hatched or abandoned, or the recording became from any reason unusable (probe took out of the nest, datalogger error,...)
logger      whether the recordings was taken by tiny tag (tnt), dht logger (dht), digital video-recorder (dvr), MPIO RFID logger (rfid), 
            rfid modul from the ZAJDA logger (zajda_rfid), temperature/humidity modul from the ZAJDA logger (zajda_thm), or fake egg (lup_egg)
length      length of used recording in days
year    year when the nest was active



