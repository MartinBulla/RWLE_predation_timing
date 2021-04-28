Column definitions 

nest_data.txt contain data on each nest used in the paper
nest    unique identification of the nest
year    year when the nest was active
first_egg   estimated date of the first egg (yyyy-mm-dd)
start_expo  date and time when the nest was found (yyyy-mm-dd hh:mm:ss), indicates start of the observation period used in the survival analyses
last_ok     date and time (yyyy-mm-dd hh:mm:ss) of the last physical visit when the nest was active, or the last time when the nest was active based on logger recordings (note that terminal event such as predation or one chick hatched indicate the end of the time when a nest was active and hence precise last_ok datetime for logger nests)
last_visit    date and time (yyyy-mm-dd hh:mm:ss) of the last (final) visit of the nest (visit when fate was determined)
end_expo    date and time when the incubation has ended (yyyy-mm-dd hh:mm:ss), estimated as indicated in "end_type"
fate        was the nest predated (0) or did it hatch (1), was deserted or failed for another reasons (2) or has unknown fate (5); 2 and 5 survived for the time of exposure so for some models can be used as 1
end_type    indicates how end_expo is calculated

- for a predated nest with loggers end_expo is based on the logger data (logger) and represents precise date and time, i.e. end_expo equals last_ok 

- for a predated nest without loggers end_expo represents the mid-time between the last_ok and last_visit, unless expected hatch date was earlier, in which case the end_expo represent expected hatch date (half_rule). If last_ok and predation event were after expected hatch date end_expo represents last_ok plus one day (last_ok+1)

- for a successful nest with loggers end_expo is based on the logger data, i.e. on the precise time when parents left the nest with chicks, and estimated as last_ok minus one day (logger_min1day), or if we found both eggs and chicks in a nest (a partly hatched nest) end_expo is based on last_ok minus half a day (logger_minhalfday)

- for a successful nest without loggers end_expo is based on the visit when all chicks were found in/near a nest and estimated as last_visit minus 1-day (visit_min1day), based on the visit where we found both eggs and chicks in a nest (a partly hatched nest) and estimated as last_visit minus 0.5 day (visit_minhalfday) or based on the expected date of hatching (hde), unless last_ok was after expected hatch date as then end_expo represents last_ok plus one day (last_ok+1); note hde and last_ok+1 represent cases were tiny-eggshell pieces (indicating hatching) were found on the nest and/or parents were seen witch chicks of unknown age

- for successful nests found at hatching start_expo = end_expo = last_visit; these nests are not used in the survival analyses (found_at_hatching)

- for deserted nests and nests with unknown fate end_expo equals last_ok, which is based on visits (visit) or logger data (logger)

lat   the latitude of the nest
lon   the longitude of the nest
start_type indicates how the first_egg is calculated

- for nests found with incomplete clutch first_egg is calculated by subtracting the number of eggs x 1.5 days from the date when the nest was found (laying)

- for nests found with complete clutch first_egg is calculated by floatation method (float)

- for nests found during hatching first_egg is calculated by subtracting the 30 days from the date when the nest was found (hatching)

- remains unknown for nests with unknown first_egg (NA)

predation_logger indicates the type of the datalogger used for the determination of the time of the predation (for nests used for the analysis of the timing of predation)

- "RFID/T" for nests monitored during the predation event by both the RFID device and the temperature/humidity datalogger

- "T" for nests monitored during the predation event only by the temperature/humidity datalogger

- "RFID_1" for nests monitored during the predation event only by the RFID device with only 1 marked parent

- "RFID_2" for nests monitored during the predation event only by the RFID device with both marked parents

chicks_found indicates whether at least one chick assigned to the nest was ringed at any time after hatching ("yes") or not ("no")
remarks adds additional information, e.g. whether the nest was 'deserted after partial predation', or we found 'egg remains on the nest' after the predation.



temperatures.txt contains ground temperature measurements based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.
hr hour of the day
date date in format yyyy-mm-dd hh
median_t median hourly temperature (°C) based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.
mean mean hourly temperature (°C) based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.
min min hourly temperature (°C) based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.
max max hourly temperature (°C) based on all ground temperature recordings (next to the nests) from the given hour in the whole study area.

logger_data.txt contains information about all loggers used on nests
nest    unique identification of the nest
datetime_start    date and time when the logger started on the nest (yyyy-mm-dd hh:mm:ss)
datetime_end   date and time when the logger ended on the nest (yyyy-mm-dd hh:mm:ss)
logger      indicates type of the logger: dht (CZU designed temperature and humidity logger measuring in 1s intervals), dvr (video recorder), lup_egg, rfid (Max Planck designed), tnt (TinyTag temperature logger recording at 1min interval), zajda_rfid (CZU designed rfid with temperature and humidity measurements), zajda_thm
length  period (in days), for which the logger was recording on the nest