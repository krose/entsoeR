
#' TZ mappings.
#' 
#' inspired by the entsoe-py
#' 
tz_mappings <- function(){
  
  TIMEZONE_MAPPINGS = tibble::tribble(
    ~short_name, ~long_name,
    'AL', 'Europe/Tirane',
    'AT', 'Europe/Vienna',
    'BA', 'Europe/Sarajevo',
    'BE', 'Europe/Brussels',
    'BG', 'Europe/Sofia',
    'BY', 'Europe/Minsk',
    'CH', 'Europe/Zurich',
    'CZ', 'Europe/Prague',
    'DE', 'Europe/Berlin',
    'DK', 'Europe/Copenhagen',
    'EE', 'Europe/Talinn',
    'ES', 'Europe/Madrid',
    'FI', 'Europe/Helsinki',
    'FR', 'Europe/Paris',
    'GB', 'Europe/London',
    'GB-NIR', 'Europe/Belfast',
    'GR', 'Europe/Athens',
    'HR', 'Europe/Zagreb',
    'HU', 'Europe/Budapest',
    'IE', 'Europe/Dublin',
    'IT', 'Europe/Rome',
    'LT', 'Europe/Vilnius',
    'LU', 'Europe/Luxembourg',
    'LV', 'Europe/Riga',
    # 'MD', 'MD',
    'ME', 'Europe/Podgorica',
    'MK', 'Europe/Skopje',
    'MT', 'Europe/Malta',
    'NL', 'Europe/Amsterdam',
    'NO', 'Europe/Oslo',
    'PL', 'Europe/Warsaw',
    'PT', 'Europe/Lisbon',
    'RO', 'Europe/Bucharest',
    'RS', 'Europe/Belgrade',
    'RU', 'Europe/Moscow',
    'RU-KGD', 'Europe/Kaliningrad',
    'SE', 'Europe/Stockholm',
    'SI', 'Europe/Ljubljana',
    'SK', 'Europe/Bratislava',
    'TR', 'Europe/Istanbul',
    'UA', 'Europe/Kiev'
  )
  
  TIMEZONE_MAPPINGS
}