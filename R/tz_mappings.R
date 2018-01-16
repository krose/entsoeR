#' TZ mappings.
#' 
#' inspired by the entsoe-py
#' 
#' @param area_code Two digit area code for the timezone.
tz_mapping <- function(area_code){
  
  if(length(area_code) > 1){
    stop("The param area_code has to be length one.", call. = FALSE)
  }
  
  timezone_mapping <- tibble::tribble(
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
  
  timezone_mapping <- timezone_mapping$long_name[timezone_mapping$short_name == area_code]
  
  if(length(timezone_mapping) < 1){
    stop("The area code is not supported.", call. = FALSE)
  } 
  
  timezone_mapping
}
