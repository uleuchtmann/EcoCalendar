#' Create a df of economic releases from the list in 'ecoitems.csv', 
#' that are due to be released between 'startTime' and 'endTime'.
#' 
#' @param ecotiems The file name with the list of items. For every item, the
#'   file must include the following variables: 'Ticker', 'Name_en', 'Unit_en',
#'   'Name_de', 'Unit_de'. File format must be csv. Defaults to 'ecoitems.csv'.
#' @param regioitems File name of a file including a translation table for 
#'   country names. For each country, the file must include the following 
#'   variables: 'REGION_OR_COUNTRY' (the Bloomberg country field), 'Region_en' 
#'   and 'Region_de' (the country descriptions in the English and German tables
#'   respectively). File format must be csv. Defaults to 'regions.csv'.
#' @param inTZ Time zone designation (Olson format) of the input, i.e. of 
#'   the Bloomberg date/time data and of the 'startTime' and 'endTime' 
#'   parameters (described below). Defaults to 'Europe/Berlin'.
#' @param outTZ Time zone designation (Olson format) of the output time
#'   variable. Defaults to 'UTC'.
#' @param startTime The starting point of the period for which economic releases 
#'   should be included in the output.
#' @param endTime The end point of the period for which economic releases 
#'   should be included in the output.
#' @returns A tibble with the following fields for each item: 'Ticker' (as 
#'   provided in 'ecoitems'), 'Date' (release date), 'Time' (release time),
#'   'Region_en' (English region description from 'regions.csv'), Region_de 
#'   (German region description from 'regions.csv'), 'Name_en' (English item 
#'   name from 'ecoitems'), 'Name_de' (German item name from 'ecoitems'), 
#'   'Unit_en' (English unit description from 'ecoitems'), 'Unit_de' (German 
#'   unit description from 'ecoitems'), 'Period' (period description 
#'   from Bloomberg), 'Survey' (median analysts' expetation from Bloomberg), 
#'   'Prior' (previous release vale from Bloomberg), 'Digits' (maximum number of
#'   decimals to display from Bloomberg), 'Source' (source information from 
#'   Bloomberg). The items are sorted in the way consistent with the Daily data 
#'   calendar.
#'   
#' @importFrom magrittr %>%
#'   
#' @export
ecocal <- function(ecoitems = "ecoitems.csv", regioitems = "regions.csv",
                   inTZ = "Europe/Berlin", outTZ = "UTC",
                   startTime = paste(Sys.Date(),   "07:00:00"),
                   endTime   = paste(Sys.Date()+2, "08:00:00")) {
  startTime <- lubridate::as_datetime(startTime, tz = inTZ)
  endTime   <- lubridate::as_datetime(endTime,   tz = inTZ)
  startDate <- lubridate::date(startTime)
  endDate   <- lubridate::date(endTime)
  foo <- readr::read_csv(ecoitems, comment = "#", col_types = readr::cols()) %>%
    # take the ticker list from 'ecoitems' and download required data 
    # for these tickers from Bloomberg
    dplyr::pull(Ticker) %>%
    Rblpapi::bdp(c("ECO_RELEASE_DT", "ECO_RELEASE_TIME", "REGION_OR_COUNTRY",
                   "OBSERVATION_PERIOD", "RT_BN_SURVEY_MEDIAN", 
                   "PREV_CLOSE_VAL", "PX_DISP_FORMAT_MAX_NUM_DEC", 
                   "INDX_SOURCE")) %>%
    # coerce bdp output to tibble:
    tibble::as_tibble(rownames = "Ticker") %>%
    # calculate proper release 'Time'; therefore
    #   replace empty cells in 'ECO_RELEASE_TIME' with NAs
    dplyr::mutate(ECO_RELEASE_TIME = dplyr::na_if(ECO_RELEASE_TIME, "")) %>%
    #   create 'Time' variable from 'ECO_RELEASE_DT' and 'ECO_RELEASE_TIME' 
    #   (while keeping NAs) with 'inTZ' time zone
    dplyr::mutate(Time = lubridate::as_datetime(
      stringr::str_c(ECO_RELEASE_DT, ECO_RELEASE_TIME, sep = " "), 
      tz = inTZ)) %>% 
    #   change TZ to 'outTZ'
    dplyr::mutate(Time = lubridate::with_tz(Time, tzone = outTZ)) %>% 
    # add 'Region_en' and 'Region_de' information from 'regions.csv'
    #   (but change 'REGION_OR_COUNTRY' variable to common spelling first)
    dplyr::mutate(REGION_OR_COUNTRY = stringr::str_to_title(REGION_OR_COUNTRY)) %>%
    dplyr::left_join(readr::read_csv("regions.csv", col_types = readr::cols()), 
                     key = REGION_OR_COUNTRY) %>%
    # Add 'Name_en', 'Unit_en', 'Name_de' and 'Unit_de' information 
    # from ecoitems
    dplyr::inner_join(readr::read_csv(ecoitems, comment = "#", 
                                      col_types = readr::cols()), 
                      by = "Ticker") %>% 
    # select variables for output
    dplyr::transmute(Ticker = Ticker, Date = ECO_RELEASE_DT, Time = Time, 
                     Region_en = Region_en, Region_de = Region_de, 
                     Name_en = Name_en, Name_de = Name_de, 
                     Unit_en = Unit_en, Unit_de = Unit_de,
                     Period = OBSERVATION_PERIOD, Survey = RT_BN_SURVEY_MEDIAN, 
                     Prior = PREV_CLOSE_VAL, 
                     Digits = PX_DISP_FORMAT_MAX_NUM_DEC,
                     Source = INDX_SOURCE) %>%
    # sort by 'Date' and 'Time' (with 'Time' == NA entries first in day-wise 
    # blocks)
    dplyr::group_by(Date) %>%
    dplyr::arrange(!is.na(Time), Time, .by_group = TRUE) %>%
    # select proper time range
    dplyr::filter(( is.na(Time) & Date >= startDate & Date <= endDate) | (
                   !is.na(Time) & Time >= startTime & Time <= endTime))
  return(foo)
}

#' Takes a tibble as created by 'ecocal', produces a formatted table and exports 
#' it to a png file.
#' 
#' @param thecal The output of the 'ecocal' function.
#' @param lang Language selector. Either 'en' or 'de'. Defaults to 'en'.
#' @param outTZ Time zone designation (Olson format) used in the output table.
#' 
#' @importFrom magrittr %>%
#' 
#' @export
ecocal_format <- function(thecal, lang = c("en", "de"), outTZ = "UTC") {
  lang <- match.arg(lang)
  Sources <- paste(unique(c(dplyr::pull(thecal, Source), "Bloomberg")), collapse = ", ")
  thelocale <- ifelse(lang == "en", "en_US.utf8", "de_DE.utf8")
  dateFormat <- lubridate::stamp("26 Apr", orders = "dm", locale = thelocale)
  timeFormat <- lubridate::stamp("07:35", orders = "HM", locale = thelocale)
  thedecimal_sign = ifelse(lang == "en", ".", ",")
  numbFormat <- function(x, digits, decimal_sign = thedecimal_sign, na_str = "-") {
    format_str <- paste0("%.", digits, "f")
    foo <- sprintf(format_str, x)
    foo <- sub("\\.", decimal_sign, x)
    foo <- ifelse(is.na(foo), na_str, foo)
    return(foo)
  }
  is.repeater <- function(x) {
    x == dplyr::lag(x) & !is.na(dplyr::lag(x))
  }
  foo <- thecal %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Time = lubridate::with_tz(Time, tzone = outTZ)) %>%
    dplyr::mutate(Date   = dateFormat(Date), 
                  Time   = timeFormat(Time), 
                  Survey = numbFormat(Survey, Digits, 
                                      decimal_sign = thedecimal_sign),
                  Prior  = numbFormat(Prior,  Digits, 
                                      decimal_sign = thedecimal_sign)) %>%
    dplyr::mutate(Region = if(lang=="en") Region_en else Region_de) %>%
    dplyr::mutate(Name   = if(lang=="en") Name_en   else Name_de  ) %>%
    dplyr::mutate(Unit   = if(lang=="en") Unit_en   else Unit_de  ) %>%
    dplyr::mutate(Name = ifelse(is.repeater(Date) &
                                  is.repeater(Time) & 
                                  is.repeater(Region) & 
                                  is.repeater(Name), "", Name)) %>%
    dplyr::mutate(Region = ifelse(is.repeater(Date) &
                                    is.repeater(Time) & 
                                    is.repeater(Region), "", Region)) %>%
    dplyr::mutate(Time = ifelse(Region == "", "", Time)) %>%
    dplyr::mutate(Date = ifelse(is.repeater(Date), "", Date)) %>%
    dplyr::select(Date, Time, Region, Name, Unit, Period, Survey, Prior) 
  bg_col <- "#ffffff"
  for(i in 2:nrow(foo)) {
    if(foo[i,"Time"] == "") bg_col <- c(bg_col,bg_col[i-1])
    else bg_col <- c(bg_col,ifelse(bg_col[i-1]=="#ffffff","#dce6e6","#ffffff"))
  }
  flextable::set_flextable_defaults(font.family = "Arial", 
                                    font.size = 8,
                                    font.color = "black")
  flextable::fp_border_default(color = "#00414b", width = 2)
  nameFile    <- paste0("ecocal-", lang, "-", Sys.Date(), ".png")
  nameTZ      <- ifelse(lang=="en", "Time zone:", "Zeitzone:")
  nameSources <- ifelse(lang=="en", "Sources:",   "Quellen:")
  
  flextable::flextable(foo) %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::footnote(i=1, j=2, value = flextable::as_paragraph(paste(nameTZ,outTZ)), 
                        part = "header") %>%
    flextable::add_footer_lines(paste(nameSources, Sources)) %>%
    flextable::border_remove() %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::align(j = c("Date","Name"), align = "left", part = "footer") %>% 
    flextable::bg(bg = "#91afb4", part = "header") %>%
    flextable::bold(part = "header") %>%
    flextable::bg(bg = bg_col, part = "body") %>%
    flextable::bg(j = 1, bg = "#ffffff", part = "body") %>%
    flextable::bold(j=1, part = "body") %>%
    flextable::hline_bottom() %>%
    flextable::width(width = 5.35) %>%
    flextable::save_as_image(path = nameFile)
}

#' Wrapper to produce a set of German and English calender png files with the 
#' help of functions 'ecocal' and 'ecocal_format'. Uses usual defaults for a 
#' typical morning shift task. 
#' 
#' @importFrom magrittr %>%
#' 
#' @examples 
#' \dontrun{
#' options(blpHost = "192.168.2.141", blpPort = 18194L)
#' Rblpapi::blpConnect()
#' easy_ecocals()
#' }
#' 
#' @export
easy_ecocals <- function() {
  ecocal(startTime = paste(Sys.Date(),   "07:00:00"),
         endTime   = paste(Sys.Date()+1, "08:00:00"),
         inTZ = "Europe/Berlin") %>%
    ecocal_format(lang = "de", outTZ = "Europe/Berlin")
  
  ecocal(startTime = paste(Sys.Date(),   "08:00:00"),
         endTime   = paste(Sys.Date()+1, "09:00:00"),
         inTZ = "Europe/Berlin") %>%
    ecocal_format(lang = "en", outTZ = "UTC")
}

easy_ecocals()
