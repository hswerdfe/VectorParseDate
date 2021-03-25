#' @importFrom magrittr %>%
NULL

#library(magrittr)

#' vector formats that will be attempted by default
#'
#' @export
vector_parse_date_formats <- function(){
  unique(c(
    'dby',
    'dmy',
    'ymd',
    'mdy',
    'ydm',
    'ybd',
    'bdy',
    'ydb'
  ))

  #' c(
  #'   '%d-%b-%Y',
  #'   #'%d %b %Y',
  #'   #'%Y%m%d',
  #'   '%d-%m-%y',
  #'   '%y-%m-%d',
  #'   '%m-%d-%y',
  #'   '%y-%d-%m',
  #'   #'%d-%m-%Y',
  #'   #'%Y-%m-%d',
  #'   #'%m-%d-%Y',
  #'   #'%Y-%d-%m',
  #'   '%d-%b-%y',
  #'   '%y-%b-%d',
  #'   '%b-%d-%y',
  #'   '%y-%d-%b'#,
  #'   #'%d-%b-%Y',
  #'   #'%Y-%b-%d',
  #'   #'%Y-%d-%b'
  #' )
}




#' cleans incoming dates a little bit so they are a little simmilar and we don't need to try as many formats
#'
#' @param dts vecotor of date strings
#' @param TIME_SPLIT where to split to get rid of the time
#' @param seps what the date seperators might be
#'
#' @export
vector_parse_date_first_clean <- function(dts,
                                          TIME_SPLIT = 'T',
                                          seps = '[-.:/\\s+]'){

  stringr::str_split(dts,pattern = TIME_SPLIT, n = 1) %>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(pattern = seps, replacement = "-")
}




#' always returns true
#'
#' @param dt a POSIXct object
#'
#' @export
vector_parse_date_usually_true <- function(dt){
  return(as.double(1))
}



#' true if the date is in the past
#'
#' @param dt a POSIXct object
#'
#' @export
vector_parse_date_not_future <- function(dt){
  #dt = lubridate::parse_date_time('2010-03-14', 'Ymd')
  return(as.double(dt <= Sys.Date()))
}

vector_parse_recent_better <- function(dt){
  if (as.double(dt <= Sys.Date())){
    return(1.0/(1+as.double(difftime(Sys.Date(), dt, units = "days"))))
  }else{
    return(0.0)
  }
}
#' adjust years of a date that are feed in
#'
#' @param dt a single string that may be a date
#' @year
#'
#' @export
vector_parse_date_adust_year <- function(dt, year_threshold=1922){
  m <- year(dt) %% 100
  year(dt) <- ifelse(m > year_threshold %% 100, 1900+m, 2000+m)
  dt
}

#####################################
#'
#' return true if it is 4 digit year
#'
#' @param dt_str
#' @param fmt
#' @param sep
vector_parse_date_is_two_digit_year <- function(dt_str, fmt, sep = "-"){
  #dt_str = 19970422
  dt_str <- vector_parse_date_first_clean(dt_str)

  index_of_year = stringr::str_locate_all(pattern ="y", string = fmt)[[1]][[1]]

  year_a <-
    tryCatch(
      stringr::str_split(dt_str, pattern = sep) %>% unlist() %>%
        magrittr::extract2(index_of_year) %>%
        as.integer(),
      error = function(e){
        return(-1)
      },

      finally = ""
    )

  if(year_a == -1)
    return(FALSE)

  year_b = suppressWarnings(lubridate::parse_date_time(dt_str, fmt)) %>% year()%>% as.integer()
  return(year_a == year_b)
}

#vector_parse_date_is_two_digit_year(dt_str = "03-22-97", fmt = 'mdy')
#vector_parse_date_is_two_digit_year(dt_str = "03-22-1997", fmt = 'mdy')

#' returns either NA or a date depending on if the dt_str and fmt make sense as a possible date
#'
#' @param dt_str a single string that may be a date
#' @param fmt a single format to try
#' @check_func function return 1 if the date parsed matches business logic
#' @... passed to check_func
#'
#' @export
vector_parse_date_only_one <- function(dt_str, fmt, check_func = vector_parse_date_not_future, ...){
  #dt_str = "2010-03-14"
  #dt_str = "03/22/97"
  #fmt = "Ymd"
  #fmt = "mdY"
  #fmt = '%m-%d-%y'

  #suppressWarnings(lubridate::as_date(x, ...))
  ret_dt = suppressWarnings(lubridate::parse_date_time(dt_str, fmt))
  if (is.na(ret_dt)){
    return( as.POSIXct(NA))
  }
  if( ! check_func(ret_dt, ...)){
    dt_str2 = vector_parse_date_first_clean(dt_str)
    if (  vector_parse_date_is_two_digit_year(dt_str2, fmt)){
      ret_dt = vector_parse_date_adust_year(ret_dt, ...)
    }

    if (is.na(ret_dt)){
      return( as.POSIXct(NA))
    }
    if( ! check_func(ret_dt, ...)){
      return( as.POSIXct(NA))
    }
  }

  return(ret_dt)
}





#' Return true if the string makes sense as a date in that format
#'
#' @param dt_str a single string that may be a date
#' @param fmt a single format to try
#' @check_func function return true if the date parsed matches business logic
#'
#' @export
vector_parse_date_possibl_correct_format <- function(dt_str, fmt, check_func = vector_parse_date_not_future, ...){
  #dt_str = "4-17-90"
  #dt_str = "1990-11-23"
  #fmt = "%d-%b-%Y"
  #fmt = "%Y-%m-%d"
  ret_dt = vector_parse_date_only_one(dt_str = dt_str, fmt = fmt, check_func = check_func, ...)
  if (is.na(ret_dt)){
    return( 0.0)
  }
  return (check_func(dt = ret_dt, ...))
}

#' returns a wide tibble showing success or failure of each parsing format on each date
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns 1 if if matches some business logic for a valid date
#'
#' @export
vector_parse_success_grid <- function(dts,
                                      fmts= vector_parse_date_formats(),
                                      check_func = vector_parse_date_not_future,
                                      ...){
  #dts = c("2011/01/21", "2011/02/22", "2011/01/04", "2011/01/21", "Junk",       NA )
  dts_fmts <-
    #vector_parse_date_first_clean(dts) %>%
    tidyr::expand_grid(dts_orig = dts, fmt = fmts) %>%
    dplyr::mutate(dts = vector_parse_date_first_clean(dts_orig))

  dts_fmts["valid_dts"] <-
    as.POSIXct(purrr::map2(dts_fmts$dts, dts_fmts$fmt, vector_parse_date_only_one, check_func = check_func, ... = ...) %>% unlist(), origin = "1970-01-01", tz = "GMT")

  dts_fmts["valid_fmt"] <-
    as.double(purrr::map2(dts_fmts$dts, dts_fmts$fmt, vector_parse_date_possibl_correct_format, check_func = check_func, ... = ...) %>% unlist())


  dts_fmts %>%
    dplyr::group_by(dts_orig, fmt) %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::ungroup() %>% #dplyr::filter(dts_orig == "Junk")
    dplyr::group_by(fmt) %>% dplyr::mutate(fmt_sum = sum(valid_fmt))%>% dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(fmt_sum)) %>% dplyr::select(-fmt_sum) %>%
    dplyr::group_by(dts_orig) %>% dplyr::mutate(valid_fmt = sum(valid_fmt)/n) %>% dplyr::ungroup() %>%
    tidyr::pivot_wider(names_from = 'fmt', values_from = 'valid_dts', values_fill  = as.POSIXct(NA), values_fn = mean ) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::select(-dts)
}


#' returns a tibble with formats tried and success rate of format in aggregate
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns 1 if if matches some business logic for a valid date
#'
#' @export
vector_parse_success_rates <- function(dts,
                                       fmts= vector_parse_date_formats(),
                                       check_func = vector_parse_date_not_future,
                                       ...){
  dts_fmts <-
    vector_parse_date_first_clean(dts) %>%
    tidyr::expand_grid(dts = ., fmt = fmts)

  dts_fmts["valid_fmt"] <-
    purrr::map2(dts_fmts$dts, dts_fmts$fmt, vector_parse_date_possibl_correct_format, check_func = check_func, ... = ...) %>% unlist()

  fmts_summary <-
    dts_fmts %>%
    dplyr::group_by(fmt) %>%
    dplyr::summarise(n = sum(valid_fmt)) %>%
    dplyr::arrange(desc(n))
  return(fmts_summary)
}


#' returns a string for the best guess date, if there is no best guess it return an empty string
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns 1 if if matches some business logic for a valid date
#'
#' @export
vector_parse_dates_guess_at_format <- function(dts,
                                              fmts= vector_parse_date_formats(),
                                              check_func = vector_parse_date_not_future,
                                              ...){

  fmts_summary <- vector_parse_success_rates(dts = dts, fmts = fmts, check_func = check_func, ... = ...)

  if (nrow(fmts_summary) == 1){
    return(fmts_summary %>% dplyr::pull(fmt))
  }
  if (nrow(fmts_summary) == 0){
    return('')
  }
  if (nrow(fmts_summary) > 1){
    ns <- fmts_summary %>% dplyr::pull(n)
    if (ns[1] > ns[2]){
      return(fmts_summary %>% utils::head(1) %>% dplyr::pull(fmt))
    }else{
      return('')
    }
  }
  return('')
}





#' given a vector of dates this will try to parse them using vector of formats, and then parse them as best it can using information on if the other dates parse in a given format
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns true if if matches some business logic for a valid date
#'
#' @export
vector_parse_dates <- function(dts,
                              fmts = vector_parse_date_formats(),
                              check_func = vector_parse_date_not_future,
                              ...){


  best_guess_format <- vector_parse_dates_guess_at_format(dts = dts, fmts = fmts, check_func = check_func,  ... = ...)
  dts2 <- vector_parse_date_first_clean(dts)


  purrr::map(dts2,vector_parse_date_only_one, fmt = best_guess_format, check_func = check_func, ... = ...) %>%
    purrr::reduce(c)

}



