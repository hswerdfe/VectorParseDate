#' @importFrom magrittr %>%
NULL

#library(magrittr)

#' vector formats that will be attempted by default
#'
#'
#' @examples
#'   vector_parse_date_formats()
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

}




#' cleans incoming dates a little bit so they are a little similar and we don't need to try as many formats
#'
#' @param dts vecotor of date strings
#' @param TIME_SPLIT where to split to get rid of the time
#' @param seps what the date separators might be
#' @param replace_sep what to replace the seperators with
#'
#' @examples
#'  vector_parse_date_first_clean( c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
#'  vector_parse_date_first_clean(dts = c("2018-11-01 08:30:00", "2017-09-19 08:30:00", "2017-02-28 08:30:00"), TIME_SPLIT = " ")
#'  vector_parse_date_first_clean(dts = c("2018-11-01T08:30:00", "2017-09-19T08:30:00", "2017-02-28T08:30:00"), TIME_SPLIT = "T")
#' @export
vector_parse_date_first_clean <- function(dts,
                                          TIME_SPLIT = 'T',
                                          seps = '[-.:/\\s+]',
                                          replace_sep = "-"){

  stringr::str_split(dts,pattern = TIME_SPLIT, n = 2) %>%
    purrr::map(., 1)%>%
    unlist() %>%
    stringr::str_trim() %>%
    stringr::str_replace_all(pattern = seps, replacement = replace_sep)
}



#' Can be passed to multiple functions as the 'check_func' parameter,
#' always returns true
#'
#' @param dt a POSIXct object
#'
#' @examples
#'   vector_parse_date_usually_true(as.Date("2021-01-17"))
#'   vector_parse_date_usually_true(as.Date("2037-01-17"))
#'   vector_parse_date_usually_true(as.Date("1282-03-27"))
#' @export
vector_parse_date_usually_true <- function(dt){
  return(as.double(1))
}


#' Can be passed to multiple functions as the 'check_func' parameter,
#' true if the date is in the past
#'
#' @param dt a POSIXct object
#'
#' @examples
#'   vector_parse_date_not_future(as.Date("2021-01-17"))
#'   vector_parse_date_not_future(as.Date("2037-01-17"))
#'
#' @export
vector_parse_date_not_future <- function(dt){
  #dt = lubridate::parse_date_time('2010-03-14', 'Ymd')
  return(as.double(dt <= Sys.Date()))
}

#' Can be passed to multiple functions as the 'check_func' parameter,
#' true if the date is in the past 120 years
#'
#' @param dt a POSIXct object
#'
#' @examples
#'   vector_parse_date_last_120_yr(as.Date("2021-01-17"))
#'   vector_parse_date_last_120_yr(as.Date("2037-01-17"))
#'   vector_parse_date_last_120_yr(as.Date("0473-7-12"))
#' @export
vector_parse_date_last_120_yr <- function(dt){
  #dt = lubridate::parse_date_time('2010-03-14', 'Ymd')
  ret_val <- as.double(dt <= Sys.Date()) & dt >= Sys.Date() - lubridate::years(100)
  return(ret_val)
}









#############################
#' Can be passed to multiple functions as the 'check_func' parameter,
#' Returns a larger number for dates closer to the current date
#'
#' @param dt  a POSIXct object
#'
#' @examples
#'   vector_parse_recent_better(as.Date("2021-01-17"))
#'
#' @export
vector_parse_recent_better <- function(dt){
  if (as.double(dt <= Sys.Date())){
    return(1.0/(1+as.double(difftime(Sys.Date(), dt, units = "days"))))
  }else{
    return(0.0)
  }
}

#############################
#' adjust years of a date that are feed in
#'
#' @param dt a single string that may be a date
#' @param year_threshold  year where we will try to adjust by 100 years
#'
#' @export
vector_parse_date_adust_year <- function(dt, year_threshold=1922){
  m <- lubridate::year(dt) %% 100
  lubridate::year(dt) <- ifelse(m > year_threshold %% 100, 1900+m, 2000+m)
  dt
}

#####################################
#'
#' Internal function return true if it is 4 digit year
#'
#' @param dt_str date string
#' @param fmt format for the date string sometging like 'mdy'
#' @param sep default separator
#'
#' @examples
#' vector_parse_date_is_two_digit_year(dt_str = "03-22-97", fmt = 'mdy')
#' vector_parse_date_is_two_digit_year(dt_str = "03-22-1997", fmt = 'mdy')
#' vector_parse_date_is_two_digit_year(dt_str = "2035-01-29", fmt = 'ymd')
#' vector_parse_date_is_two_digit_year(dt_str = "35-01-29", fmt = 'ymd')
#' vector_parse_date_is_two_digit_year(dt_str = "20110401", fmt = 'ymd')
#'
#'@export
vector_parse_date_is_two_digit_year <- function(dt_str, fmt, sep = "-"){
  #dt_str = 19970422
  dt_str <- vector_parse_date_first_clean(dt_str)

  index_of_year = stringr::str_locate_all(pattern ="y", string = fmt)[[1]][[1]]

  if (stringr::str_count(dt_str, pattern = sep) == 0){
    return(FALSE)
  }

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
    return(TRUE)

  year_b = suppressWarnings(lubridate::parse_date_time(dt_str, fmt)) %>% lubridate::year()%>% as.integer()
  return(year_a != year_b)
}

#' Returns either NA or a date depending on if the dt_str and fmt make sense as a possible date, along with the business logic of check_func
#'
#' @param dt_str a single string that may be a date
#' @param fmt a single format to try
#' @param check_func function return 1 if the date parsed matches business logic, or a decimal for probability.
#' @param ... passed to check_func
#'
#' #' @example
#'    vector_parse_date_only_one(dt_str="03/03/92", fmt="mdy")
#'    vector_parse_date_only_one(dt_str="03/03/92", fmt="ymd")
#'    vector_parse_date_only_one(dt_str="20110401", fmt = "dmy", check_func =vector_parse_date_last_120_yr )
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
    if (  vector_parse_date_is_two_digit_year(dt_str = dt_str2, fmt)){
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
#' @param check_func function return true if the date parsed matches business logic
#' @param ... passed to vector_parse_date_only_one
#'
#' @examples
#'    vector_parse_date_possibl_correct_format(dt_str="03/03/92", fmt="mdy")
#'    vector_parse_date_possibl_correct_format(dt_str="03/03/92", fmt="ymd")
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
#' @param origin passed to as.POSIXct
#' @param tz passed to as.POSIXct
#'
#' @examples
#'    vector_parse_success_grid(dts=c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
#'
#' @export
vector_parse_success_grid <- function(dts,
                                      fmts= vector_parse_date_formats(),
                                      check_func = vector_parse_date_not_future,
                                      origin = "1970-01-01",
                                      tz = "GMT",
                                      ...){
  #dts = c("2011/01/21", "2011/02/22", "2011/01/04", "2011/01/21", "Junk",       NA )
  dts_fmts <-
    #vector_parse_date_first_clean(dts) %>%
    tidyr::expand_grid(dts_orig = dts, fmt = fmts) %>%
    dplyr::mutate(dts = vector_parse_date_first_clean(dts_orig))

  dts_fmts["valid_dts"] <-
    as.POSIXct(purrr::map2(dts_fmts$dts, dts_fmts$fmt, vector_parse_date_only_one, check_func = check_func, ... = ...) %>% unlist(), origin = origin, tz = tz)

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
#' @param check_func optional function that takes a date and returns 1 if if matches some business logic for a valid date
#' @param origin Origin for postix conversion
#' @param tz timezone for postix conversion
#' @param ... passed to vector_parse_date_only_one ,the nto check_func
#'
#'
#'
#' @examples
#'  vector_parse_dates_guess_at_format_lazy(dts=c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
#'
#'
#' @export
vector_parse_dates_guess_at_format_lazy <- function(dts,
                                                    fmts= vector_parse_date_formats(),
                                                    check_func = vector_parse_date_not_future,
                                                    origin = "1970-01-01",
                                                    tz = "GMT",
                                                    ...){
  dts <- sample(dts)
  fmts_cntr <- dplyr::tibble(fmts_nm = fmts, fmt_wrked = TRUE)
  for (dt in dts){
    fmts_cntr$fmt_wrked <- !
    fmts_cntr$fmts_nm %>%
      purrr::map (function(f){
             vector_parse_date_only_one(dt_str = dt, fmt = f,  check_func = check_func, ...)
          }) %>% unlist() %>%
      as.POSIXct(origin = origin, tz = tz) %>%
      is.na()

    fmts_cntr <- fmts_cntr %>%
      dplyr::filter(fmt_wrked == TRUE)

    if (nrow(fmts_cntr) <= 1){
      break
    }
  }

  return(fmts_cntr$fmts_nm)
}




#' returns a tibble with formats tried and success rate of format in aggregate
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns 1 if if matches some business logic for a valid date
#'
#' @examples
#'   vector_parse_success_rates(dts=c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
#' @export
vector_parse_success_rates <- function(dts,
                                       fmts= vector_parse_date_formats(),
                                       check_func = vector_parse_date_not_future,
                                       ...){
  dts_fmts <-
    vector_parse_date_first_clean(dts) %>%
    tidyr::expand_grid(dts = ., fmt = fmts)


  #no_cores <- detectCores(logical = TRUE)
  #cl <- makeCluster(no_cores-1)
  #registerDoParallel(cl)



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
#' @examples
#'   vector_parse_dates_guess_at_format(dts=c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
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






#' Given a vector of dates this will try to parse them using vector of formats, and then parse them as best it can using information on if the other dates parse in a given format
#'
#' @param dts vector of strings
#' @param fmts optional vector of formats to check
#' @param check_func function that takes a date and returns true if if matches some business logic for a valid date
#' @param cleaning_args list of arguments passed on to vector_parse_dates_guess_at_format
#' @param method optional how to guess either "lazy" or "regular"
#' @param ... passed to other methods then eventually to check_func
#'
#' @examples
#'  vector_parse_dates(dts=c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
#'  vector_parse_dates(dts = c("2018-11-01 08:30:00", "2017-09-19 08:30:00", "2017-02-28 08:30:00"), cleaning_args = list(TIME_SPLIT = " "))
#'  vector_parse_dates(dts = c("2018-11-01T08:30:00", "2017-09-19T08:30:00", "2017-02-28T08:30:00"))
#'
#'
#'  dts <- cansim::list_cansim_tables() %>% pull(date_published)
#'  vector_parse_dates(dts, cleaning_args = list(TIME_SPLIT = " " ))
#' @export
vector_parse_dates <- function(dts,
                              fmts = vector_parse_date_formats(),
                              check_func = vector_parse_date_not_future,
                              cleaning_args = list(),
                              method =NULL,
                              ...){

  if (is.null(method)){
    method <-
      if (length(dts) > 100) {"lazy"
      #}else if (TRUE) {"sample"
      }else {"regular"
      }

  }


  cleaning_args[["dts"]] = dts
  dts_str <- do.call(vector_parse_date_first_clean, cleaning_args)

  best_guess_format <-
    if (method == "lazy"){
      vector_parse_dates_guess_at_format_lazy(dts = dts_str, fmts = fmts, check_func = check_func,  ... = ...)
    }else{
      vector_parse_dates_guess_at_format(dts = dts_str, fmts = fmts, check_func = check_func,  ... = ...)
    }



  purrr::map(dts_str,vector_parse_date_only_one, fmt = best_guess_format, check_func = check_func, ... = ...) %>%
    purrr::reduce(c)

}

