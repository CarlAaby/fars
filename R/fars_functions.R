#' Function \code{fars_read}
#'
#'
#' This function reads the contents of a csv file.
#' If the file can not be found, an error message is returned
#'
#' @return A data.frame object
#'
#' @param filename The name of the file to load
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @note This is an internal function not to be used from outside the package

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Function \code{make_filename}
#'
#' This function will make a file name for a given year
#'
#' @param year A numeric representing the year
#' @return A string representing a file name
#'
#'@note This is an internal function not to be used from outside the package
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata", sprintf("accident_%d.csv.bz2", year), package = "fars")
}

#' Function \code{fars_read_years}
#'
#' A function that returns the number of deaths in a year per each month of the year
#' Will return with a warning message, if the year can not be found
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @param years The specified year
#'
#' @return A data frame with number of deaths for each month in the given year
#'
#'@note This is an internal function not to be used from outside the package
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Function \code{fars_summarize_years}
#'
#'This functions calculates the number of traffic deaths for each month during a given year
#'Will return with a warning if data for the specified year not can be found
#'
#'@return A data frame with two columns, representing the months and the number of traffic deaths for each month
#'
#'@param years numeric the year
#'
#'@examples
#'fars_summarize_years(2015)
#'
#'@importFrom dplyr bind_rows group_by summarize
#'@importFrom tidyr spread
#'@importFrom magrittr %>%
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Function \code{fars_map_state}
#'
#' This function displays the locations of traffic deaths
#' for a given state and year on a graphic map
#'
#'@param state.num numeric the number of the state alphalbeticly
#'@param year the year of the death
#'
#'@examples
#'fars_map_state(1, 2015)
#'@examples
#'fars_map_state(10, 2013)
#'
#'@return A map of a state, with each traffic death location shown on the map
#'
#'@importFrom maps map
#'
#'@note Will display a warning message if no file for the given year not can be found
#'@note Will display a warning message a corresponding state can not be found
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}



