#' Clean Nowcasts for a Supplied Date
#'
#' @description USE WITH CARE. This function removes nowcasts in the format produced by `EpiNow` from a target
#' directory for the date supplied.
#' @param date Date object. Defaults to todays date
#' @param nowcast_dir Character string giving the filepath to the nowcast results directory.
#'
#' @return
#' @export
#'
#' @examples
#' 
clean_nowcasts <- function(date = NULL, nowcast_dir = NULL) {

  if (is.null(date)) {
    date <- Sys.Date()
  }
  
  if (is.null(nowcast_dir)) {
    nowcast_dir <- "."
  }
  
  dirs <- list.dirs(nowcast_dir, recursive = FALSE)

  purrr::walk(dirs,
              function(dir) {
                remove_dir <- file.path(dir, date)
                if (dir.exists(remove_dir)) {
                  message("Removing files from: ", remove_dir)
                  lapply(list.files(file.path(remove_dir)),
                                    function(file){
                                      file.remove(
                                        file.path(remove_dir, file)
                                      )
                                    })

                }

              })


}
