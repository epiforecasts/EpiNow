#' Copy the Nowcast Report with an Appended YAML
#'
#' @param yaml Character string giving the path to the YAML to append.
#' @param report Character string giving the path to the report.
#' @param lines_to_cut Numeric, number of lines to remove from the report. Defaults to `NULL`. 
#' Useful when updating the author list etc.
#' @param date A character string date in the format `"yyyy-mm-dd`. Indicates what date to add
#' to the report
#' @param report_target A character string identifying the target directory for the report.
#'
#' @return
#' @export
#'
#' @examples
#' 
copy_report<- function (yaml = NULL, report = NULL, lines_to_cut = NULL,
                        date = NULL, report_target = NULL) {
  # read in the YAML
  yaml <- readLines(yaml)

  # Add target date to yaml
  yaml[grep("update", yaml)] <- paste0("update: ", date)

  # Read in report
  report <- readLines(report)

  if (!is.null(report)) {
    report <- report[-c(lines_to_cut)]
  }
  
 
  # put the yaml in
  final_report <- append(yaml, report)

  # write out to a temp file
  tmp_file <- file.path(tempdir(), basename(report_target))
  writeLines(final_report, tmp_file)

  # copy back to the current directory.
  file.copy(tmp_file, report_target, overwrite = T)
}
