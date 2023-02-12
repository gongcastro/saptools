#' Initialise Git
#'
#' @export
create_git <- function() {
  git_init()
}

#' Create project files
#'
#' @param x A dataframe
#' @export
create_folder_files <- function() {
  # download APA 7th citation style
  download.file(
    "https://raw.githubusercontent.com/citation-style-language/styles/master/apa.csl",
    destfile = "files/bib/apa.csl"
  )
  # creating Bib file for references
  file.create("files/references.bib")

  # creating README.md
  file.create("README.md")

  # create LICENSE file
  use_mit_license()
}

#' Create project folders
#'
#' @param x A dataframe
#' @export
create_project_folders <- function(folders = NULL) {
  if (is.null(folders)) {
    folders <- c(
      "files",
      "files/bib/",
      "data",
      "data-raw",
      "docs",
      "img",
      "manuscript",
      "manuscript/img/",
      "R",
      "results",
      "tests"
    )
  }
  invisible(lapply(folders, dir.create))
}

#' Transform any list column in a dataframe to collapsed character vector
#'
#' @param x A dataframe
#' @export
flatten_columns <- function(x) {
  mutate_if(x,
    .predicate = is.list,
    ~ unlist(map(., ~ paste0(., collapse = ", ")))
  )
}


#' Save an R object as data or results as Arrow Parquet, CSV, or RDS files
#'
#' @param x A tabular R object
#' @param folder Folder in which to write the file. If the resulting path does not exist, a new directory will be generated.
#' @param formats Formats in which to write the files. Must be at least one of 'parquet', 'csv', or 'rds'
#' @param .sep Path separator, takes '/' by default
#' @export
save_files <- function(x,
                       folder,
                       file_name = deparse(substitute(x)),
                       formats = c("parquet", "csv", "rds"),
                       .sep = "/") {
  # check arguments
  if (!all(formats %in% c("parquet", "csv", "rds"))) {
    cli_abort("formats must be 'parquet', 'csv' or 'rds'")
  }

  # create directories if missing
  dirs <- glue("{folder}{.sep}{formats}")
  dirs_exist <- dir.exists(dirs)
  if (any(!dirs_exist)) {
    missing_dir <-
      glue("{folder}{.sep}{formats[which(!dirs_exist)]}{.sep}")
    invisible(map(missing_dir, dir.create))
    cli_alert_warning("Created {.path {missing_dir}}")
  }

  # save files
  file_paths <-
    glue("{folder}{.sep}{formats}{.sep}{file_name}.{formats}")
  write_csv_arrow(flatten_columns(x), file_paths[grepl(".parquet", file_paths)])
  write_parquet(flatten_columns(x), file_paths[grepl(".csv", file_paths)])
  saveRDS(x, file_paths[grepl(".rds", file_paths)])
  cli_alert_success("Saved to {.path {folder}}")
}


#' Remove nul file that get created sometimes when knitting Rmarkdown files to PDF
#'
#' @export
remove_nul <- function() {
  system(
    "rename \\.\\C:\\Users\\U155880\\Documents\\trajectories\\nul. deletefile.txt"
  )
}
