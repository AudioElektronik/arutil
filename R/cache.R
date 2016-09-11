#' Taken from https://github.com/hadley/dplyr
#' /blob/f45629630b0b55452e752a3e4c9948fbb6226ca1/R/data.r

# Environment for caching connections etc
cache <- new.env(parent = emptyenv())

is_cached <- function(name) exists(name, envir = cache)
set_cache <- function(name, value) {
  #   message("Setting ", name, " in cache")
  assign(name, value, envir = cache)
  value
}
get_cache <- function(name) {
  #   message("Getting ", name, " from cache")
  get(name, envir = cache)
}

#' Caching computation in a local environment
#'
#' Computation is assigned a key, and everytime it's called, it is retrieed
#' from a cache in local environment. This is different than memoise because
#' connections can also be cached since they are put inside and environemnt.
#' @export
cache_computation <- function(name, computation) {
  if (is_cached(name)) {
    get_cache(name)
  } else {
    res <- force(computation)
    set_cache(name, res)
    res
  }
}

load_srcs <- function(f, src_names, quiet = NULL) {
  if (is.null(quiet)) {
    quiet <- !identical(Sys.getenv("NOT_CRAN"), "true")
  }


  srcs <- lapply(src_names, function(x) {
    out <- NULL
    try(out <- f(x), silent = TRUE)
    if (is.null(out) && !quiet) {
      message("Could not instantiate ", x, " src")
    }
    out
  })

  compact(setNames(srcs, src_names))
}

#' Check that database location is writable
#'
#' @param path if there is a specific path for the db, this parameter is used
#' @param pkg_name if there is no path, root of the pkg_name package is used
#'
#' @export
db_location <- function(filename, path = NULL, pkg_name = NULL) {
  if (!is.null(path)) {
    # Check that path is a directory and is writeable
    if (!file.exists(path) || !file.info(path)$isdir) {
      stop(path, " is not a directory", call. = FALSE)
    }
    if (!is_writeable(path)) stop("Can not write to ", path, call. = FALSE)
    return(file.path(path, filename))
  }

  pkg <- file.path(system.file("db", package = pkg_name))
  if (is_writeable(pkg)) return(file.path(pkg, filename))

  stop("Could not find writeable location to cache db", call. = FALSE)
}

is_writeable <- function(x) {
  unname(file.access(x, 2) == 0)
}
