#' A utility function for progress
#'
#' This function makes a linear model prints a status
#' message about the process that is tring to be completed.
#'
#' @param identifier what the process is about
#' @param start_count how many of the things trying to be done is already done
#' @param end_count how many things will have to be done eventually
#'
#' @return a function that takes the current count as an argument. When it is called,
#'      it prints a status message
#' @export
status_update_fn_generator <- function(identifier, current_count, end_count) {
  start_time <- Sys.time()

  # Saved to see how the current process progresses
  start_count <- current_count

  update_fn <- function(increment_by) {
    current_count <<- current_count + increment_by

    run_mins <- as.numeric(
      difftime(Sys.time(), start_time,
               unit = "mins"))

    done_count <- current_count - start_count
    done_per_mins <- done_count / run_mins

    remaining <- end_count - current_count
    remaining_mins <- remaining / done_per_mins

    completed_perc <- current_count / end_count

    msg <- sprintf(
      "%s: Running for %s - Remaining %s - Speed %.1f/s - Completed %% %.1f - Count %s",
      identifier,
      human_mins(run_mins),
      human_mins(remaining_mins),
      done_per_mins / 60,
      completed_perc * 100,
      format(current_count, big.mark = ",", scientific = FALSE))

    message(msg)
  }

  return(update_fn)
}

#' Converts numeric minutes to human readable time
#'
#' 90 -> 01:30
#' @export
human_mins <- function(mins) {
  hours <- mins %/% 60
  rem_mins <- mins %% 60
  sprintf("%02.0f:%02.0f", hours, rem_mins)
}
