#' @title List Objects
#'
#' @description
#' Internal helper function to list objects in an environment.
#'
#' @details
#' Internal helper function to retrieve a list of objects from a workspace.
#' Take an environment as argument and optionally an object class.
#' Returns a list of objects of the specified class in the environment.
#'
#' @param env environment in which to search for objects.
#' @param obj_class character string or vector specifying the object class.
#' @param sort character string "time", "alpha", "size" specifying the sorting order. Default = NULL.
#' @param decreasing logical used to indicate order when sorting is not NULL. Default = TRUE.
#' @param debug logical indicating printing debug information.
#'
#' @return character vector with the object names or NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List data frames in the workspace.
#' list_objects(obj_class = "data.frame")
#' # List functions in the workspace.
#' list_objects(obj_class = "function")
#' }
#'
list_objects <- function(env = parent.frame(), obj_class = NULL,
                        sort = NULL, decreasing = TRUE,
                        debug = FALSE) {
  if (debug) {
    print(paste("IN:", match.call()[[1]]))
  }

  # Result vector.
  res <- character()

  # List objects in environment.
  wsObj <- ls(env)

  if (debug) {
    print("Objects:")
    print(wsObj)
    print("obj_class:")
    print(obj_class)
  }

  # Check if specified object class.
  if (!is.null(obj_class)) {
    classes <- list()

    # Loop to save all class information.
    for (i in seq(along = wsObj)) {
      obj <- get(wsObj[i], envir = env)
      classes[i] <- list(class(obj))
    }

    # Filter objects with specified classes.
    for (c in seq(along = obj_class)) {
      for (i in seq(along = classes)) {
        if (obj_class[c] %in% classes[[i]]) {
          res <- c(res, wsObj[i])
        }
      }
    }
  } else {
    # Return all objects.
    res <- wsObj
  }

  # Check if sorting is requested.
  if (!is.null(sort)) {
    # Create sorting vector.
    new.order <- vector(mode = "character", length = length(res))

    if (sort == "time") {
      # Loop over objects and retrieve attribute.
      for (o in seq(along = res)) {
        tmp <- attr(
          x = get(x = res[o], envir = env),
          which = "timestamp", exact = TRUE
        )
        new.order[o] <- ifelse(is.null(tmp), as.character(NA), tmp)
      }

      if (debug) {
        message("time, decreasing=", decreasing)
        message(paste(new.order[order(new.order, decreasing = decreasing)],
          collapse = ", "
        ))
      }

      # Sort according to new order.
      res <- res[order(new.order, decreasing = decreasing)]
    }

    if (sort == "alpha") {
      if (debug) {
        message("alpha, decreasing=", decreasing)
        message(paste(res[order(res, decreasing = decreasing)],
          collapse = ", "
        ))
      }

      # Sort according to new order.
      res <- res[order(res, decreasing = decreasing)]
    }

    if (sort == "size") {
      # Loop over objects and retrieve attribute.
      for (o in seq(along = res)) {
        new.order[o] <- object.size(x = get(x = res[o], envir = env))
      }

      if (debug) {
        message("Size, decreasing=", decreasing)
        message(paste(new.order[order(new.order, decreasing = decreasing)],
          collapse = ", "
        ))
      }

      # Sort according to new order.
      res <- res[order(new.order, decreasing = decreasing)]
    }
  }

  # Check if NA.
  if (all(is.na(res))) {
    res <- NULL
  }

  if (debug) {
    print("Returned objects:")
    print(res)
    print(paste("EXIT:", match.call()[[1]]))
  }

  return(res)
}
