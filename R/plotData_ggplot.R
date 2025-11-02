  library(ggplot2)
  library(RColorBrewer)
  
  # Command-line Plot Function
  general_plot <- function(data, x, y, wrap_by = NULL, facet_by = NULL, color_scheme = "default", 
                           plot_title = NULL, x_title = NULL, y_title = NULL) {
    # Default titles
    if (is.null(plot_title)) plot_title <- deparse(substitute(data))
    if (is.null(x_title)) x_title <- x
    if (is.null(y_title)) y_title <- y
    
    # Color schemes
    if (color_scheme == "colorblind") {
      palette <- scale_color_viridis_d()
    } else if (color_scheme == "grayscale") {
      palette <- scale_color_grey()
    } else if (color_scheme == "kit") {
      palette <- scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"))
    } else {
      palette <- scale_color_brewer(palette = "Set1")
    }
    
    # Basic ggplot object
    p <- ggplot(data, aes(.data[[x]], .data[[y]])) +
      geom_point() +
      palette +
      labs(title = plot_title, x = x_title, y = y_title)
    
    # Add wrapping or faceting if specified
    if (!is.null(wrap_by)) {
      p <- p + facet_wrap(as.formula(paste("~", wrap_by)))
    }
    if (!is.null(facet_by)) {
      p <- p + facet_grid(as.formula(paste(facet_by, "~ .")))
    }
    
    # Print the plot
    print(p)
  }
  
  # GUI Wrapper Function
  general_plot_gui <- function() {
    library(gWidgets2)
    library(gWidgets2tcltk)
    
    # Create a window
    main_win <- gwindow("General Plot GUI", visible = FALSE)
    main_group <- ggroup(container = main_win, horizontal = FALSE)
    
    # Data input
    glabel("Select a dataset:", container = main_group)
    data_input <- gcombobox(ls(envir = .GlobalEnv), container = main_group, handler = function(h, ...) {
      cat("Handler triggered for dataset selection
  ")
      cat("Fetching dataset: ", svalue(data_input), "
  ")
      data <- get(svalue(data_input), envir = .GlobalEnv)
      cat("Dataset columns: ", names(data), "
  ")
      column_names <- names(data)
      x_axis[] <- column_names
      y_axis[] <- column_names
      wrap_by[] <- c("None", column_names)
      facet_by[] <- c("None", column_names)
    })
    
    # Axis selection
    glabel("Select x-axis column:", container = main_group)
    x_axis <- gcombobox(character(0), container = main_group)
    glabel("Select y-axis column:", container = main_group)
    y_axis <- gcombobox(character(0), container = main_group)
    
    # Optional inputs
    glabel("Optional: Wrap by column:", container = main_group)
    wrap_by <- gcombobox(character(0), container = main_group)
    glabel("Optional: Facet by column:", container = main_group)
    facet_by <- gcombobox(character(0), container = main_group)
    glabel("Select color scheme:", container = main_group)
    color_scheme <- gcombobox(c("default", "colorblind", "grayscale", "kit"), container = main_group)
    
    # Titles
    glabel("Plot title (optional):", container = main_group)
    plot_title <- gedit("Enter plot title (optional)", container = main_group)
    glabel("X-axis title (optional):", container = main_group)
    x_title <- gedit("Enter x-axis title (optional)", container = main_group)
    glabel("Y-axis title (optional):", container = main_group)
    y_title <- gedit("Enter y-axis title (optional)", container = main_group)
    
    # Plot button
    gbutton("Plot", container = main_group, handler = function(h, ...) {
      cat("Handler triggered for dataset selection
  ")
      if (is.null(svalue(data_input)) || svalue(data_input) == "") {
        gmessage("Please select a dataset.", title = "Error")
        return()
      }
      if (is.null(svalue(x_axis)) || svalue(x_axis) == "") {
        gmessage("Please select an x-axis column.", title = "Error")
        return()
      }
      if (is.null(svalue(y_axis)) || svalue(y_axis) == "") {
        gmessage("Please select a y-axis column.", title = "Error")
        return()
      }
      cat("Fetching dataset: ", svalue(data_input), "
  ")
      data <- get(svalue(data_input), envir = .GlobalEnv)
      x <- svalue(x_axis)
      y <- svalue(y_axis)
      cat("Wrap by value: ", svalue(wrap_by), "
  ")
      wrap <- if (!is.null(svalue(wrap_by)) && svalue(wrap_by) != "None" && !is.na(svalue(wrap_by)) && nzchar(svalue(wrap_by))) svalue(wrap_by) else NULL
      cat("Facet by value: ", svalue(facet_by), "
  ")
      facet <- if (!is.null(svalue(facet_by)) && svalue(facet_by) != "None" && !is.na(svalue(facet_by)) && nzchar(svalue(facet_by))) svalue(facet_by) else NULL
      color <- svalue(color_scheme)
      title <- if (nchar(svalue(plot_title)) > 0) svalue(plot_title) else NULL
      x_lab <- if (nchar(svalue(x_title)) > 0) svalue(x_title) else NULL
      y_lab <- if (nchar(svalue(y_title)) > 0) svalue(y_title) else NULL
      
      cat("Plotting with parameters: 
  ")
      cat("x: ", x, "
  ")
      cat("y: ", y, "
  ")
      cat("wrap: ", wrap, "
  ")
      cat("facet: ", facet, "
  ")
      cat("color: ", color, "
  ")
      cat("title: ", title, "
  ")
      cat("x_lab: ", x_lab, "
  ")
      cat("y_lab: ", y_lab, "
  ")
      general_plot(data, x, y, wrap, facet, color, title, x_lab, y_lab)
    })
    
    # Show the window
    visible(main_win) <- TRUE
  }
  
  # Example usage
  general_plot_gui()
