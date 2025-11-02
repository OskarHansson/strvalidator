library(plotly)

# Function to create plots using plotly
create_plot <- function(data, x, y, labels = NULL, x_title = NULL, y_title = NULL, 
                        plot_type = "scatter", wrap_by_dye = FALSE, facet_by_marker = FALSE, 
                        save_to_file = NULL, show_plot = TRUE, color_scheme = "Viridis", empty_facets = FALSE) {
  # Check if the input columns exist in the data
  if (!(x %in% names(data))) stop("The specified x column does not exist in the data")
  if (!(y %in% names(data))) stop("The specified y column does not exist in the data")
  
  # Specify the labels if available
  hover_text <- NULL
  if (!is.null(labels)) {
    if (!all(labels %in% names(data))) stop("One or more specified label columns do not exist in the data")
    hover_text <- apply(data[, labels, with = FALSE], 1, paste, collapse = ", ")
  }
  
  # Set default axis titles
  if (is.null(x_title)) x_title <- x
  if (is.null(y_title)) y_title <- y
  
  # Set color scale based on the user selection
  color_scales <- list(
    "Viridis" = "Viridis",
    "Colorblind" = "Cividis",
    "GrayScale" = "Greys"
  )
  color_scale <- color_scales[[color_scheme]]
  if (is.null(color_scale)) stop("Invalid color scheme selected")
  
  # Prepare the base plot based on the specified plot type
  base_plot <- function(sub_data, subplot_title = NULL) {
    p <- NULL
    if (plot_type == "scatter") {
      p <- plot_ly(sub_data, x = ~get(x), y = ~get(y), type = "scatter", mode = "markers", 
                   text = hover_text, hoverinfo = "text", marker = list(colorscale = color_scale))
    } else if (plot_type == "bar") {
      p <- plot_ly(sub_data, x = ~get(x), y = ~get(y), type = "bar", 
                   text = hover_text, hoverinfo = "text", marker = list(colorscale = color_scale))
    } else if (plot_type == "heatmap") {
      p <- plot_ly(sub_data, x = ~get(x), y = ~get(y), z = ~get(y), type = "heatmap", colorscale = color_scale)
    } else {
      stop("Invalid plot type specified")
    }
    if (!is.null(subplot_title)) {
      p <- p %>% layout(annotations = list(
        list(
          text = subplot_title,
          x = 0.5,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          font = list(size = 10)
        )
      ))
    }
    return(p)
  }
  
  # Create facets or wrap if specified
  if (wrap_by_dye && "Dye" %in% names(data) && facet_by_marker && "Marker" %in% names(data)) {
    dye_levels <- unique(data$Dye)
    plots <- lapply(dye_levels, function(dye) {
      # Get markers for the current dye, and add all markers if empty_facets is TRUE
      marker_levels <- unique(data$Marker[data$Dye == dye])
      if (empty_facets) {
        marker_levels <- unique(c(marker_levels, setdiff(unique(data$Marker), marker_levels)))
      }
      subplots <- lapply(marker_levels, function(marker) {
        sub_data <- data[data$Dye == dye & data$Marker == marker, ]
        if (nrow(sub_data) == 0 && empty_facets) {
          sub_data <- data.frame(x = numeric(0), y = numeric(0))
          names(sub_data) <- c(x, y)
        }
        base_plot(sub_data, subplot_title = marker)
      })
      subplot(subplots, nrows = 1, shareX = FALSE, shareY = TRUE) %>% layout(xaxis = list(title = NULL, showticklabels = TRUE), yaxis = list(title = NULL, showticklabels = TRUE))
    })
    p <- subplot(plots, nrows = length(dye_levels), margin = 0.05, shareX = TRUE, shareY = TRUE)
  } else if (wrap_by_dye && "Dye" %in% names(data)) {
    dye_levels <- unique(data$Dye)
    plots <- lapply(dye_levels, function(dye) {
      # Get markers for the current dye, and add all markers if empty_facets is TRUE
      marker_levels <- unique(data$Marker[data$Dye == dye])
      if (empty_facets) {
        marker_levels <- unique(c(marker_levels, setdiff(unique(data$Marker), marker_levels)))
      }
      subplots <- lapply(marker_levels, function(marker) {
        sub_data <- data[data$Dye == dye & data$Marker == marker, ]
        if (nrow(sub_data) == 0 && empty_facets) {
          sub_data <- data.frame(x = numeric(0), y = numeric(0))
          names(sub_data) <- c(x, y)
        }
        base_plot(sub_data, subplot_title = marker)
      })
      subplot(subplots, nrows = 1, shareX = TRUE, shareY = TRUE) %>% layout(annotations = list(
        list(
          text = paste("Dye: ", dye),
          x = 0.5,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          yanchor = "bottom",
          font = list(size = 12)
        )
      ))
    })
    p <- subplot(plots, nrows = length(dye_levels), margin = 0.05, shareX = TRUE, shareY = TRUE)
  } else if (facet_by_marker && "Marker" %in% names(data)) {
    marker_levels <- unique(data$Marker)
    plots <- lapply(marker_levels, function(marker) {
      sub_data <- data[data$Marker == marker, ]
      if (nrow(sub_data) == 0 && empty_facets) {
        sub_data <- data.frame(x = numeric(0), y = numeric(0))
        names(sub_data) <- c(x, y)
      }
      base_plot(sub_data, subplot_title = marker)
    })
    p <- subplot(plots, nrows = ceiling(length(marker_levels) / 3), margin = 0.05, shareX = TRUE, shareY = TRUE)
  } else {
    p <- base_plot(data)
  }
  
  # Set axis titles globally
  p <- p %>% layout(
    xaxis = list(title = x_title, titlefont = list(size = 14), automargin = TRUE, showticklabels = TRUE, title_standoff = 15, anchor = 'center', titleposition = 'middle'),
    yaxis = list(title = y_title, titlefont = list(size = 14), automargin = TRUE, showticklabels = TRUE, title_standoff = 15, anchor = 'center', titleposition = 'middle'),
    showlegend = FALSE,
    title = list(text = 'Your Plot Title', x = 0.5, xanchor = 'center', yanchor = 'top', y = 1.15, font = list(size = 18))
  )
  
  # Save the plot to file if required
  if (!is.null(save_to_file)) {
    htmlwidgets::saveWidget(as_widget(p), save_to_file)
  }
  
  # Show the plot if specified
  if (show_plot) {
    print(p)
  }
  
  return(p)
}

# Example usage
# create_plot(data = your_data, x = "column1", y = "column2", labels = c("column3"), plot_type = "scatter")

create_plot(data = set6_hb, x = "MPH", y = "Hb", wrap_by_dye = TRUE, facet_by_marker = TRUE, empty_facets = FALSE)

