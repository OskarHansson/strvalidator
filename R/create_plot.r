library(plotly)

create_plot <- function(data, x, y, labels = NULL, x_title = NULL, y_title = NULL,
                        plot_type = "scatter", wrap_by_dye = FALSE, facet_by_marker = FALSE,
                        save_to_file = NULL, show_plot = TRUE, color_scheme = "Viridis",
                        empty_facets = FALSE, log_x = FALSE, log_y = FALSE) {
  
  if (!(x %in% names(data))) stop("The specified x column does not exist in the data")
  if (!(y %in% names(data))) stop("The specified y column does not exist in the data")
  if (is.null(x_title)) x_title <- x
  if (is.null(y_title)) y_title <- y
  
  color_scales <- list(Viridis = "Viridis", Colorblind = "Cividis", GrayScale = "Greys")
  color_scale <- color_scales[[color_scheme]]
  if (is.null(color_scale)) stop("Invalid color scheme selected")
  
  # Per-panel plot builder
  base_plot <- function(sub_data, subplot_title = NULL) {
    # (optional) hover text per subset
    htx <- if (!is.null(labels)) {
      if (!all(labels %in% names(sub_data))) stop("One or more label columns missing in sub_data")
      apply(sub_data[, labels, drop = FALSE], 1, paste, collapse = ", ")
    } else NULL
    
    if (plot_type == "scatter") {
      p <- plot_ly(sub_data,
                   x = ~get(x), y = ~get(y),
                   type = "scatter", mode = "markers",
                   text = htx, hoverinfo = if (is.null(htx)) "x+y" else "text",
                   marker = list(colorscale = color_scale))
    } else if (plot_type == "bar") {
      p <- plot_ly(sub_data,
                   x = ~get(x), y = ~get(y),
                   type = "bar",
                   text = htx, hoverinfo = if (is.null(htx)) "x+y" else "text",
                   marker = list(colorscale = color_scale))
    } else stop("Invalid plot type")
    
    # IMPORTANT: blank per-panel axis titles so subplot doesn't print 'get(x)' / 'get(y)'
    p <- p %>% layout(
      xaxis = list(title = "", automargin = TRUE),
      yaxis = list(title = "", automargin = TRUE)
    )
    
    # Per-panel title (marker name) via annotation so each mini-panel shows it
    if (!is.null(subplot_title)) {
      p <- p %>% layout(annotations = list(
        list(text = subplot_title, x = 0.5, y = 1.06,
             xref = "paper", yref = "paper",
             showarrow = FALSE, xanchor = "center", yanchor = "bottom",
             font = list(size = 11))
      ))
    }
    p
  }
  
  # Build the grid
  if (wrap_by_dye && facet_by_marker && all(c("Dye","Marker") %in% names(data))) {
    dye_levels <- unique(data$Dye)
    rows <- lapply(dye_levels, function(dye) {
      marker_levels <- unique(data$Marker[data$Dye == dye])
      if (empty_facets) marker_levels <- union(marker_levels, unique(data$Marker))
      cells <- lapply(marker_levels, function(marker) {
        sub <- data[data$Dye == dye & data$Marker == marker, , drop = FALSE]
        if (!nrow(sub) && empty_facets) return(plotly::plotly_empty())
        base_plot(sub, subplot_title = marker)
      })
      subplot(cells, nrows = 1, shareX = TRUE, shareY = TRUE,
              margin = 0.03, titleX = FALSE, titleY = FALSE) %>%
        layout(annotations = list(
          list(text = paste("Dye:", dye), x = 0, y = 1.18,
               xref = "paper", yref = "paper",
               showarrow = FALSE, xanchor = "left", yanchor = "top",
               font = list(size = 12))
        ))
    })
    p <- subplot(rows, nrows = length(rows), shareX = TRUE, shareY = TRUE,
                 margin = 0.06, titleX = FALSE, titleY = FALSE)
  } else if (wrap_by_dye && "Dye" %in% names(data)) {
    dye_levels <- unique(data$Dye)
    rows <- lapply(dye_levels, function(dye) {
      sub <- data[data$Dye == dye, , drop = FALSE]
      base_plot(sub, subplot_title = paste("Dye:", dye))
    })
    p <- subplot(rows, nrows = length(rows), shareX = TRUE, shareY = TRUE,
                 margin = 0.05, titleX = FALSE, titleY = FALSE)
  } else if (facet_by_marker && "Marker" %in% names(data)) {
    marker_levels <- unique(data$Marker)
    cells <- lapply(marker_levels, function(marker) {
      sub <- data[data$Marker == marker, , drop = FALSE]
      if (!nrow(sub) && empty_facets) return(plotly::plotly_empty())
      base_plot(sub, subplot_title = marker)
    })
    p <- subplot(cells, nrows = ceiling(length(cells) / 3), shareX = TRUE, shareY = TRUE,
                 margin = 0.05, titleX = FALSE, titleY = FALSE)
  } else {
    p <- base_plot(data)
  }
  
  # Global titles + (optional) log axes
  p <- p %>% layout(
    showlegend = FALSE,
    xaxis = list(title = x_title, automargin = TRUE,
                 type = if (log_x) "log" else "linear", title_standoff = 15),
    yaxis = list(title = y_title, automargin = TRUE,
                 type = if (log_y) "log" else "linear", title_standoff = 15),
    title = list(text = 'Your Plot Title', x = 0.5, y = 0.98, xanchor = 'center')
  )
  
  if (!is.null(save_to_file)) htmlwidgets::saveWidget(as_widget(p), save_to_file)
  if (show_plot) print(p)
  p
}
