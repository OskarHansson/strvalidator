#' Create a generalized interactive Plotly plot
#'
#' Builds an interactive scatter, bar, or heatmap plot using \pkg{plotly},
#' with optional faceting or wrapping by columns such as \code{Dye} and/or
#' \code{Marker}. Supports multiple color schemes, optional log scales, and 
#' saving to HTML.
#'
#' @param data A data frame containing the variables to plot.
#' @param x,y Character strings giving the column names for the x and y axes.
#' @param labels Optional character vector of column names to include in hover
#' text.
#' @param plot_type Character; one of \code{"scatter"}, \code{"bar"},
#' or \code{"heatmap"}.
#' @param wrap_by_dye Logical; if \code{TRUE}, create separate panels
#' per \code{Dye}.
#' @param facet_by_marker Logical; if \code{TRUE}, create separate panels
#' per \code{Marker}.
#' @param color_scheme Character; one of \code{"Viridis"}, \code{"Cividis"}, 
#' \code{"GrayScale"}, or \code{"Plasma"}.
#' @param empty_facets Logical; if \code{TRUE}, show empty panels where 
#' combinations are missing.
#' @param x_title,y_title Optional axis titles. Defaults to variable names.
#' @param log_x,log_y Logical; apply log10 scaling to respective axes.
#' @param save_to_file Optional path to save the resulting HTML file.
#' @param show_plot Logical; if \code{TRUE}, print the plot after creation.
#'
#' @return A \pkg{plotly} plot object (invisible).
#'
#' @examples
#' \dontrun{
#' # Simple scatter plot
#' create_plot(mtcars, "mpg", "hp", plot_type = "scatter")
#'
#' # Faceted heatmap
#' df <- expand.grid(Dye = letters[1:3], Marker = LETTERS[1:3])
#' df$Intensity <- runif(nrow(df))
#' create_plot(df, "Dye", "Marker", plot_type = "heatmap", y_title = "Marker")
#' }
#'
#' @importFrom plotly plot_ly subplot layout plotly_empty
#' @importFrom htmlwidgets saveWidget
#' @importFrom magrittr %>%
#' @export
#'
create_plot <- function(data,
                        x,
                        y,
                        labels = NULL,
                        plot_type = c("scatter", "bar", "heatmap"),
                        wrap_by_dye = FALSE,
                        facet_by_marker = FALSE,
                        color_scheme = c("Viridis", "Cividis", "GrayScale", "Plasma"),
                        empty_facets = FALSE,
                        x_title = NULL,
                        y_title = NULL,
                        log_x = FALSE,
                        log_y = FALSE,
                        save_to_file = NULL,
                        show_plot = TRUE) {
  
  # --- Validate inputs ---------------------------------------------------------
  plot_type <- match.arg(plot_type)
  color_scheme <- match.arg(color_scheme)
  
  if (!(x %in% names(data))) stop("The specified x column does not exist in the data.")
  if (!(y %in% names(data))) stop("The specified y column does not exist in the data.")
  
  # Get default titles
  if (is.null(x_title)) x_title <- x
  if (is.null(y_title)) y_title <- y
  
  # --- Prepare hover text ------------------------------------------------------
  hover_text <- NULL
  if (!is.null(labels)) {
    if (!all(labels %in% names(data))) stop("One or more label columns are missing in data.")
    if (nrow(data) > 0) {
      hover_text <- apply(data[, labels, drop = FALSE], 1, paste, collapse = ", ")
    }
  }
  
  # --- Define color scales -----------------------------------------------------
  color_scales <- list(
    Viridis = "Viridis",
    Cividis = "Cividis",
    GrayScale = "Greys",
    Plasma = "Plasma"
  )
  color_scale <- color_scales[[color_scheme]]
  
  # --- Define the base plot builder --------------------------------------------
  base_plot <- function(sub_data, subplot_title = NULL) {
    
    if (nrow(sub_data) == 0) return(plotly::plotly_empty())
    
    htx <- if (!is.null(labels) && nrow(sub_data) > 0) {
      apply(sub_data[, labels, drop = FALSE], 1, paste, collapse = ", ")
    } else NULL
    
    if (plot_type == "scatter") {
      p <- plot_ly(
        sub_data,
        x = ~get(x), y = ~get(y),
        type = "scatter", mode = "markers",
        text = htx,
        hoverinfo = if (is.null(htx)) "x+y" else "text",
        marker = list(colorscale = color_scale)
      )
    } else if (plot_type == "bar") {
      p <- plot_ly(
        sub_data,
        x = ~get(x), y = ~get(y),
        type = "bar",
        text = htx,
        hoverinfo = if (is.null(htx)) "x+y" else "text",
        marker = list(colorscale = color_scale)
      )
    } else if (plot_type == "heatmap") {
      z_var <- if ("z" %in% names(sub_data)) "z" else y
      p <- plot_ly(
        sub_data,
        x = ~get(x), y = ~get(y),
        z = ~get(z_var),
        type = "heatmap",
        colorscale = color_scale
      )
    } else {
      stop("Unsupported plot type.")
    }
    
    if (!is.null(subplot_title)) {
      p <- p %>%
        layout(
          annotations = list(
            list(
              text = subplot_title,
              x = 0.5, y = 1.06,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              xanchor = "center", yanchor = "bottom",
              font = list(size = 11)
            )
          )
        )
    }
    
    p %>% layout(xaxis = list(title = ""), yaxis = list(title = ""))
  }
  
  # --- Build grid layout (facet/wrap) -----------------------------------------
  if (wrap_by_dye && facet_by_marker && all(c("Dye", "Marker") %in% names(data))) {
    dye_levels <- unique(data$Dye)
    rows <- lapply(dye_levels, function(dye) {
      marker_levels <- unique(data$Marker[data$Dye == dye])
      if (empty_facets) marker_levels <- union(marker_levels, unique(data$Marker))
      cells <- lapply(marker_levels, function(marker) {
        sub <- data[data$Dye == dye & data$Marker == marker, , drop = FALSE]
        base_plot(sub, subplot_title = marker)
      })
      subplot(cells, nrows = 1, shareX = TRUE, shareY = TRUE, margin = 0.03) %>%
        layout(
          annotations = list(
            list(
              text = paste("Dye:", dye),
              x = 0, y = 1.18,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              xanchor = "left", yanchor = "top",
              font = list(size = 12)
            )
          )
        )
    })
    p <- subplot(rows, nrows = length(rows), shareX = TRUE, shareY = TRUE, margin = 0.06)
  } else if (wrap_by_dye && "Dye" %in% names(data)) {
    dye_levels <- unique(data$Dye)
    rows <- lapply(dye_levels, function(dye) {
      sub <- data[data$Dye == dye, , drop = FALSE]
      base_plot(sub, subplot_title = paste("Dye:", dye))
    })
    p <- subplot(rows, nrows = length(rows), shareX = TRUE, shareY = TRUE, margin = 0.05)
  } else if (facet_by_marker && "Marker" %in% names(data)) {
    marker_levels <- unique(data$Marker)
    cells <- lapply(marker_levels, function(marker) {
      sub <- data[data$Marker == marker, , drop = FALSE]
      base_plot(sub, subplot_title = marker)
    })
    p <- subplot(cells, nrows = ceiling(length(cells) / 3), shareX = TRUE, shareY = TRUE, margin = 0.05)
  } else {
    p <- base_plot(data)
  }
  
  # --- Apply global layout and styling ----------------------------------------
  title_text <- paste(toupper(substr(plot_type, 1, 1)), substring(plot_type, 2), "Plot")
  
  p <- p %>%
    layout(
      showlegend = FALSE,
      xaxis = list(
        title = x_title,
        automargin = TRUE,
        type = if (log_x) "log" else "linear",
        title_standoff = 15
      ),
      yaxis = list(
        title = y_title,
        automargin = TRUE,
        type = if (log_y) "log" else "linear",
        title_standoff = 15
      ),
      title = list(
        text = title_text,
        x = 0.5, y = 0.98,
        xanchor = "center", yanchor = "top"
      )
    )
  
  # --- Save or print ----------------------------------------------------------
  if (!is.null(save_to_file)) htmlwidgets::saveWidget(p, save_to_file)
  if (show_plot) print(p)
  
  # --- Attach metadata for GUI / reproducibility ------------------------------
  attr(p, "settings") <- list(
    plot_type = plot_type,
    color_scheme = color_scheme,
    wrap_by_dye = wrap_by_dye,
    facet_by_marker = facet_by_marker
  )
  
  invisible(p)
}
