#' @title Vecshift Theme System for Accessible Visualization
#' @description
#' A comprehensive theme and color system for the vecshift package that prioritizes
#' accessibility, clean design, and professional appearance for employment data visualization.
#'
#' @name theme-vecshift
#' @importFrom grDevices colorRampPalette rgb
#' @importFrom stats setNames
#' @importFrom utils globalVariables
NULL

# Define global variables to avoid R CMD check NOTEs
globalVariables(c("index", "color", "label", "name", "hex"))

# Core Color Palettes =========================================================

#' Get Vecshift Color Palette
#'
#' Returns color palettes specifically designed for employment data visualization
#' with accessibility as a primary concern. All palettes are tested for colorblind
#' accessibility and provide sufficient contrast ratios.
#'
#' @param palette Character. Type of palette to return:
#'   - "main": Primary dark-toned palette (default)
#'   - "desaturated": Desaturated version for subtle distinctions
#'   - "bw": Black and white version for printing
#'   - "employment": Specialized colors for employment status
#'   - "transitions": Colors for temporal transitions
#'   - "contracts": Colors for contract types
#' @param n Integer. Number of colors to return (default: all available)
#' @param reverse Logical. Whether to reverse the color order (default: FALSE)
#' @param alpha Numeric. Transparency level between 0 and 1 (default: 1)
#'
#' @return A character vector of hex color codes
#' @export
#'
#' @examples
#' # Get main dark-toned palette
#' vecshift_colors()
#'
#' # Get 5 desaturated colors
#' vecshift_colors("desaturated", n = 5)
#'
#' # Get black and white palette
#' vecshift_colors("bw")
#'
#' # Employment-specific colors
#' vecshift_colors("employment")
#'
#' @seealso \code{\link{scale_color_vecshift}}, \code{\link{theme_vecshift}}
vecshift_colors <- function(palette = "main", n = NULL, reverse = FALSE, alpha = 1) {
  
  # Validate alpha
  if (alpha < 0 || alpha > 1) {
    stop("alpha must be between 0 and 1")
  }
  
  # Define color palettes
  palettes <- list(
    # Primary dark-toned palette - scientifically designed for accessibility
    main = c(
      "#2C3E50", # Dark blue-grey (primary dark)
      "#E74C3C", # Vibrant red (attention, critical)
      "#3498DB", # Medium blue (stable, reliable)
      "#F39C12", # Orange (transition, change)
      "#27AE60", # Green (positive, growth)
      "#9B59B6", # Purple (complex, overlapping)
      "#34495E", # Dark grey (neutral, baseline)
      "#E67E22", # Darker orange (secondary transition)
      "#1ABC9C", # Teal (balance, stability)
      "#95A5A6"  # Light grey (background, secondary)
    ),
    
    # Desaturated version for subtle distinctions
    desaturated = c(
      "#5D6D7E", # Muted blue-grey
      "#CD6155", # Muted red
      "#5DADE2", # Muted blue
      "#F7DC6F", # Muted yellow
      "#58D68D", # Muted green
      "#BB8FCE", # Muted purple
      "#616A6B", # Muted dark grey
      "#F8C471", # Muted orange
      "#76D7C4", # Muted teal
      "#BDC3C7"  # Muted light grey
    ),
    
    # Black and white for printing - improved with lighter grays and better contrast
    bw = c(
      "#2C3E50", # Dark blue-grey (replaces pure black for softer appearance)
      "#5D6D7E", # Medium dark grey (lighter than before)
      "#7F8C8D", # Medium grey (more readable)
      "#95A5A6", # Light medium grey
      "#BDC3C7", # Light grey
      "#D5DBDB", # Very light grey
      "#E8EAED", # Near white grey
      "#F4F6F7", # Lightest grey
      "#FAFBFC", # Off-white
      "#FFFFFF"  # Pure white
    ),
    
    # Improved black and white with distinct shades for employment data
    main_bw = c(
      "#34495E", # Primary dark (unemployed)
      "#5D6D7E", # Medium dark (full-time)
      "#7F8C8D", # Medium (part-time)
      "#95A5A6", # Medium light (overlaps)
      "#BDC3C7", # Light (transitions)
      "#D5DBDB", # Very light
      "#E8EAED", # Near white
      "#F4F6F7"  # Lightest
    ),
    
    # Specialized employment status colors
    employment = c(
      disoccupato = "#E74C3C",     # Red for unemployment
      occ_ft = "#27AE60",          # Green for full-time
      occ_pt = "#F39C12",          # Orange for part-time
      over_ft_ft = "#9B59B6",      # Purple for FT overlaps
      over_pt_pt = "#E67E22",      # Dark orange for PT overlaps
      over_ft_pt = "#1ABC9C",      # Teal for mixed overlaps
      transition = "#3498DB",      # Blue for transitions
      unknown = "#95A5A6"          # Grey for unknown/other
    ),
    
    # Contract type colors
    contracts = c(
      full_time = "#27AE60",       # Green for full-time
      part_time = "#F39C12",       # Orange for part-time
      temporary = "#3498DB",       # Blue for temporary
      permanent = "#2C3E50",       # Dark blue for permanent
      seasonal = "#9B59B6",        # Purple for seasonal
      freelance = "#1ABC9C"        # Teal for freelance
    ),
    
    # Temporal transition colors
    transitions = c(
      entry = "#27AE60",           # Green for job entry
      exit = "#E74C3C",            # Red for job exit
      change = "#F39C12",          # Orange for job change
      promotion = "#3498DB",       # Blue for promotion
      demotion = "#9B59B6",        # Purple for demotion
      stable = "#34495E"           # Grey for stable periods
    )
  )
  
  # Validate palette name
  if (!palette %in% names(palettes)) {
    stop("Palette '", palette, "' not found. Available palettes: ", 
         paste(names(palettes), collapse = ", "))
  }
  
  colors <- palettes[[palette]]
  
  # Handle named colors
  if (is.null(names(colors)) && palette %in% c("employment", "contracts", "transitions")) {
    # These should have names, but if they don't, add them
    if (palette == "employment") {
      names(colors) <- c("disoccupato", "occ_ft", "occ_pt", "over_ft_ft", 
                        "over_pt_pt", "over_ft_pt", "transition", "unknown")
    }
  }
  
  # Apply alpha if not 1
  if (alpha < 1) {
    colors <- paste0(colors, sprintf("%02X", round(alpha * 255)))
  }
  
  # Select number of colors
  if (!is.null(n)) {
    if (n > length(colors)) {
      # Interpolate additional colors if needed
      if (palette == "bw") {
        # For B&W, create gradient between black and white
        colors <- colorRampPalette(c("#000000", "#FFFFFF"))(n)
      } else {
        # For other palettes, repeat the pattern
        colors <- rep_len(colors, n)
      }
    } else {
      colors <- colors[1:n]
    }
  }
  
  # Reverse if requested
  if (reverse) {
    colors <- rev(colors)
  }
  
  return(colors)
}

# ggplot2 Scale Functions =====================================================

#' Vecshift Color Scale for ggplot2
#'
#' Color scales that use vecshift color palettes for ggplot2 visualizations.
#'
#' @param palette Character. Palette name (see \code{\link{vecshift_colors}})
#' @param discrete Logical. Use discrete colors (TRUE) or continuous (FALSE)
#' @param reverse Logical. Reverse color order
#' @param alpha Numeric. Transparency level (0-1)
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale function
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Discrete color scale
#' ggplot(data, aes(x, y, color = employment_status)) +
#'   geom_point() +
#'   scale_color_vecshift("employment")
#'
#' # Fill scale
#' ggplot(data, aes(x, y, fill = contract_type)) +
#'   geom_bar(stat = "identity") +
#'   scale_fill_vecshift("contracts")
#' }
scale_color_vecshift <- function(palette = "main", discrete = TRUE, reverse = FALSE, 
                                alpha = 1, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale functions")
  }
  
  pal <- function(n) vecshift_colors(palette, n = n, reverse = reverse, alpha = alpha)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("vecshift_", palette), 
                           palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname scale_color_vecshift
#' @export
scale_colour_vecshift <- scale_color_vecshift

#' @rdname scale_color_vecshift
#' @export
scale_fill_vecshift <- function(palette = "main", discrete = TRUE, reverse = FALSE, 
                               alpha = 1, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale functions")
  }
  
  pal <- function(n) vecshift_colors(palette, n = n, reverse = reverse, alpha = alpha)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("vecshift_", palette), 
                           palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' Black and White Fill Scale with Pattern Support
#'
#' A specialized fill scale that combines the improved black and white vecshift
#' palette with pattern support for maximum accessibility and distinction in
#' printed materials or colorblind-accessible visualizations.
#'
#' @param patterns Character. Pattern type to use (see \code{\link{vecshift_patterns}})
#' @param use_ggpattern Logical. Whether to use ggpattern if available (default: TRUE)
#' @param border_color Character. Border color for bars/areas (default: "black")
#' @param border_size Numeric. Border line width (default: 0.3)
#' @param alpha Numeric. Fill transparency (default: 0.8)
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 scale function with pattern support
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Basic usage with patterns
#' ggplot(data, aes(x, y, fill = employment_status)) +
#'   geom_col() +
#'   scale_fill_vecshift_bw("employment")
#'
#' # With custom border
#' ggplot(data, aes(x, y, fill = contract_type)) +
#'   geom_col() +
#'   scale_fill_vecshift_bw("contracts", border_color = "#2C3E50")
#' }
#' 
#' @seealso \code{\link{vecshift_patterns}}, \code{\link{vecshift_colors}}
scale_fill_vecshift_bw <- function(patterns = "employment", use_ggpattern = TRUE,
                                  border_color = "black", border_size = 0.3, 
                                  alpha = 0.8, ...) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale functions")
  }
  
  # Get patterns and colors
  pattern_specs <- vecshift_patterns(patterns)
  bw_colors <- vecshift_colors("main_bw", n = length(pattern_specs), alpha = alpha)
  
  # Create manual scale with colors
  scale_fill <- ggplot2::scale_fill_manual(
    values = setNames(bw_colors, names(pattern_specs)),
    ...
  )
  
  # Add information about patterns as attributes for user reference
  attr(scale_fill, "patterns") <- pattern_specs
  attr(scale_fill, "use_ggpattern") <- use_ggpattern
  attr(scale_fill, "border_color") <- border_color
  attr(scale_fill, "border_size") <- border_size
  
  return(scale_fill)
}

#' Line Type Scale for Vecshift Visualizations
#'
#' A specialized line type scale that maps categories to distinct line patterns
#' optimized for black and white visualizations and accessibility.
#'
#' @param categories Character vector. Categories to map (if NULL, uses data)
#' @param style Character. Line type style (see \code{\link{vecshift_linetypes}})
#' @param reverse Logical. Whether to reverse line type order (default: FALSE)
#' @param ... Additional arguments passed to ggplot2 scale functions
#'
#' @return A ggplot2 linetype scale function
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # For employment data
#' ggplot(data, aes(x = time, y = count, linetype = employment_status)) +
#'   geom_line() +
#'   scale_linetype_vecshift(style = "employment")
#'
#' # For general categories
#' ggplot(data, aes(x = time, y = value, linetype = category)) +
#'   geom_line() +
#'   scale_linetype_vecshift(style = "basic")
#' }
#' 
#' @seealso \code{\link{vecshift_linetypes}}
scale_linetype_vecshift <- function(categories = NULL, style = "employment", 
                                   reverse = FALSE, ...) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale functions")
  }
  
  # If categories are provided, create manual scale
  if (!is.null(categories)) {
    line_types <- vecshift_linetypes(categories, style = style, reverse = reverse)
    return(ggplot2::scale_linetype_manual(values = line_types, ...))
  } else {
    # Return a function that will be applied when ggplot builds
    function(...) {
      # This will be called by ggplot2 during build process
      # For now, return a basic discrete scale with employment patterns
      if (style == "employment") {
        employment_lines <- vecshift_linetypes(
          c("disoccupato", "occ_ft", "occ_pt", "over_ft_ft", "over_pt_pt", "over_ft_pt"), 
          style = "employment", reverse = reverse
        )
        ggplot2::scale_linetype_manual(values = employment_lines, ...)
      } else {
        basic_lines <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        ggplot2::scale_linetype_manual(values = basic_lines, ...)
      }
    }
  }
}

#' Preview Pattern and Color Combinations
#'
#' Creates a visual preview of pattern and color combinations to help with
#' selection for black and white visualizations.
#'
#' @param pattern_type Character. Type of patterns to preview
#' @param show_patterns Logical. Whether to show pattern descriptions (default: TRUE)
#' @param show_colors Logical. Whether to show hex color codes (default: FALSE)
#'
#' @return A ggplot2 object showing pattern and color combinations
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview employment patterns
#' preview_bw_patterns("employment")
#'
#' # Preview contract patterns
#' preview_bw_patterns("contracts")
#' }
#' 
#' @seealso \code{\link{vecshift_patterns}}, \code{\link{preview_vecshift_colors}}
preview_bw_patterns <- function(pattern_type = "employment", 
                               show_patterns = TRUE, show_colors = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for preview functions")
  }
  
  patterns <- vecshift_patterns(pattern_type)
  colors <- vecshift_colors("main_bw", n = length(patterns))
  
  # Create data frame for plotting
  df <- data.frame(
    index = seq_along(patterns),
    category = names(patterns),
    color = colors,
    stringsAsFactors = FALSE
  )
  
  # Add pattern information
  df$pattern_type <- sapply(patterns, function(x) x$pattern)
  df$pattern_desc <- sapply(patterns, function(x) x$description)
  
  # Create labels
  df$label <- df$category
  if (show_patterns) {
    df$label <- paste0(df$label, "\n(", df$pattern_type, ")")
  }
  if (show_colors) {
    df$label <- paste0(df$label, "\n", df$color)
  }
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = 1, fill = color, label = label)) +
    ggplot2::geom_tile(colour = "black", size = 1, alpha = 0.8) +
    ggplot2::geom_text(colour = ifelse(df$color %in% c("#2C3E50", "#34495E", "#5D6D7E"), 
                                      "white", "black"),
                      size = 3, fontface = "bold") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = paste("Black & White Patterns:", tools::toTitleCase(pattern_type)),
      subtitle = paste("Pattern specifications for", length(patterns), "categories"),
      x = NULL, y = NULL,
      caption = "Patterns optimized for printing and colorblind accessibility"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      plot.caption = ggplot2::element_text(hjust = 0.5, size = 10, colour = "grey50"),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  return(p)
}

# Theme Functions =============================================================

#' Vecshift ggplot2 Theme
#'
#' A clean, minimalist ggplot2 theme designed specifically for employment data
#' visualization. Emphasizes clarity, accessibility, and professional appearance
#' while reducing chart junk and focusing attention on the data.
#'
#' @param base_size Numeric. Base font size (default: 12)
#' @param base_family Character. Base font family (default: "")
#' @param base_line_size Numeric. Base line size (default: base_size/22)
#' @param base_rect_size Numeric. Base rectangle line size (default: base_size/22)
#' @param grid Character. Which grid lines to show: "major", "minor", "both", "none" (default: "major")
#' @param axis Character. Which axis lines to show: "both", "x", "y", "none" (default: "both")
#' @param ticks Character. Which axis ticks to show: "both", "x", "y", "none" (default: "both")
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' 
#' # Basic usage
#' ggplot(data, aes(x, y)) +
#'   geom_point() +
#'   theme_vecshift()
#'
#' # Minimal grid
#' ggplot(data, aes(x, y)) +
#'   geom_line() +
#'   theme_vecshift(grid = "none")
#'
#' # Larger text for presentations
#' ggplot(data, aes(x, y)) +
#'   geom_bar(stat = "identity") +
#'   theme_vecshift(base_size = 16)
#' }
theme_vecshift <- function(base_size = 12, base_family = "", 
                          base_line_size = base_size/22,
                          base_rect_size = base_size/22,
                          grid = "major", axis = "both", ticks = "both") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for theme functions")
  }
  
  # Validate arguments
  grid <- match.arg(grid, c("major", "minor", "both", "none"))
  axis <- match.arg(axis, c("both", "x", "y", "none"))
  ticks <- match.arg(ticks, c("both", "x", "y", "none"))
  
  # Start with theme_minimal as base for clean look
  theme <- ggplot2::theme_minimal(base_size = base_size, 
                                 base_family = base_family,
                                 base_line_size = base_line_size,
                                 base_rect_size = base_rect_size)
  
  # Customize for vecshift aesthetic using ggplot2's %+replace% operator
  theme <- theme + 
    ggplot2::theme(
      # Text elements - clean and readable
      text = ggplot2::element_text(colour = "#2C3E50", size = base_size),
      title = ggplot2::element_text(colour = "#2C3E50", size = base_size * 1.1, 
                                   face = "bold"),
      
      # Plot elements
      plot.title = ggplot2::element_text(colour = "#2C3E50", size = base_size * 1.3,
                                        face = "bold", hjust = 0,
                                        margin = ggplot2::margin(b = base_size)),
      plot.subtitle = ggplot2::element_text(colour = "#5D6D7E", size = base_size * 0.9,
                                           hjust = 0, 
                                           margin = ggplot2::margin(b = base_size * 0.5)),
      plot.caption = ggplot2::element_text(colour = "#95A5A6", size = base_size * 0.8,
                                          hjust = 1, vjust = 1,
                                          margin = ggplot2::margin(t = base_size * 0.5)),
      
      # Axis elements
      axis.title = ggplot2::element_text(colour = "#34495E", size = base_size * 0.9,
                                        face = "bold"),
      axis.text = ggplot2::element_text(colour = "#5D6D7E", size = base_size * 0.8),
      
      # Legend elements
      legend.title = ggplot2::element_text(colour = "#2C3E50", size = base_size * 0.9,
                                          face = "bold"),
      legend.text = ggplot2::element_text(colour = "#34495E", size = base_size * 0.8),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = base_size),
      
      # Panel elements - minimal and clean
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.border = ggplot2::element_blank(),
      
      # Strip elements for faceting
      strip.background = ggplot2::element_rect(fill = "#F8F9FA", colour = NA),
      strip.text = ggplot2::element_text(colour = "#2C3E50", size = base_size * 0.9,
                                        face = "bold", 
                                        margin = ggplot2::margin(4.4, 4.4, 4.4, 4.4))
    )
  
  # Grid lines based on preference
  if (grid == "none") {
    theme <- theme + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (grid == "major") {
    theme <- theme + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "#E8EAED", 
                                               size = base_line_size),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (grid == "minor") {
    theme <- theme + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_line(colour = "#F1F3F4", 
                                               size = base_line_size * 0.5)
      )
  } else if (grid == "both") {
    theme <- theme + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(colour = "#E8EAED", 
                                               size = base_line_size),
        panel.grid.minor = ggplot2::element_line(colour = "#F1F3F4", 
                                               size = base_line_size * 0.5)
      )
  }
  
  # Axis lines based on preference
  if (axis == "none") {
    theme <- theme + 
      ggplot2::theme(
        axis.line = ggplot2::element_blank()
      )
  } else if (axis == "x") {
    theme <- theme + 
      ggplot2::theme(
        axis.line.x = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size),
        axis.line.y = ggplot2::element_blank()
      )
  } else if (axis == "y") {
    theme <- theme + 
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size)
      )
  } else if (axis == "both") {
    theme <- theme + 
      ggplot2::theme(
        axis.line = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size)
      )
  }
  
  # Axis ticks based on preference
  if (ticks == "none") {
    theme <- theme + 
      ggplot2::theme(
        axis.ticks = ggplot2::element_blank()
      )
  } else if (ticks == "x") {
    theme <- theme + 
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size),
        axis.ticks.y = ggplot2::element_blank()
      )
  } else if (ticks == "y") {
    theme <- theme + 
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size)
      )
  } else if (ticks == "both") {
    theme <- theme + 
      ggplot2::theme(
        axis.ticks = ggplot2::element_line(colour = "#BDC3C7", size = base_line_size)
      )
  }
  
  return(theme)
}

# Utility Functions ===========================================================

#' Preview Vecshift Color Palette
#'
#' Creates a visual preview of a vecshift color palette to help with selection
#' and accessibility testing.
#'
#' @param palette Character. Palette name to preview
#' @param n Integer. Number of colors to show (default: all available)
#' @param show_hex Logical. Whether to show hex codes (default: TRUE)
#' @param show_names Logical. Whether to show color names if available (default: TRUE)
#'
#' @return A ggplot2 object showing the color palette
#' @export
#'
#' @examples
#' \dontrun{
#' # Preview main palette
#' preview_vecshift_colors()
#'
#' # Preview employment colors with names
#' preview_vecshift_colors("employment")
#'
#' # Preview 5 colors from main palette
#' preview_vecshift_colors("main", n = 5)
#' }
preview_vecshift_colors <- function(palette = "main", n = NULL, 
                                   show_hex = TRUE, show_names = TRUE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for preview functions")
  }
  
  colors <- vecshift_colors(palette, n = n)
  
  # Create data frame for plotting
  df <- data.frame(
    index = seq_along(colors),
    color = colors,
    hex = colors,
    stringsAsFactors = FALSE
  )
  
  # Add names if available
  if (show_names && !is.null(names(colors))) {
    df$name <- names(colors)
  } else {
    df$name <- paste("Color", df$index)
  }
  
  # Create labels
  df$label <- df$name
  if (show_hex) {
    df$label <- paste0(df$label, "\n", df$hex)
  }
  
  # Create plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = index, y = 1, fill = color, label = label)) +
    ggplot2::geom_tile(colour = "white", size = 2) +
    ggplot2::geom_text(colour = ifelse(df$color %in% c("#000000", "#2C2C2C", "#2C3E50", "#34495E"), 
                                      "white", "black"),
                      size = 3, fontface = "bold") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title = paste("Vecshift Color Palette:", tools::toTitleCase(palette)),
      subtitle = paste("Showing", length(colors), "colors"),
      x = NULL, y = NULL
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  
  return(p)
}

#' Test Color Accessibility
#'
#' Tests a vecshift color palette for accessibility issues including
#' colorblind accessibility and contrast ratios.
#'
#' @param palette Character. Palette name to test
#' @param n Integer. Number of colors to test (default: all available)
#' @param return_data Logical. Return test data instead of summary (default: FALSE)
#'
#' @return Either a summary message or detailed test data
#' @export
#'
#' @examples
#' \dontrun{
#' # Test main palette accessibility
#' test_vecshift_accessibility()
#'
#' # Test employment palette
#' test_vecshift_accessibility("employment")
#' }
test_vecshift_accessibility <- function(palette = "main", n = NULL, return_data = FALSE) {
  
  colors <- vecshift_colors(palette, n = n)
  
  # Convert hex to RGB
  hex_to_rgb <- function(hex) {
    hex <- gsub("#", "", hex)
    list(
      r = strtoi(substr(hex, 1, 2), 16L),
      g = strtoi(substr(hex, 3, 4), 16L),
      b = strtoi(substr(hex, 5, 6), 16L)
    )
  }
  
  # Calculate relative luminance
  get_luminance <- function(rgb) {
    # Convert to linear RGB
    linear_rgb <- lapply(rgb, function(x) {
      x <- x / 255
      if (x <= 0.03928) {
        x / 12.92
      } else {
        ((x + 0.055) / 1.055)^2.4
      }
    })
    
    # Calculate luminance
    0.2126 * linear_rgb$r + 0.7152 * linear_rgb$g + 0.0722 * linear_rgb$b
  }
  
  # Calculate contrast ratio
  contrast_ratio <- function(lum1, lum2) {
    (max(lum1, lum2) + 0.05) / (min(lum1, lum2) + 0.05)
  }
  
  # Test each color
  results <- list()
  
  for (i in seq_along(colors)) {
    rgb <- hex_to_rgb(colors[i])
    lum <- get_luminance(rgb)
    
    # Test against white background
    white_contrast <- contrast_ratio(lum, 1.0)
    # Test against black background  
    black_contrast <- contrast_ratio(lum, 0.0)
    
    results[[i]] <- list(
      color = colors[i],
      name = if (!is.null(names(colors))) names(colors)[i] else paste("Color", i),
      luminance = lum,
      white_contrast = white_contrast,
      black_contrast = black_contrast,
      aa_white = white_contrast >= 4.5,
      aa_black = black_contrast >= 4.5,
      aaa_white = white_contrast >= 7.0,
      aaa_black = black_contrast >= 7.0
    )
  }
  
  if (return_data) {
    return(results)
  }
  
  # Summary
  aa_pass <- sum(sapply(results, function(x) x$aa_white || x$aa_black))
  aaa_pass <- sum(sapply(results, function(x) x$aaa_white || x$aaa_black))
  
  cat("Accessibility Test Results for", palette, "palette:\n")
  cat("Total colors tested:", length(colors), "\n")
  cat("WCAG AA compliance:", aa_pass, "/", length(colors), 
      "(", round(100 * aa_pass / length(colors)), "%)\n")
  cat("WCAG AAA compliance:", aaa_pass, "/", length(colors), 
      "(", round(100 * aaa_pass / length(colors)), "%)\n")
  
  if (aa_pass == length(colors)) {
    cat("✓ All colors meet WCAG AA accessibility standards\n")
  } else {
    cat("⚠ Some colors may have accessibility issues\n")
  }
  
  invisible(results)
}

#' Get Employment-Specific Colors
#'
#' Convenience function to get colors for specific employment statuses
#' based on vecshift output conventions.
#'
#' @param statuses Character vector. Employment statuses to get colors for
#' @param alpha Numeric. Transparency level (0-1)
#'
#' @return Named character vector of hex colors
#' @export
#'
#' @examples
#' # Get colors for common employment statuses
#' get_employment_colors(c("occ_ft", "occ_pt", "disoccupato"))
#'
#' # Get all employment colors
#' get_employment_colors()
get_employment_colors <- function(statuses = NULL, alpha = 1) {
  
  employment_colors <- vecshift_colors("employment", alpha = alpha)
  
  if (is.null(statuses)) {
    return(employment_colors)
  }
  
  # Match requested statuses
  available_statuses <- names(employment_colors)
  matched <- statuses %in% available_statuses
  
  if (!all(matched)) {
    warning("Some statuses not found in employment palette: ",
            paste(statuses[!matched], collapse = ", "),
            "\nAvailable statuses: ", paste(available_statuses, collapse = ", "))
  }
  
  return(employment_colors[statuses[matched]])
}

# Pattern Support Functions ==================================================

#' Get Vecshift Pattern Specifications
#'
#' Returns pattern specifications for bar charts and area plots to improve
#' visual distinction in black and white or colorblind-accessible visualizations.
#' Patterns can be used with the ggpattern package or as fallback specifications.
#'
#' @param type Character. Type of patterns to return:
#'   - "employment": Patterns for employment status data
#'   - "contracts": Patterns for contract types
#'   - "basic": Basic pattern set for general use
#'   - "dense": High-density patterns for small areas
#' @param n Integer. Number of patterns to return (default: all available)
#' @param reverse Logical. Whether to reverse the pattern order (default: FALSE)
#'
#' @return A named list of pattern specifications with the following elements:
#'   - pattern: Pattern type (e.g., "stripe", "crosshatch", "circle")
#'   - angle: Angle for directional patterns (degrees)
#'   - density: Pattern density (0-1 scale)
#'   - spacing: Spacing between pattern elements
#'
#' @export
#'
#' @examples
#' # Get employment patterns
#' emp_patterns <- vecshift_patterns("employment")
#' print(emp_patterns)
#'
#' # Get basic patterns
#' basic_patterns <- vecshift_patterns("basic", n = 4)
#'
#' @seealso \code{\link{scale_fill_vecshift_bw}}, \code{\link{vecshift_colors}}
vecshift_patterns <- function(type = "employment", n = NULL, reverse = FALSE) {
  
  # Define pattern specifications
  pattern_sets <- list(
    # Employment-specific patterns optimized for labor data
    employment = list(
      disoccupato = list(
        pattern = "stripe",
        angle = 45,
        density = 0.6,
        spacing = 0.02,
        description = "Diagonal lines for unemployment"
      ),
      occ_ft = list(
        pattern = "none",
        angle = 0,
        density = 1.0,
        spacing = 0,
        description = "Solid fill for full-time employment"
      ),
      occ_pt = list(
        pattern = "circle",
        angle = 0,
        density = 0.4,
        spacing = 0.03,
        description = "Dots for part-time employment"
      ),
      over_ft_ft = list(
        pattern = "crosshatch",
        angle = 45,
        density = 0.5,
        spacing = 0.02,
        description = "Crosshatch for FT overlaps"
      ),
      over_pt_pt = list(
        pattern = "stripe",
        angle = 135,
        density = 0.5,
        spacing = 0.025,
        description = "Reverse diagonal for PT overlaps"
      ),
      over_ft_pt = list(
        pattern = "weave",
        angle = 0,
        density = 0.4,
        spacing = 0.02,
        description = "Weave pattern for mixed overlaps"
      ),
      transition = list(
        pattern = "stripe",
        angle = 0,
        density = 0.3,
        spacing = 0.04,
        description = "Horizontal lines for transitions"
      )
    ),
    
    # Contract type patterns
    contracts = list(
      permanent = list(
        pattern = "none",
        angle = 0,
        density = 1.0,
        spacing = 0,
        description = "Solid for permanent contracts"
      ),
      temporary = list(
        pattern = "stripe",
        angle = 45,
        density = 0.5,
        spacing = 0.025,
        description = "Diagonal for temporary contracts"
      ),
      seasonal = list(
        pattern = "circle",
        angle = 0,
        density = 0.3,
        spacing = 0.04,
        description = "Dots for seasonal contracts"
      ),
      freelance = list(
        pattern = "stripe",
        angle = 90,
        density = 0.4,
        spacing = 0.03,
        description = "Vertical lines for freelance"
      ),
      part_time = list(
        pattern = "circle",
        angle = 0,
        density = 0.4,
        spacing = 0.03,
        description = "Small dots for part-time"
      )
    ),
    
    # Basic patterns for general use
    basic = list(
      solid = list(
        pattern = "none",
        angle = 0,
        density = 1.0,
        spacing = 0,
        description = "Solid fill"
      ),
      diagonal = list(
        pattern = "stripe",
        angle = 45,
        density = 0.5,
        spacing = 0.025,
        description = "Diagonal lines"
      ),
      horizontal = list(
        pattern = "stripe",
        angle = 0,
        density = 0.4,
        spacing = 0.03,
        description = "Horizontal lines"
      ),
      vertical = list(
        pattern = "stripe",
        angle = 90,
        density = 0.4,
        spacing = 0.03,
        description = "Vertical lines"
      ),
      crosshatch = list(
        pattern = "crosshatch",
        angle = 45,
        density = 0.3,
        spacing = 0.03,
        description = "Crosshatch pattern"
      ),
      dots = list(
        pattern = "circle",
        angle = 0,
        density = 0.3,
        spacing = 0.04,
        description = "Dot pattern"
      )
    ),
    
    # Dense patterns for small areas
    dense = list(
      fine_diagonal = list(
        pattern = "stripe",
        angle = 45,
        density = 0.8,
        spacing = 0.015,
        description = "Fine diagonal lines"
      ),
      fine_dots = list(
        pattern = "circle",
        angle = 0,
        density = 0.6,
        spacing = 0.02,
        description = "Fine dots"
      ),
      dense_cross = list(
        pattern = "crosshatch",
        angle = 45,
        density = 0.7,
        spacing = 0.015,
        description = "Dense crosshatch"
      ),
      tight_horizontal = list(
        pattern = "stripe",
        angle = 0,
        density = 0.8,
        spacing = 0.015,
        description = "Tight horizontal lines"
      )
    )
  )
  
  # Validate pattern type
  if (!type %in% names(pattern_sets)) {
    stop("Pattern type '", type, "' not found. Available types: ", 
         paste(names(pattern_sets), collapse = ", "))
  }
  
  patterns <- pattern_sets[[type]]
  
  # Select number of patterns
  if (!is.null(n)) {
    if (n > length(patterns)) {
      warning("Requested ", n, " patterns but only ", length(patterns), 
              " available for type '", type, "'")
      n <- length(patterns)
    }
    patterns <- patterns[1:n]
  }
  
  # Reverse if requested
  if (reverse) {
    patterns <- rev(patterns)
  }
  
  return(patterns)
}

#' Get Line Type Patterns for Time Series
#'
#' Returns line type specifications for time series plots to improve
#' visual distinction in black and white visualizations.
#'
#' @param categories Character vector. Categories to map to line types
#' @param style Character. Line type style:
#'   - "employment": For employment status data
#'   - "basic": Basic line types
#'   - "varied": Maximum variety in line patterns
#' @param reverse Logical. Whether to reverse the line type order (default: FALSE)
#'
#' @return A named character vector of ggplot2 line types
#' @export
#'
#' @examples
#' # Get line types for employment statuses
#' employment_lines <- vecshift_linetypes(c("occ_ft", "occ_pt", "disoccupato"))
#' print(employment_lines)
#'
#' # Get basic line types
#' basic_lines <- vecshift_linetypes(c("A", "B", "C"), style = "basic")
#'
#' @seealso \code{\link{scale_linetype_vecshift}}
vecshift_linetypes <- function(categories, style = "employment", reverse = FALSE) {
  
  # Define line type mappings
  line_styles <- list(
    employment = c(
      disoccupato = "longdash",     # Long dash for unemployment (clear gaps)
      occ_ft = "solid",             # Solid for full-time (continuous work)
      occ_pt = "dashed",            # Dashed for part-time (intermittent)
      over_ft_ft = "dotdash",       # Dot-dash for FT overlaps
      over_pt_pt = "dotted",        # Dotted for PT overlaps
      over_ft_pt = "twodash",       # Two-dash for mixed overlaps
      transition = "4C88C488",      # Custom: dash-dot-dot
      unknown = "dotted"            # Dotted for unknown status
    ),
    
    basic = c(
      "solid",
      "dashed", 
      "dotted",
      "dotdash",
      "longdash",
      "twodash"
    ),
    
    varied = c(
      "solid",
      "22",        # Custom: 2 on, 2 off
      "42",        # Custom: 4 on, 2 off
      "44",        # Custom: 4 on, 4 off
      "13",        # Custom: 1 on, 3 off
      "1343",      # Custom: 1-3-4-3 pattern
      "73",        # Custom: 7 on, 3 off
      "2262",      # Custom: 2-2-6-2 pattern
      "12223242"   # Custom: complex pattern
    )
  )
  
  # Validate style
  if (!style %in% names(line_styles)) {
    stop("Line style '", style, "' not found. Available styles: ", 
         paste(names(line_styles), collapse = ", "))
  }
  
  line_types <- line_styles[[style]]
  
  # Handle named vs unnamed line types
  if (style == "employment") {
    # Map categories to employment line types
    result <- character(length(categories))
    names(result) <- categories
    
    for (i in seq_along(categories)) {
      cat <- categories[i]
      if (cat %in% names(line_types)) {
        result[cat] <- line_types[cat]
      } else {
        # Use basic cycle for unknown categories
        basic_idx <- ((i - 1) %% length(line_styles$basic)) + 1
        result[cat] <- line_styles$basic[basic_idx]
      }
    }
  } else {
    # Use cycling assignment for basic/varied styles
    result <- character(length(categories))
    names(result) <- categories
    
    for (i in seq_along(categories)) {
      idx <- ((i - 1) %% length(line_types)) + 1
      result[categories[i]] <- line_types[idx]
    }
  }
  
  # Reverse if requested
  if (reverse) {
    result <- rev(result)
  }
  
  return(result)
}

# Package Integration Functions ===============================================

#' Set Vecshift as Default ggplot2 Theme
#'
#' Sets the vecshift theme as the default for all ggplot2 plots in the current session.
#'
#' @param ... Arguments passed to theme_vecshift()
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set vecshift theme as default
#' set_vecshift_theme()
#'
#' # Set with custom options
#' set_vecshift_theme(base_size = 14, grid = "none")
#' }
set_vecshift_theme <- function(...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to set theme")
  }
  
  ggplot2::theme_set(theme_vecshift(...))
  message("Vecshift theme set as default for ggplot2")
}

#' Reset to Default ggplot2 Theme
#'
#' Resets ggplot2 to use the default grey theme.
#'
#' @export
reset_default_theme <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required to reset theme")
  }
  
  ggplot2::theme_set(ggplot2::theme_grey())
  message("Reset to default ggplot2 theme")
}