#' @title Vecshift Visualization Functions for Employment Data Analysis
#' @description
#' Comprehensive visualization functions designed specifically for analyzing vecshift 
#' output data. These functions provide accessible, publication-ready plots for 
#' employment data analysis with full support for colorblind accessibility, 
#' black-and-white printing, and professional presentation.
#' 
#' @name vecshift-plots
#' @importFrom data.table data.table setorder setorderv rbindlist 
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_rect geom_bar
#' @importFrom ggplot2 geom_area geom_tile geom_density geom_violin
#' @importFrom ggplot2 facet_wrap facet_grid scale_x_date scale_y_continuous
#' @importFrom ggplot2 labs theme element_text element_blank coord_flip
#' @importFrom ggplot2 guide_legend guides position_dodge geom_text
#' @importFrom ggplot2 scale_fill_manual scale_color_manual
#' @importFrom stats median quantile
#' @importFrom utils head tail
NULL

# Color Consistency Helper Functions =========================================

#' Get Standardized Employment Colors
#'
#' Internal function to ensure consistent employment status colors across all 
#' plotting functions. This function guarantees that the same employment state 
#' (e.g., unemployment, full-time, part-time) gets the same color in all visualizations,
#' which is critical for publication quality and preventing reader confusion.
#'
#' @param statuses Character vector of employment statuses present in the data
#'
#' @return Named character vector of hex colors, where names are employment statuses
#' @keywords internal
get_standardized_employment_colors <- function(statuses) {
  # Get the base employment color palette
  employment_colors <- get_employment_colors()
  
  # Filter to available statuses in the data
  available_statuses <- statuses
  colors <- employment_colors[names(employment_colors) %in% available_statuses]
  
  # Add colors for any missing statuses using fallback colors
  missing_statuses <- setdiff(available_statuses, names(colors))
  if (length(missing_statuses) > 0) {
    # Use consistent fallback colors from main palette
    additional_colors <- vecshift_colors("main", n = length(missing_statuses))
    names(additional_colors) <- missing_statuses
    colors <- c(colors, additional_colors)
  }
  
  return(colors)
}

# Core Visualization Functions ===============================================

#' Plot Employment Timeline
#'
#' Creates timeline visualizations of individual or grouped employment histories,
#' showing employment periods, unemployment gaps, and overlapping employment 
#' situations. Optimized for temporal analysis of employment patterns.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param n_people Integer. Maximum number of people to show (default: 10)
#' @param date_breaks Character. Date breaks for x-axis (default: "3 months")
#' @param show_overlaps Logical. Highlight overlapping employment periods (default: TRUE)
#' @param show_gaps Logical. Show unemployment gaps (default: TRUE)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param height_per_person Numeric. Height per person in timeline (default: 1)
#' @param alpha Numeric. Transparency for employment bars (default: 0.8)
#' @param border_color Character. Border color for employment bars (default: "white")
#' @param border_size Numeric. Border size (default: 0.3)
#'
#' @return A ggplot2 object showing employment timelines
#' @export
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' 
#' # Create sample data
#' sample_data <- data.table(
#'   cf = rep(c("Person_A", "Person_B"), each = 3),
#'   inizio = as.Date(c("2023-01-01", "2023-04-01", "2023-08-01",
#'                      "2023-02-01", "2023-06-01", "2023-10-01")),
#'   fine = as.Date(c("2023-03-31", "2023-07-31", "2023-12-31",
#'                    "2023-05-31", "2023-09-30", "2023-12-31")),
#'   arco = c(1, 0, 1, 1, 1, 2),
#'   prior = c(1, 0, 1, 0, 1, 1),
#'   id = c(1, 0, 2, 3, 4, 5),
#'   durata = c(89, 122, 153, 119, 122, 92),
#'   stato = c("occ_ft", "disoccupato", "occ_ft", "occ_pt", "occ_ft", "over_ft_ft")
#' )
#' 
#' # Basic timeline
#' plot_employment_timeline(sample_data)
#' 
#' # Timeline with faceting
#' plot_employment_timeline(sample_data, facet_by = "cf")
#' 
#' # Black and white version
#' plot_employment_timeline(sample_data, use_bw = TRUE)
#' }
#'
#' @seealso \code{\link{plot_employment_gantt}}, \code{\link{vecshift_colors}}
plot_employment_timeline <- function(data, 
                                   person_col = "cf", 
                                   time_col = "inizio", 
                                   end_col = "fine",
                                   status_col = "stato",
                                   facet_by = NULL,
                                   n_people = 10,
                                   date_breaks = "3 months",
                                   show_overlaps = TRUE,
                                   show_gaps = TRUE,
                                   palette = "employment",
                                   use_bw = FALSE,
                                   height_per_person = 1,
                                   alpha = 0.8,
                                   border_color = "white",
                                   border_size = 0.3) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Input 'data' must be a data.table object")
  }
  
  required_cols <- c(person_col, time_col, end_col, status_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Limit to n_people if specified
  if (!is.null(n_people) && n_people > 0) {
    people_to_show <- head(unique(data[[person_col]]), n_people)
    plot_data <- data[get(person_col) %in% people_to_show]
  } else {
    plot_data <- copy(data)
  }
  
  # Create y-position for each person
  people <- unique(plot_data[[person_col]])
  plot_data[, y_pos := match(get(person_col), people)]
  
  # Filter data based on show_gaps and show_overlaps
  if (!show_gaps) {
    plot_data <- plot_data[get(status_col) != "disoccupato"]
  }
  
  if (!show_overlaps) {
    plot_data <- plot_data[!grepl("^over_", get(status_col))]
  }
  
  # Get colors using standardized employment color palette
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    # Use standardized employment colors for consistency across all plotting functions
    colors <- get_standardized_employment_colors(unique(plot_data[[status_col]]))
  }
  
  # Create the base plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    xmin = get(time_col), 
    xmax = get(end_col),
    ymin = y_pos - height_per_person/2,
    ymax = y_pos + height_per_person/2,
    fill = get(status_col)
  )) +
    ggplot2::geom_rect(alpha = alpha, 
                      color = border_color, 
                      size = border_size) +
    ggplot2::scale_fill_manual(values = colors,
                              name = "Employment Status") +
    ggplot2::scale_x_date(date_breaks = date_breaks,
                         date_labels = "%b %Y",
                         expand = c(0.02, 0)) +
    ggplot2::scale_y_continuous(breaks = seq_along(people),
                               labels = people,
                               expand = c(0.02, 0)) +
    ggplot2::labs(
      title = "Employment Timeline",
      subtitle = paste("Showing", length(people), "individuals"),
      x = "Time Period",
      y = "Individuals",
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 11, grid = "major", axis = "both") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(plot_data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y")
  }
  
  return(p)
}

#' Plot Employment Status Distribution
#'
#' Creates distribution plots showing how employment statuses change over time
#' or across different dimensions. Supports various visualization types including
#' stacked areas, bars, and proportional views.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param agg_period Character. Time aggregation period: "month", "quarter", "year" (default: "month")
#' @param plot_type Character. Type of plot: "area", "bar", "proportion" (default: "area")
#' @param group_by Character. Column to group by (default: NULL)
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param show_counts Logical. Show actual counts vs proportions (default: TRUE)
#' @param smooth_trend Logical. Add smooth trend lines (default: FALSE)
#' @param alpha Numeric. Transparency (default: 0.7)
#'
#' @return A ggplot2 object showing employment status distributions
#' @export
#'
#' @examples
#' \dontrun{
#' # Area plot of employment distribution over time
#' plot_employment_distribution(data, plot_type = "area")
#' 
#' # Bar chart by quarter
#' plot_employment_distribution(data, agg_period = "quarter", plot_type = "bar")
#' 
#' # Proportional view
#' plot_employment_distribution(data, plot_type = "proportion", show_counts = FALSE)
#' }
plot_employment_distribution <- function(data,
                                       time_col = "inizio", 
                                       status_col = "stato",
                                       agg_period = "month",
                                       plot_type = "area",
                                       group_by = NULL,
                                       facet_by = NULL,
                                       palette = "employment",
                                       use_bw = FALSE,
                                       show_counts = TRUE,
                                       smooth_trend = FALSE,
                                       alpha = 0.7) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Input 'data' must be a data.table object")
  }
  
  plot_type <- match.arg(plot_type, c("area", "bar", "proportion"))
  agg_period <- match.arg(agg_period, c("month", "quarter", "year"))
  
  # Create time aggregation
  plot_data <- copy(data)
  
  if (agg_period == "month") {
    plot_data[, time_period := as.Date(format(get(time_col), "%Y-%m-01"))]
    date_format <- "%b %Y"
    date_breaks <- "3 months"
  } else if (agg_period == "quarter") {
    plot_data[, time_period := as.Date(paste0(format(get(time_col), "%Y"), "-", 
                                             sprintf("%02d", ((as.numeric(format(get(time_col), "%m")) - 1) %/% 3) * 3 + 1), 
                                             "-01"))]
    date_format <- "Q%q %Y"
    date_breaks <- "6 months"
  } else {
    plot_data[, time_period := as.Date(paste0(format(get(time_col), "%Y"), "-01-01"))]
    date_format <- "%Y"
    date_breaks <- "1 year"
  }
  
  # Aggregate data
  if (is.null(group_by)) {
    if (show_counts) {
      agg_data <- plot_data[, .(count = .N), by = .(time_period, status = get(status_col))]
    } else {
      agg_data <- plot_data[, .(duration = sum(durata, na.rm = TRUE)), by = .(time_period, status = get(status_col))]
      setnames(agg_data, "duration", "count")
    }
  } else {
    if (show_counts) {
      agg_data <- plot_data[, .(count = .N), by = c("time_period", "status" = status_col, group_by)]
    } else {
      agg_data <- plot_data[, .(count = sum(durata, na.rm = TRUE)), by = c("time_period", "status" = status_col, group_by)]
    }
  }
  
  # Calculate proportions for proportion plot
  if (plot_type == "proportion") {
    if (is.null(group_by)) {
      agg_data[, proportion := count / sum(count), by = time_period]
    } else {
      agg_data[, proportion := count / sum(count), by = c("time_period", group_by)]
    }
  }
  
  # Get colors using standardized employment color palette
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(agg_data$status)))
    names(colors) <- unique(agg_data$status)
  } else {
    # Use standardized employment colors for consistency across all plotting functions
    colors <- get_standardized_employment_colors(unique(agg_data$status))
  }
  
  # Create base plot
  if (plot_type == "area") {
    p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = time_period, y = count, fill = status)) +
      ggplot2::geom_area(alpha = alpha, position = "stack") +
      ggplot2::labs(y = if (show_counts) "Count" else "Total Duration (days)")
      
  } else if (plot_type == "bar") {
    p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = time_period, y = count, fill = status)) +
      ggplot2::geom_bar(stat = "identity", alpha = alpha, position = "stack") +
      ggplot2::labs(y = if (show_counts) "Count" else "Total Duration (days)")
      
  } else if (plot_type == "proportion") {
    p <- ggplot2::ggplot(agg_data, ggplot2::aes(x = time_period, y = proportion, fill = status)) +
      ggplot2::geom_area(alpha = alpha, position = "stack") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::labs(y = "Proportion")
  }
  
  # Add colors and theme
  p <- p +
    ggplot2::scale_fill_manual(values = colors, name = "Employment Status") +
    ggplot2::scale_x_date(date_breaks = date_breaks,
                         date_labels = date_format,
                         expand = c(0.02, 0)) +
    ggplot2::labs(
      title = paste("Employment Status Distribution Over Time"),
      subtitle = paste("Aggregated by", agg_period, "- showing", 
                      if (show_counts) "counts" else "total duration"),
      x = "Time Period",
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 11, grid = "major") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  # Add smooth trend lines if requested
  if (smooth_trend && plot_type != "proportion") {
    p <- p + ggplot2::geom_smooth(ggplot2::aes(color = status), 
                                 method = "loess", se = FALSE, alpha = 0.8)
  }
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free_y")
  }
  
  return(p)
}

#' Plot Overlapping Employment Patterns
#'
#' Visualizes patterns of overlapping employment (multiple concurrent jobs),
#' showing the intensity and duration of overlaps across individuals or time.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param arco_col Character. Column name for overlap count (default: "arco")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param plot_type Character. Type of plot: "heatmap", "timeline", "distribution" (default: "heatmap")
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param n_people Integer. Maximum number of people to show (default: 20)
#' @param min_overlap Integer. Minimum overlap level to show (default: 2)
#' @param palette Character. Color palette to use (default: "main")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param alpha Numeric. Transparency (default: 0.8)
#'
#' @return A ggplot2 object showing overlap patterns
#' @export
#'
#' @examples
#' \dontrun{
#' # Heatmap of overlap intensity
#' plot_overlap_patterns(data, plot_type = "heatmap")
#' 
#' # Timeline view of overlaps
#' plot_overlap_patterns(data, plot_type = "timeline", n_people = 10)
#' 
#' # Distribution of overlap levels
#' plot_overlap_patterns(data, plot_type = "distribution")
#' }
plot_overlap_patterns <- function(data,
                                 person_col = "cf",
                                 time_col = "inizio", 
                                 end_col = "fine",
                                 arco_col = "arco",
                                 status_col = "stato",
                                 plot_type = "heatmap",
                                 facet_by = NULL,
                                 n_people = 20,
                                 min_overlap = 2,
                                 palette = "main",
                                 use_bw = FALSE,
                                 alpha = 0.8) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  plot_type <- match.arg(plot_type, c("heatmap", "timeline", "distribution"))
  
  # Filter for overlapping employment only
  overlap_data <- data[get(arco_col) >= min_overlap]
  
  if (nrow(overlap_data) == 0) {
    warning("No overlapping employment periods found with minimum overlap >= ", min_overlap)
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                          label = paste("No overlaps found with arco >=", min_overlap)),
                             size = 6) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "No Overlapping Employment Found"))
  }
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw")
  } else {
    colors <- vecshift_colors(palette)
  }
  
  if (plot_type == "heatmap") {
    # Create monthly aggregation for heatmap
    overlap_data[, month := as.Date(format(get(time_col), "%Y-%m-01"))]
    overlap_data[, overlap_level := pmin(get(arco_col), 5)]  # Cap at 5 for visualization
    
    # Aggregate by month and person
    heatmap_data <- overlap_data[, .(
      avg_overlap = as.numeric(mean(overlap_level, na.rm = TRUE)),
      max_overlap = as.numeric(max(overlap_level, na.rm = TRUE)),
      n_periods = as.integer(.N)
    ), by = .(person = get(person_col), month)]
    
    # Limit to n_people
    if (!is.null(n_people) && n_people > 0) {
      people_to_show <- head(unique(heatmap_data$person), n_people)
      heatmap_data <- heatmap_data[person %in% people_to_show]
    }
    
    p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = month, y = person, fill = avg_overlap)) +
      ggplot2::geom_tile(alpha = alpha, color = "white", size = 0.2) +
      ggplot2::scale_fill_gradient2(
        low = colors[1], mid = colors[3], high = colors[5],
        midpoint = 2.5,
        name = "Avg\nOverlap\nLevel",
        guide = ggplot2::guide_colorbar(title.position = "top")
      ) +
      ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      ggplot2::labs(
        title = "Employment Overlap Intensity Heatmap",
        subtitle = paste("Monthly average overlap levels for", length(people_to_show), "individuals"),
        x = "Month",
        y = "Individuals",
        caption = "Generated with vecshift visualization functions"
      ) +
      theme_vecshift(base_size = 10, grid = "none") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text.y = ggplot2::element_text(size = 8),
        legend.position = "right"
      )
      
  } else if (plot_type == "timeline") {
    # Timeline view similar to employment timeline but focused on overlaps
    if (!is.null(n_people) && n_people > 0) {
      people_to_show <- head(unique(overlap_data[[person_col]]), n_people)
      plot_data <- overlap_data[get(person_col) %in% people_to_show]
    } else {
      plot_data <- copy(overlap_data)
    }
    
    people <- unique(plot_data[[person_col]])
    plot_data[, y_pos := match(get(person_col), people)]
    plot_data[, overlap_intensity := pmin(get(arco_col), 5)]
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      xmin = get(time_col), 
      xmax = get(end_col),
      ymin = y_pos - 0.4,
      ymax = y_pos + 0.4,
      fill = factor(overlap_intensity)
    )) +
      ggplot2::geom_rect(alpha = alpha, color = "white", size = 0.2) +
      ggplot2::scale_fill_manual(
        values = colors[1:length(unique(plot_data$overlap_intensity))],
        name = "Overlap\nLevel"
      ) +
      ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      ggplot2::scale_y_continuous(breaks = seq_along(people), labels = people) +
      ggplot2::labs(
        title = "Overlapping Employment Timeline",
        subtitle = paste("Showing overlap intensity for", length(people), "individuals"),
        x = "Time Period",
        y = "Individuals",
        caption = "Generated with vecshift visualization functions"
      ) +
      theme_vecshift(base_size = 11, grid = "major") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
      
  } else if (plot_type == "distribution") {
    # Distribution of overlap levels
    p <- ggplot2::ggplot(overlap_data, ggplot2::aes(x = factor(get(arco_col)), fill = factor(get(arco_col)))) +
      ggplot2::geom_bar(alpha = alpha, color = "white", size = 0.3) +
      ggplot2::scale_fill_manual(
        values = colors[1:length(unique(overlap_data[[arco_col]]))],
        name = "Overlap\nLevel"
      ) +
      ggplot2::labs(
        title = "Distribution of Employment Overlap Levels",
        subtitle = paste("Total periods with overlaps:", nrow(overlap_data)),
        x = "Number of Concurrent Jobs (Arco Level)",
        y = "Count of Employment Periods",
        caption = "Generated with vecshift visualization functions"
      ) +
      theme_vecshift(base_size = 11, grid = "major") +
      ggplot2::theme(legend.position = "none")  # Colors are self-explanatory
  }
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free")
  }
  
  return(p)
}

#' Plot Duration Analysis
#'
#' Analyzes and visualizes the duration of employment periods by status,
#' showing distributions, comparisons, and patterns in employment lengths.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param duration_col Character. Column name for duration (default: "durata")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param plot_type Character. Type of plot: "boxplot", "violin", "histogram", "density" (default: "boxplot")
#' @param group_by Character. Column to group by (default: NULL)
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param log_scale Logical. Use log scale for duration (default: FALSE)
#' @param show_outliers Logical. Show outliers in boxplot (default: TRUE)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param alpha Numeric. Transparency (default: 0.7)
#' @param min_duration Integer. Minimum duration to include (default: 1)
#' @param max_duration Integer. Maximum duration to include (default: NULL)
#'
#' @return A ggplot2 object showing duration analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Boxplot of durations by employment status
#' plot_duration_analysis(data, plot_type = "boxplot")
#' 
#' # Violin plot with log scale
#' plot_duration_analysis(data, plot_type = "violin", log_scale = TRUE)
#' 
#' # Histogram faceted by status
#' plot_duration_analysis(data, plot_type = "histogram", facet_by = "stato")
#' }
plot_duration_analysis <- function(data,
                                 duration_col = "durata",
                                 status_col = "stato", 
                                 person_col = "cf",
                                 plot_type = "boxplot",
                                 group_by = NULL,
                                 facet_by = NULL,
                                 log_scale = FALSE,
                                 show_outliers = TRUE,
                                 palette = "employment",
                                 use_bw = FALSE,
                                 alpha = 0.7,
                                 min_duration = 1,
                                 max_duration = NULL) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  plot_type <- match.arg(plot_type, c("boxplot", "violin", "histogram", "density"))
  
  # Filter data
  plot_data <- copy(data)
  plot_data <- plot_data[get(duration_col) >= min_duration]
  
  if (!is.null(max_duration)) {
    plot_data <- plot_data[get(duration_col) <= max_duration]
  }
  
  # Remove rows with missing duration
  plot_data <- plot_data[!is.na(get(duration_col))]
  
  if (nrow(plot_data) == 0) {
    stop("No valid duration data found after filtering")
  }
  
  # Get colors using standardized employment color palette
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    # Use standardized employment colors for consistency across all plotting functions
    colors <- get_standardized_employment_colors(unique(plot_data[[status_col]]))
  }
  
  # Create base plot
  if (plot_type == "boxplot") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = get(status_col), y = get(duration_col), 
                                                 fill = get(status_col))) +
      ggplot2::geom_boxplot(alpha = alpha, outlier.alpha = if (show_outliers) 0.6 else 0) +
      ggplot2::labs(x = "Employment Status", y = "Duration (days)")
      
  } else if (plot_type == "violin") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = get(status_col), y = get(duration_col), 
                                                 fill = get(status_col))) +
      ggplot2::geom_violin(alpha = alpha, trim = FALSE) +
      ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, outlier.alpha = if (show_outliers) 0.6 else 0) +
      ggplot2::labs(x = "Employment Status", y = "Duration (days)")
      
  } else if (plot_type == "histogram") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = get(duration_col), fill = get(status_col))) +
      ggplot2::geom_histogram(alpha = alpha, bins = 30, position = "identity") +
      ggplot2::labs(x = "Duration (days)", y = "Count")
      
  } else if (plot_type == "density") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = get(duration_col), fill = get(status_col))) +
      ggplot2::geom_density(alpha = alpha) +
      ggplot2::labs(x = "Duration (days)", y = "Density")
  }
  
  # Add colors and theme
  p <- p +
    ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
    ggplot2::labs(
      title = paste("Duration Analysis:", tools::toTitleCase(plot_type)),
      subtitle = paste("Analysis of", nrow(plot_data), "employment periods"),
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 11, grid = "major") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  # Apply log scale if requested
  if (log_scale) {
    p <- p + ggplot2::scale_y_log10(
      breaks = c(1, 7, 30, 90, 180, 365, 730),
      labels = c("1 day", "1 week", "1 month", "3 months", "6 months", "1 year", "2 years")
    )
    p <- p + ggplot2::labs(subtitle = paste(p$labels$subtitle, "(log scale)"))
  }
  
  # Add summary statistics annotation for boxplot/violin
  if (plot_type %in% c("boxplot", "violin")) {
    summary_stats <- plot_data[, .(
      median = as.numeric(median(get(duration_col), na.rm = TRUE)),
      mean = as.numeric(mean(get(duration_col), na.rm = TRUE)),
      q75 = as.numeric(quantile(get(duration_col), 0.75, na.rm = TRUE)),
      n = as.integer(.N)
    ), by = .(status = get(status_col))]
    
    # Add text annotations could be implemented here if desired
  }
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(plot_data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free")
  }
  
  return(p)
}

#' Plot Employment Transition Flows
#'
#' Creates alluvial flow diagrams showing how individuals transition between employment 
#' statuses over time. Uses ggalluvial to create proper flow ribbons that connect 
#' employment categories across multiple time periods, showing both the stacked 
#' distribution at each time point and the flowing transitions between them.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param plot_type Character. Type of plot: "alluvial", "flow", "sankey" (default: "alluvial")
#' @param agg_period Character. Time aggregation: "year", "quarter", "month" (default: "year")
#' @param min_freq Integer. Minimum frequency to include status-period combinations (default: 10)
#' @param max_categories Integer. Maximum employment categories to show (default: 8)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param alpha Numeric. Flow transparency (default: 0.7)
#' @param show_percentages Logical. Show percentages in stratum labels (default: TRUE)
#' @param stratum_width Numeric. Width of the stacked bars/strata (default: 0.3)
#'
#' @return A ggplot2 object showing transition flows with proper alluvial ribbons
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic alluvial diagram showing flows across years
#' plot_transition_flows(data, plot_type = "alluvial", agg_period = "year")
#' 
#' # Flow-only visualization (no stacked bars)
#' plot_transition_flows(data, plot_type = "flow", agg_period = "quarter")
#' 
#' # Sankey summary (first to last status)
#' plot_transition_flows(data, plot_type = "sankey")
#' }
plot_transition_flows <- function(data,
                                 person_col = "cf",
                                 time_col = "inizio", 
                                 status_col = "stato",
                                 plot_type = "alluvial",
                                 agg_period = "year",
                                 min_freq = 10,
                                 max_categories = 8,
                                 palette = "employment",
                                 use_bw = FALSE,
                                 alpha = 0.7,
                                 show_percentages = TRUE,
                                 stratum_width = 0.3) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("Package 'ggalluvial' is required for alluvial plots")
  }
  
  # Input validation
  plot_type <- match.arg(plot_type, c("alluvial", "flow", "sankey"))
  agg_period <- match.arg(agg_period, c("year", "quarter", "month"))
  
  # Create time periods for flow analysis
  plot_data <- copy(data)
  setorderv(plot_data, c(person_col, time_col))
  
  # Create time period grouping
  if (agg_period == "year") {
    plot_data[, time_period := format(as.Date(get(time_col)), "%Y")]
  } else if (agg_period == "quarter") {
    plot_data[, time_period := paste0(format(as.Date(get(time_col)), "%Y-Q"), 
                                     ceiling(as.numeric(format(as.Date(get(time_col)), "%m"))/3))]
  } else {
    plot_data[, time_period := format(as.Date(get(time_col)), "%Y-%m")]
  }
  
  # For each person, get their status in each time period
  # Take the most frequent status per person per period
  person_status_by_period <- plot_data[, {
    if (.N > 0) {
      status_counts <- table(get(status_col))
      if (length(status_counts) > 0) {
        most_frequent_status <- names(status_counts)[which.max(status_counts)]
        .(status = most_frequent_status)
      } else {
        .(status = character(0))
      }
    } else {
      .(status = character(0))
    }
  }, by = c(person_col, "time_period")]
  
  # Remove any empty results
  person_status_by_period <- person_status_by_period[status != ""]
  
  # Filter to keep only people with data across multiple periods
  people_with_transitions <- person_status_by_period[, .N, by = person_col][N >= 2]
  person_status_by_period <- person_status_by_period[get(person_col) %in% people_with_transitions[[person_col]]]
  
  if (nrow(person_status_by_period) == 0) {
    warning("No individuals found with employment across multiple time periods")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                          label = "No longitudinal employment data found"),
                             size = 6) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "Insufficient Longitudinal Data"))
  }
  
  # Limit categories if needed - keep most common statuses
  all_statuses <- unique(person_status_by_period$status)
  if (length(all_statuses) > max_categories) {
    status_freq <- person_status_by_period[, .N, by = status]
    setorderv(status_freq, "N", -1)
    keep_statuses <- head(status_freq$status, max_categories)
    person_status_by_period <- person_status_by_period[status %in% keep_statuses]
  }
  
  # Filter by minimum frequency
  status_period_counts <- person_status_by_period[, .N, by = .(time_period, status)]
  valid_combinations <- status_period_counts[N >= min_freq]
  
  if (nrow(valid_combinations) == 0) {
    warning(paste("No time period-status combinations with >=", min_freq, "individuals"))
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                          label = paste("No combinations with >=", min_freq, "people")),
                             size = 5) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "Insufficient Data for Flow Visualization"))
  }
  
  # Ensure we have at least 2 time periods
  unique_periods <- unique(person_status_by_period$time_period)
  if (length(unique_periods) < 2) {
    warning("Need at least 2 time periods for flow visualization")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                          label = "Need multiple time periods for flows"),
                             size = 6) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "Insufficient Time Periods"))
  }
  
  # Sort time periods chronologically
  person_status_by_period[, time_period := factor(time_period, levels = sort(unique(time_period)))]
  
  # Get standardized colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(person_status_by_period$status)))
    names(colors) <- unique(person_status_by_period$status)
  } else {
    colors <- get_standardized_employment_colors(unique(person_status_by_period$status))
  }
  
  if (plot_type == "alluvial") {
    # Create proper alluvial plot using ggalluvial
    p <- ggplot2::ggplot(person_status_by_period,
                        ggplot2::aes(x = time_period, stratum = status, 
                                    alluvium = get(person_col), fill = status)) +
      ggalluvial::geom_flow(alpha = alpha, curve_type = "cubic", 
                           width = stratum_width) +
      ggalluvial::geom_stratum(alpha = 0.8, width = stratum_width) +
      ggplot2::geom_text(stat = ggalluvial::StatStratum, 
                        ggplot2::aes(label = ggplot2::after_stat(stratum)),
                        size = 3, color = "white", fontface = "bold") +
      ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
      ggplot2::labs(
        title = "Employment Status Flows Over Time",
        subtitle = paste("Showing employment transitions for", 
                        length(unique(person_status_by_period[[person_col]])), "individuals"),
        x = paste("Time Period (", agg_period, ")", sep = ""),
        y = "Number of Individuals",
        caption = "Flow ribbons show individual transitions between employment states"
      ) +
      theme_vecshift(base_size = 11, grid = "major", axis = "both") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right",
        legend.box = "vertical"
      )
    
    # Add percentages to stratum labels if requested
    if (show_percentages) {
      period_status_counts <- person_status_by_period[, .(count = .N), by = .(time_period, status)]
      period_totals <- period_status_counts[, .(total = sum(count)), by = time_period]
      period_status_counts <- merge(period_status_counts, period_totals, by = "time_period")
      period_status_counts[, percentage := round(100 * count / total, 1)]
      period_status_counts[, label := paste0(status, "\n(", percentage, "%)")]
      
      p <- p + ggplot2::geom_text(stat = ggalluvial::StatStratum, 
                                 ggplot2::aes(label = paste0(ggplot2::after_stat(stratum), "\n(",
                                                           round(100 * ggplot2::after_stat(count) / 
                                                                sum(ggplot2::after_stat(count)), 1), "%)")),
                                 size = 2.5, color = "white", fontface = "bold")
    }
    
  } else if (plot_type == "flow") {
    # Create flow-only visualization (flows without strata)
    p <- ggplot2::ggplot(person_status_by_period,
                        ggplot2::aes(x = time_period, stratum = status, 
                                    alluvium = get(person_col), fill = status)) +
      ggalluvial::geom_flow(alpha = alpha, curve_type = "cubic") +
      ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
      ggplot2::labs(
        title = "Employment Status Flow Transitions",
        subtitle = paste("Flow patterns for", 
                        length(unique(person_status_by_period[[person_col]])), "individuals"),
        x = paste("Time Period (", agg_period, ")", sep = ""),
        y = "Flow Volume",
        caption = "Pure flow visualization showing transition patterns"
      ) +
      theme_vecshift(base_size = 11, grid = "major", axis = "x") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
      
  } else {
    # Sankey-style with transition summary
    transitions <- person_status_by_period[, .SD[c(1, .N)], by = person_col]
    if (nrow(transitions) > 0 && ncol(transitions) >= 4) {
      transition_summary <- transitions[, {
        if (.N == 2) {
          .(from_status = status[1], to_status = status[2], 
            from_period = time_period[1], to_period = time_period[2])
        }
      }, by = person_col]
      
      if (nrow(transition_summary) > 0) {
        transition_counts <- transition_summary[, .N, by = .(from_status, to_status)]
        setnames(transition_counts, "N", "count")
        
        p <- ggplot2::ggplot(transition_counts, 
                            ggplot2::aes(axis1 = from_status, axis2 = to_status, y = count)) +
          ggalluvial::geom_alluvium(ggplot2::aes(fill = from_status), alpha = alpha) +
          ggalluvial::geom_stratum(alpha = 0.8) +
          ggplot2::geom_text(stat = ggalluvial::StatStratum, ggplot2::aes(label = ggplot2::after_stat(stratum)),
                            size = 3, color = "white", fontface = "bold") +
          ggplot2::scale_x_discrete(limits = c("Initial Status", "Final Status"), expand = c(0.1, 0)) +
          ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
          ggplot2::labs(
            title = "Employment Status Transitions Summary",
            subtitle = paste("Initial to final status for", nrow(transition_summary), "individuals"),
            y = "Number of Individuals",
            caption = "Sankey diagram showing overall transition patterns"
          ) +
          theme_vecshift(base_size = 11, grid = "major", axis = "y")
      }
    }
  }
  
  return(p)
}

#' Plot Employment Gantt Chart
#'
#' Creates Gantt chart visualizations of employment periods, ideal for showing
#' individual employment histories, project timelines, or contract overlaps
#' with precise temporal detail. The function uses standardized employment colors
#' for consistency across all longworkR visualizations and supports explicit
#' person selection for publication reproducibility.
#' 
#' @details
#' The function creates professional Gantt charts with the following features:
#' \itemize{
#'   \item **Standardized Color Consistency**: Uses the same employment status colors
#'     across all longworkR functions to prevent reader confusion
#'   \item **Publication Reproducibility**: Supports explicit person_ids parameter
#'     to ensure identical visualizations across analyses
#'   \item **Accessible Design**: Colorblind-friendly palettes and clear visual hierarchy
#'   \item **Flexible Annotations**: Optional contract IDs and duration labels
#'   \item **Professional Theming**: Uses theme_vecshift() for publication-ready output
#' }
#' 
#' **Color Consistency**: This function automatically uses standardized employment
#' colors from get_standardized_employment_colors() to ensure that the same employment
#' state (e.g., "occ_ft", "disoccupato") gets identical colors in all visualizations.
#' This is critical for multi-panel figures and cross-referenced publications.
#'
#' @param data Data.table output from vecshift() containing employment segments.
#'   Required columns: cf (person ID), inizio (start date), fine (end date),
#'   stato (employment status), durata (duration in days)
#' @param person_ids Character or numeric vector. Specific person identifiers to visualize.
#'   When provided, shows only these persons and ignores max_persons parameter.
#'   When NULL (default), uses max_persons to select first N individuals.
#'   **For reproducible research**, always specify person_ids explicitly rather than
#'   relying on automatic selection. Example: c(6, 8, 21) or c("Person_A", "Person_B")
#' @param max_persons Integer. Maximum number of people to show when person_ids is NULL.
#'   Used only for backward compatibility and exploratory analysis (default: 10)
#' @param sort_by Character. Sort method for person selection when using max_persons:
#'   "total_duration", "first_contract", "last_contract" (default: "total_duration").
#'   Not used when person_ids is specified
#' @param title Character. Main title for the plot (default: "Employment Gantt Chart")
#' @param show_legend Logical. Whether to show the employment status legend (default: TRUE).
#'   Set to FALSE for multi-panel figures where legend appears elsewhere
#' @param time_unit Character. Time unit for axis labels: "days", "weeks", "months", "years".
#'   Currently used for documentation; axis formatting is automatic (default: "months")
#' @param color_palette Character. Color palette name. Use "employment" for standardized
#'   employment colors (recommended) or "main" for general palette (default: "employment")
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for period start dates (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param contract_col Character. Column name for contract ID numbers (default: "id")
#' @param facet_by Character. Column to use for faceting into subplots (default: NULL)
#' @param date_breaks Character. Date breaks for x-axis labels (default: "2 months")
#' @param show_contract_ids Logical. Show contract ID numbers on employment bars (default: FALSE).
#'   Useful for detailed contract analysis but can clutter visualization with many contracts
#' @param show_durations Logical. Show duration labels (e.g., "3m", "1.2y") on bars (default: FALSE).
#'   Automatically formats as days (d), months (m), or years (y) based on duration length
#' @param palette Character. Alternative parameter name for color_palette (default: "employment")
#' @param use_bw Logical. Use black and white palette for print publications (default: FALSE)
#' @param bar_height Numeric. Height of Gantt bars as proportion of available space (default: 0.6)
#' @param alpha Numeric. Transparency level for bars, 0 (transparent) to 1 (opaque) (default: 0.8)
#' @param text_size Numeric. Size of text labels for contract IDs and durations (default: 2.5)
#'
#' @return A ggplot2 object containing the employment Gantt chart.
#'   The plot includes milestone markers for contract start/end dates and uses
#'   standardized employment colors for consistency across publications.
#'   
#' @section Color Standards:
#' This function uses standardized employment colors to ensure consistency:
#' \itemize{
#'   \item Same employment status always gets same color across all plots
#'   \item Colors are colorblind-accessible and print-friendly
#'   \item Consistent with plot_employment_gantt_advanced() and other functions
#' }
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Load sample employment data
#' employment_data <- readRDS("data/sample.rds")
#' 
#' # Basic Gantt chart (backward compatible - shows first 10 people)
#' plot_employment_gantt(employment_data)
#' 
#' # Reproducible research: specify exact persons
#' plot_employment_gantt(employment_data, person_ids = c(6, 8, 21))
#' 
#' # Publication-ready chart with annotations
#' plot_employment_gantt(
#'   employment_data,
#'   person_ids = c("Person_A", "Person_B", "Person_C"),
#'   show_contract_ids = TRUE,
#'   show_durations = TRUE,
#'   title = "Employment Histories: Sample Cohort"
#' )
#' 
#' # Black and white version for print
#' plot_employment_gantt(
#'   employment_data,
#'   person_ids = c(6, 8),
#'   use_bw = TRUE,
#'   show_legend = TRUE
#' )
#' 
#' # Detailed analysis with contract focus
#' plot_employment_gantt(
#'   employment_data,
#'   max_persons = 5,
#'   show_contract_ids = TRUE,
#'   date_breaks = "1 month",
#'   alpha = 0.9
#' )
#' }
#' 
#' @seealso 
#' \code{\link{plot_employment_gantt_advanced}} for combined Gantt and metrics visualization,
#' \code{\link{plot_employment_timeline}} for simpler timeline plots,
#' \code{\link{theme_vecshift}} for the underlying plot theme,
#' \code{\link{get_standardized_employment_colors}} for color consistency
plot_employment_gantt <- function(data,
                                 person_ids = NULL,
                                 max_persons = 10,
                                 sort_by = "total_duration",
                                 title = "Employment Gantt Chart",
                                 show_legend = TRUE,
                                 time_unit = "months",
                                 color_palette = "employment",
                                 person_col = "cf",
                                 time_col = "inizio", 
                                 end_col = "fine",
                                 status_col = "stato",
                                 contract_col = "id",
                                 facet_by = NULL,
                                 date_breaks = "2 months",
                                 show_contract_ids = FALSE,
                                 show_durations = FALSE,
                                 palette = "employment",
                                 use_bw = FALSE,
                                 bar_height = 0.6,
                                 alpha = 0.8,
                                 text_size = 2.5) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  if (!inherits(data, "data.table")) {
    stop("Input 'data' must be a data.table object")
  }
  
  # Determine people to visualize - consistent with plot_employment_gantt_advanced
  if (!is.null(person_ids)) {
    # Use specified person IDs
    available_people <- unique(data[[person_col]])
    
    # Enhanced validation with better error messages
    if (length(available_people) == 0) {
      stop("No person identifiers found in data. Check that the 'cf' column exists and contains valid IDs.")
    }
    
    # Convert person_ids to same type as data for robust intersection
    # Handle potential type mismatches gracefully
    if (is.character(person_ids) && is.numeric(available_people)) {
      person_ids_converted <- suppressWarnings(as.numeric(person_ids))
      if (any(is.na(person_ids_converted))) {
        stop("Cannot convert character person_ids to numeric to match data format")
      }
      person_ids <- person_ids_converted
    } else if (is.numeric(person_ids) && is.character(available_people)) {
      person_ids <- as.character(person_ids)
    }
    
    valid_people <- intersect(person_ids, available_people)
    
    if (length(valid_people) == 0) {
      stop(paste0("None of the specified person_ids found in the data.\n",
                  "Requested IDs: ", paste(person_ids, collapse = ", "), "\n",
                  "Available IDs (sample): ", paste(head(available_people, 10), collapse = ", "), 
                  if (length(available_people) > 10) "..." else "",
                  "\nTotal available: ", length(available_people)))
    }
    
    if (length(valid_people) < length(person_ids)) {
      missing_people <- setdiff(person_ids, valid_people)
      warning(paste("Person IDs not found in data:", paste(missing_people, collapse = ", ")))
    }
    
    plot_data <- data[get(person_col) %in% valid_people]
  } else {
    # Default behavior: use max_persons parameter for backward compatibility
    if (!is.null(max_persons) && max_persons > 0) {
      people_to_show <- head(unique(data[[person_col]]), max_persons)
      plot_data <- data[get(person_col) %in% people_to_show]
    } else {
      plot_data <- copy(data)
    }
  }
  
  # Verify we have data after filtering
  if (nrow(plot_data) == 0) {
    stop(paste0("No employment data found for the selected person IDs after filtering.\n",
                "This might indicate a data filtering issue or missing employment records."))
  }
  
  # Create y-position for each person (reverse order for typical Gantt layout)
  people <- unique(plot_data[[person_col]])
  people <- rev(people)  # Reverse so first person appears at top
  plot_data[, y_pos := length(people) + 1 - match(get(person_col), people)]
  
  # Get colors using standardized employment color palette
  # Support both 'palette' and 'color_palette' parameters for flexibility
  active_palette <- if (!missing(color_palette)) color_palette else palette
  
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    # Use standardized employment colors for consistency across all plotting functions
    colors <- get_standardized_employment_colors(unique(plot_data[[status_col]]))
  }
  
  # Create the base Gantt chart
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    xmin = get(time_col), 
    xmax = get(end_col),
    ymin = y_pos - bar_height/2,
    ymax = y_pos + bar_height/2,
    fill = get(status_col)
  )) +
    ggplot2::geom_rect(alpha = alpha, color = "white", size = 0.3) +
    ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
    ggplot2::scale_x_date(
      date_breaks = date_breaks,
      date_labels = "%b %Y",
      expand = c(0.02, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(people),
      labels = people,
      expand = c(0.02, 0)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Employment timeline for", length(people), "individuals"),
      x = "Time Period",
      y = "Individuals",
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 11, grid = "major") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9),
      legend.position = if(show_legend) "bottom" else "none",
      legend.box = "horizontal",
      panel.grid.major.y = ggplot2::element_line(color = "#E8EAED", size = 0.3),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add contract ID labels if requested
  if (show_contract_ids) {
    # Calculate text position (center of each bar)
    plot_data[, text_x := get(time_col) + (get(end_col) - get(time_col)) / 2]
    plot_data[, text_y := y_pos]
    
    # Only show contract IDs for non-unemployment periods
    contract_labels <- plot_data[get(status_col) != "disoccupato" & get(contract_col) != 0]
    
    if (nrow(contract_labels) > 0) {
      p <- p + ggplot2::geom_text(
        data = contract_labels,
        ggplot2::aes(x = text_x, y = text_y, label = get(contract_col)),
        size = text_size,
        color = "white",
        fontface = "bold",
        inherit.aes = FALSE
      )
    }
  }
  
  # Add duration labels if requested
  if (show_durations) {
    # Calculate text position and format duration
    plot_data[, text_x := get(time_col) + (get(end_col) - get(time_col)) / 2]
    plot_data[, text_y := y_pos]
    plot_data[, duration_label := ifelse(durata < 30, 
                                        paste0(durata, "d"),
                                        ifelse(durata < 365,
                                               paste0(round(durata/30.4, 1), "m"),
                                               paste0(round(durata/365.25, 1), "y")))]
    
    # Only show durations for periods longer than 7 days to avoid clutter
    duration_labels <- plot_data[durata >= 7]
    
    if (nrow(duration_labels) > 0) {
      p <- p + ggplot2::geom_text(
        data = duration_labels,
        ggplot2::aes(x = text_x, y = text_y, label = duration_label),
        size = text_size,
        color = "white",
        fontface = "bold",
        inherit.aes = FALSE
      )
    }
  }
  
  # Add milestone markers for employment start/end dates
  milestones <- plot_data[, .(
    date = c(get(time_col), get(end_col)),
    type = rep(c("start", "end"), each = .N),
    person = rep(get(person_col), 2),
    y_pos = rep(y_pos, 2)
  )]
  
  # Remove duplicate dates per person to avoid overplotting
  milestones <- unique(milestones, by = c("date", "person"))
  
  # Add subtle milestone markers
  p <- p + ggplot2::geom_point(
    data = milestones,
    ggplot2::aes(x = date, y = y_pos),
    shape = "|",
    size = 2,
    color = "#2C3E50",
    alpha = 0.4,
    inherit.aes = FALSE
  )
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), 
                                scales = "free_y", ncol = 2)
  }
  
  return(p)
}

#' Plot Advanced Employment Gantt Chart with Career Metrics Summary
#'
#' Creates a sophisticated two-panel visualization combining employment Gantt charts
#' with career metrics summary displays. This advanced function integrates employment
#' timelines with comprehensive career performance indicators, providing deep insights
#' into career trajectories and professional development patterns. The function ensures
#' perfect color consistency with plot_employment_gantt() and uses identical person
#' selection logic for reproducible research.
#' 
#' @importFrom scales percent_format
#'
#' @details
#' **Two-Panel Design:**
#' The visualization consists of two perfectly aligned panels:
#' \itemize{
#'   \item **Left Panel**: Employment Gantt chart showing contract timelines, employment statuses,
#'     and transitions using standardized employment colors
#'   \item **Right Panel**: Career metrics summary displayed as bar charts, dot plots, or 
#'     radar charts showing comparative performance indicators across individuals
#' }
#' 
#' **Perfect Alignment**: Both panels use identical y-axis scaling and person ordering
#' to ensure visual alignment. This is critical for comparing individual employment
#' patterns with their corresponding career metrics.
#' 
#' **Color Consistency**: 
#' \itemize{
#'   \item Employment statuses use identical colors to plot_employment_gantt()
#'   \item Career metrics use Set2 palette (RColorBrewer) for accessibility
#'   \item All colors are colorblind-friendly and print-compatible
#' }
#' 
#' **Publication Reproducibility**:
#' Like plot_employment_gantt(), this function supports explicit person_ids
#' specification for identical results across analyses. The person selection
#' logic is identical between both functions.
#'
#' **Accessibility Features**:
#' - Uses colorblind-friendly palettes (Set2, employment colors) with good contrast ratios
#' - Clear legends and comprehensive annotations
#' - Graceful handling of missing data and mismatched datasets
#' - Optimized for both screen display and print reproduction
#' - Supports black and white output for print publications
#'
#' **Supported Career Metrics Visualizations**:
#' - **Bar Charts**: Best for comparing metric values across individuals with clear visual distinction
#' - **Dot Plot**: Effective for precise value comparison with minimal visual clutter and faceting support
#' - **Radar Chart**: Ideal for showing multi-dimensional metric profiles per person in circular format
#'
#' @param data Data.table or data.frame containing employment segments with required columns:
#'   cf (person ID), inizio (start date), fine (end date), stato (employment status).
#'   Should be output from vecshift() or compatible format.
#' @param career_metrics_data Data.table or data.frame from calculate_comprehensive_career_metrics()
#'   containing career performance indicators. Must include 'cf' column for person matching.
#'   When NULL, shows only the Gantt chart (default: NULL)
#' @param person_ids Character or numeric vector. Specific person identifiers to visualize.
#'   **For reproducible research**, always specify person_ids explicitly. When provided,
#'   shows only these persons regardless of max_persons parameter. When NULL, uses 
#'   max_persons to select first N individuals. Example: c(6, 8, 21) (default: NULL)
#' @param metrics_to_show Character vector. Career metrics to display in right panel.
#'   Must match column names in career_metrics_data. Common metrics include:
#'   "career_success_index", "career_advancement_index", "employment_security_index",
#'   "career_complexity_index", "mobility_rate", "employment_rate" 
#'   (default: c("career_success_index", "career_advancement_index", "employment_security_index", "career_complexity_index"))
#' @param metrics_viz_type Character. Visualization type for career metrics panel:
#'   "bar" (side-by-side bars), "dot" (dot plot with faceting), "radar" (circular/polar plot) (default: "bar")
#' @param max_persons Integer. Maximum number of persons to show when person_ids is NULL.
#'   Used only for exploratory analysis; specify person_ids for reproducible research (default: 5)
#' @param palette Character. Color palette for career metrics: "Set2" uses RColorBrewer Set2 palette
#'   (recommended for accessibility), or fallback to vecshift colors (default: "Set2")
#' @param title Character. Main title for the combined visualization (default: "Employment Timeline and Career Metrics")
#' @param show_legend Logical. Whether to show legends for both panels (default: TRUE).
#'   Set to FALSE for cleaner multi-panel publications where legends appear elsewhere
#'
#' @return A list containing three elements:
#'   \item{combined_plot}{Combined plot object using patchwork if available, otherwise a list of plots or single plot}
#'   \item{gantt_plot}{Individual Gantt chart (ggplot2 object) showing employment timeline}  
#'   \item{metrics_plot}{Individual career metrics visualization (ggplot2 object), NULL if no career_metrics_data provided}
#'   
#'   The combined_plot can be directly printed or saved. Individual plots allow for
#'   custom arrangements or separate analysis.
#'
#' @section Color Standards and Consistency:
#' This function maintains perfect color consistency with other longworkR functions:
#' \itemize{
#'   \item Employment statuses use identical colors to plot_employment_gantt()
#'   \item Career metrics use Set2 palette for clear distinction from employment colors
#'   \item All palettes are colorblind-accessible and print-friendly
#'   \item Black and white mode available for print publications
#' }
#' 
#' @section Perfect Panel Alignment:
#' The function ensures perfect vertical alignment between panels:
#' \itemize{
#'   \item Identical y-axis scaling and person ordering
#'   \item Consistent person labeling across both panels
#'   \item Synchronized data filtering and validation
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load sample employment data
#' employment_data <- readRDS("data/sample.rds")
#' 
#' # Basic Gantt chart only (no metrics) - exploratory analysis
#' gantt_only <- plot_employment_gantt_advanced(
#'   data = employment_data,
#'   max_persons = 3
#' )
#' print(gantt_only$combined_plot)
#' 
#' # Calculate comprehensive career metrics first
#' career_metrics <- calculate_comprehensive_career_metrics(employment_data)
#' 
#' # Publication-ready: Advanced Gantt with career metrics using specific persons
#' advanced_plot <- plot_employment_gantt_advanced(
#'   data = employment_data,
#'   career_metrics_data = career_metrics,
#'   person_ids = c(6, 8, 21),  # Explicit specification for reproducibility
#'   title = "Employment and Career Development Analysis"
#' )
#' print(advanced_plot$combined_plot)
#' 
#' # Dot plot visualization with custom metrics selection
#' dot_analysis <- plot_employment_gantt_advanced(
#'   data = employment_data,
#'   career_metrics_data = career_metrics,
#'   person_ids = c("Person_A", "Person_B", "Person_C"),
#'   metrics_viz_type = "dot",
#'   metrics_to_show = c("career_success_index", "employment_security_index"),
#'   title = "Career Development Analysis: Selected Metrics"
#' )
#' 
#' # Radar chart for multi-dimensional profile comparison
#' radar_profiles <- plot_employment_gantt_advanced(
#'   data = employment_data,
#'   career_metrics_data = career_metrics,
#'   person_ids = c(1, 5, 10, 15),
#'   metrics_viz_type = "radar",
#'   show_legend = FALSE,
#'   title = "Career Profile Comparison"
#' )
#' 
#' # Access individual plots for custom arrangements
#' gantt_chart <- advanced_plot$gantt_plot
#' metrics_chart <- advanced_plot$metrics_plot
#' 
#' # Print individual components
#' print(gantt_chart)
#' print(metrics_chart)
#' 
#' # Save for publication
#' ggsave("employment_analysis.png", advanced_plot$combined_plot, 
#'        width = 12, height = 8, dpi = 300)
#' }
#'
#' @seealso 
#' \code{\link{plot_employment_gantt}} for basic Gantt charts with identical color consistency,
#' \code{\link{calculate_comprehensive_career_metrics}} for computing career performance indicators,
#' \code{\link{theme_vecshift}} for the underlying plot theme used in both panels,
#' \code{\link{get_standardized_employment_colors}} for employment color standardization,
#' \code{\link{vecshift_colors}} for the broader color palette system
plot_employment_gantt_advanced <- function(
  data,
  career_metrics_data = NULL,
  person_ids = NULL,
  metrics_to_show = c("career_success_index", "career_advancement_index", 
                      "employment_security_index", "career_complexity_index"),
  metrics_viz_type = "bar",
  max_persons = 5,
  palette = "Set2",
  title = "Employment Timeline and Career Metrics",
  show_legend = TRUE
) {
  
  # Check required packages
  required_packages <- c("ggplot2", "data.table")
  missing_packages <- character(0)
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # Check optional packages
  optional_packages <- c("patchwork", "RColorBrewer")
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      warning(paste("Package", pkg, "is recommended for enhanced functionality but not required"))
    }
  }
  
  if (length(missing_packages) > 0) {
    stop(paste("Required packages not available:", paste(missing_packages, collapse = ", ")))
  }
  
  # Input validation
  if (!inherits(data, "data.table")) {
    if (inherits(data, "data.frame")) {
      data <- data.table::as.data.table(data)
    } else {
      stop("Input 'data' must be a data.table or data.frame object")
    }
  }
  
  # Validate required columns
  required_cols <- c("cf", "inizio", "fine", "stato")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Validate career metrics data if provided
  if (!is.null(career_metrics_data)) {
    if (!inherits(career_metrics_data, "data.table")) {
      if (inherits(career_metrics_data, "data.frame")) {
        career_metrics_data <- data.table::as.data.table(career_metrics_data)
      } else {
        stop("Input 'career_metrics_data' must be a data.table or data.frame object")
      }
    }
    
    if (!"cf" %in% names(career_metrics_data)) {
      stop("career_metrics_data must contain 'cf' column for person identification")
    }
  }
  
  # Validate metrics_viz_type
  metrics_viz_type <- match.arg(metrics_viz_type, c("bar", "dot", "radar"))
  
  # Determine people to visualize
  if (!is.null(person_ids)) {
    # Use specified person IDs
    available_people <- unique(data[["cf"]])
    
    # Enhanced validation with better error messages
    if (length(available_people) == 0) {
      stop("No person identifiers found in data. Check that the 'cf' column exists and contains valid IDs.")
    }
    
    # Convert person_ids to same type as data for robust intersection
    # Handle potential type mismatches gracefully
    if (is.character(person_ids) && is.numeric(available_people)) {
      person_ids_converted <- suppressWarnings(as.numeric(person_ids))
      if (any(is.na(person_ids_converted))) {
        stop("Cannot convert character person_ids to numeric to match data format")
      }
      person_ids <- person_ids_converted
    } else if (is.numeric(person_ids) && is.character(available_people)) {
      person_ids <- as.character(person_ids)
    }
    
    valid_people <- intersect(person_ids, available_people)
    
    if (length(valid_people) == 0) {
      stop(paste0("None of the specified person_ids found in the data.\n",
                  "Requested IDs: ", paste(person_ids, collapse = ", "), "\n",
                  "Available IDs (sample): ", paste(head(available_people, 10), collapse = ", "), 
                  if (length(available_people) > 10) "..." else "",
                  "\nTotal available: ", length(available_people)))
    }
    
    if (length(valid_people) < length(person_ids)) {
      missing_people <- setdiff(person_ids, valid_people)
      warning(paste("Person IDs not found in data:", paste(missing_people, collapse = ", ")))
    }
  } else {
    # Use first max_persons people from the dataset
    all_people <- unique(data[["cf"]])
    if (length(all_people) == 0) {
      stop("No person identifiers found in data. Check that the 'cf' column exists and contains valid IDs.")
    }
    valid_people <- head(all_people, max_persons)
  }
  
  # Filter data to selected people
  plot_data <- data[get("cf") %in% valid_people]
  
  # Verify we have data after filtering
  if (nrow(plot_data) == 0) {
    stop(paste0("No employment data found for the selected person IDs after filtering.\n",
                "Selected IDs: ", paste(valid_people, collapse = ", "), "\n",
                "This might indicate a data filtering issue or missing employment records."))
  }
  
  # Create time range for alignment
  time_range <- range(c(plot_data[["inizio"]], plot_data[["fine"]]), na.rm = TRUE)
  
  # ============================================================================
  # ESTABLISH SHARED PERSON ORDERING FOR PERFECT ALIGNMENT
  # ============================================================================
  
  # Critical: Create consistent person ordering that will be used by BOTH panels
  # This ensures perfect vertical alignment between gantt and metrics charts
  people_order <- sort(unique(plot_data[["cf"]]))
  n_people <- length(people_order)
  
  # ============================================================================
  # PANEL 1: Employment Gantt Chart
  # ============================================================================
  
  # Prepare data for Gantt chart using shared people_order
  plot_data[, y_pos := match(get("cf"), people_order)]
  
  # Get colors for employment status - ALWAYS use standardized employment colors
  # This ensures perfect consistency with plot_employment_gantt() function
  gantt_colors <- get_standardized_employment_colors(unique(plot_data[["stato"]]))
  
  # Create Gantt chart
  gantt_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(
    xmin = get("inizio"), 
    xmax = get("fine"),
    ymin = y_pos - 0.4,
    ymax = y_pos + 0.4,
    fill = get("stato")
  )) +
    ggplot2::geom_rect(alpha = 0.8, color = "white", size = 0.3) +
    ggplot2::scale_fill_manual(values = gantt_colors, name = "Employment Status") +
    ggplot2::scale_x_date(
      limits = time_range,
      date_breaks = "3 months",
      date_labels = "%b %Y",
      expand = c(0.02, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(people_order),
      labels = as.character(people_order),
      limits = c(0.5, n_people + 0.5),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = "Employment Timeline",
      x = "Time Period",
      y = "Individuals"
    ) +
    theme_vecshift(base_size = 11, grid = "major") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = if (show_legend) "bottom" else "none",
      plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
  
  # ============================================================================
  # PANEL 2: Career Metrics Visualization (if data provided)
  # ============================================================================
  
  metrics_plot <- NULL
  
  if (!is.null(career_metrics_data)) {
    # Filter career metrics to selected people
    metrics_data <- career_metrics_data[get("cf") %in% valid_people]
    
    # Validate that requested metrics exist
    available_metrics <- names(metrics_data)
    valid_metrics <- intersect(metrics_to_show, available_metrics)
    
    if (length(valid_metrics) == 0) {
      warning("None of the specified metrics found in career_metrics_data")
      valid_metrics <- NULL
    } else {
      if (length(valid_metrics) < length(metrics_to_show)) {
        missing_metrics <- setdiff(metrics_to_show, valid_metrics)
        warning(paste("Metrics not found in data:", paste(missing_metrics, collapse = ", ")))
      }
      
      # Prepare career metrics data for visualization
      # Career metrics are summary statistics (single values per person)
      # We need to reshape them for appropriate summary visualizations
      
      # Convert to long format for ggplot
      metrics_long <- data.table::melt(
        metrics_data[, c("cf", valid_metrics), with = FALSE],
        id.vars = "cf",
        measure.vars = valid_metrics,
        variable.name = "metric",
        value.name = "value"
      )
      
      # Handle missing values gracefully
      metrics_long <- metrics_long[!is.na(value)]
      
      # CRITICAL: Ensure identical ordering and y-axis alignment with Gantt chart
      # Use the same people_order and create consistent factor levels
      metrics_long[, person_label := as.character(cf)]
      
      # Convert person_label to factor with identical levels/ordering as Gantt chart
      person_label_levels <- as.character(people_order)
      metrics_long[, person_label := factor(person_label, levels = person_label_levels)]
      
      # Filter to only include people that are in both datasets
      metrics_long <- metrics_long[!is.na(person_label)]
      
      if (nrow(metrics_long) > 0) {
        # Get colors for metrics
        if (palette == "Set2" && requireNamespace("RColorBrewer", quietly = TRUE)) {
          n_metrics <- length(valid_metrics)
          metrics_colors <- RColorBrewer::brewer.pal(max(3, min(n_metrics, 8)), "Set2")[1:n_metrics]
        } else {
          metrics_colors <- vecshift_colors("main", n = length(valid_metrics))
        }
        names(metrics_colors) <- valid_metrics
        
        # Create metrics visualization based on type
        if (metrics_viz_type == "bar") {
          metrics_plot <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = person_label, y = value, fill = metric)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.8, width = 0.7) +
            ggplot2::scale_fill_manual(values = metrics_colors, name = "Career Metrics") +
            ggplot2::coord_flip() +
            # CRITICAL: Use identical y-axis scale as Gantt chart for perfect alignment
            ggplot2::scale_y_discrete(limits = person_label_levels, drop = FALSE)
          
        } else if (metrics_viz_type == "dot") {
          metrics_plot <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = value, y = person_label, color = metric)) +
            ggplot2::geom_point(size = 4, alpha = 0.8) +
            ggplot2::scale_color_manual(values = metrics_colors, name = "Career Metrics") +
            # CRITICAL: Use identical y-axis scale as Gantt chart for perfect alignment
            ggplot2::scale_y_discrete(limits = person_label_levels, drop = FALSE) +
            ggplot2::facet_wrap(~ metric, scales = "free_x", ncol = 2)
          
        } else if (metrics_viz_type == "radar") {
          # For radar chart, we need a different approach - create a circular representation
          # Using a workaround with polar coordinates
          metrics_plot <- ggplot2::ggplot(metrics_long, ggplot2::aes(x = metric, y = value, group = person_label, color = person_label)) +
            ggplot2::geom_polygon(alpha = 0.2, fill = NA, size = 1) +
            ggplot2::geom_point(size = 3, alpha = 0.8) +
            ggplot2::coord_polar() +
            ggplot2::scale_color_manual(values = metrics_colors[1:length(unique(metrics_long$person_label))], name = "Person")
        }
        
        # Add common elements to metrics plot
        if (!is.null(metrics_plot)) {
          # Common scale and theme elements
          if (metrics_viz_type == "bar") {
            # For bar chart with coord_flip(), y becomes the value axis (after flip)
            metrics_plot <- metrics_plot +
              ggplot2::scale_y_continuous(
                limits = c(0, 1),
                labels = scales::percent_format()
              )
          } else if (metrics_viz_type == "dot") {
            metrics_plot <- metrics_plot +
              ggplot2::scale_x_continuous(
                limits = c(0, 1),
                labels = scales::percent_format()
              )
          } else if (metrics_viz_type == "radar") {
            metrics_plot <- metrics_plot +
              ggplot2::scale_y_continuous(
                limits = c(0, 1),
                labels = scales::percent_format()
              )
          }
          
          # Add labels and theme
          metrics_plot <- metrics_plot +
            ggplot2::labs(
              title = "Career Metrics Summary",
              x = if (metrics_viz_type == "bar") "Metric Value (%)" else if (metrics_viz_type == "dot") "Metric Value (%)" else "Metrics",
              y = if (metrics_viz_type == "bar") "Individuals" else if (metrics_viz_type == "dot") "Individuals" else "Value"
            ) +
            theme_vecshift(base_size = 11, grid = "major") +
            ggplot2::theme(
              axis.text.x = if (metrics_viz_type == "radar") ggplot2::element_text(angle = 45, hjust = 1) else ggplot2::element_text(),
              axis.text.y = ggplot2::element_text(),
              legend.position = if (show_legend) "bottom" else "none",
              strip.text = if (metrics_viz_type == "dot") ggplot2::element_text(size = 10) else ggplot2::element_blank(),
              plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm")
            )
        }
      } else {
        warning("No valid metric data available for visualization")
      }
    }
  }
  
  # ============================================================================
  # COMBINE PANELS
  # ============================================================================
  
  combined_plot <- NULL
  
  if (!is.null(metrics_plot) && requireNamespace("patchwork", quietly = TRUE)) {
    # Create combined plot using patchwork with proper alignment
    combined_plot <- gantt_plot | metrics_plot + 
      patchwork::plot_layout(widths = c(2, 1)) +
      patchwork::plot_annotation(
        title = title,
        caption = "Generated with longworkR advanced visualization functions"
      )
  } else if (!is.null(metrics_plot)) {
    # Fallback: create a simple combined plot without patchwork
    warning("Package 'patchwork' not available. Returning separate plots.")
    combined_plot <- list(gantt = gantt_plot, metrics = metrics_plot)
  } else {
    # Only Gantt chart
    combined_plot <- gantt_plot + ggplot2::labs(title = title)
  }
  
  # ============================================================================
  # RETURN RESULTS
  # ============================================================================
  
  result <- list(
    combined_plot = combined_plot,
    gantt_plot = gantt_plot,
    metrics_plot = metrics_plot
  )
  
  return(result)
}

#' Plot Employment Heatmap
#'
#' Creates heatmap visualizations showing employment density patterns over time
#' and across populations, useful for identifying seasonal patterns, regional
#' differences, or temporal clustering of employment events.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param heatmap_type Character. Type of heatmap: "density", "status", "duration", "transitions" (default: "density")
#' @param time_unit Character. Time aggregation unit: "month", "quarter", "year" (default: "month")
#' @param group_by Character. Column to group by for comparison (default: NULL)
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param n_people Integer. Maximum number of people to show (default: 50)
#' @param palette Character. Color palette to use (default: "main")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param show_values Logical. Show values in heatmap cells (default: FALSE)
#' @param alpha Numeric. Transparency (default: 0.9)
#'
#' @return A ggplot2 object showing employment heatmap
#' @export
#'
#' @examples
#' \dontrun{
#' # Employment density heatmap
#' plot_employment_heatmap(data, heatmap_type = "density")
#' 
#' # Status distribution heatmap
#' plot_employment_heatmap(data, heatmap_type = "status")
#' 
#' # Duration patterns by month
#' plot_employment_heatmap(data, heatmap_type = "duration", time_unit = "month")
#' }
plot_employment_heatmap <- function(data,
                                   person_col = "cf",
                                   time_col = "inizio", 
                                   end_col = "fine",
                                   status_col = "stato",
                                   heatmap_type = "density",
                                   time_unit = "month",
                                   group_by = NULL,
                                   facet_by = NULL,
                                   n_people = 50,
                                   palette = "main",
                                   use_bw = FALSE,
                                   show_values = FALSE,
                                   alpha = 0.9) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  heatmap_type <- match.arg(heatmap_type, c("density", "status", "duration", "transitions"))
  time_unit <- match.arg(time_unit, c("month", "quarter", "year"))
  
  # Limit to n_people if specified
  if (!is.null(n_people) && n_people > 0) {
    people_to_show <- head(unique(data[[person_col]]), n_people)
    plot_data <- data[get(person_col) %in% people_to_show]
  } else {
    plot_data <- copy(data)
  }
  
  # Create time aggregation
  if (time_unit == "month") {
    plot_data[, time_period := as.Date(format(get(time_col), "%Y-%m-01"))]
    date_format <- "%b %Y"
  } else if (time_unit == "quarter") {
    plot_data[, time_period := as.Date(paste0(format(get(time_col), "%Y"), "-", 
                                             sprintf("%02d", ((as.numeric(format(get(time_col), "%m")) - 1) %/% 3) * 3 + 1), 
                                             "-01"))]
    date_format <- "Q%q %Y"
  } else {
    plot_data[, time_period := as.Date(paste0(format(get(time_col), "%Y"), "-01-01"))]
    date_format <- "%Y"
  }
  
  # Prepare heatmap data based on type
  if (heatmap_type == "density") {
    # Employment activity density
    heatmap_data <- plot_data[, .(
      value = .N,
      metric = "Employment Periods"
    ), by = .(time_period, person = get(person_col))]
    
  } else if (heatmap_type == "status") {
    # Status distribution heatmap
    status_numeric <- plot_data[, {
      # Convert status to numeric for heatmap
      status_map <- c("disoccupato" = 0, "occ_pt" = 1, "occ_ft" = 2, 
                      "over_pt_pt" = 3, "over_ft_pt" = 4, "over_ft_ft" = 5)
      status_val <- status_map[get(status_col)]
      if (is.na(status_val)) status_val <- 0  # Default for unknown status
      .(value = status_val, metric = "Employment Status")
    }, by = .(time_period, person = get(person_col))]
    heatmap_data <- status_numeric
    
  } else if (heatmap_type == "duration") {
    # Average duration patterns
    heatmap_data <- plot_data[, .(
      value = as.numeric(mean(durata, na.rm = TRUE)),
      metric = "Average Duration (days)"
    ), by = .(time_period, person = get(person_col))]
    
  } else if (heatmap_type == "transitions") {
    # Transition frequency heatmap
    setorderv(plot_data, c(person_col, time_col))
    transitions <- plot_data[, {
      if (.N > 1) {
        transitions_count <- .N - 1
        .(value = transitions_count, metric = "Number of Transitions")
      } else {
        .(value = 0, metric = "Number of Transitions")
      }
    }, by = .(time_period = get(time_col), person = get(person_col))]
    heatmap_data <- transitions
  }
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw")
  } else {
    colors <- vecshift_colors(palette)
  }
  
  # Create the heatmap
  p <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = time_period, y = person, fill = value)) +
    ggplot2::geom_tile(alpha = alpha, color = "white", size = 0.1) +
    ggplot2::scale_x_date(date_breaks = if (time_unit == "month") "3 months" else "1 year",
                         date_labels = date_format) +
    ggplot2::labs(
      title = paste("Employment", tools::toTitleCase(heatmap_type), "Heatmap"),
      subtitle = paste("Showing", unique(heatmap_data$metric), "for", 
                      length(unique(heatmap_data$person)), "individuals"),
      x = "Time Period",
      y = "Individuals",
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 10, grid = "none") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_blank(),  # Too many people to show names
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "right"
    )
  
  # Apply appropriate color scale based on heatmap type
  if (heatmap_type == "density" || heatmap_type == "transitions") {
    p <- p + ggplot2::scale_fill_gradient(
      low = colors[1], high = colors[length(colors)],
      name = paste(strwrap(unique(heatmap_data$metric), width = 10), collapse = "\n")
    )
  } else if (heatmap_type == "status") {
    # Discrete color scale for status using standardized colors
    status_colors <- get_standardized_employment_colors(c("disoccupato", "over_ft_ft"))
    p <- p + ggplot2::scale_fill_gradient(
      low = status_colors["disoccupato"], high = status_colors["over_ft_ft"],
      name = "Employment\nLevel",
      breaks = c(0, 1, 2, 3, 4, 5),
      labels = c("Unemployed", "PT", "FT", "PT Overlap", "Mixed Overlap", "FT Overlap")
    )
  } else if (heatmap_type == "duration") {
    p <- p + ggplot2::scale_fill_gradient2(
      low = colors[1], mid = colors[3], high = colors[5],
      midpoint = as.numeric(median(heatmap_data$value, na.rm = TRUE)),
      name = "Duration\n(days)"
    )
  }
  
  # Add values to cells if requested
  if (show_values) {
    # Only show values if not too many cells
    if (nrow(heatmap_data) <= 200) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = round(value, 1)),
        size = 2,
        color = "white",
        fontface = "bold"
      )
    } else {
      warning("Too many cells to display values clearly. Use show_values = FALSE or reduce data size.")
    }
  }
  
  # Add faceting if requested
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    p <- p + ggplot2::facet_wrap(as.formula(paste("~", facet_by)), scales = "free")
  }
  
  return(p)
}

#' Integrated Employment Timeline and Career Metrics Visualization
#'
#' @description
#' Creates a unified ggplot visualization that combines employment timelines with career
#' metrics in a single, perfectly aligned plot. This function solves alignment issues
#' that occur with multi-panel approaches by using a shared y-axis coordinate system.
#' The visualization displays employment periods as a Gantt chart on the left portion
#' and career metrics as heatmap or bar elements on the right portion, ensuring
#' perfect person-to-person alignment across both sections.
#'
#' The function provides a comprehensive solution for visualizing both temporal 
#' employment patterns and summary career performance indicators simultaneously,
#' making it ideal for career trajectory analysis, comparative studies, and
#' reporting dashboards.
#'
#' @param employment_data A data.table containing employment records processed by
#'   the vecshift package. Must include columns for person identifiers, employment
#'   start/end dates, employment status, and duration. Each row represents an
#'   employment period for an individual.
#' @param career_metrics Optional data.table containing career performance metrics,
#'   typically generated by \code{\link{calculate_comprehensive_career_metrics}}.
#'   If NULL, only the employment timeline will be displayed. Must contain a person
#'   identifier column matching the employment data and numeric metric columns.
#' @param person_ids Optional character vector of specific person identifiers to
#'   include in the visualization. If NULL (default), the function will
#'   automatically select persons based on data availability and \code{max_persons}.
#'   When specified, overrides automatic selection.
#' @param max_persons Integer specifying the maximum number of individuals to
#'   include when using automatic person selection. Default is 10. Ignored if
#'   \code{person_ids} is explicitly provided. Used to prevent overcrowded plots
#'   with large datasets.
#' @param metrics_to_show Character vector specifying which career metrics to
#'   display in the right panel. Default includes the four core career indices:
#'   "career_success_index", "career_advancement_index", "employment_security_index",
#'   and "career_complexity_index". Only metrics present in both this parameter
#'   and the career_metrics data will be displayed.
#' @param timeline_width Numeric value between 0 and 1 specifying the proportion
#'   of total plot width allocated to the employment timeline section. Default is
#'   0.7 (70% of width). Must sum with \code{metrics_width} to 1.0 for proper layout.
#' @param metrics_width Numeric value between 0 and 1 specifying the proportion
#'   of total plot width allocated to the career metrics section. Default is
#'   0.3 (30% of width). Must sum with \code{timeline_width} to 1.0 for proper layout.
#' @param title Character string for the plot title. Default is "Integrated
#'   Employment Timeline and Career Metrics". Set to NULL or empty string to
#'   suppress the title.
#' @param person_col Character string specifying the column name containing
#'   person identifiers in the employment data. Default is "cf" (codice fiscale).
#'   This column is used to group records by individual and must match the
#'   identifier column in career_metrics if provided.
#' @param time_col Character string specifying the column name containing
#'   employment period start dates in the employment data. Default is "inizio".
#'   Should contain Date or POSIXct values for proper timeline rendering.
#' @param end_col Character string specifying the column name containing
#'   employment period end dates in the employment data. Default is "fine".
#'   Should contain Date or POSIXct values. Used to calculate period durations
#'   and create Gantt chart segments.
#' @param status_col Character string specifying the column name containing
#'   employment status information. Default is "stato". Used for color coding
#'   and categorizing employment periods in the timeline visualization.
#' @param duration_col Character string specifying the column name containing
#'   pre-calculated period durations (typically in days). Default is "durata".
#'   If not available, durations will be calculated from start and end dates.
#' @param date_breaks Character string specifying the interval for x-axis date
#'   breaks in the timeline. Default is "6 months". Common values include
#'   "3 months", "1 year", or "2 years" depending on the time span of data.
#' @param show_gaps Logical indicating whether to display unemployment periods
#'   (gaps between employment spells) in the timeline. Default is TRUE. When
#'   FALSE, only active employment periods are shown.
#' @param palette Character string specifying the color palette to use for
#'   employment status visualization. Default is "employment". Should correspond
#'   to predefined palettes in the vecshift color system.
#' @param use_bw Logical indicating whether to use black and white color scheme
#'   instead of the color palette. Default is FALSE. Useful for publications
#'   or presentations requiring monochrome output.
#' @param base_size Numeric specifying the base text size for all plot elements.
#'   Default is 11. Affects titles, axis labels, legends, and annotations.
#'   Larger values increase readability but may require more plot space.
#' @param person_order Character string specifying how to order individuals on
#'   the y-axis. Options are "alphabetical" (by person identifier), "metrics"
#'   (by career performance metrics, default), or "timeline" (by employment
#'   chronology). Affects the visual grouping and comparison of similar profiles.
#'
#' @return A ggplot2 object containing the integrated visualization. The plot
#'   features a shared y-axis with person identifiers, employment timeline as
#'   Gantt chart segments on the left portion, and career metrics as colored
#'   elements on the right portion. The object can be further customized using
#'   standard ggplot2 syntax or saved using ggsave().
#'
#' @details
#' This function addresses a common challenge in employment data visualization:
#' maintaining perfect alignment between temporal employment patterns and
#' summary career metrics. Unlike approaches using separate plots or faceting,
#' this single-plot solution guarantees that each person appears at exactly
#' the same y-coordinate in both the timeline and metrics sections.
#'
#' The visualization handles missing data gracefully:
#' - If career_metrics is NULL, displays timeline only
#' - If specific metrics are missing, shows available metrics with warnings
#' - If employment gaps exist, optionally displays them as distinct periods
#'
#' Visual encoding follows consistent conventions:
#' - Timeline uses Gantt chart representation with color-coded employment status
#' - Metrics use intensity or categorical encoding depending on metric type
#' - Shared legend explains both timeline and metric encodings
#' - Consistent person ordering facilitates pattern recognition
#'
#' @examples
#' \dontrun{
#' # Load employment data processed by vecshift
#' employment_data <- readRDS("data/sample.rds")
#' 
#' # Calculate comprehensive career metrics
#' career_metrics <- calculate_comprehensive_career_metrics(employment_data)
#' 
#' # Basic integrated visualization with default settings
#' plot_integrated_employment_metrics(
#'   employment_data = employment_data,
#'   career_metrics = career_metrics
#' )
#' 
#' # Timeline-only visualization when metrics are not available
#' plot_integrated_employment_metrics(
#'   employment_data = employment_data,
#'   career_metrics = NULL,
#'   max_persons = 15
#' )
#' 
#' # Custom metric selection with adjusted layout proportions
#' plot_integrated_employment_metrics(
#'   employment_data = employment_data,
#'   career_metrics = career_metrics,
#'   metrics_to_show = c("career_success_index", "employment_security_index"),
#'   timeline_width = 0.75,
#'   metrics_width = 0.25,
#'   title = "Career Success and Security Analysis"
#' )
#' 
#' # Focus on specific individuals with custom styling
#' plot_integrated_employment_metrics(
#'   employment_data = employment_data,
#'   career_metrics = career_metrics,
#'   person_ids = c("PERSON001", "PERSON002", "PERSON003"),
#'   person_order = "alphabetical",
#'   base_size = 12,
#'   date_breaks = "1 year"
#' )
#' 
#' # Black and white version for publication
#' plot_integrated_employment_metrics(
#'   employment_data = employment_data,
#'   career_metrics = career_metrics,
#'   use_bw = TRUE,
#'   show_gaps = FALSE,
#'   max_persons = 8
#' )
#' }
#'
#' @seealso 
#' \code{\link{calculate_comprehensive_career_metrics}} for generating career metrics,
#' \code{\link{plot_employment_gantt}} and \code{\link{plot_employment_gantt_advanced}} 
#' for timeline-only visualizations that may have alignment issues when combined 
#' with separate metric plots,
#' \code{\link{theme_vecshift}} for the underlying plot theme system
#'
#' @importFrom ggplot2 ggplot aes geom_rect geom_point scale_x_date scale_fill_manual 
#'   labs theme element_text element_blank coord_cartesian annotation_custom
#' @importFrom data.table data.table setorder copy
#' @export
plot_integrated_employment_metrics <- function(employment_data,
                                             career_metrics = NULL,
                                             person_ids = NULL,
                                             max_persons = 10,
                                             metrics_to_show = c("career_success_index", 
                                                               "career_advancement_index",
                                                               "employment_security_index", 
                                                               "career_complexity_index"),
                                             timeline_width = 0.7,
                                             metrics_width = 0.3,
                                             title = "Integrated Employment Timeline and Career Metrics",
                                             person_col = "cf",
                                             time_col = "inizio",
                                             end_col = "fine",
                                             status_col = "stato",
                                             duration_col = "durata",
                                             date_breaks = "6 months",
                                             show_gaps = TRUE,
                                             palette = "employment",
                                             use_bw = FALSE,
                                             base_size = 11,
                                             person_order = "metrics") {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Input validation
  if (!inherits(employment_data, "data.table")) {
    stop("employment_data must be a data.table object")
  }
  
  # Check required columns
  required_emp_cols <- c(person_col, time_col, end_col, status_col)
  missing_emp_cols <- setdiff(required_emp_cols, names(employment_data))
  if (length(missing_emp_cols) > 0) {
    stop("Missing employment data columns: ", paste(missing_emp_cols, collapse = ", "))
  }
  
  # Handle case when career_metrics is NULL
  if (is.null(career_metrics)) {
    warning("No career metrics provided. Showing employment timeline only.")
    career_metrics <- data.table()
    metrics_to_show <- character(0)
  } else {
    if (!inherits(career_metrics, "data.table")) {
      stop("career_metrics must be a data.table object")
    }
    
    # Validate metrics to show exist in career_metrics
    if (nrow(career_metrics) > 0) {
      available_metrics <- intersect(metrics_to_show, names(career_metrics))
      if (length(available_metrics) == 0) {
        warning("None of the specified metrics found in career_metrics data. Showing employment timeline only.")
        metrics_to_show <- character(0)
      } else {
        metrics_to_show <- available_metrics
      }
    } else {
      metrics_to_show <- character(0)
    }
  }
  
  # Step 1: Get complete person list
  if (!is.null(person_ids)) {
    # Use specified person IDs
    selected_people <- intersect(person_ids, unique(employment_data[[person_col]]))
    if (length(selected_people) == 0) {
      stop("None of the specified person IDs found in employment data")
    }
  } else {
    # Auto-select people based on ordering method
    people_in_employment <- unique(employment_data[[person_col]])
    
    if (nrow(career_metrics) > 0) {
      people_in_metrics <- unique(career_metrics[[person_col]])
      all_people <- union(people_in_employment, people_in_metrics)
    } else {
      all_people <- people_in_employment
    }
    
    # Step 2: Order people according to specified method
    if (person_order == "metrics" && nrow(career_metrics) > 0 && "career_success_index" %in% names(career_metrics)) {
      # Order by primary career metric (descending - best performers first)
      person_ordering <- career_metrics[, .(person = get(person_col), 
                                          order_metric = career_success_index)][
        order(-order_metric, person)]
      ordered_people <- person_ordering$person
      # Add any missing people at the end
      missing_people <- setdiff(all_people, ordered_people)
      ordered_people <- c(ordered_people, sort(missing_people))
    } else if (person_order == "timeline") {
      # Order by earliest employment start date
      person_ordering <- employment_data[, .(earliest_date = min(get(time_col), na.rm = TRUE)), 
                                       by = person_col][
        order(earliest_date, get(person_col))]
      ordered_people <- person_ordering[[person_col]]
      # Add any missing people at the end
      missing_people <- setdiff(all_people, ordered_people)
      ordered_people <- c(ordered_people, sort(missing_people))
    } else {
      # Alphabetical ordering
      ordered_people <- sort(all_people)
    }
    
    # Step 3: Select top max_persons
    if (max_persons > 0) {
      selected_people <- head(ordered_people, max_persons)
    } else {
      selected_people <- ordered_people
    }
  }
  
  # Step 4: Create unified data structure with extended x-axis
  
  # Filter data to selected people
  plot_employment_data <- employment_data[get(person_col) %in% selected_people]
  
  if (nrow(career_metrics) > 0) {
    plot_metrics_data <- career_metrics[get(person_col) %in% selected_people]
  } else {
    plot_metrics_data <- data.table()
  }
  
  # Filter gaps if requested
  if (!show_gaps) {
    plot_employment_data <- plot_employment_data[get(status_col) != "disoccupato"]
  }
  
  # Get time range from employment data
  if (nrow(plot_employment_data) > 0) {
    date_range <- range(c(plot_employment_data[[time_col]], 
                         plot_employment_data[[end_col]]), na.rm = TRUE)
    timeline_start <- date_range[1]
    timeline_end <- date_range[2]
  } else {
    # Fallback if no employment data
    timeline_start <- as.Date("2020-01-01")
    timeline_end <- as.Date("2024-12-31")
  }
  
  # Calculate timeline duration and metrics section positioning
  timeline_duration <- as.numeric(timeline_end - timeline_start)
  gap_duration <- timeline_duration * 0.05  # 5% gap between sections
  metrics_start <- timeline_end + gap_duration
  metrics_section_width <- timeline_duration * (metrics_width / timeline_width)
  
  # Create y-position mapping (shared across both sections) - ensure all persons appear
  y_positions <- data.table(
    person = selected_people,
    y_pos = seq_along(selected_people)
  )
  
  # Step 5: Prepare timeline data
  # Use proper dynamic join syntax: person column in y_positions matches person_col in employment data
  join_spec <- setNames(person_col, "person")
  timeline_plot_data <- y_positions[plot_employment_data, on = join_spec]
  timeline_plot_data <- timeline_plot_data[!is.na(y_pos)]
  
  # Step 6: Prepare metrics data with extended x-axis positioning
  # Transform metrics to long format for plotting
  metrics_long <- data.table()
  
  if (nrow(plot_metrics_data) > 0 && length(metrics_to_show) > 0) {
    metrics_melt <- melt(plot_metrics_data, 
                        id.vars = person_col,
                        measure.vars = metrics_to_show,
                        variable.name = "metric_name",
                        value.name = "metric_value")
    
    # Add y-positions and x-positions for metrics - ensure all persons appear
    join_spec_metrics <- setNames(person_col, "person")
    metrics_long <- y_positions[, .(person, y_pos)][
      metrics_melt, on = join_spec_metrics, allow.cartesian = TRUE]
    
    # Handle missing values - set to 0 for persons without metrics
    metrics_long[is.na(metric_value), metric_value := 0]
    
    # Calculate x-positions for metrics (spread across metrics section)
    n_metrics <- length(metrics_to_show)
    metric_spacing <- metrics_section_width / n_metrics
    metric_positions <- data.table(
      metric_name = factor(metrics_to_show),
      x_pos = metrics_start + (seq_len(n_metrics) - 0.5) * metric_spacing
    )
    
    metrics_long <- metrics_long[metric_positions, on = "metric_name"]
  }
  
  # Step 7: Get colors
  if (nrow(timeline_plot_data) > 0) {
    if (use_bw) {
      employment_colors <- vecshift_colors("bw", n = length(unique(timeline_plot_data[[status_col]])))
      names(employment_colors) <- unique(timeline_plot_data[[status_col]])
    } else {
      employment_colors <- get_standardized_employment_colors(unique(timeline_plot_data[[status_col]]))
    }
  } else {
    employment_colors <- c()
  }
  
  # Step 8: Create the integrated plot
  
  # Calculate extended x-axis limits
  if (nrow(metrics_long) > 0) {
    x_limits <- c(timeline_start, metrics_start + metrics_section_width)
  } else {
    x_limits <- c(timeline_start, timeline_end)
  }
  
  # Start with base plot
  p <- ggplot2::ggplot()
  
  # Add timeline section (employment rectangles)
  if (nrow(timeline_plot_data) > 0) {
    p <- p + ggplot2::geom_rect(
      data = timeline_plot_data,
      ggplot2::aes(
        xmin = get(time_col),
        xmax = get(end_col),
        ymin = y_pos - 0.4,
        ymax = y_pos + 0.4,
        fill = get(status_col)
      ),
      alpha = 0.8,
      color = "white",
      linewidth = 0.3
    )
  }
  
  # Add metrics section (points with size based on metric value)
  if (nrow(metrics_long) > 0) {
    p <- p + ggplot2::geom_point(
      data = metrics_long,
      ggplot2::aes(
        x = x_pos,
        y = y_pos,
        size = pmax(0, metric_value),  # Ensure non-negative sizes
        alpha = pmax(0.3, pmin(1.0, metric_value))  # Alpha between 0.3 and 1.0
      ),
      color = "#2C3E50",  # Dark blue-grey from vecshift palette
      shape = 16
    )
    
    # Add visual separator line
    separator_x <- timeline_end + (gap_duration / 2)
    p <- p + ggplot2::geom_vline(
      xintercept = separator_x,
      color = "#95A5A6",
      linetype = "dashed",
      alpha = 0.7
    )
  }
  
  # Step 9: Configure scales and aesthetics
  
  # Configure x-axis with custom breaks and labels
  timeline_breaks <- seq(timeline_start, timeline_end, length.out = 4)
  
  # Metric x-positions for labels
  if (nrow(metrics_long) > 0) {
    metric_breaks <- unique(metrics_long$x_pos)
    metric_labels <- as.character(unique(metrics_long$metric_name))
    # Clean up metric labels for better readability
    metric_labels <- gsub("_", " ", metric_labels)
    metric_labels <- tools::toTitleCase(metric_labels)
  } else {
    metric_breaks <- numeric(0)
    metric_labels <- character(0)
  }
  
  all_x_breaks <- c(timeline_breaks, metric_breaks)
  all_x_labels <- c(format(timeline_breaks, "%b %Y"), metric_labels)
  
  p <- p + ggplot2::scale_x_continuous(
    breaks = all_x_breaks,
    labels = all_x_labels,
    limits = x_limits,
    expand = c(0.02, 0)
  )
  
  # Configure y-axis (shared for perfect alignment)
  p <- p + ggplot2::scale_y_continuous(
    breaks = y_positions$y_pos,
    labels = y_positions$person,
    expand = c(0.02, 0),
    name = "Persons"
  )
  
  # Configure fill scale for employment status
  if (length(employment_colors) > 0) {
    p <- p + ggplot2::scale_fill_manual(
      values = employment_colors,
      name = "Employment Status",
      na.value = "#CCCCCC"
    )
  }
  
  # Configure size scale for metrics
  if (nrow(metrics_long) > 0) {
    p <- p + ggplot2::scale_size_continuous(
      name = "Metric Value",
      range = c(1, 8),
      limits = c(0, 1),
      guide = ggplot2::guide_legend(override.aes = list(alpha = 1))
    )
  }
  
  # Configure alpha scale
  p <- p + ggplot2::scale_alpha_identity()
  
  # Step 10: Add labels and theme
  p <- p + ggplot2::labs(
    title = title,
    subtitle = paste("Timeline (left)",
                    ifelse(nrow(metrics_long) > 0, "and Career Metrics (right)", ""),
                    "for", length(selected_people), "individuals"),
    x = ifelse(nrow(metrics_long) > 0, "Time Period / Metrics", "Time Period"),
    caption = "Generated with longworkR visualization functions"
  )
  
  # Apply vecshift theme
  p <- p + theme_vecshift(base_size = base_size, grid = "major", axis = "both")
  
  # Customize theme for integrated layout
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = base_size * 0.8),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major.x = ggplot2::element_line(color = "#E8EAED", size = 0.5),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#F4F6F7", size = 0.3),
    plot.title = ggplot2::element_text(size = base_size * 1.2, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = base_size * 0.9, color = "#5D6D7E")
  )
  
  # Step 11: Add section labels for clarity
  if (nrow(timeline_plot_data) > 0 && nrow(metrics_long) > 0) {
    # Add section headers
    timeline_label_x <- timeline_start + (timeline_end - timeline_start) / 2
    metrics_label_x <- metrics_start + metrics_section_width / 2
    label_y <- max(y_positions$y_pos) + 0.8
    
    p <- p + ggplot2::annotate(
      "text",
      x = timeline_label_x,
      y = label_y,
      label = "Employment Timeline",
      size = base_size * 0.9 * 0.35,
      fontface = "bold",
      color = "#2C3E50"
    ) + ggplot2::annotate(
      "text",
      x = metrics_label_x,
      y = label_y,
      label = "Career Metrics",
      size = base_size * 0.9 * 0.35,
      fontface = "bold",
      color = "#2C3E50"
    )
  }
  
  return(p)
}
