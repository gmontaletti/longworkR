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
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    employment_colors <- get_employment_colors()
    # Match available colors to statuses in data
    available_statuses <- unique(plot_data[[status_col]])
    colors <- employment_colors[names(employment_colors) %in% available_statuses]
    
    # Add colors for any missing statuses
    missing_statuses <- setdiff(available_statuses, names(colors))
    if (length(missing_statuses) > 0) {
      additional_colors <- vecshift_colors("main", n = length(missing_statuses))
      names(additional_colors) <- missing_statuses
      colors <- c(colors, additional_colors)
    }
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
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(agg_data$status)))
    names(colors) <- unique(agg_data$status)
  } else {
    employment_colors <- get_employment_colors()
    available_statuses <- unique(agg_data$status)
    colors <- employment_colors[names(employment_colors) %in% available_statuses]
    
    # Add colors for any missing statuses
    missing_statuses <- setdiff(available_statuses, names(colors))
    if (length(missing_statuses) > 0) {
      additional_colors <- vecshift_colors("main", n = length(missing_statuses))
      names(additional_colors) <- missing_statuses
      colors <- c(colors, additional_colors)
    }
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
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    employment_colors <- get_employment_colors()
    available_statuses <- unique(plot_data[[status_col]])
    colors <- employment_colors[names(employment_colors) %in% available_statuses]
    
    # Add colors for any missing statuses
    missing_statuses <- setdiff(available_statuses, names(colors))
    if (length(missing_statuses) > 0) {
      additional_colors <- vecshift_colors("main", n = length(missing_statuses))
      names(additional_colors) <- missing_statuses
      colors <- c(colors, additional_colors)
    }
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
#' Creates flow diagrams showing transitions between employment statuses,
#' using Sankey-style or alluvial visualizations to show employment pathways.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param plot_type Character. Type of plot: "sankey", "alluvial", "chord" (default: "alluvial")
#' @param min_transitions Integer. Minimum number of transitions to show (default: 5)
#' @param max_categories Integer. Maximum categories to show (default: 8)
#' @param time_window Character. Time window for transitions: "all", "year", "quarter" (default: "all")
#' @param group_by Character. Column to group transitions by (default: NULL)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param alpha Numeric. Transparency (default: 0.7)
#' @param show_percentages Logical. Show transition percentages (default: TRUE)
#'
#' @return A ggplot2 object showing transition flows
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic alluvial diagram
#' plot_transition_flows(data, plot_type = "alluvial")
#' 
#' # Sankey diagram with minimum transitions
#' plot_transition_flows(data, plot_type = "sankey", min_transitions = 10)
#' 
#' # Transitions within a year window
#' plot_transition_flows(data, time_window = "year")
#' }
plot_transition_flows <- function(data,
                                 person_col = "cf",
                                 time_col = "inizio", 
                                 status_col = "stato",
                                 plot_type = "alluvial",
                                 min_transitions = 5,
                                 max_categories = 8,
                                 time_window = "all",
                                 group_by = NULL,
                                 palette = "employment",
                                 use_bw = FALSE,
                                 alpha = 0.7,
                                 show_percentages = TRUE) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  plot_type <- match.arg(plot_type, c("alluvial", "sankey", "chord"))
  time_window <- match.arg(time_window, c("all", "year", "quarter"))
  
  # Order data by person and time
  setorderv(data, c(person_col, time_col))
  
  # Create transitions data
  transitions <- data[, {
    if (.N > 1) {
      from_status <- get(status_col)[1:(.N-1)]
      to_status <- get(status_col)[2:.N]
      time_from <- get(time_col)[1:(.N-1)]
      time_to <- get(time_col)[2:.N]
      
      data.table(
        from = from_status,
        to = to_status,
        time_from = time_from,
        time_to = time_to
      )
    } else {
      data.table(from = character(0), to = character(0), 
                 time_from = as.Date(character(0)), time_to = as.Date(character(0)))
    }
  }, by = person_col]
  
  if (nrow(transitions) == 0) {
    warning("No transitions found in the data")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, label = "No transitions found"),
                             size = 6) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "No Employment Transitions Found"))
  }
  
  # Apply time window filtering
  if (time_window != "all") {
    # Add time period grouping and filter
    if (time_window == "year") {
      transitions[, time_period := format(time_from, "%Y")]
      # Use the most recent year for simplicity
      recent_year <- max(transitions$time_period, na.rm = TRUE)
      transitions <- transitions[time_period == recent_year]
    } else if (time_window == "quarter") {
      transitions[, time_period := paste0(format(time_from, "%Y-Q"), 
                                         ceiling(as.numeric(format(time_from, "%m"))/3))]
      # Use the most recent quarter
      recent_quarter <- max(transitions$time_period, na.rm = TRUE)
      transitions <- transitions[time_period == recent_quarter]
    }
  }
  
  # Aggregate transitions
  transition_counts <- transitions[, .N, by = .(from, to)]
  setnames(transition_counts, "N", "count")
  
  # Filter by minimum transitions
  transition_counts <- transition_counts[count >= min_transitions]
  
  # Limit categories if needed
  all_statuses <- unique(c(transition_counts$from, transition_counts$to))
  if (length(all_statuses) > max_categories) {
    # Keep the most common statuses
    status_counts <- data[, .N, by = .(status = get(status_col))]
    setorderv(status_counts, "N", -1)
    keep_statuses <- head(status_counts[[1]], max_categories)
    transition_counts <- transition_counts[from %in% keep_statuses & to %in% keep_statuses]
  }
  
  if (nrow(transition_counts) == 0) {
    warning("No transitions meet the filtering criteria")
    return(ggplot2::ggplot() + 
           ggplot2::geom_text(ggplot2::aes(x = 0.5, y = 0.5, 
                                          label = paste("No transitions with >=", min_transitions, "occurrences")),
                             size = 5) +
           ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
           theme_vecshift() +
           ggplot2::labs(title = "Insufficient Transition Data"))
  }
  
  # Add percentages
  if (show_percentages) {
    transition_counts[, percentage := round(100 * count / sum(count), 1)]
    transition_counts[, label := paste0(from, " → ", to, "\n(", count, ", ", percentage, "%)")]
  } else {
    transition_counts[, label := paste0(from, " → ", to, "\n(", count, ")")]
  }
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(all_statuses))
    names(colors) <- all_statuses
  } else {
    employment_colors <- get_employment_colors()
    colors <- employment_colors[names(employment_colors) %in% all_statuses]
    
    # Add colors for any missing statuses
    missing_statuses <- setdiff(all_statuses, names(colors))
    if (length(missing_statuses) > 0) {
      additional_colors <- vecshift_colors("main", n = length(missing_statuses))
      names(additional_colors) <- missing_statuses
      colors <- c(colors, additional_colors)
    }
  }
  
  if (plot_type == "alluvial") {
    # Create alluvial-style plot using basic ggplot2
    # For a proper alluvial plot, we'll create a simplified flow diagram
    
    # Create coordinates for the flow
    transition_counts[, from_y := match(from, unique(c(from, to)))]
    transition_counts[, to_y := match(to, unique(c(from, to)))]
    transition_counts[, flow_width := sqrt(count) / 2]  # Scale width by count
    
    p <- ggplot2::ggplot(transition_counts) +
      # From status rectangles
      ggplot2::geom_rect(ggplot2::aes(xmin = 1, xmax = 1.5, 
                                     ymin = from_y - flow_width, 
                                     ymax = from_y + flow_width,
                                     fill = from), 
                        alpha = alpha) +
      # To status rectangles
      ggplot2::geom_rect(ggplot2::aes(xmin = 3.5, xmax = 4, 
                                     ymin = to_y - flow_width, 
                                     ymax = to_y + flow_width,
                                     fill = to), 
                        alpha = alpha) +
      # Flow connections (simplified as lines)
      ggplot2::geom_segment(ggplot2::aes(x = 1.5, y = from_y, 
                                        xend = 3.5, yend = to_y,
                                        color = from,
                                        size = count), 
                           alpha = alpha * 0.6) +
      ggplot2::scale_fill_manual(values = colors, name = "Employment\nStatus") +
      ggplot2::scale_color_manual(values = colors, guide = "none") +
      ggplot2::scale_size_continuous(range = c(0.5, 3), guide = "none") +
      ggplot2::scale_y_continuous(breaks = seq_along(unique(c(transition_counts$from, transition_counts$to))),
                                 labels = unique(c(transition_counts$from, transition_counts$to))) +
      ggplot2::labs(
        title = "Employment Status Transition Flows",
        subtitle = paste("Showing", nrow(transition_counts), "transition patterns"),
        x = NULL,
        y = "Employment Status",
        caption = "Line width proportional to transition frequency"
      ) +
      theme_vecshift(base_size = 11, grid = "none", axis = "y") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        legend.position = "bottom"
      ) +
      ggplot2::annotate("text", x = 1.25, y = max(transition_counts$from_y) + 1, 
                       label = "From", fontface = "bold", size = 4) +
      ggplot2::annotate("text", x = 3.75, y = max(transition_counts$to_y) + 1, 
                       label = "To", fontface = "bold", size = 4)
      
  } else if (plot_type == "sankey" || plot_type == "chord") {
    # For now, create a chord-style plot using a heatmap approach
    # Create transition matrix
    all_statuses <- unique(c(transition_counts$from, transition_counts$to))
    transition_matrix <- matrix(0, nrow = length(all_statuses), ncol = length(all_statuses))
    rownames(transition_matrix) <- all_statuses
    colnames(transition_matrix) <- all_statuses
    
    for (i in 1:nrow(transition_counts)) {
      row_idx <- which(rownames(transition_matrix) == transition_counts$from[i])
      col_idx <- which(colnames(transition_matrix) == transition_counts$to[i])
      transition_matrix[row_idx, col_idx] <- transition_counts$count[i]
    }
    
    # Convert to long format for ggplot2
    matrix_long <- expand.grid(from = all_statuses, to = all_statuses, stringsAsFactors = FALSE)
    matrix_long$count <- as.vector(transition_matrix)
    matrix_long <- matrix_long[matrix_long$count > 0, ]
    
    if (show_percentages) {
      matrix_long$percentage <- round(100 * matrix_long$count / sum(matrix_long$count), 1)
      matrix_long$label <- ifelse(matrix_long$count >= min_transitions,
                                 paste0(matrix_long$count, "\n(", matrix_long$percentage, "%)"),
                                 "")
    } else {
      matrix_long$label <- ifelse(matrix_long$count >= min_transitions, 
                                 as.character(matrix_long$count), "")
    }
    
    p <- ggplot2::ggplot(matrix_long, ggplot2::aes(x = to, y = from, fill = count)) +
      ggplot2::geom_tile(alpha = alpha, color = "white", size = 0.5) +
      ggplot2::geom_text(ggplot2::aes(label = label), color = "white", 
                        fontface = "bold", size = 3) +
      ggplot2::scale_fill_gradient(low = colors[1], high = colors[length(colors)],
                                  name = "Transition\nCount") +
      ggplot2::labs(
        title = "Employment Status Transition Matrix",
        subtitle = paste("Heatmap showing", sum(matrix_long$count > 0), "transition patterns"),
        x = "To Status",
        y = "From Status",
        caption = "Numbers show transition counts and percentages"
      ) +
      theme_vecshift(base_size = 11, grid = "none") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  }
  
  return(p)
}

#' Plot Employment Gantt Chart
#'
#' Creates Gantt chart visualization of employment periods, ideal for showing
#' individual employment histories, project timelines, or contract overlaps
#' with precise temporal detail.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param contract_col Character. Column name for contract ID (default: "id")
#' @param facet_by Character. Column to use for faceting (default: NULL)
#' @param n_people Integer. Maximum number of people to show (default: 15)
#' @param date_breaks Character. Date breaks for x-axis (default: "2 months")
#' @param show_contract_ids Logical. Show contract IDs on bars (default: FALSE)
#' @param show_durations Logical. Show duration labels (default: FALSE)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param bar_height Numeric. Height of Gantt bars (default: 0.6)
#' @param alpha Numeric. Transparency (default: 0.8)
#' @param text_size Numeric. Size of text labels (default: 2.5)
#'
#' @return A ggplot2 object showing employment Gantt chart
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic Gantt chart
#' plot_employment_gantt(data)
#' 
#' # Gantt chart with contract IDs
#' plot_employment_gantt(data, show_contract_ids = TRUE)
#' 
#' # Gantt chart with duration labels
#' plot_employment_gantt(data, show_durations = TRUE, n_people = 5)
#' }
plot_employment_gantt <- function(data,
                                 person_col = "cf",
                                 time_col = "inizio", 
                                 end_col = "fine",
                                 status_col = "stato",
                                 contract_col = "id",
                                 facet_by = NULL,
                                 n_people = 15,
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
  
  # Limit to n_people if specified
  if (!is.null(n_people) && n_people > 0) {
    people_to_show <- head(unique(data[[person_col]]), n_people)
    plot_data <- data[get(person_col) %in% people_to_show]
  } else {
    plot_data <- copy(data)
  }
  
  # Create y-position for each person (reverse order for typical Gantt layout)
  people <- unique(plot_data[[person_col]])
  people <- rev(people)  # Reverse so first person appears at top
  plot_data[, y_pos := length(people) + 1 - match(get(person_col), people)]
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw", n = length(unique(plot_data[[status_col]])))
    names(colors) <- unique(plot_data[[status_col]])
  } else {
    employment_colors <- get_employment_colors()
    available_statuses <- unique(plot_data[[status_col]])
    colors <- employment_colors[names(employment_colors) %in% available_statuses]
    
    # Add colors for any missing statuses
    missing_statuses <- setdiff(available_statuses, names(colors))
    if (length(missing_statuses) > 0) {
      additional_colors <- vecshift_colors("main", n = length(missing_statuses))
      names(additional_colors) <- missing_statuses
      colors <- c(colors, additional_colors)
    }
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
      title = "Employment Gantt Chart",
      subtitle = paste("Employment timeline for", length(people), "individuals"),
      x = "Time Period",
      y = "Individuals",
      caption = "Generated with vecshift visualization functions"
    ) +
    theme_vecshift(base_size = 11, grid = "major") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 9),
      legend.position = "bottom",
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
    # Discrete color scale for status
    status_colors <- get_employment_colors()
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