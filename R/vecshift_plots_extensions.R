#' @title Vecshift Visualization Extensions and Utilities
#' @description
#' Additional functions for the vecshift visualization system including
#' dashboard summaries, sample data generation, and utility functions.
#' 
#' @name vecshift-plots-extensions
NULL

#' Plot Employment Summary Dashboard
#'
#' Creates a comprehensive dashboard-style summary plot combining multiple
#' visualizations to provide an overview of employment patterns, statistics,
#' and key insights from the vecshift analysis.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param person_col Character. Column name for person identifier (default: "cf")
#' @param time_col Character. Column name for time periods (default: "inizio")
#' @param end_col Character. Column name for period end dates (default: "fine")
#' @param status_col Character. Column name for employment status (default: "stato")
#' @param duration_col Character. Column name for duration (default: "durata")
#' @param dashboard_type Character. Type of dashboard: "overview", "executive" (default: "overview")
#' @param time_unit Character. Time aggregation unit: "month", "quarter" (default: "month")
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param include_stats Logical. Include statistical summaries (default: TRUE)
#' @param alpha Numeric. Transparency (default: 0.7)
#'
#' @return A ggplot2 object showing employment dashboard
#' @export
#'
#' @examples
#' \dontrun{
#' # Overview dashboard
#' plot_employment_summary(data, dashboard_type = "overview")
#' 
#' # Executive summary
#' plot_employment_summary(data, dashboard_type = "executive")
#' }
plot_employment_summary <- function(data,
                                   person_col = "cf",
                                   time_col = "inizio", 
                                   end_col = "fine",
                                   status_col = "stato",
                                   duration_col = "durata",
                                   dashboard_type = "overview",
                                   time_unit = "month",
                                   palette = "employment",
                                   use_bw = FALSE,
                                   include_stats = TRUE,
                                   alpha = 0.7) {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Input validation
  dashboard_type <- match.arg(dashboard_type, c("overview", "executive"))
  time_unit <- match.arg(time_unit, c("month", "quarter"))
  
  # Get colors
  if (use_bw) {
    colors <- vecshift_colors("main_bw")
  } else {
    colors <- vecshift_colors(palette)
  }
  
  # Calculate key statistics
  total_people <- length(unique(data[[person_col]]))
  total_periods <- nrow(data)
  date_range <- range(data[[time_col]], na.rm = TRUE)
  avg_duration <- mean(data[[duration_col]], na.rm = TRUE)
  
  # Status distribution
  status_counts <- table(data[[status_col]])
  
  # Employment rate (non-unemployment periods)
  employment_rate <- 1 - (status_counts["disoccupato"] / sum(status_counts))
  if (is.na(employment_rate)) employment_rate <- 1  # If no unemployment periods
  
  # Overlap statistics
  overlap_periods <- sum(data$arco > 1, na.rm = TRUE)
  overlap_rate <- overlap_periods / total_periods
  
  if (dashboard_type == "overview") {
    # Create overview dashboard
    exec_plot <- ggplot2::ggplot() +
      ggplot2::annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, 
                       fill = "#F8F9FA", alpha = 0.3) +
      ggplot2::annotate("text", x = 0.5, y = 0.8, 
                       label = paste("Employment Analysis Summary"),
                       size = 6, fontface = "bold", color = "#2C3E50") +
      ggplot2::annotate("text", x = 0.25, y = 0.65, 
                       label = paste("Total Individuals\n", format(total_people, big.mark = ",")),
                       size = 4, fontface = "bold", color = "#34495E") +
      ggplot2::annotate("text", x = 0.75, y = 0.65, 
                       label = paste("Total Periods\n", format(total_periods, big.mark = ",")),
                       size = 4, fontface = "bold", color = "#34495E") +
      ggplot2::annotate("text", x = 0.25, y = 0.4, 
                       label = paste("Employment Rate\n", round(employment_rate * 100, 1), "%"),
                       size = 4, fontface = "bold", color = "#27AE60") +
      ggplot2::annotate("text", x = 0.75, y = 0.4, 
                       label = paste("Multiple Jobs\n", round(overlap_rate * 100, 1), "%"),
                       size = 4, fontface = "bold", color = "#9B59B6") +
      ggplot2::annotate("text", x = 0.5, y = 0.15, 
                       label = paste("Analysis Period:", format(date_range[1], "%b %Y"), 
                                    "to", format(date_range[2], "%b %Y")),
                       size = 3, color = "#7F8C8D") +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
      ggplot2::labs(title = "Employment Analysis Dashboard") +
      theme_vecshift(base_size = 12, grid = "none", axis = "none") +
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = "#BDC3C7", fill = NA, size = 1),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
      )
    
    return(exec_plot)
    
  } else if (dashboard_type == "executive") {
    # High-level trend analysis
    monthly_data <- data[, .(
      employment_rate = as.numeric(1 - sum(get(status_col) == "disoccupato") / .N),
      avg_duration = as.numeric(mean(get(duration_col), na.rm = TRUE)),
      overlap_rate = as.numeric(sum(arco > 1) / .N),
      n_people = as.integer(length(unique(get(person_col)))),
      n_periods = as.integer(.N)
    ), by = .(month = format(get(time_col), "%Y-%m"))]
    
    monthly_data[, date := as.Date(paste0(month, "-01"))]
    
    # Executive trend plot
    exec_plot <- ggplot2::ggplot(monthly_data, ggplot2::aes(x = date)) +
      ggplot2::geom_line(ggplot2::aes(y = employment_rate * 100, color = "Employment Rate"), 
                        size = 1.2, alpha = 0.8) +
      ggplot2::geom_line(ggplot2::aes(y = overlap_rate * 100, color = "Multiple Jobs Rate"), 
                        size = 1.2, alpha = 0.8) +
      ggplot2::scale_color_manual(
        values = c("Employment Rate" = colors[3], "Multiple Jobs Rate" = colors[5]),
        name = "Metric"
      ) +
      ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
      ggplot2::labs(
        title = "Executive Summary: Employment Trends",
        subtitle = paste("Analysis of", total_people, "individuals across", 
                        round(as.numeric(difftime(date_range[2], date_range[1], units = "days"))/30.4, 1), "months"),
        x = "Time Period",
        y = "Rate (%)",
        caption = paste("Employment Rate:", round(employment_rate * 100, 1), 
                       "% | Multiple Jobs:", round(overlap_rate * 100, 1), "%")
      ) +
      theme_vecshift(base_size = 12, grid = "major") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        plot.caption = ggplot2::element_text(size = 11, fontface = "bold")
      )
    
    return(exec_plot)
  }
}

#' Create Sample Employment Data for Visualization Testing
#'
#' Generates synthetic employment data that demonstrates various employment
#' patterns and can be used for testing and demonstrating the visualization functions.
#'
#' @param n_people Integer. Number of individuals to simulate (default: 20)
#' @param n_periods Integer. Average number of employment periods per person (default: 4)
#' @param date_range Character vector of length 2. Start and end dates (default: c("2022-01-01", "2024-12-31"))
#' @param include_overlaps Logical. Include overlapping employment periods (default: TRUE)
#' @param include_gaps Logical. Include unemployment gaps (default: TRUE)
#' @param seed Integer. Random seed for reproducibility (default: 123)
#'
#' @return A data.table with sample employment data ready for vecshift analysis
#' @export
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' sample_data <- create_sample_employment_data(n_people = 10, n_periods = 3)
#' 
#' # Apply vecshift transformation
#' result <- vecshift(sample_data)
#' 
#' # Create visualizations
#' plot_employment_timeline(result)
#' plot_employment_distribution(result)
#' }
create_sample_employment_data <- function(n_people = 20,
                                         n_periods = 4,
                                         date_range = c("2022-01-01", "2024-12-31"),
                                         include_overlaps = TRUE,
                                         include_gaps = TRUE,
                                         seed = 123) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for sample data generation")
  }
  
  set.seed(seed)
  
  start_date <- as.Date(date_range[1])
  end_date <- as.Date(date_range[2])
  
  sample_data <- data.table::data.table()
  contract_id <- 1
  
  for (person_idx in 1:n_people) {
    person_id <- sprintf("PERSON_%03d", person_idx)
    
    # Generate random number of periods around the average
    n_person_periods <- max(1, rpois(1, n_periods))
    
    # Generate employment periods for this person
    person_periods <- data.table::data.table()
    
    # Start from beginning of date range with some random offset
    current_date <- start_date + sample(0:90, 1)  # Random start within first 3 months
    
    for (period_idx in 1:n_person_periods) {
      # Duration: weighted toward common employment durations
      duration_weights <- c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05, 0.05)  # 1m, 3m, 6m, 1y, 2y, 3y, longer
      duration_choices <- c(30, 90, 180, 365, 730, 1095, 1460)
      base_duration <- sample(duration_choices, 1, prob = duration_weights)
      actual_duration <- max(7, round(base_duration * runif(1, 0.5, 1.5)))  # Add randomness
      
      period_start <- current_date
      period_end <- pmin(period_start + actual_duration, end_date)
      
      # Employment type: 70% full-time, 30% part-time
      is_fulltime <- runif(1) > 0.3
      prior_value <- if (is_fulltime) sample(1:3, 1) else sample(c(-1, 0), 1)
      
      # Add this period
      person_periods <- data.table::rbindlist(list(
        person_periods,
        data.table::data.table(
          id = contract_id,
          cf = person_id,
          INIZIO = period_start,
          FINE = period_end,
          prior = prior_value
        )
      ))
      
      contract_id <- contract_id + 1
      
      # Calculate next start date
      if (include_gaps && runif(1) > 0.4) {  # 60% chance of gap
        gap_duration <- sample(c(7, 30, 90, 180), 1, prob = c(0.4, 0.3, 0.2, 0.1))
        current_date <- period_end + gap_duration
      } else {
        # Small gap or consecutive employment
        current_date <- period_end + sample(0:7, 1)  # 0-7 day gap
      }
      
      # Stop if we've exceeded the date range
      if (current_date >= end_date) break
    }
    
    # Add overlapping periods if requested
    if (include_overlaps && nrow(person_periods) > 0 && runif(1) > 0.7) {  # 30% chance
      # Pick a random existing period to create overlap with
      base_period <- person_periods[sample(1:nrow(person_periods), 1)]
      
      # Create overlapping period (part-time job during existing employment)
      overlap_start <- base_period$INIZIO + sample(30:60, 1)  # Start 1-2 months in
      overlap_duration <- sample(c(60, 90, 120), 1)  # 2-4 months
      overlap_end <- pmin(overlap_start + overlap_duration, base_period$FINE)
      
      if (overlap_start < overlap_end && overlap_end <= end_date) {
        person_periods <- data.table::rbindlist(list(
          person_periods,
          data.table::data.table(
            id = contract_id,
            cf = person_id,
            INIZIO = overlap_start,
            FINE = overlap_end,
            prior = 0  # Part-time overlap
          )
        ))
        
        contract_id <- contract_id + 1
      }
    }
    
    sample_data <- data.table::rbindlist(list(sample_data, person_periods))
  }
  
  # Ensure dates are within range
  sample_data <- sample_data[INIZIO >= start_date & FINE <= end_date]
  sample_data <- sample_data[INIZIO < FINE]  # Remove invalid periods
  
  # Sort by person and start date
  data.table::setorderv(sample_data, c("cf", "INIZIO"))
  
  return(sample_data)
}

#' Quick Employment Data Visualization
#'
#' Creates a quick overview visualization combining timeline and distribution
#' plots for rapid exploration of vecshift results.
#'
#' @param data Data.table output from vecshift() containing employment segments
#' @param n_people_timeline Integer. Number of people to show in timeline (default: 8)
#' @param palette Character. Color palette to use (default: "employment")
#' @param use_bw Logical. Use black and white palette (default: FALSE)
#' @param title Character. Overall title for the plot (default: "Vecshift Analysis Summary")
#'
#' @return A ggplot2 object with combined visualizations
#' @export
#'
#' @examples
#' \dontrun{
#' # Quick visualization
#' result <- vecshift(sample_data)
#' quick_employment_viz(result)
#' 
#' # Black and white version
#' quick_employment_viz(result, use_bw = TRUE)
#' }
quick_employment_viz <- function(data,
                                n_people_timeline = 8,
                                palette = "employment",
                                use_bw = FALSE,
                                title = "Vecshift Analysis Summary") {
  
  # Check required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting functions")
  }
  
  # Get basic statistics
  total_people <- length(unique(data$cf))
  total_periods <- nrow(data)
  employment_rate <- 1 - sum(data$stato == "disoccupato") / total_periods
  
  # Create a simple combined plot showing key insights
  # This is a simplified version - for complex layouts users would need gridExtra
  
  # Timeline for subset of people
  timeline_plot <- plot_employment_timeline(
    data, 
    n_people = n_people_timeline,
    palette = palette,
    use_bw = use_bw,
    alpha = 0.8
  ) +
    ggplot2::labs(
      title = paste(title, "- Employment Timeline"),
      subtitle = paste("Showing", min(n_people_timeline, total_people), "of", total_people, "individuals")
    ) +
    ggplot2::theme(legend.position = "bottom")
  
  return(timeline_plot)
}