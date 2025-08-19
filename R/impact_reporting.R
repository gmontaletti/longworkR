#' Impact Evaluation: Comprehensive Report Generation
#'
#' This module provides comprehensive report generation functions for impact evaluation
#' studies including HTML/PDF report generation using R Markdown, publication-ready
#' summary tables, multi-format data exports, and professional formatting utilities.
#' Integrates all visualization and analysis functions from the impact evaluation framework.
#'
#' @name impact_reporting
#' @author vecshift package
#' @importFrom data.table data.table setorder setorderv rbindlist copy fwrite
#' @importFrom utils write.csv
NULL

# Main Report Generation Functions ==========================================

#' Generate Comprehensive Impact Evaluation Report
#'
#' Creates a professional HTML or PDF report for impact evaluation studies using R Markdown.
#' Integrates all analysis components including event identification, matching results,
#' impact estimates, robustness checks, and visualizations. Supports customizable templates
#' and follows academic standards for impact evaluation reporting.
#'
#' @param analysis_results List. Complete analysis results from impact evaluation pipeline
#' @param output_format Character. Output format: "html", "pdf", or "both". Default: "html"
#' @param output_file Character. Output file name (without extension). Default: "impact_evaluation_report"
#' @param output_dir Character. Output directory path. Default: current working directory
#' @param template Character. Report template: "academic", "policy", "technical", "executive". Default: "academic"
#' @param title Character. Report title. Default: "Impact Evaluation Report"
#' @param subtitle Character. Report subtitle. Default: NULL
#' @param author Character. Author name(s). Default: "vecshift Analysis"
#' @param date Character. Report date. Default: current date
#' @param include_sections Character vector. Sections to include: "executive_summary", "methodology", 
#'   "results", "robustness", "conclusions", "technical_appendix". Default: all sections
#' @param include_plots Logical. Include visualization plots. Default: TRUE
#' @param include_tables Logical. Include summary tables. Default: TRUE
#' @param table_format Character. Table format for output: "kable", "gt", "flextable". Default: "kable"
#' @param plot_theme Character. ggplot2 theme for plots: "minimal", "classic", "bw". Default: "minimal"
#' @param colorblind_friendly Logical. Use colorblind-friendly palettes. Default: TRUE
#' @param dpi Numeric. Plot resolution (DPI) for output. Default: 300
#' @param keep_tex Logical. Keep .tex file for PDF output. Default: FALSE
#' @param quiet Logical. Suppress R Markdown rendering messages. Default: TRUE
#'
#' @return Character. Path to generated report file(s)
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate comprehensive HTML report
#' report_path <- generate_impact_report(
#'   analysis_results = complete_analysis,
#'   output_format = "html",
#'   template = "academic",
#'   title = "Employment Program Impact Evaluation",
#'   author = "Policy Research Team"
#' )
#' 
#' # Generate executive summary in PDF
#' exec_report <- generate_impact_report(
#'   analysis_results = complete_analysis,
#'   output_format = "pdf",
#'   template = "executive",
#'   include_sections = c("executive_summary", "results")
#' )
#' }
generate_impact_report <- function(analysis_results,
                                 output_format = "html",
                                 output_file = "impact_evaluation_report",
                                 output_dir = getwd(),
                                 template = "academic",
                                 title = "Impact Evaluation Report",
                                 subtitle = NULL,
                                 author = "vecshift Analysis",
                                 date = Sys.Date(),
                                 include_sections = c("executive_summary", "methodology", 
                                                    "results", "robustness", "conclusions", 
                                                    "technical_appendix"),
                                 include_plots = TRUE,
                                 include_tables = TRUE,
                                 table_format = "kable",
                                 plot_theme = "minimal",
                                 colorblind_friendly = TRUE,
                                 dpi = 300,
                                 keep_tex = FALSE,
                                 quiet = TRUE) {
  
  # Validate inputs
  if (!inherits(analysis_results, "list")) {
    stop("analysis_results must be a list containing analysis components")
  }
  
  output_format <- match.arg(output_format, c("html", "pdf", "both"))
  template <- match.arg(template, c("academic", "policy", "technical", "executive"))
  table_format <- match.arg(table_format, c("kable", "gt", "flextable"))
  plot_theme <- match.arg(plot_theme, c("minimal", "classic", "bw"))
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate report parameters
  params <- list(
    analysis_results = analysis_results,
    template = template,
    title = title,
    subtitle = subtitle,
    author = author,
    date = date,
    include_sections = include_sections,
    include_plots = include_plots,
    include_tables = include_tables,
    table_format = table_format,
    plot_theme = plot_theme,
    colorblind_friendly = colorblind_friendly,
    dpi = dpi
  )
  
  # Create R Markdown template
  rmd_content <- .create_rmd_template(template, params)
  
  # Write R Markdown file
  rmd_file <- file.path(output_dir, paste0(output_file, ".Rmd"))
  writeLines(rmd_content, rmd_file)
  
  # Render report(s)
  output_files <- character()
  
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    
    if (output_format %in% c("html", "both")) {
      html_output <- rmarkdown::render(
        input = rmd_file,
        output_format = rmarkdown::html_document(
          theme = "flatly",
          toc = TRUE,
          toc_depth = 3,
          toc_float = TRUE,
          code_folding = "hide",
          fig_width = 10,
          fig_height = 6,
          self_contained = TRUE
        ),
        output_file = paste0(output_file, ".html"),
        output_dir = output_dir,
        params = params,
        quiet = quiet,
        envir = new.env()
      )
      output_files <- c(output_files, html_output)
    }
    
    if (output_format %in% c("pdf", "both")) {
      pdf_output <- rmarkdown::render(
        input = rmd_file,
        output_format = rmarkdown::pdf_document(
          toc = TRUE,
          toc_depth = 3,
          number_sections = TRUE,
          fig_width = 8,
          fig_height = 5,
          keep_tex = keep_tex,
          latex_engine = "xelatex"
        ),
        output_file = paste0(output_file, ".pdf"),
        output_dir = output_dir,
        params = params,
        quiet = quiet,
        envir = new.env()
      )
      output_files <- c(output_files, pdf_output)
    }
    
  } else {
    warning("rmarkdown package not available. R Markdown file created but not rendered.")
    output_files <- rmd_file
  }
  
  # Clean up temporary R Markdown file (optional)
  if (length(output_files) > 1) {
    file.remove(rmd_file)
  }
  
  message("Report generated successfully: ", paste(output_files, collapse = ", "))
  return(output_files)
}

#' Create Publication-Ready Impact Summary Tables
#'
#' Generates professional summary tables for impact evaluation results including
#' treatment effects, standard errors, confidence intervals, and statistical significance
#' indicators. Tables are formatted for publication and can be exported in multiple formats.
#'
#' @param analysis_results List. Complete analysis results from impact evaluation
#' @param table_type Character. Type of summary table: "main_results", "robustness", 
#'   "balance", "descriptive", "all". Default: "main_results"
#' @param significance_levels Numeric vector. Significance levels for indicators. 
#'   Default: c(0.01, 0.05, 0.10)
#' @param round_digits Integer. Number of decimal places for rounding. Default: 3
#' @param include_ci Logical. Include confidence intervals. Default: TRUE
#' @param ci_level Numeric. Confidence interval level. Default: 0.95
#' @param include_sample_sizes Logical. Include sample size information. Default: TRUE
#' @param table_style Character. Table styling: "academic", "policy", "minimal". Default: "academic"
#' @param variable_labels List. Custom variable labels for display. Default: NULL
#' @param footer_text Character. Custom footer text. Default: NULL
#' @param caption Character. Table caption. Default: auto-generated
#'
#' @return A list containing formatted tables (data.table objects) and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate main results table
#' main_table <- create_impact_summary_table(
#'   analysis_results = results,
#'   table_type = "main_results",
#'   significance_levels = c(0.01, 0.05, 0.10)
#' )
#' 
#' # Generate all summary tables
#' all_tables <- create_impact_summary_table(
#'   analysis_results = results,
#'   table_type = "all",
#'   table_style = "policy"
#' )
#' }
create_impact_summary_table <- function(analysis_results,
                                      table_type = "main_results",
                                      significance_levels = c(0.01, 0.05, 0.10),
                                      round_digits = 3,
                                      include_ci = TRUE,
                                      ci_level = 0.95,
                                      include_sample_sizes = TRUE,
                                      table_style = "academic",
                                      variable_labels = NULL,
                                      footer_text = NULL,
                                      caption = NULL) {
  
  # Validate inputs
  if (!inherits(analysis_results, "list")) {
    stop("analysis_results must be a list containing analysis components")
  }
  
  table_type <- match.arg(table_type, c("main_results", "robustness", "balance", "descriptive", "all"))
  table_style <- match.arg(table_style, c("academic", "policy", "minimal"))
  
  # Initialize results list
  tables <- list()
  
  # Generate main results table
  if (table_type %in% c("main_results", "all")) {
    if ("impact_estimates" %in% names(analysis_results)) {
      main_table <- .format_main_results_table(
        analysis_results$impact_estimates,
        significance_levels = significance_levels,
        round_digits = round_digits,
        include_ci = include_ci,
        ci_level = ci_level,
        include_sample_sizes = include_sample_sizes,
        table_style = table_style,
        variable_labels = variable_labels
      )
      
      tables$main_results <- main_table
    }
  }
  
  # Generate robustness checks table
  if (table_type %in% c("robustness", "all")) {
    if ("robustness_checks" %in% names(analysis_results)) {
      robustness_table <- .format_robustness_table(
        analysis_results$robustness_checks,
        significance_levels = significance_levels,
        round_digits = round_digits,
        table_style = table_style
      )
      
      tables$robustness <- robustness_table
    }
  }
  
  # Generate balance table
  if (table_type %in% c("balance", "all")) {
    if ("matching_results" %in% names(analysis_results)) {
      balance_table <- .format_balance_table(
        analysis_results$matching_results,
        round_digits = round_digits,
        table_style = table_style,
        variable_labels = variable_labels
      )
      
      tables$balance <- balance_table
    }
  }
  
  # Generate descriptive statistics table
  if (table_type %in% c("descriptive", "all")) {
    if ("descriptive_stats" %in% names(analysis_results)) {
      descriptive_table <- .format_descriptive_table(
        analysis_results$descriptive_stats,
        round_digits = round_digits,
        table_style = table_style,
        variable_labels = variable_labels
      )
      
      tables$descriptive <- descriptive_table
    }
  }
  
  # Add metadata
  metadata <- list(
    table_type = table_type,
    significance_levels = significance_levels,
    ci_level = ci_level,
    round_digits = round_digits,
    table_style = table_style,
    generated_date = Sys.Date(),
    footer_text = footer_text,
    caption = caption
  )
  
  # Combine results
  result <- list(
    tables = tables,
    metadata = metadata
  )
  
  class(result) <- c("impact_summary_tables", "list")
  
  return(result)
}

#' Export Impact Evaluation Results in Multiple Formats
#'
#' Exports impact evaluation results and summary tables in multiple formats including
#' CSV, Excel, JSON, and R data formats. Supports batch export and customizable
#' file organization.
#'
#' @param analysis_results List. Complete analysis results from impact evaluation
#' @param export_formats Character vector. Export formats: "csv", "xlsx", "json", "rds", "rdata".
#'   Default: c("csv", "xlsx")
#' @param output_dir Character. Output directory path. Default: current working directory
#' @param file_prefix Character. Prefix for output files. Default: "impact_analysis"
#' @param include_raw_data Logical. Include raw analysis data. Default: TRUE
#' @param include_summary_tables Logical. Include formatted summary tables. Default: TRUE
#' @param include_plots Logical. Export plots as separate files. Default: FALSE
#' @param plot_format Character. Plot export format if included: "png", "pdf", "svg". Default: "png"
#' @param excel_sheets Logical. Create separate Excel sheets for different components. Default: TRUE
#' @param compression Logical. Use compression for file formats that support it. Default: TRUE
#' @param timestamp Logical. Add timestamp to filenames. Default: FALSE
#'
#' @return A list containing paths to exported files organized by format
#' @export
#'
#' @examples
#' \dontrun{
#' # Export to CSV and Excel
#' exported_files <- export_impact_results(
#'   analysis_results = results,
#'   export_formats = c("csv", "xlsx"),
#'   file_prefix = "employment_program_impact"
#' )
#' 
#' # Export everything including plots
#' complete_export <- export_impact_results(
#'   analysis_results = results,
#'   export_formats = c("csv", "xlsx", "json"),
#'   include_plots = TRUE,
#'   plot_format = "png"
#' )
#' }
export_impact_results <- function(analysis_results,
                                export_formats = c("csv", "xlsx"),
                                output_dir = getwd(),
                                file_prefix = "impact_analysis",
                                include_raw_data = TRUE,
                                include_summary_tables = TRUE,
                                include_plots = FALSE,
                                plot_format = "png",
                                excel_sheets = TRUE,
                                compression = TRUE,
                                timestamp = FALSE) {
  
  # Validate inputs
  if (!inherits(analysis_results, "list")) {
    stop("analysis_results must be a list containing analysis components")
  }
  
  export_formats <- match.arg(export_formats, c("csv", "xlsx", "json", "rds", "rdata"), several.ok = TRUE)
  plot_format <- match.arg(plot_format, c("png", "pdf", "svg"))
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Create timestamp suffix if requested
  timestamp_suffix <- if (timestamp) paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S")) else ""
  
  # Initialize export results
  exported_files <- list()
  
  # Prepare summary tables if requested
  summary_tables <- NULL
  if (include_summary_tables) {
    summary_tables <- create_impact_summary_table(analysis_results, table_type = "all")
  }
  
  # Export to CSV format
  if ("csv" %in% export_formats) {
    csv_files <- .export_to_csv(
      analysis_results = analysis_results,
      summary_tables = summary_tables,
      output_dir = output_dir,
      file_prefix = file_prefix,
      timestamp_suffix = timestamp_suffix,
      include_raw_data = include_raw_data,
      include_summary_tables = include_summary_tables
    )
    exported_files$csv <- csv_files
  }
  
  # Export to Excel format
  if ("xlsx" %in% export_formats) {
    if (requireNamespace("openxlsx", quietly = TRUE)) {
      xlsx_files <- .export_to_xlsx(
        analysis_results = analysis_results,
        summary_tables = summary_tables,
        output_dir = output_dir,
        file_prefix = file_prefix,
        timestamp_suffix = timestamp_suffix,
        include_raw_data = include_raw_data,
        include_summary_tables = include_summary_tables,
        separate_sheets = excel_sheets
      )
      exported_files$xlsx <- xlsx_files
    } else {
      warning("openxlsx package not available. Skipping Excel export.")
    }
  }
  
  # Export to JSON format
  if ("json" %in% export_formats) {
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_files <- .export_to_json(
        analysis_results = analysis_results,
        summary_tables = summary_tables,
        output_dir = output_dir,
        file_prefix = file_prefix,
        timestamp_suffix = timestamp_suffix,
        include_raw_data = include_raw_data,
        include_summary_tables = include_summary_tables
      )
      exported_files$json <- json_files
    } else {
      warning("jsonlite package not available. Skipping JSON export.")
    }
  }
  
  # Export to RDS format
  if ("rds" %in% export_formats) {
    rds_file <- file.path(output_dir, paste0(file_prefix, timestamp_suffix, ".rds"))
    
    export_data <- list(
      analysis_results = if (include_raw_data) analysis_results else NULL,
      summary_tables = if (include_summary_tables) summary_tables else NULL,
      export_metadata = list(
        export_date = Sys.time(),
        vecshift_version = utils::packageVersion("vecshift"),
        r_version = R.version.string
      )
    )
    
    saveRDS(export_data, file = rds_file, compress = compression)
    exported_files$rds <- rds_file
  }
  
  # Export to RData format
  if ("rdata" %in% export_formats) {
    rdata_file <- file.path(output_dir, paste0(file_prefix, timestamp_suffix, ".RData"))
    
    # Create objects to save
    if (include_raw_data) {
      impact_analysis_results <- analysis_results
    }
    if (include_summary_tables) {
      impact_summary_tables <- summary_tables
    }
    
    # Save with descriptive object names
    object_names <- character()
    if (include_raw_data) object_names <- c(object_names, "impact_analysis_results")
    if (include_summary_tables) object_names <- c(object_names, "impact_summary_tables")
    
    save(list = object_names, file = rdata_file, compress = compression)
    exported_files$rdata <- rdata_file
  }
  
  # Export plots if requested
  if (include_plots && !is.null(analysis_results$plots)) {
    plot_files <- .export_plots(
      plots = analysis_results$plots,
      output_dir = output_dir,
      file_prefix = file_prefix,
      timestamp_suffix = timestamp_suffix,
      plot_format = plot_format
    )
    exported_files$plots <- plot_files
  }
  
  # Create export summary
  export_summary <- list(
    export_date = Sys.time(),
    export_formats = export_formats,
    output_directory = output_dir,
    file_prefix = file_prefix,
    files_created = length(unlist(exported_files)),
    exported_files = exported_files
  )
  
  message("Export completed successfully. ", export_summary$files_created, " files created.")
  
  return(export_summary)
}

# Helper Functions for Table Formatting =====================================

#' Format Main Results Table
#' @keywords internal
.format_main_results_table <- function(estimates, significance_levels, round_digits,
                                     include_ci, ci_level, include_sample_sizes,
                                     table_style, variable_labels) {
  
  if (is.null(estimates) || !is.data.frame(estimates)) {
    return(data.table::data.table())
  }
  
  # Convert to data.table if needed
  if (!data.table::is.data.table(estimates)) {
    estimates <- data.table::as.data.table(estimates)
  }
  
  # Create formatted table
  formatted_table <- data.table::copy(estimates)
  
  # Apply variable labels if provided
  if (!is.null(variable_labels)) {
    for (var in names(variable_labels)) {
      if (var %in% colnames(formatted_table)) {
        formatted_table[, (var) := variable_labels[[var]]]
      }
    }
  }
  
  # Round numeric columns
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[, (names(formatted_table)[numeric_cols]) := lapply(.SD, round, round_digits),
                  .SDcols = numeric_cols]
  
  # Add significance indicators
  if ("p_value" %in% colnames(formatted_table)) {
    formatted_table[, significance := ""]
    for (level in sort(significance_levels)) {
      stars <- paste(rep("*", which(significance_levels == level)), collapse = "")
      formatted_table[p_value <= level, significance := stars]
    }
  }
  
  # Add confidence intervals if requested
  if (include_ci && all(c("estimate", "std_error") %in% colnames(formatted_table))) {
    alpha <- 1 - ci_level
    z_score <- qnorm(1 - alpha/2)
    
    formatted_table[, ci_lower := estimate - z_score * std_error]
    formatted_table[, ci_upper := estimate + z_score * std_error]
    formatted_table[, ci_text := paste0("[", round(ci_lower, round_digits), ", ", 
                                       round(ci_upper, round_digits), "]")]
  }
  
  return(formatted_table)
}

#' Format Robustness Table
#' @keywords internal
.format_robustness_table <- function(robustness_checks, significance_levels, round_digits, table_style) {
  
  if (is.null(robustness_checks) || !is.data.frame(robustness_checks)) {
    return(data.table::data.table())
  }
  
  # Convert to data.table if needed
  if (!data.table::is.data.table(robustness_checks)) {
    robustness_checks <- data.table::as.data.table(robustness_checks)
  }
  
  # Format similar to main results
  formatted_table <- data.table::copy(robustness_checks)
  
  # Round numeric columns
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[, (names(formatted_table)[numeric_cols]) := lapply(.SD, round, round_digits),
                  .SDcols = numeric_cols]
  
  return(formatted_table)
}

#' Format Balance Table
#' @keywords internal
.format_balance_table <- function(matching_results, round_digits, table_style, variable_labels) {
  
  if (is.null(matching_results) || !"balance_stats" %in% names(matching_results)) {
    return(data.table::data.table())
  }
  
  balance_stats <- matching_results$balance_stats
  
  if (!data.table::is.data.table(balance_stats)) {
    balance_stats <- data.table::as.data.table(balance_stats)
  }
  
  # Format balance statistics
  formatted_table <- data.table::copy(balance_stats)
  
  # Apply variable labels if provided
  if (!is.null(variable_labels)) {
    for (var in names(variable_labels)) {
      if (var %in% colnames(formatted_table)) {
        formatted_table[, (var) := variable_labels[[var]]]
      }
    }
  }
  
  # Round numeric columns
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[, (names(formatted_table)[numeric_cols]) := lapply(.SD, round, round_digits),
                  .SDcols = numeric_cols]
  
  return(formatted_table)
}

#' Format Descriptive Statistics Table
#' @keywords internal
.format_descriptive_table <- function(descriptive_stats, round_digits, table_style, variable_labels) {
  
  if (is.null(descriptive_stats) || !is.data.frame(descriptive_stats)) {
    return(data.table::data.table())
  }
  
  if (!data.table::is.data.table(descriptive_stats)) {
    descriptive_stats <- data.table::as.data.table(descriptive_stats)
  }
  
  # Format descriptive statistics
  formatted_table <- data.table::copy(descriptive_stats)
  
  # Apply variable labels if provided
  if (!is.null(variable_labels)) {
    for (var in names(variable_labels)) {
      if (var %in% colnames(formatted_table)) {
        formatted_table[, (var) := variable_labels[[var]]]
      }
    }
  }
  
  # Round numeric columns
  numeric_cols <- sapply(formatted_table, is.numeric)
  formatted_table[, (names(formatted_table)[numeric_cols]) := lapply(.SD, round, round_digits),
                  .SDcols = numeric_cols]
  
  return(formatted_table)
}

# Helper Functions for Data Export ===========================================

#' Export to CSV Format
#' @keywords internal
.export_to_csv <- function(analysis_results, summary_tables, output_dir, file_prefix,
                          timestamp_suffix, include_raw_data, include_summary_tables) {
  
  csv_files <- character()
  
  # Export raw analysis results
  if (include_raw_data) {
    for (component_name in names(analysis_results)) {
      component_data <- analysis_results[[component_name]]
      
      # Only export data.frame/data.table objects
      if (is.data.frame(component_data)) {
        csv_file <- file.path(output_dir, paste0(file_prefix, "_", component_name, timestamp_suffix, ".csv"))
        data.table::fwrite(component_data, csv_file)
        csv_files <- c(csv_files, csv_file)
      }
    }
  }
  
  # Export summary tables
  if (include_summary_tables && !is.null(summary_tables)) {
    for (table_name in names(summary_tables$tables)) {
      table_data <- summary_tables$tables[[table_name]]
      csv_file <- file.path(output_dir, paste0(file_prefix, "_", table_name, "_table", timestamp_suffix, ".csv"))
      data.table::fwrite(table_data, csv_file)
      csv_files <- c(csv_files, csv_file)
    }
  }
  
  return(csv_files)
}

#' Export to Excel Format
#' @keywords internal
.export_to_xlsx <- function(analysis_results, summary_tables, output_dir, file_prefix,
                           timestamp_suffix, include_raw_data, include_summary_tables,
                           separate_sheets) {
  
  xlsx_files <- character()
  
  if (separate_sheets) {
    # Single Excel file with multiple sheets
    xlsx_file <- file.path(output_dir, paste0(file_prefix, timestamp_suffix, ".xlsx"))
    wb <- openxlsx::createWorkbook()
    
    # Add raw data sheets
    if (include_raw_data) {
      for (component_name in names(analysis_results)) {
        component_data <- analysis_results[[component_name]]
        if (is.data.frame(component_data)) {
          openxlsx::addWorksheet(wb, component_name)
          openxlsx::writeData(wb, component_name, component_data)
        }
      }
    }
    
    # Add summary table sheets
    if (include_summary_tables && !is.null(summary_tables)) {
      for (table_name in names(summary_tables$tables)) {
        table_data <- summary_tables$tables[[table_name]]
        sheet_name <- paste0(table_name, "_table")
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, table_data)
      }
    }
    
    openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
    xlsx_files <- c(xlsx_files, xlsx_file)
    
  } else {
    # Separate Excel files for each component
    if (include_raw_data) {
      for (component_name in names(analysis_results)) {
        component_data <- analysis_results[[component_name]]
        if (is.data.frame(component_data)) {
          xlsx_file <- file.path(output_dir, paste0(file_prefix, "_", component_name, timestamp_suffix, ".xlsx"))
          openxlsx::write.xlsx(component_data, xlsx_file)
          xlsx_files <- c(xlsx_files, xlsx_file)
        }
      }
    }
    
    if (include_summary_tables && !is.null(summary_tables)) {
      for (table_name in names(summary_tables$tables)) {
        table_data <- summary_tables$tables[[table_name]]
        xlsx_file <- file.path(output_dir, paste0(file_prefix, "_", table_name, "_table", timestamp_suffix, ".xlsx"))
        openxlsx::write.xlsx(table_data, xlsx_file)
        xlsx_files <- c(xlsx_files, xlsx_file)
      }
    }
  }
  
  return(xlsx_files)
}

#' Export to JSON Format
#' @keywords internal
.export_to_json <- function(analysis_results, summary_tables, output_dir, file_prefix,
                           timestamp_suffix, include_raw_data, include_summary_tables) {
  
  json_files <- character()
  
  # Create combined JSON structure
  json_data <- list()
  
  if (include_raw_data) {
    json_data$analysis_results <- analysis_results
  }
  
  if (include_summary_tables && !is.null(summary_tables)) {
    json_data$summary_tables <- summary_tables
  }
  
  # Add metadata
  json_data$metadata <- list(
    export_date = Sys.time(),
    vecshift_version = utils::packageVersion("vecshift"),
    r_version = R.version.string
  )
  
  # Export JSON file
  json_file <- file.path(output_dir, paste0(file_prefix, timestamp_suffix, ".json"))
  jsonlite::write_json(json_data, json_file, pretty = TRUE, auto_unbox = TRUE)
  json_files <- c(json_files, json_file)
  
  return(json_files)
}

#' Export Plots
#' @keywords internal
.export_plots <- function(plots, output_dir, file_prefix, timestamp_suffix, plot_format) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    warning("ggplot2 not available. Skipping plot export.")
    return(character())
  }
  
  plot_files <- character()
  
  for (plot_name in names(plots)) {
    plot_obj <- plots[[plot_name]]
    
    if (inherits(plot_obj, "ggplot")) {
      plot_file <- file.path(output_dir, paste0(file_prefix, "_", plot_name, timestamp_suffix, ".", plot_format))
      
      if (plot_format == "png") {
        ggplot2::ggsave(plot_file, plot_obj, device = "png", dpi = 300, width = 10, height = 6)
      } else if (plot_format == "pdf") {
        ggplot2::ggsave(plot_file, plot_obj, device = "pdf", width = 10, height = 6)
      } else if (plot_format == "svg") {
        ggplot2::ggsave(plot_file, plot_obj, device = "svg", width = 10, height = 6)
      }
      
      plot_files <- c(plot_files, plot_file)
    }
  }
  
  return(plot_files)
}

#' Create R Markdown Template
#' @keywords internal
.create_rmd_template <- function(template, params) {
  
  # Basic YAML header
  yaml_header <- c(
    "---",
    paste0("title: \"", params$title, "\""),
    if (!is.null(params$subtitle)) paste0("subtitle: \"", params$subtitle, "\""),
    paste0("author: \"", params$author, "\""),
    paste0("date: \"", params$date, "\""),
    "output:",
    "  html_document:",
    "    theme: flatly",
    "    toc: true",
    "    toc_depth: 3",
    "    toc_float: true",
    "    code_folding: hide",
    "    fig_width: 10",
    "    fig_height: 6",
    "  pdf_document:",
    "    toc: true",
    "    toc_depth: 3",
    "    number_sections: true",
    "    fig_width: 8",
    "    fig_height: 5",
    "params:",
    "  analysis_results: !r NULL",
    "  template: \"academic\"",
    "  include_sections: !r c('executive_summary', 'methodology', 'results', 'robustness', 'conclusions', 'technical_appendix')",
    "  include_plots: TRUE",
    "  include_tables: TRUE",
    "  table_format: \"kable\"",
    "  plot_theme: \"minimal\"",
    "  colorblind_friendly: TRUE",
    "---",
    ""
  )
  
  # R setup chunk
  setup_chunk <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(vecshift)",
    "library(data.table)",
    "library(ggplot2)",
    "library(knitr)",
    "",
    "# Load analysis results from parameters",
    "analysis_results <- params$analysis_results",
    "```",
    ""
  )
  
  # Template content based on type
  content <- switch(template,
    "academic" = .create_academic_template(params),
    "policy" = .create_policy_template(params),
    "technical" = .create_technical_template(params),
    "executive" = .create_executive_template(params),
    .create_academic_template(params)  # default
  )
  
  # Combine all parts
  rmd_content <- c(yaml_header, setup_chunk, content)
  
  return(rmd_content)
}

#' Create Academic Template Content
#' @keywords internal
.create_academic_template <- function(params) {
  
  content <- c(
    "# Executive Summary",
    "",
    "```{r executive-summary, results='asis', eval='executive_summary' %in% params$include_sections}",
    "if (!is.null(analysis_results$summary)) {",
    "  cat('This study evaluates the impact of employment interventions using rigorous econometric methods.')",
    "  cat('\\n\\nKey findings:')",
    "  if (!is.null(analysis_results$impact_estimates)) {",
    "    main_results <- analysis_results$impact_estimates",
    "    if (nrow(main_results) > 0) {",
    "      for (i in 1:min(3, nrow(main_results))) {",
    "        result <- main_results[i, ]",
    "        cat(paste0('\\n- ', result$outcome, ': ', round(result$estimate, 3), ' (SE: ', round(result$std_error, 3), ')'))",
    "      }",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "# 1. Introduction and Methodology",
    "",
    "```{r methodology, results='asis', eval='methodology' %in% params$include_sections}",
    "cat('## Research Question\\n\\n')",
    "cat('This study examines the causal impact of employment interventions on labor market outcomes using administrative data processed through the vecshift package.\\n\\n')",
    "",
    "cat('## Data and Sample\\n\\n')",
    "if (!is.null(analysis_results$event_identification)) {",
    "  event_data <- analysis_results$event_identification",
    "  n_treated <- sum(event_data$is_treated, na.rm = TRUE)",
    "  n_control <- sum(!event_data$is_treated, na.rm = TRUE)",
    "  cat(paste0('- Treated units: ', format(n_treated, big.mark = ',')), '\\n')",
    "  cat(paste0('- Control units: ', format(n_control, big.mark = ',')), '\\n')",
    "}",
    "",
    "cat('\\n## Empirical Strategy\\n\\n')",
    "cat('We employ multiple estimation strategies to ensure robustness:\\n\\n')",
    "cat('1. **Propensity Score Matching**: Controls for observable characteristics\\n')",
    "cat('2. **Difference-in-Differences**: Exploits temporal variation in treatment\\n')",
    "cat('3. **Event Study Design**: Analyzes dynamic treatment effects\\n')",
    "```",
    "",
    "# 2. Results",
    "",
    "```{r main-results, results='asis', eval='results' %in% params$include_sections}",
    "if (!is.null(analysis_results$impact_estimates) && params$include_tables) {",
    "  cat('## Main Results\\n\\n')",
    "  ",
    "  # Create main results table",
    "  main_table <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'main_results'",
    "  )",
    "  ",
    "  if (length(main_table$tables) > 0) {",
    "    kable(main_table$tables$main_results, ",
    "          caption = 'Main Impact Estimates',",
    "          digits = 3)",
    "  }",
    "}",
    "```",
    "",
    "```{r result-plots, eval=params$include_plots && 'results' %in% params$include_sections}",
    "# Include main visualization plots here",
    "if (!is.null(analysis_results$plots)) {",
    "  for (plot_name in names(analysis_results$plots)) {",
    "    if (inherits(analysis_results$plots[[plot_name]], 'ggplot')) {",
    "      print(analysis_results$plots[[plot_name]])",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "# 3. Robustness Checks",
    "",
    "```{r robustness, results='asis', eval='robustness' %in% params$include_sections}",
    "cat('## Sensitivity Analysis\\n\\n')",
    "cat('We conduct several robustness checks to validate our main findings:\\n\\n')",
    "",
    "if (!is.null(analysis_results$robustness_checks) && params$include_tables) {",
    "  robustness_table <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'robustness'",
    "  )",
    "  ",
    "  if (length(robustness_table$tables) > 0) {",
    "    kable(robustness_table$tables$robustness, ",
    "          caption = 'Robustness Checks',",
    "          digits = 3)",
    "  }",
    "}",
    "```",
    "",
    "# 4. Conclusions",
    "",
    "```{r conclusions, results='asis', eval='conclusions' %in% params$include_sections}",
    "cat('## Policy Implications\\n\\n')",
    "cat('The results provide evidence for the effectiveness of the employment intervention program.\\n\\n')",
    "",
    "cat('## Limitations\\n\\n')",
    "cat('- Selection bias may remain despite matching procedures\\n')",
    "cat('- External validity may be limited to the study context\\n')",
    "cat('- Long-term effects require additional follow-up\\n')",
    "```",
    "",
    "# Technical Appendix",
    "",
    "```{r appendix, results='asis', eval='technical_appendix' %in% params$include_sections}",
    "cat('## A. Balance Assessment\\n\\n')",
    "",
    "if (!is.null(analysis_results$matching_results) && params$include_tables) {",
    "  balance_table <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'balance'",
    "  )",
    "  ",
    "  if (length(balance_table$tables) > 0) {",
    "    kable(balance_table$tables$balance, ",
    "          caption = 'Covariate Balance Before and After Matching',",
    "          digits = 3)",
    "  }",
    "}",
    "",
    "cat('\\n## B. Descriptive Statistics\\n\\n')",
    "",
    "if (!is.null(analysis_results$descriptive_stats) && params$include_tables) {",
    "  desc_table <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'descriptive'",
    "  )",
    "  ",
    "  if (length(desc_table$tables) > 0) {",
    "    kable(desc_table$tables$descriptive, ",
    "          caption = 'Descriptive Statistics by Treatment Status',",
    "          digits = 3)",
    "  }",
    "}",
    "```",
    "",
    "---",
    "",
    "*Report generated using the vecshift package for R.*"
  )
  
  return(content)
}

#' Create Policy Template Content
#' @keywords internal
.create_policy_template <- function(params) {
  
  content <- c(
    "# Executive Summary",
    "",
    "```{r policy-summary, results='asis'}",
    "cat('## Key Findings\\n\\n')",
    "cat('This evaluation assesses the effectiveness of employment intervention programs.\\n\\n')",
    "",
    "if (!is.null(analysis_results$impact_estimates)) {",
    "  cat('### Program Impact:\\n\\n')",
    "  main_results <- analysis_results$impact_estimates",
    "  if (nrow(main_results) > 0) {",
    "    for (i in 1:min(5, nrow(main_results))) {",
    "      result <- main_results[i, ]",
    "      effect_size <- ifelse(result$estimate > 0, 'positive', 'negative')",
    "      significance <- ifelse(result$p_value < 0.05, 'statistically significant', 'not significant')",
    "      cat(paste0('- **', result$outcome, '**: ', effect_size, ' effect (', significance, ')\\n'))",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "# Program Evaluation Results",
    "",
    "```{r policy-results, results='asis'}",
    "if (!is.null(analysis_results$impact_estimates) && params$include_tables) {",
    "  main_table <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'main_results',",
    "    table_style = 'policy'",
    "  )",
    "  ",
    "  if (length(main_table$tables) > 0) {",
    "    kable(main_table$tables$main_results, ",
    "          caption = 'Program Impact Estimates',",
    "          digits = 2)",
    "  }",
    "}",
    "```",
    "",
    "# Recommendations",
    "",
    "```{r recommendations, results='asis'}",
    "cat('## Policy Recommendations\\n\\n')",
    "cat('Based on the evaluation results:\\n\\n')",
    "cat('1. **Continue Program Implementation**: Evidence supports program effectiveness\\n')",
    "cat('2. **Scale Considerations**: Assess capacity for broader implementation\\n')",
    "cat('3. **Target Population**: Focus on groups showing strongest response\\n')",
    "cat('4. **Cost-Benefit Analysis**: Conduct economic evaluation of program costs\\n')",
    "```"
  )
  
  return(content)
}

#' Create Technical Template Content
#' @keywords internal
.create_technical_template <- function(params) {
  
  content <- c(
    "# Technical Documentation",
    "",
    "```{r technical-overview, results='asis'}",
    "cat('## Analysis Pipeline\\n\\n')",
    "cat('This technical report details the complete impact evaluation pipeline implemented using the vecshift package.\\n\\n')",
    "",
    "cat('### Data Processing Steps:\\n\\n')",
    "cat('1. **Event Identification**: Treatment and control group definition\\n')",
    "cat('2. **Matching Procedures**: Propensity score estimation and matching\\n')",
    "cat('3. **Impact Estimation**: Multiple econometric approaches\\n')",
    "cat('4. **Robustness Testing**: Sensitivity analysis and validation\\n')",
    "```",
    "",
    "# Detailed Results",
    "",
    "```{r technical-results, results='asis'}",
    "# Include all available tables and detailed statistics",
    "if (params$include_tables) {",
    "  all_tables <- create_impact_summary_table(",
    "    analysis_results = analysis_results,",
    "    table_type = 'all',",
    "    table_style = 'technical'",
    "  )",
    "  ",
    "  for (table_name in names(all_tables$tables)) {",
    "    cat(paste0('## ', stringr::str_to_title(gsub('_', ' ', table_name)), '\\n\\n'))",
    "    print(kable(all_tables$tables[[table_name]], digits = 4))",
    "    cat('\\n\\n')",
    "  }",
    "}",
    "```",
    "",
    "# Code and Reproducibility",
    "",
    "```{r code-info, results='asis'}",
    "cat('## Reproducibility Information\\n\\n')",
    "cat(paste0('- **vecshift version**: ', utils::packageVersion('vecshift'), '\\n'))",
    "cat(paste0('- **R version**: ', R.version.string, '\\n'))",
    "cat(paste0('- **Analysis date**: ', Sys.Date(), '\\n'))",
    "```"
  )
  
  return(content)
}

#' Create Executive Template Content
#' @keywords internal
.create_executive_template <- function(params) {
  
  content <- c(
    "# Executive Summary",
    "",
    "```{r exec-overview, results='asis'}",
    "cat('## Program Evaluation Overview\\n\\n')",
    "",
    "if (!is.null(analysis_results$event_identification)) {",
    "  event_data <- analysis_results$event_identification",
    "  n_treated <- sum(event_data$is_treated, na.rm = TRUE)",
    "  n_total <- nrow(event_data)",
    "  ",
    "  cat(paste0('**Sample Size**: ', format(n_total, big.mark = ','), ' individuals\\n'))",
    "  cat(paste0('**Treatment Group**: ', format(n_treated, big.mark = ','), ' individuals\\n\\n'))",
    "}",
    "",
    "cat('## Key Results\\n\\n')",
    "",
    "if (!is.null(analysis_results$impact_estimates)) {",
    "  main_results <- analysis_results$impact_estimates",
    "  if (nrow(main_results) > 0) {",
    "    # Focus on most important outcomes",
    "    key_results <- head(main_results[order(-abs(estimate))], 3)",
    "    ",
    "    for (i in 1:nrow(key_results)) {",
    "      result <- key_results[i, ]",
    "      direction <- ifelse(result$estimate > 0, 'increased', 'decreased')",
    "      magnitude <- abs(round(result$estimate * 100, 1))",
    "      significance <- ifelse(result$p_value < 0.05, ' (statistically significant)', '')",
    "      ",
    "      cat(paste0('- **', result$outcome, '**: ', direction, ' by ', magnitude, '%', significance, '\\n'))",
    "    }",
    "  }",
    "}",
    "```",
    "",
    "# Business Impact",
    "",
    "```{r business-impact, results='asis'}",
    "cat('## Economic Implications\\n\\n')",
    "cat('The program demonstrates measurable impact on employment outcomes with potential for significant economic returns.\\n\\n')",
    "",
    "cat('## Strategic Recommendations\\n\\n')",
    "cat('1. **Immediate Actions**: Continue current program implementation\\n')",
    "cat('2. **Scale Strategy**: Develop plan for broader rollout\\n')",
    "cat('3. **Monitoring**: Establish ongoing evaluation framework\\n')",
    "cat('4. **Investment**: Allocate resources for program expansion\\n')",
    "```"
  )
  
  return(content)
}