# Tests for Vecshift Theme System
# File: tests/testthat/test-theme-vecshift.R

test_that("vecshift_colors returns correct palette structures", {
  # Test main palette
  main_colors <- vecshift_colors("main")
  expect_type(main_colors, "character")
  expect_true(length(main_colors) > 0)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", main_colors)))
  
  # Test desaturated palette
  desat_colors <- vecshift_colors("desaturated")
  expect_type(desat_colors, "character")
  expect_equal(length(desat_colors), length(main_colors))
  
  # Test black and white palette
  bw_colors <- vecshift_colors("bw")
  expect_type(bw_colors, "character")
  expect_true(length(bw_colors) > 0)
  
  # Test employment palette (should have names)
  emp_colors <- vecshift_colors("employment")
  expect_type(emp_colors, "character")
  expect_true(!is.null(names(emp_colors)))
  expect_true("disoccupato" %in% names(emp_colors))
  expect_true("occ_ft" %in% names(emp_colors))
  expect_true("occ_pt" %in% names(emp_colors))
})

test_that("vecshift_colors handles parameters correctly", {
  # Test n parameter
  colors_5 <- vecshift_colors("main", n = 5)
  expect_length(colors_5, 5)
  
  # Test reverse parameter
  colors_normal <- vecshift_colors("main", n = 3)
  colors_reversed <- vecshift_colors("main", n = 3, reverse = TRUE)
  expect_equal(colors_normal, rev(colors_reversed))
  
  # Test alpha parameter
  colors_alpha <- vecshift_colors("main", n = 3, alpha = 0.5)
  expect_true(all(nchar(colors_alpha) == 9)) # Should have alpha channel
  
  # Test invalid alpha
  expect_error(vecshift_colors("main", alpha = -0.1))
  expect_error(vecshift_colors("main", alpha = 1.1))
})

test_that("vecshift_colors validates palette names", {
  # Test valid palettes
  expect_no_error(vecshift_colors("main"))
  expect_no_error(vecshift_colors("desaturated"))
  expect_no_error(vecshift_colors("bw"))
  expect_no_error(vecshift_colors("employment"))
  expect_no_error(vecshift_colors("contracts"))
  expect_no_error(vecshift_colors("transitions"))
  
  # Test invalid palette
  expect_error(vecshift_colors("invalid_palette"))
})

test_that("get_employment_colors works correctly", {
  # Test all employment colors
  all_emp <- get_employment_colors()
  expect_type(all_emp, "character")
  expect_true(!is.null(names(all_emp)))
  
  # Test specific statuses
  specific <- get_employment_colors(c("occ_ft", "occ_pt", "disoccupato"))
  expect_length(specific, 3)
  expect_equal(names(specific), c("occ_ft", "occ_pt", "disoccupato"))
  
  # Test with alpha
  alpha_colors <- get_employment_colors(c("occ_ft", "occ_pt"), alpha = 0.8)
  expect_true(all(nchar(alpha_colors) == 9))
  
  # Test warning for invalid statuses
  expect_warning(get_employment_colors(c("occ_ft", "invalid_status")))
})

test_that("accessibility testing functions work", {
  # Test basic accessibility testing (without ggplot2 dependency)
  skip_if_not_installed("ggplot2")
  
  # This should run without error
  expect_no_error(test_vecshift_accessibility("main", return_data = TRUE))
  
  # Should return list structure
  results <- test_vecshift_accessibility("main", return_data = TRUE)
  expect_type(results, "list")
  expect_true(length(results) > 0)
  
  # Each result should have required fields
  expect_true(all(c("color", "name", "luminance", "white_contrast", "black_contrast") %in% 
                 names(results[[1]])))
})

test_that("color validation works correctly", {
  # Test that all main palette colors are valid hex
  main_colors <- vecshift_colors("main")
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", main_colors)))
  
  # Test employment colors have expected structure
  emp_colors <- vecshift_colors("employment")
  expected_names <- c("disoccupato", "occ_ft", "occ_pt", "over_ft_ft", 
                     "over_pt_pt", "over_ft_pt", "transition", "unknown")
  expect_true(all(expected_names %in% names(emp_colors)))
})

# Only run ggplot2-dependent tests if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  
  test_that("theme_vecshift creates valid ggplot2 theme", {
    skip_if_not_installed("ggplot2")
    
    # Basic theme creation
    theme_obj <- theme_vecshift()
    expect_s3_class(theme_obj, "theme")
    
    # Test with different parameters
    expect_no_error(theme_vecshift(base_size = 14))
    expect_no_error(theme_vecshift(grid = "none"))
    expect_no_error(theme_vecshift(axis = "x"))
    expect_no_error(theme_vecshift(ticks = "none"))
    
    # Test parameter validation
    expect_error(theme_vecshift(grid = "invalid"))
    expect_error(theme_vecshift(axis = "invalid"))
    expect_error(theme_vecshift(ticks = "invalid"))
  })
  
  test_that("scale functions work correctly", {
    skip_if_not_installed("ggplot2")
    
    # Test discrete color scale
    scale_color <- scale_color_vecshift("main")
    expect_s3_class(scale_color, "Scale")
    
    # Test fill scale
    scale_fill <- scale_fill_vecshift("employment")
    expect_s3_class(scale_fill, "Scale")
    
    # Test continuous scale
    scale_continuous <- scale_color_vecshift("main", discrete = FALSE)
    expect_s3_class(scale_continuous, "Scale")
    
    # Test British spelling
    expect_no_error(scale_colour_vecshift("main"))
  })
  
  test_that("preview function creates valid plot", {
    skip_if_not_installed("ggplot2")
    
    # Test basic preview
    p <- preview_vecshift_colors("main")
    expect_s3_class(p, "ggplot")
    
    # Test with different options
    expect_no_error(preview_vecshift_colors("employment", show_names = TRUE))
    expect_no_error(preview_vecshift_colors("main", n = 5, show_hex = FALSE))
  })
  
  test_that("theme setting functions work", {
    skip_if_not_installed("ggplot2")
    
    # Store original theme
    original_theme <- ggplot2::theme_get()
    
    # Test setting vecshift theme
    expect_message(set_vecshift_theme(), "Vecshift theme set as default")
    
    # Test reset
    expect_message(reset_default_theme(), "Reset to default")
    
    # Restore original theme
    ggplot2::theme_set(original_theme)
  })
  
} else {
  
  test_that("ggplot2 functions fail gracefully without ggplot2", {
    # Mock the requireNamespace to return FALSE
    with_mocked_bindings(
      requireNamespace = function(pkg, ...) FALSE,
      {
        expect_error(theme_vecshift(), "Package 'ggplot2' is required")
        expect_error(scale_color_vecshift(), "Package 'ggplot2' is required")
        expect_error(preview_vecshift_colors(), "Package 'ggplot2' is required")
        expect_error(set_vecshift_theme(), "Package 'ggplot2' is required")
        expect_error(reset_default_theme(), "Package 'ggplot2' is required")
      }
    )
  })
}