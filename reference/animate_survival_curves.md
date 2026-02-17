# Create Animated Survival Curves

Creates an memory-efficient animated visualization of survival curves
over time using gganimate. Optimized to handle large datasets without
memory overflow and robust against renderer failures.

## Usage

``` r
animate_survival_curves(
  survival_curves,
  animation_speed = 10,
  save_as = NULL,
  max_frames = 50,
  sample_every = 1,
  width = 800,
  height = 600
)
```

## Arguments

- survival_curves:

  List output from estimate_contract_survival()

- animation_speed:

  Numeric. Speed of animation in fps (default: 10)

- save_as:

  Character. File path to save animation (NULL to display only)

- max_frames:

  Integer. Maximum number of animation frames to reduce memory usage
  (default: 50)

- sample_every:

  Integer. Sample every nth time point to reduce data density (default:
  1)

- width:

  Integer. Animation width in pixels (default: 800)

- height:

  Integer. Animation height in pixels (default: 600)

## Value

An animated ggplot object or saved file path
