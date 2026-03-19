
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TruncatedPCQM

**TruncatedPCQM** provides a systematic methodology for estimating
population density from point-centered quarter method (PCQM) surveys
when distance measurements are truncated by a maximum search radius
(right-censored). The package implements both completely randomly
distributed (Poisson) and spatially aggregated (Negative Binomial)
models.

## Installation

You can install the development version of TruncatedPCQM from
[GitHub](https://github.com/rcjhrpyt-droid/TruncatedPCQM) with:

``` r
# install.packages("devtools")
devtools::install_github("rcjhrpyt-droid/TruncatedPCQM")
```

## Quick Start

``` r
library(TruncatedPCQM)
# Create example PCQM distance matrix
distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 7.655136, 13.815876, 10.423496,
     6.094721, 4.135461, 7.732912, 5.454545,
  9999.000000, 9999.000000, 14.787289, 15.670821,
  9999.000000, 9.825537, 11.611850, 15.757861,
  9999.000000, 9.670381, 14.055394, 17.075678,
    11.529219, 4.464136, 7.793114, 11.309553,
    13.307828, 5.864490, 13.309636, 5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 18.201084, 9999.000000,
  9999.000000, 7.809056, 12.612496, 5.601366,
     9.201294, 9999.000000, 8.353524, 9.683701,
     6.592604, 19.117869, 19.758384, 12.923507,
    15.574824, 10.643719, 9.494539, 7.382031,
     9.143077, 9999.000000, 15.551414, 5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278, 7.279454, 9.385355, 5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000, 11.853695, 14.405252
), nrow = 20, ncol = 4, byrow = TRUE)
# Negative Binomial model (spatially aggregated populations)
nbd_result <- nbd_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored",
  lambda_lower = 1e-4,
  lambda_upper = 10
)
# CSR model (randomly distributed populations)
csr_result <- csr_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  lambda_lower = 1e-4,
  lambda_upper = 1
)
# Multiple moment-based estimators
moment_result <- adjusted_moments(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored"
)
# Display results
print(nbd_result)
#> NBD-based PCQM density estimation (MLE)
#> --------------------------------------------------
#> Estimated density (lambda): 0.014366 
#> Aggregation parameter (k): 0.702610 
#> Number of focal points: 20.0000 
#> Censored sectors: 32 (40.0%)
print(csr_result)
#> CSR-based PCQM density estimation (MLE)
#> --------------------------------------------------
#> Estimated density (lambda): 0.007840 
#> Number of focal points: 20.0000 
#> Total sectors: 80 
#> Censored sectors: 32 (40.0%)
print(moment_result)
#> Adjusted Moment Estimators for PCQM Density
#> ===========================================
#> Number of focal points: 20 
#> Total sectors: 80 
#> Censored sectors: 32 (40.0%) 
#> 
#> Density Estimators:
#> -------------------
#> Cottam_censored:    0.009915 
#> Pollard_censored:   0.010331 
#> Shen_censored:      0.011546 
#> Morisita_censored:  0.011723 
#> 
#> Aggregation Parameter:
#> ----------------------
#> k_hat:  3.721403
```

## Main Functions

### `nbd_mle()`: Negative Binomial Distribution MLE

Estimates population density and aggregation parameter under the
Negative Binomial model, accounting for right-censored distances.

### `csr_mle()`: Complete Spatial Randomness MLE

Estimates population density under the Poisson (CSR) model for
right-censored PCQM data.

### `adjusted_moments()`: Adjusted Moment Estimators

Computes multiple moment-based density estimators (DK, Cottam, Pollard,
Shen, Morisita) for right-censored PCQM data.

## Data Input Formats

``` r
# Matrix format (each row = sampling point, each column = sector)
set.seed(123)
matrix_data <- matrix(runif(80, 5, 15), nrow = 20, ncol = 4)
# Data frame format
df_data <- as.data.frame(matrix_data)
# Vector format (auto-reshaped by q parameter)
vector_data <- as.vector(t(matrix_data))
# All formats should give similar results
res1 <- adjusted_moments(matrix_data, C = 20, q = 4, l = 2)
res2 <- adjusted_moments(df_data, C = 20, q = 4, l = 2)
res3 <- adjusted_moments(vector_data, C = 20, q = 4, l = 2)
cat("Matrix result:", res1$Cottam_censored, "\n")
#> Matrix result: 0.01994273
cat("Data frame result:", res2$Cottam_censored, "\n")
#> Data frame result: 0.01994273
cat("Vector result:", res3$Cottam_censored, "\n")
#> Vector result: 0.01994273
```

## Citation

If you use TruncatedPCQM in your research, please cite:

``` r
@article{,
  title   = {Population Density Estimators for Right-Censored Distance Sampling},
  author  = {Huang, Wenzhe and Shen, Guochun and Xing, Dingliang and Zhao, Jiangyan},
  year    = {2026},
  journal = {arXiv preprint arXiv:2603.08276},
  url     = {https://arxiv.org/abs/2603.08276}
}

@software{,
  title = {TruncatedPCQM: Density Estimation for Point-Centered Quarter Method with Truncated Sampling},
  author = {Huang, Wenzhe and Shen, Guochun and Xing, Dingliang and Zhao, Jiangyan},
  year = {2026},
  url = {https://github.com/rcjhrpyt-droid/TruncatedPCQM}
}
```

## License

This package is licensed under GPL (≥ 3). See the [LICENSE](LICENSE)
file for details.

## Support

- **Issues**: [GitHub
  Issues](https://github.com/rcjhrpyt-droid/TruncatedPCQM/issues)
- **Email**: <51280155097@stu.ecnu.edu.cn>
- **Documentation**: Run `?function_name` in R for detailed help

------------------------------------------------------------------------
