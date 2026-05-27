pkgname <- "TruncatedPCQM"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "TruncatedPCQM-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('TruncatedPCQM')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("adjusted_moments")
### * adjusted_moments

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: adjusted_moments
### Title: Adjusted Moment-based Estimators for Right-censored PCQM Data
### Aliases: adjusted_moments
### Keywords: density estimation moments pcqm

### ** Examples


distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000,    7.655136,   13.815876,   10.423496,
     6.094721,    4.135461,    7.732912,    5.454545,
  9999.000000, 9999.000000,   14.787289,   15.670821,
  9999.000000,    9.825537,   11.611850,   15.757861,
  9999.000000,    9.670381,   14.055394,   17.075678,
    11.529219,    4.464136,    7.793114,   11.309553,
    13.307828,    5.864490,   13.309636,    5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000,   18.201084, 9999.000000,
  9999.000000,    7.809056,   12.612496,    5.601366,
     9.201294, 9999.000000,    8.353524,    9.683701,
     6.592604,   19.117869,   19.758384,   12.923507,
    15.574824,   10.643719,    9.494539,    7.382031,
     9.143077, 9999.000000,   15.551414,    5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278,    7.279454,    9.385355,    5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000,   11.853695,   14.405252
), nrow = 20, ncol = 4, byrow = TRUE)


moment_result <- adjusted_moments(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored"
)

moment_result





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("adjusted_moments", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("csr_mle")
### * csr_mle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: csr_mle
### Title: CSR-based Maximum Likelihood Estimator for Right-censored PCQM
###   Data
### Aliases: csr_mle
### Keywords: CSR density estimation pcqm

### ** Examples


distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000,    7.655136,   13.815876,   10.423496,
     6.094721,    4.135461,    7.732912,    5.454545,
  9999.000000, 9999.000000,   14.787289,   15.670821,
  9999.000000,    9.825537,   11.611850,   15.757861,
  9999.000000,    9.670381,   14.055394,   17.075678,
    11.529219,    4.464136,    7.793114,   11.309553,
    13.307828,    5.864490,   13.309636,    5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000,   18.201084, 9999.000000,
  9999.000000,    7.809056,   12.612496,    5.601366,
     9.201294, 9999.000000,    8.353524,    9.683701,
     6.592604,   19.117869,   19.758384,   12.923507,
    15.574824,   10.643719,    9.494539,    7.382031,
     9.143077, 9999.000000,   15.551414,    5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278,    7.279454,    9.385355,    5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000,   11.853695,   14.405252
), nrow = 20, ncol = 4, byrow = TRUE)

csr_result <- csr_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  lambda_lower = 1e-4,
  lambda_upper = 1
)

csr_result






base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("csr_mle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nbd_mle")
### * nbd_mle

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nbd_mle
### Title: NBD-based Maximum Likelihood Estimator for Right-censored PCQM
###   Data
### Aliases: nbd_mle
### Keywords: NBD density estimation pcqm

### ** Examples


distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000,    7.655136,   13.815876,   10.423496,
     6.094721,    4.135461,    7.732912,    5.454545,
  9999.000000, 9999.000000,   14.787289,   15.670821,
  9999.000000,    9.825537,   11.611850,   15.757861,
  9999.000000,    9.670381,   14.055394,   17.075678,
    11.529219,    4.464136,    7.793114,   11.309553,
    13.307828,    5.864490,   13.309636,    5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000,   18.201084, 9999.000000,
  9999.000000,    7.809056,   12.612496,    5.601366,
     9.201294, 9999.000000,    8.353524,    9.683701,
     6.592604,   19.117869,   19.758384,   12.923507,
    15.574824,   10.643719,    9.494539,    7.382031,
     9.143077, 9999.000000,   15.551414,    5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278,    7.279454,    9.385355,    5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000,   11.853695,   14.405252
), nrow = 20, ncol = 4, byrow = TRUE)

nbd_result <- nbd_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored",
  lambda_lower = 1e-4,
  lambda_upper = 10
)

nbd_result









base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nbd_mle", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("print")
### * print

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: print
### Title: Print Methods for PCQM Density Estimation Objects
### Aliases: print print.pcqm_csr_mle print.pcqm_moments print.pcqm_nbd_mle
### Keywords: CSR NBD density estimation moments pcqm

### ** Examples

distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000,    7.655136,   13.815876,   10.423496,
     6.094721,    4.135461,    7.732912,    5.454545,
  9999.000000, 9999.000000,   14.787289,   15.670821,
  9999.000000,    9.825537,   11.611850,   15.757861,
  9999.000000,    9.670381,   14.055394,   17.075678,
    11.529219,    4.464136,    7.793114,   11.309553,
    13.307828,    5.864490,   13.309636,    5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000,   18.201084, 9999.000000,
  9999.000000,    7.809056,   12.612496,    5.601366,
     9.201294, 9999.000000,    8.353524,    9.683701,
     6.592604,   19.117869,   19.758384,   12.923507,
    15.574824,   10.643719,    9.494539,    7.382031,
     9.143077, 9999.000000,   15.551414,    5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278,    7.279454,    9.385355,    5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000,   11.853695,   14.405252
), nrow = 20, ncol = 4, byrow = TRUE)

# CSR-based MLE
csr_result <- csr_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  lambda_lower = 1e-4,
  lambda_upper = 1
)
print(csr_result)

# Adjusted Moment Estimators
moment_result <- adjusted_moments(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored"
)
print(moment_result)

# NBD-based MLE
nbd_result <- nbd_mle(
  distances = distances_matrix,
  C = 20,
  q = 4,
  l = 2,
  init_method = "Pollard_censored",
  lambda_lower = 1e-4,
  lambda_upper = 10
)
print(nbd_result)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("print", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("summary")
### * summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: summary
### Title: Summary Methods for PCQM Density Estimation Objects
### Aliases: summary summary.pcqm_csr_mle print.summary.pcqm_csr_mle
###   summary.pcqm_moments print.summary.pcqm_moments summary.pcqm_nbd_mle
###   print.summary.pcqm_nbd_mle
### Keywords: CSR NBD density estimation moments pcqm

### ** Examples

distances_matrix <- matrix(c(
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000,    7.655136,   13.815876,   10.423496,
     6.094721,    4.135461,    7.732912,    5.454545,
  9999.000000, 9999.000000,   14.787289,   15.670821,
  9999.000000,    9.825537,   11.611850,   15.757861,
  9999.000000,    9.670381,   14.055394,   17.075678,
    11.529219,    4.464136,    7.793114,   11.309553,
    13.307828,    5.864490,   13.309636,    5.897720,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000,   18.201084, 9999.000000,
  9999.000000,    7.809056,   12.612496,    5.601366,
     9.201294, 9999.000000,    8.353524,    9.683701,
     6.592604,   19.117869,   19.758384,   12.923507,
    15.574824,   10.643719,    9.494539,    7.382031,
     9.143077, 9999.000000,   15.551414,    5.266916,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    18.604278,    7.279454,    9.385355,    5.573127,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
  9999.000000, 9999.000000, 9999.000000, 9999.000000,
    11.980811, 9999.000000,   11.853695,   14.405252
), nrow = 20, ncol = 4, byrow = TRUE)

# CSR-based MLE Summary
csr_result <- csr_mle(distances = distances_matrix, C = 20, q = 4, l = 2,
                      lambda_lower = 1e-4, lambda_upper = 1)
summary(csr_result)

# Adjusted Moment Estimators Summary
moment_result <- adjusted_moments(distances = distances_matrix, C = 20, q = 4, l = 2,
                                  init_method = "Pollard_censored")
summary(moment_result)

# NBD-based MLE Summary
nbd_result <- nbd_mle(distances = distances_matrix, C = 20, q = 4, l = 2,
                      init_method = "Pollard_censored", lambda_lower = 1e-4, lambda_upper = 10)
summary(nbd_result)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
