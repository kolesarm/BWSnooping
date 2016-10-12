# Critical values adjusted for bandwidth-snooping

The package computes critical values adjusted for bandwidth snooping, derived in
Armstrong and Kolesár (2016, arXiv: 1412.0267). See vignette
`snooping-cv-calculation.pdf` (stored under `inst/doc/`) for description of the
package.

## Installation

You can install the package manually by downloading the source code here, or
using the function `install_github()` from the `devtools` package:

```
install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/BWSnooping")
```

## Datasets

The package also contains some datasets used in Armstrong and Kolesár (2016)
that can be used to replicated the empirical illustrations in the paper. The
datasets are documented in the package documentation (available once the package
is installed).
