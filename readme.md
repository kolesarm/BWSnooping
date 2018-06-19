# Critical values adjusted for bandwidth-snooping

The package computes critical values adjusted for bandwidth snooping, derived in
Armstrong and Kolesár (2018, Review of Economic Studies 85 (2))
([arXiv](https://arxiv.org/abs/1412.0267), [published
version](https://doi.org/10.1093/restud/rdx051)). See vignette
`snooping-cv-calculation.pdf` (stored under `inst/doc/`) for description of the
package.

This software pacakge is based upon work supported by the National Science
Foundation under grant numbers SES-1628939 (Armstrong) and SES-1628878
(Kolesár).

## Installation

You can install the package manually by downloading the source code here, or
using the function `install_github()` from the `devtools` package:

```
install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/BWSnooping")
```

## Datasets

The package also contains some datasets used in [Armstrong and Kolesár
(2018)](https://doi.org/10.1093/restud/rdx051) that can be
used to replicate the empirical illustrations in the paper. The datasets are
documented in the package documentation (available once the package is
installed).
