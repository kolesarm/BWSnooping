## Data as in Abadie and Imbens (2011) and Crump et al (2006, NBER)

## Experimental treated data: Male sub-sample from the National Supported Work
## Demonstration as used by Lalonde in his paper, with observations for which
## RE74 is available
tr <- read.table("http://users.nber.org/~rdehejia/data/nswre74_treated.txt",
                 header=FALSE)
## Non-experimental comparison groups constructed by Lalonde from the PSID
co <- read.table("http://users.nber.org/~rdehejia/data/psid_controls.txt",
                 header=FALSE)

nsw <- rbind(tr, co)
names(nsw) <- c("treatment", "age", "education", "black", "hispanic", "married",
               "nodegree", "re74", "re75", "re78")
## Construct unemployment variables
nsw$ue74 <- nsw$re74==0
nsw$ue75 <- nsw$re75==0

devtools::use_data(nsw, overwrite=TRUE, internal=FALSE)
