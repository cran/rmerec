# rmerec

R Implementation of the MEthod based on the Removal Effects of Criteria - MEREC

Given a decision matrix, the function return the Merec weight´s vector and all intermediate matrixes/vector to calculate it.

More information about the method at https://doi.org/10.3390/sym13040525

More information about the implementation at https://github.com/lucassp/rmerec

## For Use ###

### Install option 1 - from github ###
```
library("devtools");
install_github("lucassp/rmerec");
library("rmerec")
...
```

### Install option 2 - from CRAN ###
```
install.packages("rmerec")
library("rmerec")
...
```

### Calculation sample from DOI´s data ###
```

DOI´s test data

alternatives <- c("A1", "A2", "A3", "A4", "A5")
optimizations <- c("max", "max", "min", "min")

data <- matrix(c(
   c(450, 10, 100, 220, 5),          # criterion 1 values
   c(8000, 9100, 8200, 9300, 8400),  # criterion 2 values
   c(54, 2, 31, 1, 23),              # criterion 3 values
   c(145, 160, 153, 162, 158)        # criterion 4 values
), nrow=5, ncol=4)

result <- merec_weights(data, alternatives, optimizations)
