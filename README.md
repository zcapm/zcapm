## Instruction
### Requirements
* RcppArmadillo: g++ family at least version 4.6.* is required.

### Install package
#### Via R/Rstudio

* Type the following command in the R console: 

*   `remotes::install_github("zcapm/zcapm/ZCAPM")`

or with the help of `devtools` package:

* `install.packages("devtools")`

* `library(devtools)`
 
* `install_github("zcapm/zcapm/ZCAPM")`

#### Directly download or download via terminal

1. Click `Code` and choose `Download ZIP` and unzip the file. 
   
   Or use the command line `git clone https://github.com/zcapm/zcapm.git` in terminal. 

2. For Mac system, open the terminal and go to the directory contains the `ZCAPM` folder. 

* Type `R CMD build ZCAPM` to get `ZCAPM_0.1.0.tar.gz`.

* Type `R CMD INSTALL ZCAPM_0.1.0.tar.gz` or use `install.packages(path_to_the_tar.gz_file, type = "source")` in R.

2'. For a windows user, installation can be done with the help of `devtools`. 


### Example
The `example.R` file in this repository can be used as an example to illustrate the usage of the pacakge.

<!--
**zcapm/zcapm** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.
-->
