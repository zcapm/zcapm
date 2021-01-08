## Instruction
### Install package
#### via R/Rstudio

Type the following command in the R console: 

* `remotes::install_github("zcapm/zcapm/ZCAPM")`

or with the help of `devtools` package:

1. `install.packages("devtools")`

2. `library(devtools)`
 
3. `install_github("zcapm/zcapm/ZCAPM")`

#### directly download or download via terminal

1. Click `Code` and choose `Download ZIP` and unzip the file. 
   
   Or use the command line `git clone https://github.com/zcapm/zcapm.git` in terminal. 

2. Open the terminal and go to the directory contains the `ZCAPM` folder.

3. Type `R CMD build ZCAPM` to get `ZCAPM_0.1.0.tar.gz`.

4. Type `R CMD INSTALL ZCAPM_0.1.0.tar.gz` or use `install.packages(path_to_the_tar.gz_file, type = "source")` in R.

### Example
The `example.R` file in this repository can be used as an example to illustrate the usage of the pacakge.

<!--
**zcapm/zcapm** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.
-->
