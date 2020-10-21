### Instruction
#### Install package
* via R/Rstudio

`install.packages("devtools")

 library(devtools)
 
 install_github("zcapm/zcapm/ZCAPM")`

* directly download or download via terminal

1. Click `Code` and choose `Download ZIP` and unzip the file. Or use the command line `git clone https://github.com/zcapm/zcapm.git` in terminal. 

3. Open the terminal and go to the directory contains the `ZCAPM` folder

4. Type `R CMD build ZCAPM` to get `ZCAPM_0.1.0.tar.gz`

5. Type `R CMD INSTALL ZCAPM_0.1.0.tar.gz` or 

5". In R, type `install.packages(path_to_the_tar.gz_file, type = "source")`

#### Example
The `example.R` file in this repository can be used as an example to illustrate the usage of the pacakge.

<!--
**zcapm/zcapm** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.
-->
