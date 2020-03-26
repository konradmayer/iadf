## Resubmission
This is an updated version of the package. In this version
 - changes necessary due to upcoming backwards incompatible changes in dependency `dplyr 1.0.0` are introduced
 - formatting issues are fixed

## Test environments
* local x86_64, linux-gnu, R version 3.6.1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (Rhub)
* Ubuntu Linux 16.04 LTS, R-release, GCC (Rhub)
* Ubuntu Linux 16.04 LTS, R-devel, GCC (Rhub)
* Fedora Linux, R-devel, clang, gfortran (Rhub)
* win-builder, R-release
* win-builder, R-devel

## R CMD check results
The package passed CMD check on the local machine as well as stated environments using Rhub. On win-builder (both R-release and R-devel) one NOTE (see below) appears. However, I was able to access the URL on my web browser with no errors.


Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1163/22941932-00000037
    From: inst/doc/falsering-proportion.html
    Status: Error
    Message: libcurl error code 35:
      	error:1407742E:SSL routines:SSL23_GET_SERVER_HELLO:tlsv1 alert protocol version

# Reverse dependencies
No reverse dependencies for packages on CRAN found.
