## Resubmission
This is a resubmission, updating the maintainers email address as well as adding an URL for the developmental version as well as an issue tracker.

This resubmission has no changes in the code, the current version on CRAN passes all CRAN package checks.

In addition to the submission earlier today the version number was increased to 0.1.2



## Test environments
* local x86_64, linux-gnu, R version 4.0.2
* win-builder, R-release
* win-builder, R-devel
* Rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* Rhub,	Ubuntu Linux 20.04.1 LTS, R-devel, GCC
* Rhub, Fedora Linux, R-devel, clang, gfortran

## R CMD check results

The package passed CMD check on the local machine as well as stated environments using Rhub, some flagging the change of the maintainers email address (which is the reason for the resubmission). On win-builder (both R-release and R-devel) one additional NOTE (see below) appears. However, I was able to access the URL on my web browser with no errors.

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1163/22941932-00000037
    From: inst/doc/falsering-proportion.html
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
