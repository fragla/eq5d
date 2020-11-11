## Resubmission
This is a resubmission. In this version I have:

* Updated the pubmed URLs to use the new format.
* Added the trailing '/' to euroqol.org URLs

Tested on Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental)


## Test environments
* local OS X install, R 4.0.3
* Ubuntu 16.04.6 LTS (on travis-ci), R 3.6.3, R 4.0.2, devel
* OS X 10.13.6 (on travis-ci), R 3.6.3, R 4.0.2
* Windows Server 2012 R2 x64 (on appveyor-ci) R 3.6.3, R 4.0.3
* Windows release at win-builder.r-project.org
* Windows devel at win-builder.r-project.org
* Debian Linux, R-devel, GCC, no long double (on R-hub builder)
* Fedora Linux, R-devel, clang, gfortran (on R-hub builder)

## R CMD check results

0 errors | 0 warnings | 1 note

Only on Windows devel at win-builder.r-project.org. No warning on other platforms.

Found the following (possibly) invalid URLs:
  URL: https://fragla.shinyapps.io/shiny-eq5d
    From: inst/doc/eq5d.html
          README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

## Reverse dependencies
	
No reverse dependencies found using revdep_check()
