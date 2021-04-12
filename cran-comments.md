## Test environments

* local OS X install, R 4.0.3
* Ubuntu  20.04.2 (using GitHub Actions), R 4.0.5, devel
* Mac OS X 10.15.7 (using GitHub actions) R 4.0.5
* Microsoft Windows Server 2019 (using GitHub Actions), R 4.0.5
* Windows Server 2012 R2 x64 (on appveyor-ci) R 3.6.3, R 4.0.5
* Windows release at win-builder.r-project.org
* Windows devel at win-builder.r-project.org
* Debian Linux, R-devel, GCC, no long double (on R-hub builder)

## R CMD check results

0 errors | 0 warnings | 1 note

Only on Windows release at win-builder.r-project.org. No warning on other platforms.

Found the following (possibly) invalid URLs:
  URL: https://fragla.shinyapps.io/shiny-eq5d
    From: inst/doc/eq5d.html
          README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

## Reverse dependencies
	
No reverse dependencies found using revdep_check()
