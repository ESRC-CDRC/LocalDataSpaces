# Readme

Code to reproduce the excess mortality reports. Specifically:

* load_clean_mortality.R - Load and clean the monthly deaths registration data
* calc_excess_mortality.R - Load and clean historical (2015-2019) deaths registration data to estimate 'excess' mortality when compared to above file
* create_excess_mortality_reports - will produce a report for each Local Authority, using the template supplied in excess_mortality_reports.Rmd
* excess_mortality_reports.Rmd - template for reports
