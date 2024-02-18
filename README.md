# CovidCorrelations

In this work, correlations in the pandemic world are calculated between deaths from Covid-19 and ABO/Rh groups. Initial results from this code are shown in the SSRN article: http://dx.doi.org/10.2139/ssrn.3794044.

From version v0.0 onwards, data is automatically downloaded from the Our World in Data website: https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data .csv, in the current version (v0.3) there is the option to use already saved data.

The code was implemented in R, the main files are TemporalAnalysis_owd.R and TemporalAnalysis_owd_log.R.

A new structure was implemented, where the R/ folder contains source code, Data/: data on blood types and number of deaths and others from the owid website and the folder Tests/ contain results, configurings files to simulating and scripts to installs R dependencies.

The configuration file is conf-options.R, you should check it before running.

The results are saved in the directory: R/Results-cumulative-(analysis date).

Refactoring and other advancements have been made, including using weekly data from 2024 owid-covid-data.

Future work: studies of derivatives/variations based on temporal evolution are under analysis.

