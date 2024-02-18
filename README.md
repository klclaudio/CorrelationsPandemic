# CovidCorrelations
#### DOI: [10.5281/zenodo.5898679](https://zenodo.org/doi/10.5281/zenodo.5898679)
Correlations in pandemic world are calculated between Covid-19 deaths and ABO groups. The initial results from this code are shown in SSRN DOI: [10.2139/ssrn.3794044](http://dx.doi.org/10.2139/ssrn.3794044).

Since version v0.0 has automatic data file downloaded from  Our World in Data: <https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv> , in this current version there is option to use saved data.

The code was implemented in R, the main files are TemporalAnalysis_owd.R and TemporalAnalysis_owd_log.R.

A new structure was implemented, where the R/ folder contains the source code, Data/ contains country data on blood types, number of deaths and others, the Tests/ folder contains compressed files with the first results (08/2020) and the latest results from 02/2024, configuration files for simulations and scripts for installing dependencies.

Configuring file is conf-options.R, should see it before run.

The results are saved in the directory: R/Results-cumulative-(date of analysis).

Refactoring and others advances have been done, using weekly data from 2024 owid-covid-data.

Future works: studies of derivatives from temporal evolution of coefficients.
