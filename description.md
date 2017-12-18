## Epi Wizard

### Outline of the App:

Epi Wizard is an interactive app using the newly designed 'Epicalculator' package. This app contains tools specifically designed for epidemiological data analysis. We've thoughtfully included calculators for risk data, person-time data, odds ratio measurements and other common measurement parameters (e.g. Attributable Risk and Population Attributable Risk). The app is designed to output the estimated effects with 95% confidence intervals and provide a graphical comparison of crude and summary estimates. Other applications are available for crude data calculations. More functions such as chi-square hypothesis testing of homogeneity are available in the R package 'Epicalculator'.

&nbsp;

### Audience

Majorly Epidemiology concentrated student, researchers can also use it for some quick calculations.

### Data Input Instruction

For risk calculations, both crude and stratified data can be entered as text separated by commas.
Crude data format
(exposed people with disease, unexposed people with disease, exposed people without disease, unexposed people without disease).
Summary data format is the same, just remember to use a comma to separate data from different stratified table.

For rate calculations, both crude and stratified data can be entered as text separated by commas.
Crude data format:
(exposed people with disease, unexposed people with disease, exposed person-time, unexposed person-time).
Summary data format is the same, just remember to use a comma to separate data from different stratified table.

&nbsp;

### Authors: 

* Sadia Sharmin 
* Ze Zhang 
* Catrina Mueller-Leonhard

&nbsp;

### Contributions:

* Sadia Sharmin - Created the functions for risk ratio and risk difference calculations. Designed the comparison graphs and download functions. Created graph and download functions for risk data panel. Designed the layout of the App.
* Ze Zhang - Created the functions for rate ratio and rate difference calculations. Created graph and download functions for Person-time data panel. Design the structure of the App.
* Catrina Mueller-Leonhard - Created the functions for Odds Ratio, AR, AR%, PAR, PAR% calculations and graphs. Designed the theme of the App.

&nbsp;

#### References:

* Rothman KJ, Greenland S (1998). Modern Epidemiology. Lippincott Williams, & Wilkins, Philadelphia, pp. 271.
* Szklo M, Nieto J (2006). Epidemiology: Beyond the Basics 2nd Edition. Jones & Bartlett Learning, Burlington, MA, Appendix A.

&nbsp;

#### Acknowledgments:

* Inspired from Dr. Gregory Wellenius's spreadsheet-EPI202 Calculator