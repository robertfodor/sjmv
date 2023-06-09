# SJMV

This SJMV repository is a collection of the source code behind web applications designed to assist psychology students and researchers with conducting statistical reporting tasks such as descriptive statistics, linear modelling, and variance analysis. These apps aim to simplify the process of conducting statistical analyses and reporting their results, and provide an alternative to the overly complicated and mostly just annoying SPSS software.

## About the Apps

The SJMV web application has been developed using R and the Shiny framework. All you need to do is upload your data, and customize the settings as needed, and the analysis and plots will be generated for you.

Supported file types are:

- CSV with first row as column names,
- SPSS .sav files,
- and Jamovi .omv files.

## Acknowledgments

I would like to give a nod to jamovi and SPSS, as I studied their outputs and used their software for testing data sets while creating these apps.

## Contact

If you have any questions or feedback, please feel open an issue.

## Known bugs

### Variance analysis:

- Global digit change not reflecting in the tables.
- Plot gets squished if more variables are selected.

## Missing features (to be implemented):

- Outlier detection
- Correlation matrix (zero-order, partial)
- Repeated measures ANOVA
- Exploratory and confirmatory factor analysis
- Cluster analysis
- Q method analysis
