# Volcan-Barva-Functional-Diversity README
Abstract: Globally, tropical rain forests comprise some of the most diverse and functionally rich ecosystems but are increasingly degraded by human impacts. Protected areas have been shown to conserve species diversity, but their effectiveness at maintaining functional diversity over time is less well known, despite the fact that functional diversity likely reveals more ecological information than taxonomic diversity. By extension, the degree to which species loss decreases functional diversity within protected areas is also unknown; functional redundancy may buffer communities from loss of functional diversity from some local extinctions. Using eight years of camera trap data, we quantified annual functional dispersion of the large mammal community in the Volcán Barva region of Costa Rica and tested for changes in functional dispersion over time in response to environmental and anthropogenic predictors. We quantified functional redundancy based on simulated declines in functional dispersion with species loss. Observed functional dispersion did not change significantly over time and was not associated with measured environmental or anthropogenic predictors. Quantitative modeling of observed functional traits over time did not identify significant changes. We did however find qualitative trends in relative trait proportions, which could be indicative of functional change in the future. We found high functional redundancy, with average simulated functional dispersion declining significantly only after 9 out of 21 large mammal species were lost from the community. We cautiously suggest that protected tropical rain forests can conserve functional diversity over the course of a decade even in heavily fragmented landscapes.

Link to paper: https://onlinelibrary.wiley.com/doi/full/10.1111/btp.12844?casa_token=mzfqsZ7mF_sAAAAA%3AjTieYi7NasCXnDLGZFHu3Fak22S2W_pOZRUHMEbDtUBLAB8wPcsAcUk4i1lddYpDgVCPKq_qpAyRGlN8

List and description of all code and data files included in the repository:
Code:
Functional Diversity VB.R- Provides functions and code for calculating functional diversity and functional redundancy metrics, as well as all linear models and analysis used in the study
Linear Modeling.R- Provides code for all linear models in the study plus additional models of the effect of environmental variables on individual oridnal traits
Figures.R- Provides code for all figures included in the manuscript

Data:
Ordinal trait analysis.csv- Trait and environmental data, and occupnacy estimates, formated for clm ordinal regression in Functional Diversity VB.R and Linear Modeling.R
VB Ordered Categories.csv- Species trait data, described as continuous variables for body mass and average litter size, and categorical for diet, social group, habitat and activity period. Body mass is log transformed. Citations for the data can be found in the supplementary materials. Used in Functional Diversity VB.R
VB.Occurrence.csv- Median occupancy estimates for each species in each year of the study, obtained from Beaudrot et al. 2016. Used in Function Diversity VB.R
VBAnalysis_Ordered.csv- File containing all annual environmental variable measurements at Volcan Barva, including some not used in the study. Climate data were obtained from the NASA Power dataset; Forest loss data are from the Global Forest Cover (GFC) database and processed using Fragstats; further details can be foudn in the manuscript. File also contains mean (for continuous traits) and mode (for categorical) trait values for each year.

License: NA


