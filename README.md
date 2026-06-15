# SDM_MinimumEvaluationPoints
Scripts and data used to derive the analysis of the master thesis "Establishing the Minimum Number of Evaluation Points Needed for Species Distribution Modeling Assessment", written by Marc Kevin Schneider for the Department of Geography of the Philipps-University Marburg. 

## Abstract of the study
Robust evaluation of Species Distribution Modeling (SDM) requires sufficient evaluation data, yet minimum evaluation dataset size requirements remain poorly understood compared to those for model training. Using virtual species (VS), artificial distribution maps (ADMs) and a novel breakpoint analysis method, we investigated minimum evaluation sample size requirements across eight evaluation metrics, ten sampling strategies, three niche breadths and nine levels of simulated model performance for a presence-absence (PA) and two presence-only (PO) datasets within a study area in southeastern Australia. 

We found that for PA evaluation, sample sizes of $n$ = 25-60 were sufficient across metrics and experimental conditions, consistent with previous recommendations. Balanced PO evaluation produced similar thresholds ($n$ = 25-80), with larger variation between the evaluation metrics. Identified thresholds for random background sampling were highly variable, underlining the idea that evaluation for this data type should be avoided due to class imbalance, wherever possible. Contrary to expectations, neither niche breadth nor simulated model performance produced consistent trends on threshold requirements. In contrast, thresholds differed markedly between sampling strategies, especially for spatially structured sampling strategies, that generally required more evaluation points than others. 

These results show that SDM evaluation imposes its own sample size requirements that should be considered independently from model training requirements. We therefore recommend a sample size of at least 60 to 100 presence points for ecological studies, with at least 30-60 for PA and 60-80 for balanced PO evaluation, with the rest being used for model training. We also heavily discourage the use of a large random background sample for model evaluation, in line with previous research. The findings presented here highlight the importance of choosing an appropriate evaluation design and sampling strategy for ecological studies and further reinforce that class imbalance should be accounted for in SDM evaluation. 

## Structure of the repository:
- `data`: Basic data for the virtual species and ADMs
- `src`: Scripts for the analysis

## Structure of the data-folder:
- `ADM`: Artificial Distribution Maps (ADM) .tif files created by applying a gaussian random field with gradually decreasing levels of autocorrelation to the presence-absence raster of each virtual species. Ranges from 0.1 (low autocorrelation) to 0.9 (high autocorrelation) and simulates model performance from poor- to good-fit models.
- `VirtualSpecies`: Ten virtual species (VS) .RDS files which were adapted from [Grimmet et al. 2020](https://doi.org/10.1016/j.ecolmodel.2020.109194). Of these ten VS, VS1-3 show a large-sized, VS4-6 show a medium-sized and VS7-10 show a narrow-sized distribution.
- `paRaster`: Presence-Absence distribution .tif files of the ten VS, extracted from the .RDS files of each VS. 

## Structure of the src-folder:
- `functions`: Functions for the main scripts. Contains the ten sampling strategies, as well as the breakpoint detection function and the evaluation functions.
- `prep`: Preparation scripts. Mostly downloads or formats data, together with the overview maps of the study area and virtual species.
- `00_setup_project.R`: Sets the packages and folder structure.
- `01_artificial_distribution_maps.R`: Creates the ADMs for the analysis.
- `02_sampling_presence_points.R`: Samples the presence-absence and background points from the virtual species and ADMs.
- `03_evaluation.R`: Evaluates the individual sampling runs using eight evaluation metrics (AUC, TSS, Kappa, Pearson's Correlation, RMSE, MAE, Jaccard's Similarity, Sorensen's Similarity)-
- `04_analysis.R`: Evaluates the minimum evaluation size thresolds for different experimental conditions and saves the results as boxplots and .csv files.

## Copyright
You are free to download and use the contents of this repository, as long as you credit this repository and the author "Marc Kevin Schneider".

## Questions regarding this repository
For questions regarding the analysis or this repository, please contact me by writing a ticket or by sending me a mail under Schnei7q@students.uni-marburg.de. I will try to respond as soon as possible.
