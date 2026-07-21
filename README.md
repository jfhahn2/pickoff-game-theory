
# Strategies under a pickoff limit in Major League Baseball: A zero-sum sequential game with multilevel models

Scott Powers, Sivaramakrishnan Ramani, Jacob Hahn and Andrew J Schaefer
[preprint](https://arxiv.org/abs/2601.15608)

2024 Cascadia Symposium on Statistics in Sports
[slides](https://drive.google.com/file/d/17jveBX0U5hDRP7KH2GtvgGVATdS8wI7Q),
[video](https://www.youtube.com/watch?v=oOvvNnDCD5Y&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

2024 Saberseminar
[video](https://www.youtube.com/watch?v=sVQ-b1lW8nQ&list=PL40KH8fsrt-sX1lSf659bl1u341F76ue3)

In this project, we use game theoretic concepts to analyze the effects of the 2023 MLB rule changes surrounding baserunning. Specifically, we were interested in how the rule limiting pitchers to two disengagements per plate appearance affects the strategies chosen by both teams when a runner reaches first base. There has been very little analysis of leadoff and pickoff decisions, so we wanted to determine optimal strategies for these situations and calculate the value that could be gained here. We came up with a simple rule of thumb that works well in most situations: with each disengagement, runners should extend their leadoff by about two feet in order to maximize the run expectancy for their team.

## Installation

```
devtools::install_github("jfhahn2/pickoff-game-theory/package/pickoffgame")
```

Or, if you'd like to install your local version of the package:
```
devtools::install("package/pickoffgame")
```

## Reproducing Results

We are not at liberty to share the raw pitch-by-pitch lead distance data, but all other data are publicly available, and we provide code to reproduce results beyond the point of estimating the GLMMs for runner outcome probabilities. Unfortunately, parametric bootstrapping for the GLMMs would require the raw lead distance data, so bootstrap standard errors are not publicly reproducible.

To download the publicly available data we use from the MLB Stats API, run:
```
Rscript scripts/download_data.R         # this should take about 20 minutes
```

You will need to obtain the following files from where we host them (TBD):
```
input/data/arm_strength/2022.csv
input/data/arm_strength/2023.csv
input/data/batter_event.csv
input/data/batter_pitch.csv
input/data/sprint_speed/2022.csv
input/data/sprint_speed/2023.csv
output/models/fit_runner_outcome.rds
```

To run the analysis pipeline from the point after GLMM estimation, run:
```
Rscript scripts/estimate_models.R       # this should take about 10 minutes
```

After running the analysis pipeline, you can produce the results from the paper using:
```
Rscript scripts/generate_results.R      # this should take less than 1 minute
```
Both scripts write their results to the `output` folder.

## Folder Structure

```
├── articles                            # LaTeX code for papers and slides
│   └── arxiv
├── input                               # data go here
├── output                              # models, figures, tables go here
└── scripts                             # R code for reproducing results
    ├── sandbox
    ├── bootstrap_results.R
    ├── estimate_models.R
    └── generate_results.R
```
