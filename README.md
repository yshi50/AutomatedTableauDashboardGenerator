# GTableauPipeline
> Version: 2.0

A package for automating Tableau Dashboard generation.

## Quick Start

### Installation
```
# Input Your Personal Access Token Here

Sys.setenv(GITHUB_PAT = "")

devtools::install_github("yshi50/AutomatedTableauDashboardGenerator", ref = 'main')
```
### Help
Help document is available through `?GTableauPipeline` and `?Template`.

## Tutorial
***Warning:***

***All data presented here are entirely simulated and generated through various randomization techniques, including simple randomization, block randomization, stratified randomization, and covariate-adaptive randomization. These data are purely fictional and have been created solely for demonstration purposes.***
>### TemplateApp
#### About
This function launches a Shiny App to provide a user-friendly GUI.
#### Function
```
TemplateApp()
```
#### Example
```
library(GTableauPipeline)

TemplateApp()
```
#### Dependency
This function is also an application of the Molecular Data Warehouse, **no longer available**.

Through this integration, the pipeline combines the powerful dashboard capabilities of Tableau with the data warehouse, achieving optimal performance.

>### Template
#### About
This function automatically generates a Tableau dashboard from a template dashboard.
#### Function
```
Template(abk_0, adsl_0 = NA, definition_0 = NA, save_path = getwd(),
                    UID = "SUBJID", study_name = NA, special_name = NA, about = NA, URL = NA,
                    distinct_threshold = 4, log_fold_change = NA,
                    variable_numerical = NA, variable_categorical = NA, variable_keep = c("COUNTRY"),
                    generate_correlation = FALSE,
                    debug_mode = TRUE, save_RDS = FALSE, trash_can = FALSE, connection = "Live",
                    reference_line = NA, note = "No Message", shiny = FALSE)
```
#### Parameter
`abk_0`: ABK Dataframe, or Path (SAS or CSV).

`adsl_0`: ADSL Dataframe, or Path (SAS or CSV), Some Situations Allow for Optional.

`save_path`: Path to Output Dashboard, Default `getwd()`.

`UID`: Unique ID to Identify Subjects Shared Between ABK and ADSL, Optional, Default "SUBJID".

`study_name`: Study Name, Optional.

`special_name`: Special Name to Rename Dashboard, Optional.

`about`: Study Information, Optional.

`URL`: Website URL, 'http' Not Supported, Optional.

`log_fold_change`: Log-n Fold Change.

`reference_line`: Default Reference Line Value, Optional.

`note`: Creator Message.


#### Example

```
library(GTableauPipeline)

Template(abk_0 = system.file("Simulation/Simulated ABK.csv", package = "GTableauPipeline"),
         adsl_0 = system.file("Simulation/Simulated ADSL.csv", package = "GTableauPipeline"))
```

## Screenshot
>### TemplateApp
![App 1](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/App%201.png)
>### Template
![Page 1](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/1.png)
![Page 2](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/2.png)
![Page 3](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/3.png)
![Page 4](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/4.png)
![Page 5](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/5.png)
![Page 6](https://github.com/yshi50/AutomatedTableauDashboardGenerator/blob/main/shot/6.png)
