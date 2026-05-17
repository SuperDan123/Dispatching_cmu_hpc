
## Files

### `main/`
  - Download data `00_get_data/`
    - `get_aggregate_data.R`: Download Census data
    - `get_meshdata.py`: Download mesh data

  - Data construction `01_data_construction/`
    - `data_construction.sh`: Run all files below
    - `make_rename_files_old_data.py`: Make do files for making variable names
    - `read_data_old.do`: Read raw data (in csv) and save them as dta files
    - `clean_data_aggregate.R`: Data cleaning for Census data
    - `clean_data_establishment.R`: Data cleaning for establishment data

  - Make additional variables `02_data_construction_additional/`
    - `data_construction_additional.sh`: Run all files below
    - `municipal_merger_history.R`: Summarize municipal mergers after 2005
    - `update_commuting_zone.R`: Update 2005 commuting zones to take into account mergers after 2005
    - `adjacent_municipalities.R`: Identify adjacent municipalities using meshdata
    - `count_number_of_platforms.R`: Count the number of establishments in commuting zones
    - `create_IV.R`: Make differentiation IV
    - `generate_variables_establishment.R`: Generate establishment-level variables
    - `generate_variables_aggregate.R`: Generate commuting zone-level variables
    
  - Descriptive & Reduced form analysis `03_estimation_reduced_form/`
    - `estimate_reduced_form.sh`: Run the files below and run Rmd files related to these
    - `make_summary_statistics.R`: Make summary stat tables for the draft
    - `estimate_competition_impact.R`: Estimate the competition impact
    - `estimate_demand_reduced_form.R`: Estimate the reduced-form demand functions

  - Estimation of structural model `04_estimation_structural/`
    - `estimation_structural.sh`: Run the files below and run Rmd files related to these
    - `estimate_matching_function.R`: Estimate the matching function
    - `estimate_model_using_small_markets.R`: Estimate the preference parameters and the cost parameters
    - `evaluate_fit.R`: Simulate the model to check the model fit
  
  - Counterfactual analysis `05_counterfactuals/`
    - `counterfactuals.R`: Run the files below and run Rmd files related to these
    - `conduct_relevant_market_test.R`: Conduct relevant market test
    - `simulate_counterfactuals_hypothetical_market.R`: Conduct counterfactual simulations using a hypothetical market
    - `simulate_counterfactuals_actual_market.R`: Conduct counterfactual simulations using actual markets

### `report/`
  - Data construction `01_data_construction/`
    - `data_codebook.Rmd`: Report the descriptive statistics of variables in the raw data

  - Reduced form analysis `03_estimation_reduced_form/`
    - `estimate_competition_impact.Rmd`: Report the estimation result of the competition impact
    - `estimate_demand_reduced_form_first_stage.Rmd`: Report the first stage estimation result of the reduced-form demand function
    - `estimate_demand_reduced_form.Rmd`: Report the estimation result of the reduced-form demand function

  - Estimation of structural model `04_estimation_structural/`
    - `estimate_matching_function.Rmd`: Report the estimation results of the matching function
    - `estimate_structural_parameters.Rmd`: Report the estimation results of the preference parameters and the cost parameters
    - `evaluate_fit.Rmd`: Report the model fit
  
  - Counterfactual analysis `05_counterfactuals/`
    - `simulate_margin_constraint.Rmd`: Report the result of the margin simulation
    - `simulate_minimum_wage.Rmd`: Report the result of the minimum wage simulation
    - `simulate_monopoly.Rmd`: Report the result of the monopoly simulation
    - `simulate_competition_impact.Rmd`: Report the result of the simulation of competition impact
    
  - Others `06_others/`
    - `compare_algorithms.Rmd`: Report the foc errors under several solution algorithms

### `R/`
  - `equilibrium.R`: Define equilibrium class
  - `competition_regression.R`: Define functions for coefficient plot
  - `gmm_estimation.R`: Define functions for the estimation of structural parameters
  - `post_estimation.R`: Define functions used for summarizing the estimation results
  - `model_fit.R`: Define functions used for drawing figures of model fit
  - `simulation.R`: Define functions for solving pricing equilibrium
  - `conterfactuals.R`: Define functions for counterfactuals
  
### `src/`
  - `functions.cpp`: Define cpp version of some functions
  
### `tests/`
  - `test_equilibrium.R`: Test functions in `R/equilibrium.R` and related functions in `src/functions.cpp`
  - `test_estimation.R`: Test functions in `R/estimation.R` and related functions in `src/functions.cpp`
  - `test_post_estimation.R`: Test functions in `R/post_estimation.R` and related functions in `src/functions.cpp`
  - `test_simulation.R`: Test functions in `R/simulation.R` and related functions in `src/functions.cpp`
  - `test_counterfactual.R`: Test functions in `R/counterfactuals.R` and related functions in `src/functions.cpp`
  
  
## Replication
  - (Not necessary) Run `bash main/00_get_data/get_data.sh` on a terminal
    - You need to get application ID and change the variable `appID` in the code `main/01_data_construction/get_aggregate_data.R` appropriately. (cf. https://www.e-stat.go.jp/api/api-info/api-guide). 
  - Run `bash main/01_data_construction/data_construction.sh` on a terminal
    - You need to be able to run a stata file on a terminal.
  - Run `bash main/02_data_construction_additional/data_construction_additional.sh` on a terminal
  - Run `bash main/03_estimation_reduced_form/estimation_reduced_form.sh`
  - Run `bash main/04_estimation_structural/estimation_structural.sh` on a terminal
  - Run `bash main/05_counterfactuals/counterfactuals.sh` on a terminal
