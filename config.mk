# set driversdata directory 
DRIVERS=$(HOME)/driversdata 

SPP_LIST:=ARTR HECO POSE PSSP
VR_LIST:=growth survival recruitment
VR_DF_LIST:=growth_survival recruitment

# project directory
DATA_DIR=data
CODE_DIR=code
FIG_DIR=figures
ARCHIVE_FILE=precip_prediction.tar.gz

# climate files
DAILY_VWC_FILE=$(CLIMATE_DIR)/daily_VWC.csv
SEASON_FILE=$(CLIMATE_DIR)/season_table.csv
SW_DATA=$(DATA_DIR)/SW_files/sw_output.RData

# Set up directories 
VR_DIR=$(DATA_DIR)/vital_rate
CLIMATE_DIR=$(DATA_DIR)/climate
TEMP_DIR=$(DATA_DIR)/temp_data
ANALYSIS_DIR=$(CODE_DIR)/analysis
FIG_SCRIPTS_DIR=$(ANALYSIS_DIR)/figure_scripts
ARCHIVE_DIR=$(subst .tar.gz,_archive, $(ARCHIVE_FILE))

# Get vital rate scripts
GET_VR_SRC=$(CODE_DIR)/get_vital_rate_data.R
GET_VR_FUN=$(CODE_DIR)/get_vital_rate_functions.R
GET_VR_EXE=Rscript $(GET_VR_SRC)

# Get climate scripts 
EXTRACT_SW_SRC=$(CODE_DIR)/climate/ExtractData_3Runs.R
GET_SPOT_SRC=$(CODE_DIR)/climate/get_spot_VWC.R
VWC_TREAT_SRC=$(CODE_DIR)/climate/find_VWC_treatment_effects.R
AGGREGATE_VWC_SRC=$(CODE_DIR)/climate/aggregate_VWC_data.R
MAKE_CLIM_VARS_SRC=$(CODE_DIR)/climate/make_climate_variables.R
PREP_CLIM_VARS_SRC=$(CODE_DIR)/climate/prepare_climate_covariates.R

# Climate files
SPOT_VWC_FILE=$(CLIMATE_DIR)/spot_VWC.csv
DAILY_SW_TREAT_FILE=$(CLIMATE_DIR)/daily_SOILWAT_VWC_treatments.csv
VWC_FILES=$(CLIMATE_DIR)/quarterly_VWC.csv $(CLIMATE_DIR)/seasonal_VWC.csv $(CLIMATE_DIR)/annual_VWC.csv
CLIM_FILES=$(CLIMATE_DIR)/quarterly_climate.csv $(CLIMATE_DIR)/seasonal_climate.csv $(CLIMATE_DIR)/monthly_climate.csv $(CLIMATE_DIR)/annual_climate.csv
PREPPED_CLIM_FILE=$(CLIMATE_DIR)/prepped_clim_vars.csv
ALL_CLIM_FILES= $(SPOT_VWC_FILE) $(DAILY_SW_TREAT_FILE) $(VWC_FILES) $(CLIM_FILES) $(PREPPED_CLIM_FILE) $(DAILY_VWC_FILE)

# Stan data files 
PREP_DF_SRC=$(CODE_DIR)/prep_vital_rate_df.R
PREP_DF_FUNS=$(CODE_DIR)/prep_vital_rate_df_functions.R
PREP_STAN_DAT_SRC=$(CODE_DIR)/prep_stan_datalist.R
PREP_STAN_FUNS=$(CODE_DIR)/prep_stan_datalist_functions.R

# Plot files 
PLOT_THEME_FILE=$(FIG_DIR)/my_plotting_theme.Rdata
PLOT_THEME_SRC=$(FIG_SCRIPTS_DIR)/save_plot_theme.R

