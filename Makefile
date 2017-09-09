include config.mk

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
GET_SPOT_SRC=$(CODE_DIR)/climate/get_spot_VWC.R
GET_SPOT_EXE=Rscript $(GET_SPOT_SRC)
VWC_TREAT_SRC=$(CODE_DIR)/climate/find_VWC_treatment_effects.R
VWC_TREAT_EXE=Rscript $(VWC_TREAT_SRC)
AGGREGATE_VWC_SRC=$(CODE_DIR)/climate/aggregate_VWC_data.R
AGGREGATE_VWC_EXE=Rscript $(AGGREGATE_VWC_SRC)
MAKE_CLIM_VARS_SRC=$(CODE_DIR)/climate/make_climate_variables.R
MAKE_CLIM_VARS_EXE=Rscript $(MAKE_CLIM_VARS_SRC)
PREP_CLIM_VARS_SRC=$(CODE_DIR)/climate/prepare_climate_covariates.R
PREP_CLIM_VARS_EXE=Rscript $(PREP_CLIM_VARS_SRC)

# Climate files
SPOT_VWC_FILE=$(CLIMATE_DIR)/spot_VWC.csv
DAILY_SW_TREAT_FILE=$(CLIMATE_DIR)/daily_SOILWAT_VWC_treatments.csv
VWC_FILES=$(CLIMATE_DIR)/quarterly_VWC.csv $(CLIMATE_DIR)/seasonal_VWC.csv $(CLIMATE_DIR)/annual_VWC.csv
CLIM_FILES=$(CLIMATE_DIR)/quarterly_climate.csv $(CLIMATE_DIR)/seasonal_climate.csv $(CLIMATE_DIR)/monthly_climate.csv $(CLIMATE_DIR)/annual_climate.csv
PREPPED_CLIM_FILE=$(CLIMATE_DIR)/prepped_clim_vars.csv
ALL_CLIM_FILES= $(SPOT_VWC_FILE) $(DAILY_SW_TREAT_FILE) $(VWC_FILES) $(CLIM_FILES) $(PREPPED_CLIM_FILE)

# Stan data files 
PREP_DF_SRC=$(CODE_DIR)/prep_vital_rate_df.R
PREP_DF_FUNS=$(CODE_DIR)/prep_vital_rate_df_functions.R
PREP_DF_EXE=Rscript $(PREP_DF_SRC)
PREP_STAN_DAT_SRC=$(CODE_DIR)/prep_stan_datalist.R
PREP_STAN_DAT_EXE=Rscript $(PREP_STAN_DAT_SRC)

PREP_DF_FILES=$(wildcard $(TEMP_DIR)/*_dataframe.RDS)
STAN_DAT_FILES=$(wildcard $(TEMP_DIR)/*_datalist_for_stan.RDS)

.PRECIOUS: $(VR_DIR)/%.csv $(TEMP_DIR)/%_dataframe.RDS
STAN_DAT_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(TEMP_DIR)/$(spp)_$(vr)_datalist_for_stan.RDS ))

# Plot files 
PLOT_THEME_FILE=$(FIG_DIR)/my_plotting_theme.Rdata
PLOT_THEME_SRC=$(FIG_SCRIPTS_DIR)/save_plot_theme.R
PLOT_THEME_EXE=Rscript $(PLOT_THEME_SRC)

# Helper functions and variables, these are used for substitutions 
growthsurvival=growth survival
recruitment=recruitment
growth=growthsurvival
survival=growthsurvival
space:=
space+=

join-with = $(subst $(space),$1,$(strip $2))
sep_name = $(subst _, ,$(notdir $1))

# Link growth_survival_dataframe.RDS to dependencies growth.csv and survival.csv 
FIND_VR_FILES = $(strip $(foreach vr, $($(word 2, $(call sep_name, $1))), $(VR_DIR)/$(word 1, $(call sep_name, $1))_$(vr).csv))

# Link growth_datalist_stan.RDS and survival_datalist_stan.RDS to growth_survival_dataframe.RDS
MN = $(TEMP_DIR)/$(call join-with,_, $(word 1, $(call sep_name, $1)) $($(word 2, $(call sep_name, $1))) dataframe.RDS)

## all		: Fetch data and run analysis
.PHONY: all 
all: $(STAN_DAT_FILES)

## Make stan data files 
.SECONDEXPANSION:
$(TEMP_DIR)/%_datalist_for_stan.RDS: $$(call MN, $$@)
	$(PREP_STAN_DAT_EXE) $(call sep_name, $*) $(TEMP_DIR)

$(TEMP_DIR)/%_dataframe.RDS: $(PREPPED_CLIM_FILE) $$(call FIND_VR_FILES, $$*) $(PREP_DF_SRC)
	$(PREP_DF_EXE) $(filter-out $(lastword $^), $^)

$(VR_DIR)/%.csv: $(DRIVERS) $(GET_VR_SRC) $(GET_VR_FUN)
	$(GET_VR_EXE) $< $(subst _, , $*)

## fetch_climate_data	: Fetch all climate data
.PHONY: fetch_climate_data
fetch_climate_data: $(PREPPED_CLIM_FILE)

$(PREPPED_CLIM_FILE): $(CLIMATE_DIR)/seasonal_climate.csv $(CLIMATE_DIR)/seasonal_VWC.csv $(PREP_CLIM_VARS_SRC)
	$(PREP_CLIM_VARS_EXE) $(wordlist 1,2, $^) 

$(CLIM_FILES): $(DRIVERS) $(SEASON_FILE) $(PLOT_THEME_FILE) $(MAKE_CLIM_VARS_SRC) 
	$(MAKE_CLIM_VARS_EXE) $(wordlist 1,3,$^)
	
$(VWC_FILES): $(DAILY_SW_TREAT_FILE) $(PLOT_THEME_FILE) $(AGGREGATE_VWC_SRC) 
	$(AGGREGATE_VWC_EXE) $(wordlist 1,2, $^)
	
$(DAILY_SW_TREAT_FILE): $(VWC_TREAT_SRC) $(DRIVERS) $(SEASON_FILE) $(DAILY_VWC_FILE) $(SPOT_VWC_FILE) $(PLOT_THEME_FILE)
	./$< $(wordlist 2, $(words $^),$^)

$(SPOT_VWC_FILE): $(DRIVERS) $(SEASON_FILE) $(DAILY_VWC_FILE) $(GET_SPOT_SRC)
	$(GET_SPOT_EXE) $(wordlist 1,3, $^)

$(PLOT_THEME_FILE): $(PLOT_THEME_SRC)
	$(PLOT_THEME_EXE)

## archive 		: make tar.gz archive of project
.PHONY: archive 
archive: $(ARCHIVE_FILE)

$(ARCHIVE_FILE): $(ARCHIVE_DIR)
	tar -czf $@ $<
	
$(ARCHIVE_DIR): LICENSE README.md Makefile config.mk $(CODE_DIR) $(DATA_DIR)
	mkdir -p $@
	cp -r $^ $@
	touch $@
	
## clean		: Remove temporary files 
.PHONY: clean
clean:
	rm -f $(VR_DIR)/*
	rm -f $(ALL_CLIM_FILES)	
	rm -rf $(ARCHIVE_DIR)
	rm -f $(ARCHIVE_FILE)
	rm -f $(PLOT_THEME_FILE)
	rm -f $(TEMP_DIR)/*
	
## variables	: Print variables.
.PHONY: variables
variables: Makefile 
	@$(foreach V,$(sort $(.VARIABLES)),$(if $(filter-out environment% default automatic,$(origin $V)),$(warning $V=$($V) ($(value $V)))))
	
## help		: Help Menu
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<
