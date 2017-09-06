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
VWC_TREAT_SRC=$(CODE_DIR)/climate/soilMoistureTreatmentEffects.R
VWC_TREAT_EXE=Rscript $(VWC_TREAT_SRC)
AGGREGATE_VWC_SRC=$(CODE_DIR)/climate/aggregate_VWC_data.R
AGGREGATE_VWC_EXE=Rscript $(AGGREGATE_VWC_SRC)
MAKE_CLIM_VARS_SRC=$(CODE_DIR)/climate/make_climate_variables.R
MAKE_CLIM_VARS_EXE=Rscript $(MAKE_CLIM_VARS_SRC)
PREP_CLIM_VARS_SRC=$(CODE_DIR)/climate/prepare_climate_covariates.R
PREP_CLIM_VARS_EXE=Rscript $(PREP_CLIM_VARS_SRC)

# VR files 
VR_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(VR_DIR)/$(spp)_$(vr).csv ))

# Climate files
SPOT_VWC_FILE=$(CLIMATE_DIR)/spot_VWC.csv
DAILY_SW_TREAT_FILE=$(CLIMATE_DIR)/daily_swVWC_treatments.csv
VWC_FILES=$(CLIMATE_DIR)/quarterly_VWC.csv $(CLIMATE_DIR)/seasonal_VWC.csv $(CLIMATE_DIR)/annual_VWC.csv
CLIM_FILES=$(CLIMATE_DIR)/quarterly_climate.csv $(CLIMATE_DIR)/seasonal_climate.csv $(CLIMATE_DIR)/monthly_climate.csv $(CLIMATE_DIR)/annual_climate.csv
PREPPED_CLIM_FILE=$(CLIMATE_DIR)/prepped_clim_vars.csv
ALL_CLIM_FILES= $(SPOT_VWC_FILE) $(DAILY_SW_TREAT_FILE) $(VWC_FILES) $(CLIM_FILES) $(PREPPED_CLIM_FILE)


# Plot files 
PLOT_THEME_FILE=$(FIG_DIR)/my_plotting_theme.Rdata
PLOT_THEME_SRC=$(FIG_SCRIPTS_DIR)/save_plot_theme.R
PLOT_THEME_EXE=Rscript $(PLOT_THEME_SRC)

## all		: Fetch data and run analysis
.PHONY : all 
all : fetch_vr_data fetch_climate_data

## fetch_vr_data	: Fetch all vital rate data for species and vital rates 
.PHONY : fetch_vr_data
fetch_vr_data : $(VR_FILES)

$(VR_FILES) : $(DRIVERS) $(GET_VR_SRC) $(GET_VR_FUN)
	$(GET_VR_EXE) $< $(subst _, , $(notdir $(basename $@)))

## fetch_climate_data	: Fetch all climate data
.PHONY : fetch_climate_data
fetch_climate_data : $(PREPPED_CLIM_FILE)

$(PREPPED_CLIM_FILE) : $(PREP_CLIM_VARS_SRC) $(VWC_FILES) $(CLIM_FILES)
	$(PREP_CLIM_VARS_EXE) $(CLIMATE_DIR)
	
$(CLIM_FILES) : $(MAKE_CLIM_VARS_SRC) $(DRIVERS) $(SEASON_FILE) $(PLOT_THEME_FILE)
	$(MAKE_CLIM_VARS_EXE) $(DRIVERS) $(CLIMATE_DIR)
	
$(VWC_FILES) : $(AGGREGATE_VWC_SRC) $(PLOT_THEME_FILE) $(DAILY_SW_TREAT_FILE)
	$(AGGREGATE_VWC_EXE) $(CLIMATE_DIR)
	
$(DAILY_SW_TREAT_FILE) : $(DRIVERS) $(VWC_TREAT_SRC) $(SPOT_VWC_FILE) $(DAILY_VWC_FILE) $(SEASON_FILE) $(PLOT_THEME_FILE)
	$(VWC_TREAT_EXE) $< $(CLIMATE_DIR)

$(SPOT_VWC_FILE) : $(DRIVERS) $(GET_SPOT_SRC) $(DAILY_VWC_FILE) $(SEASON_FILE)
	$(GET_SPOT_EXE)	$< $(CLIMATE_DIR)

$(PLOT_THEME_FILE) : $(PLOT_THEME_SRC)
	$(PLOT_THEME_EXE)

## archive 		: make tar.gz archive of project
.PHONY : archive 
archive : $(ARCHIVE_FILE)

$(ARCHIVE_FILE) : $(ARCHIVE_DIR)
	tar -czf $@ $<
	
$(ARCHIVE_DIR) : LICENSE README.md Makefile config.mk $(CODE_DIR) $(DATA_DIR)
	mkdir -p $@
	cp -r $^ $@
	touch $@
	
## clean		: Remove temporary files 
.PHONY : clean
clean :
	rm -f $(VR_DIR)/*
	rm -f $(ALL_CLIM_FILES)	
	rm -rf $(ARCHIVE_DIR)
	rm -f $(ARCHIVE_FILE)
	rm -f $(PLOT_THEME_FILE)
		
## variables	: Print variables.
.PHONY : variables
variables : Makefile 
	@$(foreach V,$(sort $(.VARIABLES)),$(if $(filter-out environment% default automatic,$(origin $V)),$(warning $V=$($V) ($(value $V)))))
	
## help		: Help Menu
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
