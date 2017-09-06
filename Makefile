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

# VR files 
VR_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(VR_DIR)/$(spp)_$(vr).csv ))

# Climate files
CLIMATE_FILES= $(SPOT_VWC_FILE) $(DAILY_SW_TREAT_FILE)
SPOT_VWC_FILE=$(CLIMATE_DIR)/spot_VWC.csv
DAILY_SW_TREAT_FILE=$(CLIMATE_DIR)/daily_swVWC_treatments.csv

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
fetch_climate_data : $(DAILY_SW_TREAT_FILE)

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
	rm -rf $(ARCHIVE_DIR)
	rm -f $(ARCHIVE_FILE)
	rm -f $(CLIMATE_FILES)
	rm -f $(PLOT_THEME_FILE)
		
## variables	: Print variables.
.PHONY : variables
variables : Makefile 
	@$(foreach V,$(sort $(.VARIABLES)),$(if $(filter-out environment% default automatic,$(origin $V)),$(warning $V=$($V) ($(value $V)))))
	
## help		: Help Menu
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
