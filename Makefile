include config.mk

# Set up directories 
VR_DIR=$(DATA_DIR)/vital_rate
CLIMATE_DIR=$(DATA_DIR)/climate
TEMP_DIR=$(DATA_DIR)/temp_data
ANALYSIS_DIR=$(CODE_DIR)/analysis
FIG_SCRIPTS_DIR=$(ANALYSIS_DIR)/figure_scripts
ARCHIVE_DIR=$(subst .tar.gz,_archive, $(ARCHIVE_FILE))

# Get VR scripts
GET_VR_SRC=$(CODE_DIR)/get_vital_rate_data.R
GET_VR_FUN=$(CODE_DIR)/get_vital_rate_functions.R
GET_VR_EXE=Rscript $(GET_VR_SRC)

# Get climate scripts 
GET_SPOT_SRC=$(CODE_DIR)/climate/get_spot_VWC.R
GET_SPOT_EXE=Rscript $(GET_SPOT_SRC)

# VR files 
VR_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(VR_DIR)/$(spp)_$(vr).csv ))

# Climate files 
SPOT_VWC_FILE=$(CLIMATE_DIR)/spot_VWC.csv



## all		: Fetch data and run analysis
.PHONY : all 
all : fetch_vr_data fetch_climate_data $(TEMP_DIR)/my_plotting_theme.Rdata

## fetch_vr_data	: Fetch all vital rate data for species and vital rates 
.PHONY : fetch_vr_data
fetch_vr_data : $(VR_FILES)

$(VR_FILES) : $(DRIVERS) $(GET_VR_SRC) $(GET_VR_FUN)
	$(GET_VR_EXE) $< $(subst _, , $(notdir $(basename $@)))

## fetch_climate_data	: Fetch all climate data
.PHONY : fetch_climate_data
fetch_climate_data : $(SPOT_VWC_FILE)

$(SPOT_VWC_FILE) : $(DRIVERS) $(GET_SPOT_SRC) $(CLIMATE_DIR)/daily_VWC.csv $(CLIMATE_DIR)/season_table.csv
	$(GET_SPOT_EXE)	$< $(CLIMATE_DIR)

$(TEMP_DIR)/my_plotting_theme.Rdata : $(FIG_SCRIPTS_DIR)/save_plot_theme.R
	Rscript $<

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
	rm -f $(SPOT_VWC_FILE)
	rm -f $(TEMP_DIR)/my_plotting_theme.Rdata
	
## variables	: Print variables.
.PHONY : variables
variables : Makefile 
	@echo DRIVERS: $(DRIVERS)
	@echo SPP_LIST: $(SPP_LIST)
	@echo VR_LIST: $(VR_LIST)
	@echo DATA_DIR: $(DATA_DIR)
	@echo CODE_DIR: $(CODE_DIR)
	@echo VR_DIR: $(VR_DIR)
	@echo VR_FILES: $(VR_FILES)
	@echo GET_VR_SRC: $(GET_VR_SRC)
	@echo GET_VR_FUN: $(GET_VR_FUN)
	@echo ARCHIVE_FILE: $(ARCHIVE_FILE)
	@echo ARCHIVE_DIR: $(ARCHIVE_DIR)
	@echo CLIMATE_DIR: $(CLIMATE_DIR)
	@echo FIG_SCRIPTS_DIR: $(FIG_SCRIPTS_DIR)
	@echo ANALYSIS_DIR: $(ANALYSIS_DIR)
	@echo TEMP_DIR: $(TEMP_DIR)
	
## help		: Help Menu
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
