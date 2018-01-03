include config.mk

CLIM_FIGS:=$(FIG_DIR)/VWC_spot_measurements.png
COVER_CHANGE_FIG=$(FIG_DIR)/start_to_finish_cover_change.png

PLOT_SPRING_VWC_SRC=$(CODE_DIR)/analysis/figure_scripts/plot_spring_soil_moisture_spot_measures.R
PLOT_TREAT_TRENDS_SRC=$(CODE_DIR)/analysis/treatment_trends_precip.R

#PREP_DF_FILES=$(wildcard $(TEMP_DIR)/*_dataframe.RDS)
#STAN_DAT_FILES=$(wildcard $(TEMP_DIR)/*_datalist_for_stan.RDS)


.PRECIOUS: $(VR_DIR)/%.csv $(TEMP_DIR)/%_dataframe.RDS
PREP_DF_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_DF_LIST), $(TEMP_DIR)/$(spp)_$(vr)_dataframe.RDS ))

# Helper functions and variables, these are used for substitutions 
growth_survival=growth survival
recruitment=recruitment
growth=growth survival
survival=growth survival
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
all: $(PREP_DF_FILES)

## plot_clim : generate climate figures 
.PHONY: plot_clim
plot_clim: $(CLIM_FIGS)

$(COVER_CHANGE_FIG): $(PLOT_TREAT_TRENDS_SRC) $(DRIVERS) $(PLOT_THEME_FILE)
	./$< 
	
$(FIG_DIR)/VWC_spot_measurements.png: $(PLOT_SPRING_VWC_SRC) $(DRIVERS) $(PLOT_THEME_FILE)
	./$<
 
.SECONDEXPANSION:
#$(TEMP_DIR)/%_datalist_for_stan.RDS: $(PREP_STAN_DAT_SRC) $$(call MN, $$@) $(PREP_STAN_FUNS)
#	./$< $(call sep_name, $*) $(TEMP_DIR)

$(TEMP_DIR)/%_dataframe.RDS: $(PREP_DF_SRC) $(PREPPED_CLIM_FILE) $$(call FIND_VR_FILES, $$*)	$(PREP_DF_FUNS)
	./$(filter-out $(lastword $^), $^)

$(VR_DIR)/%.csv: $(DRIVERS) $(GET_VR_SRC) $(GET_VR_FUN)
	./$(GET_VR_SRC) $< $(subst _, , $*)

$(PREPPED_CLIM_FILE): $(CLIMATE_DIR)/seasonal_climate.csv $(CLIMATE_DIR)/seasonal_VWC.csv $(PREP_CLIM_VARS_SRC)
	./$(PREP_CLIM_VARS_SRC) $(wordlist 1,2, $^) 

$(CLIM_FILES): $(DRIVERS) $(SEASON_FILE) $(PLOT_THEME_FILE) $(MAKE_CLIM_VARS_SRC) 
	./$(MAKE_CLIM_VARS_SRC) $(wordlist 1,3,$^)
	
$(VWC_FILES): $(DAILY_SW_TREAT_FILE) $(PLOT_THEME_FILE) $(AGGREGATE_VWC_SRC) 
	./$(AGGREGATE_VWC_SRC) $(wordlist 1,2, $^)
	
$(DAILY_SW_TREAT_FILE): $(VWC_TREAT_SRC) $(DRIVERS) $(SEASON_FILE) $(DAILY_VWC_FILE) $(SPOT_VWC_FILE) $(PLOT_THEME_FILE)
	./$< $(wordlist 2, $(words $^),$^)

$(SPOT_VWC_FILE): $(DRIVERS) $(SEASON_FILE) $(DAILY_VWC_FILE) $(GET_SPOT_SRC)
	./$(GET_SPOT_SRC) $(wordlist 1,3, $^)

$(DAILY_VWC_FILE): $(EXTRACT_SW_SRC) $(SW_DATA)
	./$^ $(DAILY_VWC_FILE)

$(PLOT_THEME_FILE): $(PLOT_THEME_SRC)
	./$(PLOT_THEME_SRC)

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
	rm -rf $(TEMP_DIR)/*
	
## variables	: Print variables.
.PHONY: variables
variables: Makefile 
	@$(foreach V,$(sort $(.VARIABLES)),$(if $(filter-out environment% default automatic,$(origin $V)),$(warning $V=$($V) ($(value $V)))))
	
## help		: Help Menu
.PHONY: help
help: Makefile
	@sed -n 's/^##//p' $<
