include config.mk

VR_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(VR_DIR)/$(spp)_$(vr).csv ))

## all		: Fetch data and run analysis


## fetch_vr_data	: fetch all vital rate data for species and vital rates 
.PHONY : fetch_vr_data
fetch_vr_data : $(VR_FILES)
	
$(VR_FILES) : $(GET_VR_SRC) $(DRIVERS)
	$(GET_VR_EXE) $(DRIVERS) $(subst _, , $(notdir $(basename $@)))

## clean		: Remove temporary files 
.PHONY : clean
clean :
	rm -f $(VR_DIR)/*

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

## help		: Help Menu
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
