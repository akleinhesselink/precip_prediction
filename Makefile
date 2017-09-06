include config.mk

VR_DIR=$(DATA_DIR)/vital_rate
GET_VR_SRC=$(CODE_DIR)/get_vital_rate_data.R
GET_VR_FUN=$(CODE_DIR)/get_vital_rate_functions.R
GET_VR_EXE=Rscript $(GET_VR_SRC)
ARCHIVE_DIR=$(subst .tar.gz,_archive, $(ARCHIVE_FILE))

VR_FILES:=$(foreach spp, $(SPP_LIST), $(foreach vr, $(VR_LIST), $(VR_DIR)/$(spp)_$(vr).csv ))

## all		: Fetch data and run analysis
.PHONY : all 
all : fetch_vr_data

## fetch_vr_data	: Fetch all vital rate data for species and vital rates 
.PHONY : fetch_vr_data
fetch_vr_data : $(VR_FILES)
	
$(VR_FILES) : $(DRIVERS) $(GET_VR_SRC) $(GET_VR_FUN)
	$(GET_VR_EXE) $< $(subst _, , $(notdir $(basename $@)))

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
	
## help		: Help Menu
.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
