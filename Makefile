PROG_DIR=$(shell pwd)
TMP=$(PROG_DIR)/tmp

PROG=srs-qloop

all: copy build clean

copy:
	@echo "make tmp directory..."
	@mkdir -p $(TMP)
	@echo "copy relevant files for the build to $(TMP)"
	@cp -R $(PROG_DIR)/CLI $(TMP)
	@cp -R $(PROG_DIR)/SRS $(TMP)
	@cp -R $(PROG_DIR)/Encode $(TMP)
	@cp -R $(PROG_DIR)/Pretty $(TMP)
	@cp $(PROG_DIR)/Main.hs $(TMP)
	@echo "done"

clean:
	@echo "clean..."
	@rm -rf $(TMP)
	@echo "done"

build:
	@echo "compile qenc"
	@cd $(TMP) && ghc -o $(PROG) -O2 $(TMP)/Main.hs
	@cp $(TMP)/$(PROG) $(PROG_DIR)
	@echo "done"
