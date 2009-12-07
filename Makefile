TARGETS = $(BEAMS)

APP_NAME=memcached_client
VERSION=0.0.1

SOURCE_DIR=src
TEST_DIR=test
EBIN_DIR=ebin
INCLUDE_DIR=include
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
BEAMS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
ERLC_FLAGS=+warn_unused_vars \
           +warn_unused_import \
           +warn_shadow_vars \
           -Wall \
           -W \
           -v \
           +debug_info \
           +bin_opt_info

TARBALL_NAME=$(APP_NAME)-$(VERSION)
DIST_TMP_DIR=tmp
DIST_TARGET=$(DIST_TMP_DIR)/$(TARBALL_NAME)

all: $(TARGETS)

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	erlc -pa $(EBIN_DIR) $(ERLC_FLAGS) -I$(INCLUDE_DIR) -o$(EBIN_DIR) $<

check: all
	@erl -pa `pwd`/ebin -eval 'io:format("~p", [ct:run_test([{auto_compile, true}, {dir, "./test"}, {logdir, "./log"}, {refresh_logs, "./log"}, {cover, "./src/coverspec"}])]).' -s init stop

test: check

install: all install_dirs
	@[ -n "$(TARGET_DIR)" ] || (echo "Please set TARGET_DIR. e.g. /usr/local/mcbench"; false)
	@[ -n "$(SBIN_DIR)" ] || (echo "Please set SBIN_DIR. e.g. /usr/sbin/"; false)
	mkdir -p $(TARGET_DIR)
	cp -rp ebin include $(TARGET_DIR)
	for script in mcbench mcbench-env; do \
		chmod 0755 scripts/$$scripts; \
		cp -p scripts/$$script $(TARGET_DIR)/sbin; \
		[ -e $(SBIN_DIR)/$$script ] || ln -s $(TARGET_DIR)/sbin/$$script $(SBIN_DIR)/$$script; \
	done

install_dirs:
	mkdir -p $(SBIN_DIR)
	mkdir -p $(TARGET_DIR)/sbin

dist: dist-clean
	mkdir $(DIST_TARGET)
	cp -r Makefile ebin src include scripts README test $(DIST_TARGET)
	chmod 0755 $(DIST_TARGET)/scripts/*
	tar -zcf $(TARBALL_NAME).tar.gz $(DIST_TARGET)
	rm -rf $(DIST_TMP_DIR)

distclean: clean
	rm -f $(LOG_PREFIX)*
	rm -f *.dump
	find . -regex '.*\(~\|#\|\.swp\|\.dump\)' -exec rm {} \;

clean:
	rm -f $(TARGETS) $(TARBALL_NAME).tar.gz
	rm -f test/*.beam
	rm -rf log/ct_run*
