TARGETS = $(BEAMS)

APP_NAME=memcached-client
VERSION=0.0.1

SOURCE_DIR=src
LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
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
PID_FILE1=/tmp/memcached-erlang1.pid
PID_FILE2=/tmp/memcached-erlang2.pid

all: $(TARGETS) doc

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl
	erlc -pa $(EBIN_DIR) $(ERLC_FLAGS) -I$(INCLUDE_DIR) -o$(EBIN_DIR) $<

check: all
	pgrep -lf memcached |grep 11411 || false
	@memcached -d -p 11411 -P ${PID_FILE1} #-vvv
	@memcached -d -p 11511 -P ${PID_FILE2} #-vvv
	@erl -pa `pwd`/ebin -eval 'io:format("~p", [ct:run_test([{auto_compile, true}, {dir, "./test"}, {logdir, "./log"}, {refresh_logs, "./log"}, {cover, "./src/coverspec"}])]).' -s init stop
	@kill `cat ${PID_FILE1}`
	@kill `cat ${PID_FILE2}`

test: check

install: all
	mkdir -p ${LIBDIR}/${APP_NAME}-${VERSION}/ebin
	for i in ebin/*.beam; do install $$i $(LIBDIR)/${APP_NAME}-${VERSION}/$$i ; done

dist: distclean
	mkdir -p $(DIST_TARGET)
	cp -r Makefile ebin src log README.md test $(DIST_TARGET)
	(cd $(DIST_TMP_DIR) &&	  tar -zcf ../$(TARBALL_NAME).tar.gz $(TARBALL_NAME))
	rm -rf $(DIST_TMP_DIR)

.PHONY: doc

doc:
	erl -eval 'edoc:run([], ["./src/memcached.erl"], [{dir, "./doc"}, {packages, true}]).' -s init stop -noshell



distclean: clean
	rm -f *.dump
	find . -regex '.*\(~\|#\|\.swp\|\.dump\)' -exec rm {} \;

clean:
	rm -f $(TARGETS) $(TARBALL_NAME).tar.gz
	rm -f test/*.beam
	rm -rf log/ct_run*
