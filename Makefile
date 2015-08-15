PROJECT = dns
DEPS = gproc
TEST_DEPS=pcapng

dep_pcapng = git /net/filer.local/volume1/repos/pcapng.git master

SHELL_OPTS = \
	-boot start_sasl \
	-config dev.config \
	-name $(PROJECT) \
	-s $(PROJECT) \
	-s rb

include erlang.mk
