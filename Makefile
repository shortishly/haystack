#-*- mode: makefile-gmake -*-
# Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
PROJECT = haystack
PROJECT_DESCRIPTION = Domain Name Service
PROJECT_VERSION = 0.1.1

DEPS = \
	cowboy \
	gproc \
	gun \
	jsx \
	recon

LOCAL_DEPS = \
	crypto \
	inets \
	runtime_tools \
	sasl \
	ssh \
	tools

TEST_DEPS = pcapng

dep_pcapng = git https://github.com/shortishly/pcapng.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git master

SHELL_OPTS = \
	-boot start_sasl \
	-config dev.config \
	-s $(PROJECT) \
	-s rb \
	-sname $(PROJECT)

include erlang.mk
