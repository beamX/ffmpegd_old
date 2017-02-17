PROJECT = ffmpegd
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = ranch lager
dep_ranch_commit = master

include erlang.mk

console:
	./_rel/ffmpegd_release/bin/ffmpegd_release console

