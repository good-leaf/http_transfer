PROJECT = http_transfer
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +'{parse_transform, lager_transform}'
DEPS = cowboy lager ibrowse jsx
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.x

include erlang.mk
