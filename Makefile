PREFIX ?= /usr/local
PRG := gw2json

# Warnings
CFLAGS += -Wall -Wextra -Wno-unused

# Includes
CFLAGS += -I/usr/local/include/gwion/util
CFLAGS += -I/usr/local/include/gwion/ast
CFLAGS += -I/usr/local/include/libtermcolor
CFLAGS += -Iinclude

#CFLAGS += -flto -Ofast
#LDFLAGS += -flto

LDFLAGS += -lprettyerr -ltermcolor

ifneq ($(shell uname), Darwin)
LDFLAGS += -static
endif

ifeq ($(shell uname), Darwin)
AR = /usr/bin/libtool
AR_OPT = -static $^ -o $@
LDFLAGS += -undefined dynamic_lookup
else
AR = ar
AR_OPT = rcs $@ $^
endif

ifeq (${BUILD_ON_WINDOWS}, 1)
CFLAGS += -DBUILD_ON_WINDOWS=1 -D_XOPEN_SOURCE=700
LDFLAGS += -Wl,--enable-auto-import
endif

all: ${PRG}

${PRG}: src/${PRG}.o
	${CC} ${CFLAGS} $? -Iinclude -lgwion_ast -lgwion_util ${LDFLAGS} -lpthread -lm -o ${PRG}

clean:
	rm -rf src/*.o ${PRG}

install: all
	install ${PRG} ${PREFIX}/bin
	install include/${PRG}.h ${PREFIX}/include/${PRG}.h

uninstall:
	rm ${PREFIX}/bin/${PRG}
	rm ${PREFIX}/include/${PRG}.h
