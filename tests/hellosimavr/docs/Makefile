ifdef MESSAGE
MESSAGEFLAG = -m "$(MESSAGE)"
endif

ifdef PUSH
PUSHFLAG = -p
endif

all: docs

docs: export PROJECT_NUMBER = $(shell git log -1 --format='%h (%cd)')
docs:
	doxygen

publish: all
	ghp-import -nf $(PUSHFLAG) $(MESSAGEFLAG) html

clean:
	rm -rf html

.PHONY: all docs publish clean
