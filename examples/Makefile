SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BINLN=bin
ifneq ($(findstring darwin,$(TARGET)),)
HFLAGS=-I/usr/include/libxml2
endif
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BINLN=
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lpthread
HFLAGS=-I/usr/local/$(TARGET)/include/libxml2
endif
ifneq ($(findstring linux,$(TARGET)),)
LFLAGS=
HFLAGS=-I/usr/local/$(TARGET)/include/libxml2
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS=-I$(abspath $(IMPORTDIR))
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

CFLAGS:=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean $(EXAMPLES)

all: $(BUILDDIR)/test_reader $(BUILDDIR)/test_writer $(BUILDDIR)/test_xml $(BUILDDIR)/test_rss $(BUILDDIR)/version $(BINLN)

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

# make build/test_serialize YAMLDIR=yaml-ada/source

YAMLDIR=../../yaml-ada/source

$(BUILDDIR)/test_serialize: test_serialize.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(YAMLDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/%

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(HFLAGS) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf *.build bin
	-rm reader1.tmp reader2.tmp reader3.tmp reader4.tmp
	-rm writer1.res writer2.res writer3.res writer4.res
	-rm test_xml.xml
