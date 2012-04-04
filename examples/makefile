SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
GCC=$(dir $(shell which $(GNATMAKE)))gcc
BUILDDIR=build
IMPORTDIR:=import
ifneq ($(findstring darwin,$(TARGET)),)
HFLAGS=-I/usr/include/libxml2
endif
else
GNATMAKE=$(TARGET)-gnatmake
GCC=$(TARGET)-gcc
BUILDDIR=build-$(TARGET)
IMPORTDIR:=import-$(TARGET)
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=
HFLAGS=-I/usr/local/i686-pc-freebsd7/include/libxml2
endif
endif

ifneq ($(DRAKE_RTSROOT),)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR:=
MFLAGS=--RTS=$(abspath $(DRAKE_RTSDIR))
endif

CFLAGS:=-gnata -gnatwa -gnatyy-3chbs

.PHONY: all clean

all: $(BUILDDIR)/test_reader $(BUILDDIR)/test_writer $(BUILDDIR)/test_xml $(BUILDDIR)/test_rss $(BUILDDIR)/version

$(BUILDDIR)/%: %.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

# make build/test_serialize YAMLDIR=yaml-ada/source

YAMLDIR=../../yaml-ada/source

$(BUILDDIR)/test_serialize: test_serialize.adb $(BUILDDIR) $(IMPORTDIR) $(wildcard $(SOURCEDIR)/*)
	cd $(BUILDDIR) && $(GNATMAKE) -g $(MFLAGS) -I../$(SOURCEDIR) -I../$(IMPORTDIR) -I../$(YAMLDIR) $(CFLAGS) ../$< -largs $(LFLAGS)

$(BUILDDIR):
	mkdir $(BUILDDIR)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada -gcc=$(GCC) $(HFLAGS) -p -D $(IMPORTDIR) $+
endif

clean:
	-rm -rf build* import*
	-rm reader1.tmp reader2.tmp reader3.tmp reader4.tmp
	-rm writer1.res writer2.res writer3.res writer4.res
	-rm test_xml.xml
