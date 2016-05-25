SOURCEDIR=../source

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

VERSION=$(shell gcc -dumpversion)

BUILDDIR=$(TARGET).build

ifeq ($(TARGET),$(HOST))
GNATPREFIX=
GCC=
BINLN=bin
else
GNATPREFIX=$(TARGET)-
GCC=$(TARGET)-gcc
BINLN=
endif

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3chbs
MFLAGS=-D $(BUILDDIR)
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifeq ($(TARGET),$(HOST))
ifneq ($(findstring darwin,$(TARGET)),)
HFLAGS=-I/usr/include/libxml2
endif
else
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
GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
IMPORTDIR=$(BUILDDIR)/import
MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),)
GFLAGS+=$(filter -m32 -m64,$(GCC))
MFLAGS+=--GCC="$(GCC)"
LFLAGS+=--GCC="$(GCC)"
HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

.PHONY: all clean $(EXAMPLES)

all: $(EXAMPLES)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GNATPREFIX)gnatmake -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatmake -b $(basename $(notdir $<)).ali $(GFLAGS) -bargs $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

# make build/test_serialize YAMLDIR=yaml-ada/source

YAMLDIR=../../yaml-ada/source

$(BUILDDIR)/test_serialize: MFLAGS+=-I$(YAMLDIR)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)

ifneq ($(IMPORTDIR),)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -rf *.build bin
	-rm reader1.tmp reader2.tmp reader3.tmp reader4.tmp
	-rm writer1.res writer2.res writer3.res writer4.res
	-rm test_xml.xml
