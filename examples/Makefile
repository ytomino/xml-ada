SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifeq ($(TARGET),$(HOST))
GCCPREFIX=
BINLN=bin
else
GCCPREFIX=$(TARGET)-
BINLN=
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3cbhms \
	$(addprefix -gnatec=,$(abspath $(wildcard *.adc)))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifeq ($(TARGET),$(HOST))
ifneq ($(findstring darwin,$(TARGET)),)
HFLAGS=-I/usr/include/libxml2
else ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lpthread
HFLAGS=-I/usr/local/$(TARGET)/include/libxml2
else ifneq ($(findstring linux-gnu,$(TARGET)),)
LFLAGS=
HFLAGS:=$(shell pkg-config --cflags-only-I libxml-2.0)
endif
endif

ifneq ($(DRAKE_RTSROOT),)
VERSION:=$(shell $(GCC) -dumpversion)
DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
endif
ifneq ($(DRAKE_RTSDIR),)
IMPORTDIR=
GFLAGS+=--RTS=$(abspath $(DRAKE_RTSDIR))
else
ifneq ($(wildcard import/$(TARGET)),)
IMPORTDIR=import/$(TARGET)
else
IMPORTDIR=$(BUILDDIR)/import
endif
MFLAGS+=-I$(IMPORTDIR)
endif

ifneq ($(GCC),gcc)
GFLAGS+=$(filter -m32 -m64,$(GCC))
MFLAGS+=--GCC="$(GCC)"
LFLAGS+=--GCC="$(GCC)"
HFLAGS+=-gcc="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

.PHONY: all clean $(EXAMPLES)

all: $(EXAMPLES)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c -I$(SOURCEDIR) $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $(basename $(notdir $<)).ali $(GFLAGS) $(BFLAGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS)

# make test_serialize YAMLDIR=yaml-ada/source

YAMLDIR=../../yaml-ada/source

$(BUILDDIR)/test_serialize: MFLAGS+=-I$(YAMLDIR)

$(BUILDDIR):
	mkdir $(BUILDDIR)

$(BINLN): | $(BUILDDIR)
	ln -s $(BUILDDIR) $(BINLN)

$(EXAMPLES): %: $(BUILDDIR)/% $(BINLN)

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -rf *.noindex bin
	-rm reader1.tmp reader2.tmp reader3.tmp reader4.tmp
	-rm writer1.res writer2.res writer3.res writer4.res
	-rm test_xml.xml
