SOURCEDIR=../source

HOST:=$(shell gcc -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

ifneq ($(TARGET),$(HOST))
 GCCPREFIX=$(TARGET)-
 BINLN=
else
 GCCPREFIX=
 BINLN=bin
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

GFLAGS=
CFLAGS=-ggdb -pipe -gnatef -gnata -gnatwa -gnatyy-3cbhms \
	$(addprefix -gnatec=,$(wildcard *.adc))
MFLAGS=-D $(BUILDDIR) -gnatA
BFLAGS=-E
LFLAGS=-ggdb
HFLAGS=

ifeq ($(TARGET),$(HOST))
 ifneq ($(findstring darwin,$(TARGET)),)
  HFLAGS+=-I/usr/include/libxml2
 else ifneq ($(findstring freebsd,$(TARGET)),)
  LFLAGS+=-lpthread
  HFLAGS+=-I/usr/local/$(TARGET)/include/libxml2
 else ifneq ($(findstring linux-gnu,$(TARGET)),)
  ifneq ($(WITH_XML),)
   XML_LFLAGS=-L$(WITH_XML)/lib
   XML_HFLAGS=-I$(WITH_XML)/libxml2/include
  else
   XML_LFLAGS:=$(shell pkg-config --libs-only-L libxml-2.0)
   XML_HFLAGS:=$(shell pkg-config --cflags-only-I libxml-2.0)
  endif
  LFLAGS+=$(XML_LFLAGS)
  HFLAGS+=$(XML_HFLAGS)
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
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check clean $(EXAMPLES)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES)) $(BINLN)

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

$(filter-out test_%,$(EXAMPLES)): %: $(BUILDDIR)/% $(BINLN)

check: all $(TESTS)

$(filter-out test_reader test_writer,$(TESTS)): %: $(BUILDDIR)/%
	$<

test_reader: $(BUILDDIR)/test_reader
	$<
	diff reader1.res $(TMPDIR)/reader1.tmp
	diff reader1.res $(TMPDIR)/reader2.tmp
	diff reader3.res $(TMPDIR)/reader3.tmp
	diff reader4.res $(TMPDIR)/reader4.tmp

test_writer: $(BUILDDIR)/test_writer
	$<
	diff writer.xml $(TMPDIR)/writer1.res
	diff writer.xml $(TMPDIR)/writer2.res
	diff writer.xml $(TMPDIR)/writer3.res
	diff writer.xml $(TMPDIR)/writer4.res

ifeq ($(IMPORTDIR),$(BUILDDIR)/import)
$(IMPORTDIR): $(SOURCEDIR)/import.h
	headmaster --to ada $(HFLAGS) -p -D $@ $+
	touch $@
endif

clean:
	-rm -r *.noindex bin
