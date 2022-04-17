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

CFLAGS=-ggdb -pipe
CFLAGS_ADA=-gnata -gnatef -gnatwa -gnatyy-3bchms
LDFLAGS=-ggdb
HFLAGS=

ifeq ($(TARGET),$(HOST))
 ifneq ($(findstring darwin,$(TARGET)),)
  HFLAGS+=-I/usr/include/libxml2
 else ifneq ($(findstring freebsd,$(TARGET)),)
  LDFLAGS+=-lpthread
  HFLAGS+=-I/usr/local/$(TARGET)/include/libxml2
 else ifneq ($(findstring linux-gnu,$(TARGET)),)
  ifneq ($(WITH_XML),)
   XML_LDFLAGS=-L$(WITH_XML)/lib
   XML_HFLAGS=-I$(WITH_XML)/libxml2/include
  else
   XML_LDFLAGS:=$(shell pkg-config --libs-only-L libxml-2.0)
   XML_HFLAGS:=$(shell pkg-config --cflags-only-I libxml-2.0)
  endif
  LDFLAGS+=$(XML_LDFLAGS)
  HFLAGS+=$(XML_HFLAGS)
 endif
endif

ifneq ($(DRAKE_RTSROOT),)
 VERSION:=$(shell $(GCC) -dumpversion)
 DRAKE_RTSDIR=$(DRAKE_RTSROOT)/$(TARGET)/$(VERSION)
 ifneq ($(wildcard $(DRAKE_RTSDIR)/debug),)
  DRAKE_RTSDIR:=$(DRAKE_RTSDIR)/debug
 endif
endif
ifneq ($(DRAKE_RTSDIR),)
 IMPORTDIR=
else
 ifneq ($(wildcard import/$(TARGET)),)
  IMPORTDIR=import/$(TARGET)
 else
  IMPORTDIR=$(BUILDDIR)/import
 endif
endif

GARGS=$(addprefix --RTS=,$(abspath $(DRAKE_RTSDIR)))
MARGS=-D $(BUILDDIR) -gnatA $(addprefix -gnatec=,$(wildcard *.adc)) \
      $(addprefix -I,$(IMPORTDIR) $(SOURCEDIR))
CARGS=$(CFLAGS) $(CFLAGS_ADA)
BARGS=-E -x
LARGS=$(LDFLAGS)

ifneq ($(GCC),gcc)
 HFLAGS+=-gcc="$(GCC)"
 GARGS+=$(filter -m32 -m64,$(GCC))
 MARGS+=--GCC="$(GCC)"
 LARGS+=--GCC="$(GCC)"
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))
TESTS=$(filter test_%,$(EXAMPLES))

.PHONY: all check clean $(EXAMPLES)

all: $(addprefix $(BUILDDIR)/,$(EXAMPLES)) $(BINLN)

$(BUILDDIR)/%: %.adb $(wildcard $(SOURCEDIR)/*) | $(BUILDDIR) $(IMPORTDIR)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c $< $(GARGS) $(MARGS) -cargs $(CARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) \
		$(basename $(notdir $<)).ali $(GARGS) $(BARGS)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ \
		$(basename $(notdir $<)).ali $(GARGS) $(LARGS)

# make test_serialize YAMLDIR=yaml-ada/source

YAMLDIR=../../yaml-ada/source

$(BUILDDIR)/test_serialize: MARGS+=-I$(YAMLDIR)

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
