CC = /usr/bin/gcc
BIGLOO = bigloo -cc $(CC)
AFILE = bglafile

BGL_COMMON_FLAGS = -L .. -copt -fpic -mkaddlib
BGL_FLAGS = $(BGL_COMMON_FLAGS) -g

UTF_BGL_MODULES = utf
UTF_OBJECTS = $(UTF_BGL_MODULES:%=%.o)
UTF_SOURCES = $(UTF_BGL_MODULES:%=%.scm)

OBJECTS	= $(UTF_OBJECTS)
SOURCES	= $(UTF_SOURCES)

UTF_LIB = utf
UTF_HEAP = $(UTF_LIB).heap
UTF_INIT = $(UTF_LIB).init
UTF_VERSION = 1.0
UTF_LIB_A = lib$(UTF_LIB)_s-$(UTF_VERSION).a
UTF_LIB_SO = lib$(UTF_LIB)_s-$(UTF_VERSION).so
UTF_LIB_OBJECTS = $(OBJECTS)

all: $(UTF_LIB_A) $(UTF_LIB_SO) $(UTF_INIT)

.PHONY: build-afile clean all

.afile: $(SOURCES)
	$(AFILE) -o $@ $(SOURCES)

o/runtime.o: $(RUNTIME_SOURCES)

o/unicode/.keep:
	mkdir -p o/unicode;
	touch $@;

%.o: %.scm .afile
	$(BIGLOO) $(BGL_FLAGS) -c -o $@ $<

clean:
	rm -f $(OBJECTS) \
	      $(UTF_HEAP) \
	      $(UTF_LIB_A) \
	      $(UTF_LIB_SO) \
	      $(UTF_INIT) \
	      unicode-reader.o unicode-reader \
	      utf-data.sch utf-category-data.sch \
	      utf-init-gen utf-init-gen.o \
	      .afile;

$(UTF_INIT): utf.init.in utf-init-gen
	./utf-init-gen -o $@ $<

$(UTF_HEAP): make-lib.scm utf.scm .afile utf.o
	bigloo $(BGL_FLAGS) -mkaddheap -mkaddlib -heap-library $(UTF_LIB) $< -addheap $@
$(UTF_LIB_A): $(UTF_HEAP) $(UTF_LIB_OBJECTS)
	rm -f $@ && \
	ar qcv $@ $(UTF_LIB_OBJECTS) && \
	ranlib $@
$(UTF_LIB_SO): $(UTF_HEAP) $(UTF_LIB_OBJECTS)
	ld -G -o $@ $(UTF_LIB_OBJECTS)

# explicit deps
utf.o: utf-data.sch utf-category-data.sch

utf-data.sch: unicode-reader
	./unicode-reader > $@
utf-category-data.sch: unicode-reader
	./unicode-reader --categories > $@

unicode-reader: unicode-reader.scm
	bigloo -Wall -g -o $@ $^

utf-init-gen: utf-init-gen.scm
	bigloo -Wall -g -o $@ $^
