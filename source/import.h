#include <string.h>
#if defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#elif defined(__FreeBSD__)
#include <stdint.h> /* before sys/cdefs.h */
#elif defined(__gnu_linux__)
#if !defined(_BITS_LIBIO_H)
#include <features.h> /* __GLIBC_PREREQ */
#if !__GLIBC_PREREQ(2, 27)
#include <libio.h> /* before stdio.h */
#else
#define _LIBIO_H
#include <bits/libio.h> /* before stdio.h */
#undef _LIBIO_H
#endif
#endif
#endif
#include <libxml/xmlversion.h>
#undef LIBXML_EXPR_ENABLED
#if defined(LIBXML_ICU_ENABLED)
#undef LIBXML_ICU_ENABLED
#endif
/* unremovable circular dependency is in xmlregexp.h and xmltree.h */
#include <libxml/xmlreader.h>
#include <libxml/xmlwriter.h>

/* undef the macro returning struct by value */
#if defined(xmlDefaultSAXLocator)
#undef xmlDefaultSAXLocator
#endif

#if defined(__gnu_linux__)
#if __GLIBC_PREREQ(2, 27)
#pragma for Ada "stdio.h" include "bits/types/FILE.h"
#endif
#endif
