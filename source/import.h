#include <string.h>
#if defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#elif defined(__FreeBSD__)
#include <stdint.h> /* before sys/cdefs.h */
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
