#include <string.h>
#if defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#endif
#include <libxml/xmlversion.h>
#undef LIBXML_EXPR_ENABLED
/* unremovable circular dependency is in xmlregexp.h and xmltree.h */
#include <libxml/xmlreader.h>
#include <libxml/xmlwriter.h>

#pragma for Ada overload \
	void fprintf (FILE *, char const *)
#pragma for Ada overload \
	void fprintf (FILE *, char const *, char const *)
#pragma for Ada overload \
	void fprintf (FILE *, char const *, char const *, char const *)
#pragma for Ada overload \
	void fprintf (FILE *, char const *, int, int, char const *, int, int)

#pragma for Ada overload \
	XMLPUBFUN int XMLCALL xmlTextWriterWriteFormatComment ( \
		xmlTextWriterPtr writer, \
		const char *format, \
		xmlChar const *)

#pragma for Ada overload \
	XMLPUBFUN int XMLCALL xmlTextWriterWriteFormatElement ( \
		xmlTextWriterPtr writer, \
		const xmlChar * name, \
		const char *format, \
		int)
