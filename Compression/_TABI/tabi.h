#include <stdio.h>
#include <string.h>
#include <math.h>
#include <wchar.h>

// Constants representing various types that may hold TABI_VALUE
enum {TABI_INTEGER=1, TABI_FLOATING=2, TABI_STRING=3, TABI_PTR=4, TABI_FUNCPTR=5, TABI_BOOL=6, TABI_WIDE_STRING=7};

// Generic function pointer type
typedef void (*VOID_FUNC)();

// Value of unknown type
typedef union _TABI_VALUE
{
	char          placeholder[16];
	long long     int_number;
	double        float_number;
	char*         str;
	wchar_t*      wstr;
	void*         ptr;
	VOID_FUNC     funcptr;
	long          bool_value;
} TABI_VALUE;

// Self-tagged and self-typed value
typedef struct _TABI_ELEMENT
{
	char*       name;
	int         type;
	TABI_VALUE  value;
} TABI_ELEMENT;

// Return type of TABI_FUNCTION
typedef int TABI_RESULT_TYPE;

// TABI-style function that accepts variable number of typed tagged parameters
typedef TABI_RESULT_TYPE TABI_FUNCTION(TABI_ELEMENT*);

#ifndef TABI_DUMPFILE
#define dumpfile stdout
#endif


#ifdef __cplusplus

class TABI_DYNAMAP;

class TABI_MAP
{
	public:
		TABI_MAP(TABI_ELEMENT *params) : p(params) {};
		operator TABI_ELEMENT*() {return p;}

#define TABI_GETTER( TYPE, FIELDTYPE, NAME, TYPECONST, FIELD)                        \
		TYPE NAME(char* n)  {return NAME(n, 0, false);}                      \
		TYPE NAME(char* n, TYPE deflt, bool use_deflt=true)                  \
		{                                                                    \
			TABI_ELEMENT *e = find(n);                                   \
			if (!e) {if (use_deflt)  return deflt;  else throw "";}      \
			switch (e->type)                                             \
			{                                                            \
       				case TYPECONST: return (TYPE)(e->value.FIELD);       \
       				default: throw "";                                   \
       			}                                                            \
		}

		TABI_GETTER( int,                 long long,           _int,        TABI_INTEGER,      int_number);
		TABI_GETTER( unsigned,            long long,           _unsigned,   TABI_INTEGER,      int_number);
		TABI_GETTER( long,                long long,           _long,       TABI_INTEGER,      int_number);
		TABI_GETTER( unsigned long,       long long,           _ulong,      TABI_INTEGER,      int_number);
		TABI_GETTER( long long,           long long,           _longlong,   TABI_INTEGER,      int_number);
		TABI_GETTER( unsigned long long,  long long,           _ulonglong,  TABI_INTEGER,      int_number);
		TABI_GETTER( double,              double,              _double,     TABI_FLOATING,     float_number);
		TABI_GETTER( char*,               char*,               _str,        TABI_STRING,       str);
		TABI_GETTER( const char*,         char*,               _cstr,       TABI_STRING,       str);
		TABI_GETTER( wchar_t*,            wchar_t*,            _wstr,       TABI_WIDE_STRING,  wstr);
		TABI_GETTER( const wchar_t*,      wchar_t*,            _cwstr,      TABI_WIDE_STRING,  wstr);
		TABI_GETTER( void*,               void*,               _ptr,        TABI_PTR,          ptr);
		TABI_GETTER( TABI_FUNCTION*,      VOID_FUNC,           _callback,   TABI_FUNCPTR,      funcptr);
		TABI_GETTER( bool,                long,                _bool,       TABI_BOOL,         bool_value);

                // Return value using "return" callback
		template<class T> TABI_RESULT_TYPE _return(T v);

                // Dump first n elements (for debugging purposes)
		void dump(char *title=NULL, int n=0)
		{
			fprintf(dumpfile, title? "  %s: " : "  TABI_MAP: ", title);
			for (int i=0; i<n?n:100; i++)
			{
				if (p[i].name == NULL)
					break;
				if (i>0)  fprintf(dumpfile, ", ");
				fprintf(dumpfile, "%s: ", p[i].name);
				switch (p[i].type)
				{
					case TABI_STRING:	if (p[i].value.str)   fprintf(dumpfile, "\"%s\"", p[i].value.str); break;
					case TABI_WIDE_STRING:	if (p[i].value.wstr)  fwprintf(dumpfile, L"L\"%s\"", p[i].value.wstr); break;
					case TABI_INTEGER:	fprintf(dumpfile, "%lld",     p[i].value.int_number); break;
					case TABI_FLOATING:	fprintf(dumpfile, "%g",       p[i].value.float_number); break;
					case TABI_PTR:	        fprintf(dumpfile, "<%p>",     p[i].value.ptr); break;
					case TABI_FUNCPTR:	fprintf(dumpfile, "func<%p>", p[i].value.funcptr); break;
					case TABI_BOOL:	        fprintf(dumpfile, "%s",       p[i].value.bool_value? "TRUE":"FALSE"); break;
				}
			}
			fprintf(dumpfile, "\n");
		}

                // Memory dump first n elements (for debugging purposes)
		void memory_dump(int n=0)
		{
			fprintf(dumpfile, "dumping TABI_MAP:\n");
			for (int i=0; i<n?n:100; i++)
			{
				for (int j= 0; j< 4; j++)   fprintf(dumpfile, "%02x ", *((unsigned char*)(p+i)+j)); fprintf(dumpfile, "  ");
				for (int j= 4; j< 8; j++)   fprintf(dumpfile, "%02x ", *((unsigned char*)(p+i)+j)); fprintf(dumpfile, "  ");
				for (int j= 8; j<24; j++)   fprintf(dumpfile, "%02x ", *((unsigned char*)(p+i)+j)); fprintf(dumpfile, "  ");
				if (p[i].name == NULL)
					break;
				fprintf(dumpfile, "%10s: ", p[i].name);
				switch (p[i].type)
				{
					case TABI_STRING:	if (p[i].value.str)   fprintf(dumpfile, "\"%s\"", p[i].value.str); break;
					case TABI_WIDE_STRING:	if (p[i].value.wstr)  fwprintf(dumpfile, L"\"%s\"", p[i].value.wstr); break;
					case TABI_INTEGER:	fprintf(dumpfile, "%lld",     p[i].value.int_number); break;
					case TABI_FLOATING:	fprintf(dumpfile, "%g",       p[i].value.float_number); break;
					case TABI_PTR:	        fprintf(dumpfile, "<%p>",     p[i].value.ptr); break;
					case TABI_FUNCPTR:	fprintf(dumpfile, "func<%p>", p[i].value.funcptr); break;
					case TABI_BOOL:	        fprintf(dumpfile, "%s",       p[i].value.bool_value? "TRUE":"FALSE"); break;
				}
				fprintf(dumpfile, "\n");
			}
			fprintf(dumpfile, "\n");
		}

	protected:
		TABI_ELEMENT* p;

	private:

		TABI_ELEMENT* find(char *n)
		{
			for(TABI_ELEMENT* e=p; e->name; e++)
				if (strcmp(e->name,n)==0)
					return e;
                        return NULL;
		}

};


// Constructor for new TABI_MAPs
class TABI_DYNAMAP : public TABI_MAP
{
	public:
		                  TABI_DYNAMAP()             : TABI_MAP(place) {i=0; p[i].name=NULL;}
		template<class T> TABI_DYNAMAP(         T v) : TABI_MAP(place) {i=0; p[i].name=NULL; (*this)("",v);}
		template<class T> TABI_DYNAMAP(char *n, T v) : TABI_MAP(place) {i=0; p[i].name=NULL; (*this)(n,v);}

#define TABI_SETTER( TYPE, FIELDTYPE, NAME, TYPECONST, FIELD)                        \
		TABI_DYNAMAP& operator() (char *n, TYPE v)                           \
		{                                                                    \
			p[i].name = n;                                               \
			p[i].type = TYPECONST;                                       \
			p[i].value.FIELD = (FIELDTYPE)v;                             \
			i++;                                                         \
			p[i].name = NULL;                                            \
			return *this;                                                \
		}

		TABI_SETTER( int,                 long long,           _int,        TABI_INTEGER,   int_number);
		TABI_SETTER( unsigned,            long long,           _unsigned,   TABI_INTEGER,   int_number);
		TABI_SETTER( long,                long long,           _long,       TABI_INTEGER,   int_number);
		TABI_SETTER( unsigned long,       long long,           _ulong,      TABI_INTEGER,   int_number);
		TABI_SETTER( long long,           long long,           _longlong,   TABI_INTEGER,   int_number);
		TABI_SETTER( unsigned long long,  long long,           _ulonglong,  TABI_INTEGER,   int_number);
		TABI_SETTER( double,              double,              _double,     TABI_FLOATING,  float_number);
		TABI_SETTER( char*,               char*,               _str,        TABI_STRING,    str);
		TABI_SETTER( const char*,         char*,               _cstr,       TABI_STRING,    str);
		TABI_SETTER( wchar_t*,            wchar_t*,            _wstr,       TABI_WIDE_STRING,  wstr);
		TABI_SETTER( const wchar_t*,      wchar_t*,            _cwstr,      TABI_WIDE_STRING,  wstr);
		TABI_SETTER( void*,               void*,               _ptr,        TABI_PTR,       ptr);
		TABI_SETTER( TABI_FUNCTION*,      VOID_FUNC,           _callback,   TABI_FUNCPTR,   funcptr);
		TABI_SETTER( bool,                long,                _bool,       TABI_BOOL,      bool_value);

	private:
		TABI_ELEMENT place[100];
		int i;
};


// Return value using "return" callback
template<class T> TABI_RESULT_TYPE TABI_MAP::_return(T v) {return _callback("return") (TABI_DYNAMAP("result",v));}


#endif // __cplusplus
