#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <string>

// string_view
// Unified way to view a string (memory and length) - without owning it
// No allocation
void parse(std::string_view inputToParse) {
	// ...
}

int _tmain(int argc, _TCHAR* argv[]) 
{
	const std::string str = "hello world";
	const char* c = "Rio de Janeiro";
	parse(std::string_view(str.data(), str.length()));
	parse(std::string_view(c, strlen(c)));

	return 0;
}
