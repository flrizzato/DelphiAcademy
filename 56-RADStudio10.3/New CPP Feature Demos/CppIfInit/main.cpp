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

int a = 5;

int* getPointer() {
	return &a;
}

std::string getString() {
	return "hello";
}

int _tmain(int argc, _TCHAR* argv[])
{
	// Pointer is convertible to bool
	if (int* p = getPointer()) {
		// Do something with p
	}

    // Needs to have a boolean expression
	if (std::string s = getString(); s.length() > 0) {
        //
	}

	return 0;
}
