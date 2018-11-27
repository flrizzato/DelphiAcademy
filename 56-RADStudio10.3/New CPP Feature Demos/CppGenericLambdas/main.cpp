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
#include <iostream>

int _tmain(int argc, _TCHAR* argv[]) 
{
	std::string hello = "hello";
	std::string world = "world";

	auto addThings = [](auto a, auto b) { return a + b; };

	auto i = addThings(1, 2);
	auto s = addThings(hello, world);

	std::cout << i << std::endl;
    std::cout << s << std::endl;

	return 0;
}
