#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <memory>

class Foo {
	//
};

int _tmain(int argc, _TCHAR* argv[])
{
	int i = 5;

	auto j = [&k = i](){};


	std::unique_ptr<Foo> foo { std::make_unique<Foo>() };

	auto f = [z = std::move(foo)](){};


	return 0;
}
