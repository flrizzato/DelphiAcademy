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

struct Foo {
	int A;
	std::string B;
};

template <int N>
auto NthMember(Foo const& foo) {
	if constexpr (N == 0) {
		return foo.A;
	} else if constexpr (N == 1) {
		return foo.B;
	}
}

int _tmain(int argc, _TCHAR* argv[]) 
{
	Foo foo;
	auto a = NthMember<1>(foo);

	return 0;
}
