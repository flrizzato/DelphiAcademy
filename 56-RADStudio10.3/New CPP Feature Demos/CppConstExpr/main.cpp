#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <cmath>
#include <memory>
#include <iostream>

constexpr int MethodCall(int a, int b) {
	return a * b;
}

constexpr auto degreesToRadians(const double deg) {
    return deg * (M_PI / 180.0);
}

constexpr int sum(int n) {
	if (n > 0) {
		return n + sum(n-1);
	} else {
        return n;
    }
}

// Example from https://tech.io/playgrounds/2205/7-features-of-c17-that-will-simplify-your-code/constexpr-if
template <typename T>
auto get_value(T t) {
	if constexpr (std::is_pointer_v<T>)
		return *t;
	else
		return t;
}

int _tmain(int argc, _TCHAR* argv[])
{
	constexpr int i = 1 + 2;
	constexpr int j = MethodCall(5, 10);

	constexpr auto rad = degreesToRadians(90);

	constexpr auto SumOneToTen = sum(10);

	int a = 5;
	int* pa = &a;

	auto valueA = get_value(a);
    auto valuePA = get_value(pa);

	std::cout << valueA << std::endl;
	std::cout << valuePA << std::endl;

	return 0;
}
