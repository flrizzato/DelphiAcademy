#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <optional>
#include <algorithm>
#include <vector>

// template auto
// https://github.com/tvaneerd/cpp17_in_TTs/blob/master/ALL_IN_ONE.md
template<auto v>
struct constant {
	static constexpr auto value = v;
};

// Nested namespaces
namespace Foo::Bar::Z {
	class Something {};
}

// Inline variables - put this in header
// Behaves like an inline method: can be defined identically in multiple
// translation units; behaves as though there is just one
inline int i = 5;

// New attributes
// Can attach to structs/classes too, to make the type undiscardable
[[nodiscard]] int MustUseReturnValue(int a) {
	return a * 2;
}

int _tmain(int argc, _TCHAR* argv[])
{
	auto v1 = constant<123>::value;
	auto v2 = constant<'z'>::value;

	Foo::Bar::Z::Something s;

	int j =
		MustUseReturnValue(5);

	// optional: may hold a value
	std::optional<int> a(5);
	std::optional<int> b;
	if (a.has_value()) {
        int z = *a + b.value_or(0); // *deref, or .value()
	}
    // also std::variant (a union) and std::any

	// Parallel STL algorithms
	std::vector<int> vec { 1, 10, 8, 7, 15, 4, 5, 3 };
	std::sort(std::execution::par, vec.begin(), vec.end());

	return 0;
}
