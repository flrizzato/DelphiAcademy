#pragma hdrstop
#pragma argsused

#ifdef _WIN32
#include <tchar.h>
#else
  typedef char _TCHAR;
  #define _tmain main
#endif

#include <stdio.h>
#include <cstdlib>
#include <string>
#include <memory>

class Employee {
	// Lots of code here
};

struct PersonID {
	std::string firstName;
	std::string lastName;
};

class Employees {
public:
	auto findEmployee(const std::string Name) { return 1; }
};

int returnAnInt() {
	return 1;
}

auto returnSomething() {
	return 1;
}

// The following will give a compiler error, because int and float are different types
//auto returnConfusion() {
//	if (std::rand() %2 == 0) {
//		return -1;
//	} else {
//		return 3.14159;
//	}
//}
int _tmain(int argc, _TCHAR* argv[])
{
	auto i { returnAnInt() };

	auto j { returnSomething() };

	// Example of a method with multiple return points, each with different types - an error
//	auto k { returnConfusion() };

	auto employeeList { std::make_unique<Employees>() };
	int person = employeeList->findEmployee("Jane Smith");

	return 0;
}
