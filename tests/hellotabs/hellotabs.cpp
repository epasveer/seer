#include <string>
#include <iostream>

void function1 (const std::string& message);

int main (int argc, char** argv) {

    // Physical \t are expanded to spaces.
    //
	// 123456789 123456789 123456789
	// 	2 4	6	 1	4  7    2 4	6

    //\tThis\tis\ta\tcomment.    Textual \t are not expanded.

	int j = 0;

	for (int i=0; i<argc; i++) {
		std::cout << "XX: "
			<< i
			<< " "
			<< argv[i]
			<< std::endl;
	}

	std::string message = "Hello, Tabs!";

	j++;

	function1(message);

	return 0;
}

