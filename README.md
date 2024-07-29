# expected

An extended version of C++23's `std::expected<T, E>` to handle variadic errors, allowing usage like `expected<T, E1, E2, ...>`.

## Table of Contents
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
  - [Basic Example](#basic-example)
  - [Advanced Example](#advanced-example)
- [Contributing](#contributing)
- [License](#license)

## Features
- Support starts from C++17
- Supports multiple error types.
- Enhances standard `std::expected` functionality.

## Installation
Clone the repository:
```sh
git clone https://github.com/mguludag/expected.git
```
Include the `include` directory in your project.

## Usage

### Basic Example
Here's a simple example of how to use `expected` with multiple error types:
```cpp
#include <mgutility/expected.hpp>

expected<int, std::string> divide(int a, int b) {
    if (b == 0) return unexpected{"Division by zero"};
    return a / b;
}

int main() {
    auto result = divide(4, 2);
    if (!result) {
        // Handle error
    }
    // Use result
}
```

### Advanced Example


#### Using `.and_then` and `.or_else` per Error Type and using `.transform`
* Chain operations that depend on the success of the previous one, and handle specific errors with `or_else`
* Transform the value inside `expected` if it is present
```cpp
#include <mgutility/expected.hpp>
#include <iostream>
#include <string>
#include <system_error>

expected<int, std::string, std::error_code> divide(int a, int b) {
    if (b == 0) return unexpected{"Division by zero"};
    if (a < 0) return unexpected{std::make_error_code(std::errc::invalid_argument)};
    return a / b;
}

expected<int, std::string, std::error_code> addOne(int x) {
    return x + 1;
}

int main() {
    auto result = divide(10, 2)
        .and_then(addOne)
        .transform([](int x) { return x * 2; })
        .or_else([](const std::string& err) {
            std::cerr << "String error: " << err << std::endl;
            return unexpected("Handled string error");
        })
        .or_else([](const std::error_code& err) {
            std::cerr << "Error code: " << err.message() << std::endl;
            return unexpected(err);
        });

    if (result) {
        std::cout << "Result: " << *result << std::endl;
    } else if (result.has_error<std::string>() {
        // use result.error<std::string>()
    } else {
      // use result.error<std::error_code>()
    }
}
```

## Contributing
Contributions are welcome! Please open issues or pull requests.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

This `README.md` provides a complete guide with basic and advanced usage examples, including multiple error types and chained handling using monadic operations.
