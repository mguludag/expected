/*
MIT License

Copyright (c) 2024 mguludag

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef MGUTILITY_EXPECTED_HPP
#define MGUTILITY_EXPECTED_HPP

#include <cstdlib>  // For std::terminate
#include <optional>
#include <stdexcept>
#include <system_error>
#include <type_traits>
#include <utility>
#include <variant>

namespace mgutility {

// Define a macro to conditionally throw exceptions or terminate
#ifdef __cpp_exceptions
#define THROW_EXCEPTION(exception) throw exception
#else
#define THROW_EXCEPTION(exception) std::terminate()
#endif

namespace detail {
// Helper metafunction to check if a type is in a list of types
template <typename T, typename... Ts>
struct is_one_of;

template <typename T>
struct is_one_of<T> : std::false_type {};

template <typename T, typename First, typename... Rest>
struct is_one_of<T, First, Rest...>
    : std::conditional_t<std::is_same_v<T, First>, std::true_type,
                         is_one_of<T, Rest...>> {};

// Type trait to check if a type is in a std::variant
template <typename T, typename Variant>
struct is_in_variant;

template <typename T, typename... Ts>
struct is_in_variant<T, std::variant<Ts...>> : is_one_of<T, Ts...> {};

// Helper alias template
template <typename T, typename Variant>
constexpr bool is_in_variant_v = is_in_variant<T, Variant>::value;

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

/**
 * @brief Helper to deduce the first argument type of a function
 */
template <typename Ret, typename Arg, typename... Rest>
Arg first_argument_helper(Ret (*)(Arg, Rest...));

template <typename Ret, typename F, typename Arg, typename... Rest>
Arg first_argument_helper(Ret (F::*)(Arg, Rest...));

template <typename Ret, typename F, typename Arg, typename... Rest>
Arg first_argument_helper(Ret (F::*)(Arg, Rest...) const);

template <typename F>
decltype(first_argument_helper(&F::operator())) first_argument_helper(F);

template <typename T>
using first_argument = decltype(first_argument_helper(std::declval<T>()));

}  // namespace detail

struct unexpect_t {
    explicit unexpect_t() = default;
};

inline constexpr unexpect_t unexpect{};

/**
 * @brief Exception class for accessing a bad expected value.
 */
template <typename E>
class bad_expected_access;

/**
 * @brief Specialization of bad_expected_access for void type.
 */
template <>
class bad_expected_access<void> : public std::exception {
   public:
    /**
     * @brief Constructs a bad_expected_access<void> exception.
     * @param error The unexpected error.
     */
    explicit bad_expected_access() noexcept : std::exception() {}

    /**
     * @brief Gets the exception message.
     * @return The exception message.
     */
    const char* what() const noexcept override {
        return "Bad expected access: no value present";
    }
};

/**
 * @brief Templated exception class for accessing a bad expected value.
 * @tparam E The type of the unexpected error.
 */
template <typename E>
class bad_expected_access : public bad_expected_access<void> {
   public:
    /**
     * @brief Constructs a bad_expected_access exception with the given
     * unexpected error.
     * @param error The unexpected error.
     */
    explicit bad_expected_access(E error) noexcept(
        std::is_nothrow_copy_constructible<E>::value)
        : error_(std::move(error)) {}

    /**
     * @brief Gets the stored unexpected error.
     * @return The stored unexpected error.
     */
    const E& error() const noexcept { return error_; }

   private:
    E error_;
};

/**
 * @brief A class representing an unexpected error.
 *
 * @tparam E The type of the error.
 */
template <typename E>
class unexpected {
   public:
    /**
     * @brief Constructs an unexpected object with an error.
     *
     * @param error The error to store in the unexpected object.
     */
    constexpr explicit unexpected(const E& error) noexcept(
        std::is_nothrow_copy_constructible<E>::value)
        : error_(error) {}

    /**
     * @brief Constructs an unexpected object with an error.
     *
     * @param error The error to store in the unexpected object.
     */
    constexpr explicit unexpected(E&& error) noexcept(
        std::is_nothrow_move_constructible<E>::value)
        : error_(std::move(error)) {}

    /**
     * @brief Constructs an unexpected object in place.
     *
     * @tparam Args The types of the arguments to construct the error.
     * @param args The arguments to construct the error.
     */
    template <typename... Args>
    constexpr explicit unexpected(std::in_place_t, Args&&... args) noexcept(
        std::is_nothrow_constructible<E, Args...>::value)
        : error_(std::forward<Args>(args)...) {}

    /**
     * @brief Gets the stored error.
     *
     * @return const E& The stored error.
     */
    constexpr const E& error() const& noexcept { return error_; }

    /**
     * @brief Gets the stored error.
     *
     * @return E& The stored error.
     */
    constexpr E& error() & noexcept { return error_; }

    /**
     * @brief Gets the stored error.
     *
     * @return const E&& The stored error.
     */
    constexpr const E&& error() const&& noexcept { return std::move(error_); }

    /**
     * @brief Gets the stored error.
     *
     * @return E&& The stored error.
     */
    constexpr E&& error() && noexcept { return std::move(error_); }

   private:
    E error_;
};

/**
 * @brief A class representing an expected value or an unexpected error.
 *
 * @tparam T The type of the expected value.
 * @tparam E The type of the primary unexpected error.
 * @tparam Es Additional types of unexpected errors.
 */
template <typename T, typename E, typename... Es>
class expected {
   public:
    using value_type = T;
    using error_type = E;

    /**
     * @brief Constructs an expected object with a value.
     *
     * @param value The value to store in the expected object.
     */
    constexpr expected(const T& value) noexcept(
        std::is_nothrow_copy_constructible<T>::value)
        : result_(value) {}

    /**
     * @brief Constructs an expected object with a value.
     *
     * @param value The value to store in the expected object.
     */
    constexpr expected(T&& value) noexcept(
        std::is_nothrow_move_constructible<T>::value)
        : result_(std::move(value)) {}

    /**
     * @brief Constructs an expected object with an unexpected error.
     *
     * @tparam Error The type of the error.
     * @param error The unexpected error to store in the expected object.
     */
    template <typename Error>
    constexpr expected(const unexpected<Error>& error) noexcept(
        std::is_nothrow_copy_constructible<unexpected<Error>>::value)
        : result_(error) {}

    /**
     * @brief Constructs an expected object with an unexpected error.
     *
     * @tparam Error The type of the error.
     * @param error The unexpected error to store in the expected object.
     */
    template <typename Error>
    constexpr expected(unexpected<Error>&& error) noexcept(
        std::is_nothrow_move_constructible<unexpected<Error>>::value)
        : result_(std::move(error)) {}

    /**
     * @brief Constructs an expected object in place.
     *
     * @tparam Args The types of the arguments to construct the value.
     * @param args The arguments to construct the value.
     */
    template <typename... Args>
    constexpr explicit expected(std::in_place_t, Args&&... args) noexcept(
        std::is_nothrow_constructible<T, Args...>::value)
        : result_(std::in_place_type<T>, std::forward<Args>(args)...) {}

    /**
     * @brief Constructs an unexpected object in place.
     *
     * @tparam Error The type of the error.
     * @tparam Args The types of the arguments to construct the value.
     * @param args The arguments to construct the value.
     */
    template <typename Error, typename... Args>
    constexpr explicit expected(unexpect_t, Args&&... args) noexcept(
        std::is_nothrow_constructible<Error, Args...>::value)
        : result_(std::in_place_type<unexpected<Error>>,
                  unexpected(std::in_place_type<Error>,
                             std::forward<Args>(args)...)) {}

    /**
     * @brief Checks if the expected object contains a value.
     *
     * @return true if the object contains a value, false otherwise.
     */
    constexpr bool has_value() const noexcept {
        return std::holds_alternative<T>(result_);
    }

    /**
     * @brief Checks if the expected object contains an unexpected error of type
     * E.
     *
     * @return true if the object contains an unexpected error of type E, false
     * otherwise.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr bool has_error() const noexcept {
        return std::holds_alternative<unexpected<E>>(result_);
    }

    /**
     * @brief Checks if the expected object contains an unexpected error of type
     * Err.
     *
     * @tparam Error The type of the error.
     * @return true if the object contains an unexpected error of type Err,
     * false otherwise.
     */
    template <typename Error>
    constexpr bool has_error() const noexcept {
        return std::holds_alternative<unexpected<Error>>(result_);
    }

    /**
     * @brief Gets the value or throws an exception if there is no value.
     *
     * @return T& The value.
     * @throws std::runtime_error If there is no value.
     */
    constexpr T& value() & {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return std::get<T>(result_);
    }

    /**
     * @brief Gets the value or throws an exception if there is no value.
     *
     * @return const T& The value.
     * @throws std::runtime_error If there is no value.
     */
    constexpr const T& value() const& {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return std::get<T>(result_);
    }

    /**
     * @brief Gets the value or throws an exception if there is no value.
     *
     * @return T&& The value.
     * @throws std::runtime_error If there is no value.
     */
    constexpr T&& value() && {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return std::get<T>(std::move(result_));
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return const E& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E& error() const& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::get<unexpected<E>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return E& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr E& error() & {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::get<unexpected<E>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return const E&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E&& error() const&& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(std::get<unexpected<E>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return E&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr E&& error() && {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(std::get<unexpected<E>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of type Err or throws an exception if
     * there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr const Error& error() const& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::get<unexpected<Error>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type Err or throws an exception if
     * there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr Error& error() & {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::get<unexpected<Error>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type Err or throws an exception if
     * there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr const Error&& error() const&& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(std::get<unexpected<Error>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of type Err or throws an exception if
     * there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr Error&& error() && {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(std::get<unexpected<Error>>(result_).error());
    }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return T& The value.
     */
    constexpr T& operator*() & noexcept { return value(); }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return const T& The value.
     */
    constexpr const T& operator*() const& noexcept { return value(); }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return T&& The value.
     */
    constexpr T&& operator*() && noexcept { return std::move(*this).value(); }

    /**
     * @brief Arrow operator to access the value.
     *
     * @return T* Pointer to the value.
     */
    constexpr T* operator->() noexcept { return std::addressof(value()); }

    /**
     * @brief Arrow operator to access the value.
     *
     * @return const T* Pointer to the value.
     */
    constexpr const T* operator->() const noexcept {
        return std::addressof(value());
    }

    /**
     * @brief Returns the value if present, otherwise returns the provided
     * default value.
     *
     * @tparam U The type of the default value.
     * @param default_value The default value to return if there is no value.
     * @return T The value or the default value.
     */
    template <typename U>
    constexpr T value_or(U&& default_value) const& noexcept(
        std::is_nothrow_copy_constructible<T>::value) {
        return has_value() ? value()
                           : static_cast<T>(std::forward<U>(default_value));
    }

    /**
     * @brief Returns the value if present, otherwise returns the provided
     * default value.
     *
     * @tparam U The type of the default value.
     * @param default_value The default value to return if there is no value.
     * @return T The value or the default value.
     */
    template <typename U>
    constexpr T value_or(U&& default_value) && noexcept(
        std::is_nothrow_move_constructible<T>::value) {
        return has_value() ? std::move(value())
                           : static_cast<T>(std::forward<U>(default_value));
    }

    /**
     * @brief Returns the error if present, otherwise returns the provided
     * default error.
     *
     * @tparam F The type of the default error.
     * @param default_error The default error to return if there is no error.
     * @return auto The error or the default error.
     */
    template <typename Error>
    constexpr auto error_or(Error&& default_error) const& noexcept {
        return has_error<Error>() ? std::get<unexpected<Error>>(result_).error()
                                  : default_error;
    }

    /**
     * @brief Returns the error if present, otherwise returns the provided
     * default error.
     *
     * @tparam F The type of the default error.
     * @param default_error The default error to return if there is no error.
     * @return auto The error or the default error.
     */
    template <typename Error>
    constexpr auto error_or(Error&& default_error) && noexcept {
        return has_error<Error>()
                   ? std::move(std::get<unexpected<Error>>(result_).error())
                   : default_error;
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) & noexcept(
        std::is_nothrow_invocable<F, T&>::value) {
        using RetType = std::invoke_result_t<F, detail::first_argument<F>>;
        if (has_value()) {
            return f(value());
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, const T&>::value) {
        using RetType = std::invoke_result_t<F, detail::first_argument<F>>;
        if (has_value()) {
            return f(value());
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) && noexcept(
        std::is_nothrow_invocable<F, T&&>::value) {
        using RetType = std::invoke_result_t<F, detail::first_argument<F>>;
        if (has_value()) {
            return f(std::move(value()));
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) & noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) && noexcept(
        std::is_nothrow_invocable<F, T&&>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return std::optional<std::invoke_result_t<F, first_argument<F>>> The
     * result of the function call or nullopt.
     */
    template <typename F>
    constexpr auto transform(F&& f) & noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        if (has_value()) {
            return Result(f(value()));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return std::optional<std::invoke_result_t<F, first_argument<F>>> The
     * result of the function call or nullopt.
     */
    template <typename F>
    constexpr auto transform(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        if (has_value()) {
            return Result(f(value()));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the value.
     * @return std::optional<std::invoke_result_t<F, first_argument<F>>> The
     * result of the function call or nullopt.
     */
    template <typename F>
    constexpr auto transform(F&& f) && noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        if (has_value()) {
            return Result(f(value()));
        }
        return *this;
    }

    /**
     * @brief Emplaces a new value in the expected object.
     *
     * @tparam Args The types of the arguments to construct the new value.
     * @param args The arguments to construct the new value.
     * @return T& The new value.
     */
    template <typename... Args>
    T& emplace(Args&&... args) noexcept(
        std::is_nothrow_constructible<T, Args...>::value) {
        result_.template emplace<T>(std::forward<Args>(args)...);
        return std::get<T>(result_);
    }

    /**
     * @brief Swaps the contents of this expected object with another.
     *
     * @param other The other expected object to swap with.
     */
    void swap(expected& other) noexcept(std::is_nothrow_swappable_v<T> &&
                                        (std::is_nothrow_swappable_v<Es> &&
                                         ...)) {
        result_.swap(other.result_);
    }

    /**
     * @brief Implicit conversion to bool to check if the expected object
     * contains a value.
     *
     * @return true if the object contains a value, false otherwise.
     */
    constexpr explicit operator bool() const noexcept { return has_value(); }

   private:
    template <typename Exp>
    Exp transform_expected() {
        return std::visit(
            [](auto&& arg) -> Exp {
                using ArgType = std::decay_t<decltype(arg)>;
                if constexpr (!std::is_same_v<ArgType, T>) {
                    return arg;
                } else if constexpr (std::is_same_v<typename Exp::value_type,
                                                    void>) {
                    return std::monostate{};
                } else {
                    return typename Exp::value_type{};
                }
            },
            result_);
    }

    // Helper function to throw a bad_expected_access with the correct error
    // type
    void throw_bad_expected_access() const {
        if (has_error<E>()) {
            THROW_EXCEPTION(bad_expected_access<E>(error<E>()));
        } else {
            std::visit(
                [](auto&& arg) -> bad_expected_access<void> {
                    using ArgType = std::decay_t<decltype(arg)>;
                    if constexpr (!std::is_same_v<ArgType, std::monostate>) {
                        THROW_EXCEPTION(bad_expected_access<ArgType>(arg));
                    } else {
                        THROW_EXCEPTION(bad_expected_access<void>());
                    }
                },
                result_);
        }
    }

   private:
    std::variant<T, unexpected<E>, unexpected<Es>...> result_;
};

/////////////////////////////////
// Specialization for T = void //
/////////////////////////////////
template <typename E, typename... Es>
class expected<void, E, Es...> {
   public:
    using value_type = void;
    using error_type = E;

    /**
     * @brief Constructs an expected object with no value (void specialization).
     */
    constexpr expected() noexcept : result_(std::monostate{}) {}

    /**
     * @brief Constructs an expected object with no value (void specialization).
     */
    constexpr expected(std::monostate) noexcept : result_(std::monostate{}) {}

    /**
     * @brief Constructs an expected object with an unexpected error.
     *
     * @tparam Error The type of the error.
     * @param error The unexpected error to store in the expected object.
     */
    template <typename Error>
    constexpr expected(const unexpected<Error>& error) noexcept(
        std::is_nothrow_copy_constructible<unexpected<Error>>::value)
        : result_(error) {}

    /**
     * @brief Constructs an expected object with an unexpected error.
     *
     * @tparam Error The type of the error.
     * @param error The unexpected error to store in the expected object.
     */
    template <typename Error>
    constexpr expected(unexpected<Error>&& error) noexcept(
        std::is_nothrow_move_constructible<unexpected<Error>>::value)
        : result_(std::move(error)) {}

    /**
     * @brief Constructs an unexpected object in place.
     *
     * @tparam Error The type of the error.
     * @tparam Args The types of the arguments to construct the value.
     * @param args The arguments to construct the value.
     */
    template <typename Error, typename... Args>
    constexpr explicit expected(unexpect_t, Args&&... args) noexcept(
        std::is_nothrow_constructible<Error, Args...>::value)
        : result_(std::in_place_type<unexpected<Error>>,
                  unexpected(std::in_place_type<Error>,
                             std::forward<Args>(args)...)) {}

    /**
     * @brief Checks if the expected object contains a value.
     *
     * @return true if the object contains a value, false otherwise.
     */
    constexpr bool has_value() const noexcept {
        return std::holds_alternative<std::monostate>(result_);
    }

    /**
     * @brief Checks if the expected object contains an unexpected error of type
     * E.
     *
     * @return true if the object contains an unexpected error of type E, false
     * otherwise.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr bool has_error() const noexcept {
        return std::holds_alternative<unexpected<E>>(result_);
    }

    /**
     * @brief Checks if the expected object contains an unexpected error of the
     * given type.
     *
     * @tparam Error The type of the error.
     * @return true if the object contains an unexpected error of the given
     * type, false otherwise.
     */
    template <typename Error>
    constexpr bool has_error() const noexcept {
        return std::holds_alternative<unexpected<Error>>(result_);
    }

    /**
     * @brief Checks if the expected object contains a value and throws an
     * exception if not.
     *
     * @throws std::runtime_error If there is no value.
     */
    constexpr void value() const {
        if (!has_value()) {
            throw_bad_expected_access();
        }
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return const E& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E& error() const& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::get<unexpected<E>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return E& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr E& error() & {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::get<unexpected<E>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return const E&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E&& error() const&& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(std::get<unexpected<E>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of type E or throws an exception if
     * there is no error.
     *
     * @return E&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0), std::enable_if_t<HasSingleError, int> = 0>
    constexpr E&& error() && {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(std::get<unexpected<E>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of the given type or throws an exception
     * if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr const Error& error() const& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::get<unexpected<Error>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of the given type or throws an exception
     * if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr Error& error() & {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::get<unexpected<Error>>(result_).error();
    }

    /**
     * @brief Gets the unexpected error of the given type or throws an exception
     * if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr const Error&& error() const&& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(std::get<unexpected<Error>>(result_).error());
    }

    /**
     * @brief Gets the unexpected error of the given type or throws an exception
     * if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error&& The error.
     * @throws std::runtime_error If there is no error.
     */
    template <typename Error>
    constexpr Error&& error() && {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(std::get<unexpected<Error>>(result_).error());
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) & noexcept(
        std::is_nothrow_invocable<F>::value) {
        using RetType = std::invoke_result_t<F>;
        if (has_value()) {
            return f();
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) const& noexcept(
        std::is_nothrow_invocable<F>::value) {
        using RetType = std::invoke_result_t<F>;
        if (has_value()) {
            return f();
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the value if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto and_then(F&& f) && noexcept(
        std::is_nothrow_invocable<F>::value) {
        using RetType = std::invoke_result_t<F>;
        if (has_value()) {
            return f();
        } else {
            return transform_expected<RetType>();
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) & noexcept(
        std::is_nothrow_invocable<F, E&>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, const E&>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the error if present, otherwise
     * returns *this.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return auto The result of the function call or *this.
     */
    template <typename F>
    constexpr auto or_else(F&& f) && noexcept(
        std::is_nothrow_invocable<F, E&&>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_in_variant_v<unexpected<Arg>, decltype(result_)>,
                      "Given functor argument type not exists!");
        if (!has_value() && has_error<Arg>()) {
            return Result(f(std::get<unexpected<Arg>>(result_).error()));
        } else {
            return *this;
        }
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return expected<void, E, Es...> The result of the function call or
     * *this.
     */
    template <typename F>
    constexpr auto transform(F&& f) & noexcept(
        std::is_nothrow_invocable<F>::value) {
        using Result = expected<void, E, Es...>;
        if (has_value()) {
            f();
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return expected<void, E, Es...> The result of the function call or
     * *this.
     */
    template <typename F>
    constexpr auto transform(F&& f) const& noexcept(
        std::is_nothrow_invocable<F>::value) {
        using Result = expected<void, E, Es...>;
        if (has_value()) {
            f();
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the value if present and returns
     * an optional result.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call.
     * @return expected<void, E, Es...> The result of the function call or
     * *this.
     */
    template <typename F>
    constexpr auto transform(F&& f) && noexcept(
        std::is_nothrow_invocable<F>::value) {
        using Result = expected<void, E, Es...>;
        if (has_value()) {
            f();
        }
        return *this;
    }

    /**
     * @brief Swaps the contents of this expected object with another.
     *
     * @param other The other expected object to swap with.
     */
    void swap(expected& other) noexcept((std::is_nothrow_swappable_v<Es> &&
                                         ...)) {
        result_.swap(other.result_);
    }

    /**
     * @brief Implicit conversion to bool to check if the expected object
     * contains a value.
     *
     * @return true if the object contains a value, false otherwise.
     */
    constexpr explicit operator bool() const noexcept { return has_value(); }

   private:
    template <typename Exp>
    Exp transform_expected() {
        return std::visit(
            [](auto&& arg) -> Exp {
                using ArgType = std::decay_t<decltype(arg)>;
                if constexpr (std::is_same_v<typename Exp::value_type, void>) {
                    return typename Exp::value_type{};
                } else {
                    return typename Exp::value_type{};
                }
            },
            result_);
    }

    // Helper function to throw a bad_expected_access with the correct error
    // type
    void throw_bad_expected_access() const {
        if (has_error<E>()) {
            THROW_EXCEPTION(bad_expected_access<E>(error<E>()));
        } else {
            std::visit(
                [](auto&& arg) -> bad_expected_access<void> {
                    using ArgType = std::decay_t<decltype(arg)>;
                    if constexpr (!std::is_same_v<ArgType, std::monostate>) {
                        THROW_EXCEPTION(bad_expected_access<ArgType>(arg));
                    } else {
                        THROW_EXCEPTION(bad_expected_access<void>());
                    }
                },
                result_);
        }
    }

   private:
    std::variant<std::monostate, unexpected<E>, unexpected<Es>...> result_;
};

template <typename... Es>
expected(std::monostate) -> expected<void, Es...>;

/**
 * @brief Swaps the contents of two expected objects.
 *
 * @tparam T The type of the expected value.
 * @tparam Es The types of the unexpected errors.
 * @param lhs The first expected object.
 * @param rhs The second expected object.
 */
template <typename T, typename... Es>
void swap(expected<T, Es...>& lhs,
          expected<T, Es...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
    lhs.swap(rhs);
}

/**
 * @brief Helper function to create an expected instance.
 *
 * @tparam T The type of the expected value.
 * @tparam Es The types of the unexpected errors.
 * @param value The value to store in the expected object.
 * @return expected<T, Es...> The created expected object.
 */
template <typename T, typename... Es>
constexpr expected<T, Es...> make_expected(T&& value) noexcept(
    std::is_nothrow_constructible<expected<T, Es...>, T&&>::value) {
    return expected<T, Es...>(std::forward<T>(value));
}

/**
 * @brief Helper function to create an unexpected instance.
 *
 * @tparam E The type of the unexpected error.
 * @param error The unexpected error to store in the unexpected object.
 * @return unexpected<E> The created unexpected object.
 */
template <typename E>
constexpr unexpected<E> make_unexpected(E&& error) noexcept(
    std::is_nothrow_constructible<unexpected<E>, E&&>::value) {
    return unexpected<E>(std::forward<E>(error));
}

/**
 * @brief Helper function to create an unexpected instance from an error code.
 *
 * @param error The error code to store in the unexpected object.
 * @return unexpected<std::error_code> The created unexpected object.
 */
unexpected<std::error_code> make_unexpected(std::errc error) noexcept {
    return unexpected<std::error_code>(std::make_error_code(error));
}

/**
 * @brief Helper function to create an unexpected instance from an error code.
 *
 * @param error The error code to store in the unexpected object.
 * @return unexpected<std::error_code> The created unexpected object.
 */
unexpected<std::error_code> make_unexpected(std::error_code error) noexcept {
    return unexpected<std::error_code>(error);
}

}  // namespace mgutility

#endif  // MGUTILITY_EXPECTED_HPP
