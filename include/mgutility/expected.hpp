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
#include <stdexcept>
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
// Helper to trigger static_assert for unhandled types
template <typename T>
struct always_false : std::false_type {};

// Define the typelist
template <typename... Types>
struct typelist {};

// Size of typelist
template <typename TList>
struct size;

template <typename... Types>
struct size<typelist<Types...>> {
    static constexpr std::size_t value = sizeof...(Types);
};

// Accessing elements
template <std::size_t N, typename TList>
struct type_at;

template <std::size_t N, typename Head, typename... Tail>
struct type_at<N, typelist<Head, Tail...>> {
    using type = typename type_at<N - 1, typelist<Tail...>>::type;
};

template <typename Head, typename... Tail>
struct type_at<0, typelist<Head, Tail...>> {
    using type = Head;
};

template <std::size_t N, typename TList>
using type_at_t = typename type_at<N, TList>::type;

// Append a type
template <typename TList, typename NewType>
struct append;

template <typename... Types, typename NewType>
struct append<typelist<Types...>, NewType> {
    using type = typelist<Types..., NewType>;
};

template <typename TList, typename NewType>
using append_t = typename append<TList, NewType>::type;

// Helper metafunction to check if a type is in a list of types
template <typename T, typename... Ts>
struct is_one_of;

template <typename T>
struct is_one_of<T> : std::false_type {
    using type = T;
};

template <typename T, typename First, typename... Rest>
struct is_one_of<T, First, Rest...>
    : std::conditional_t<std::is_same_v<T, First>, std::true_type,
                         is_one_of<T, Rest...>> {
    using type = T;
};

// Helper alias template
template <typename T, typename... Ts>
constexpr bool is_one_of_v = is_one_of<T, Ts...>::value;

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

// Helper to find the new error type corresponding to old error types
template <typename OldEs, typename NewEs>
struct find_new_error_type_impl;

template <typename... OldEs, typename... NewEs>
struct find_new_error_type_impl<typelist<OldEs...>, typelist<NewEs...>> {
    using type = typelist<NewEs...>;
};

template <typename OldE, typename... OldEs, typename NewE, typename... NewEs>
struct find_new_error_type_impl<typelist<OldE, OldEs...>,
                                typelist<NewE, NewEs...>> {
    using type = typename find_new_error_type_impl<typelist<OldEs...>,
                                                   typelist<NewE>>::type;
};

template <typename... OldEs, typename NewE, typename... NewEs>
struct find_new_error_type_impl<typelist<OldEs...>, typelist<NewE, NewEs...>> {
    using type =
        std::conditional_t<is_one_of_v<NewE, OldEs...>,
                           typename find_new_error_type_impl<
                               typelist<OldEs...>, typelist<NewEs...>>::type,
                           NewE>;
};

template <typename OldEs, typename NewEs>
using find_new_error_type_t =
    typename find_new_error_type_impl<OldEs, NewEs>::type;

// Helper type trait to check if a type T is present in a typelist Ts
template <typename T, typename TypeList>
struct is_type_in_typelist;

template <typename T>
struct is_type_in_typelist<T, typelist<>> : std::false_type {};

template <typename T, typename U, typename... Ts>
struct is_type_in_typelist<T, typelist<U, Ts...>>
    : std::conditional_t<std::is_same_v<T, U>, std::true_type,
                         is_type_in_typelist<T, typelist<Ts...>>> {};

// Primary type trait to check if all types in TypeList1 are present in
// TypeList2
template <typename TypeList1, typename TypeList2>
struct are_all_types_included;

template <typename TypeList2>
struct are_all_types_included<typelist<>, TypeList2> : std::true_type {};

template <typename T, typename... Ts, typename TypeList2>
struct are_all_types_included<typelist<T, Ts...>, TypeList2>
    : std::conditional_t<is_type_in_typelist<T, TypeList2>::value,
                         are_all_types_included<typelist<Ts...>, TypeList2>,
                         std::false_type> {};

// Helper variable template to simplify usage
template <typename TypeList1, typename TypeList2>
inline constexpr bool are_all_types_included_v =
    are_all_types_included<TypeList1, TypeList2>::value;

}  // namespace detail

template <typename E>
class unexpected;

// Helper type trait to check if a type is an unexpected type
template <typename T>
struct is_unexpected : std::false_type {};

template <typename E>
struct is_unexpected<unexpected<E>> : std::true_type {};

template <typename T>
inline constexpr bool is_unexpected_v = is_unexpected<T>::value;

template <typename E>
struct unexpect_t {
    explicit unexpect_t() = default;
};

template <typename E>
inline constexpr unexpect_t<E> unexpect{};

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
    using value_type = E;

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
    using error_types = detail::typelist<E, Es...>;

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
    constexpr explicit expected(unexpect_t<Error>, Args&&... args) noexcept(
        std::is_nothrow_constructible<Error, Args...>::value)
        : result_(std::in_place_type<unexpected<Error>>,
                  unexpected(std::in_place_t{}, std::forward<Args>(args)...)) {}

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
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
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
     * @brief Gets the value or throws a bad_expected_access exception if there
     * is no value.
     *
     * @return T& The value.
     * @throws bad_expected_access<void> If there is no value.
     */
    constexpr T& value() & {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return noexcept_get<T>();
    }

    /**
     * @brief Gets the value or throws a bad_expected_access exception if there
     * is no value.
     *
     * @return const T& The value.
     * @throws bad_expected_access<void> If there is no value.
     */
    constexpr const T& value() const& {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return noexcept_get<T>();
    }

    /**
     * @brief Gets the value or throws a bad_expected_access exception if there
     * is no value.
     *
     * @return T&& The value.
     * @throws bad_expected_access<void> If there is no value.
     */
    constexpr T&& value() && {
        if (!has_value()) {
            throw_bad_expected_access();
        }
        return std::move(noexcept_get<T>());
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return const E& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E& error() const& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return noexcept_get<unexpected<E>>().error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return E& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr E& error() & {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return noexcept_get<unexpected<E>>().error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return const E&& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E&& error() const&& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(noexcept_get<unexpected<E>>().error());
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return E&& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr E&& error() && {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(noexcept_get<unexpected<E>>().error());
    }

    /**
     * @brief Gets the unexpected error of type Err or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr const Error& error() const& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return noexcept_get<unexpected<Error>>().error();
    }

    /**
     * @brief Gets the unexpected error of type Err or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr Error& error() & {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return noexcept_get<unexpected<Error>>().error();
    }

    /**
     * @brief Gets the unexpected error of type Err or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error&& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr const Error&& error() const&& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(noexcept_get<unexpected<Error>>().error());
    }

    /**
     * @brief Gets the unexpected error of type Err or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error&& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr Error&& error() && {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(noexcept_get<unexpected<Error>>().error());
    }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return T& The value.
     */
    constexpr T& operator*() & noexcept { return noexcept_get<T>(); }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return const T& The value.
     */
    constexpr const T& operator*() const& noexcept { return noexcept_get<T>(); }

    /**
     * @brief Dereference operator to access the value.
     *
     * @return T&& The value.
     */
    constexpr T&& operator*() && noexcept {
        return std::move(*this).template noexcept_get<T>();
    }

    /**
     * @brief Arrow operator to access the value.
     *
     * @return T* Pointer to the value.
     */
    constexpr T* operator->() noexcept {
        return std::addressof(noexcept_get<T>());
    }

    /**
     * @brief Arrow operator to access the value.
     *
     * @return const T* Pointer to the value.
     */
    constexpr const T* operator->() const noexcept {
        return std::addressof(noexcept_get<T>());
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
        return has_value() ? operator*()
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
        return has_value() ? std::move(operator*())
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
        return has_error<Error>() ? noexcept_get<unexpected<Error>>().error()
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
                   ? std::move(noexcept_get<unexpected<Error>>().error())
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;

        static_assert(
            std::is_same_v<T, Arg> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
        if (has_value()) {
            return f(operator*());
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;

        static_assert(
            std::is_same_v<T, Arg> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
        if (has_value()) {
            return f(operator*());
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;

        static_assert(
            std::is_same_v<T, Arg> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
        if (has_value()) {
            return f(std::move(operator*()));
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<T, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<T, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<T, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<T, Arg>,
            "Argument type should be same type of expected value type.");
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<T, Arg>,
            "Argument type should be same type of expected value type.");
        if (has_value()) {
            return Result(f(operator*()));
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<T, Arg>,
            "Argument type should be same type of expected value type.");
        if (has_value()) {
            return Result(f(std::move(operator*())));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) & noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(
                unexpected(f(noexcept_get<unexpected<Arg>>().error())));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(
                unexpected(f(noexcept_get<unexpected<Arg>>().error())));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) && noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<T, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(unexpected(
                f(std::move(noexcept_get<unexpected<Arg>>().error()))));
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
        return noexcept_get<T>();
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
    template <typename Type>
    Type& noexcept_get() {
        return *std::get_if<Type>(&result_);
    }

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

    // Helper to transform unexpected types
    template <typename OldE, typename Exp, typename... NewEs>
    Exp transform_unexpected(detail::typelist<NewEs...>) {
        return std::visit(
            [](auto&& arg) -> Exp {
                using ArgType = std::decay_t<decltype(arg)>;
                if constexpr (!std::is_same_v<ArgType, unexpected<OldE>>) {
                    return arg;
                } else {
                    using NewE = detail::find_new_error_type_t<
                        detail::typelist<Es...>, detail::typelist<NewEs...>>;
                    return unexpected(NewE{});
                }
            },
            result_);
    }

    // Helper function to throw a bad_expected_access with the correct error
    // type
    void throw_bad_expected_access() {
        if (has_value()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        } else if (has_error<E>()) {
            THROW_EXCEPTION(bad_expected_access<E>(error<E>()));
        } else {
            std::visit(
                [](auto&& arg) -> bad_expected_access<void> {
                    using ArgType = std::decay_t<decltype(arg)>;
                    if constexpr (!std::is_same_v<ArgType, std::monostate> &&
                                  !std::is_same_v<ArgType, T> &&
                                  !std::is_same_v<ArgType, E>) {
                        THROW_EXCEPTION(
                            bad_expected_access<typename ArgType::value_type>(
                                arg.error()));
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
    using error_types = detail::typelist<E, Es...>;

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
    constexpr explicit expected(unexpect_t<Error>, Args&&... args) noexcept(
        std::is_nothrow_constructible<Error, Args...>::value)
        : result_(std::in_place_type<unexpected<Error>>,
                  unexpected<Error>(std::in_place_t{},
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
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
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
     * @throws bad_expected_access<E> If there is no value.
     */
    constexpr void value() const {
        if (!has_value()) {
            throw_bad_expected_access();
        }
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return const E& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E& error() const& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return noexcept_get<unexpected<E>>().error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return E& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr E& error() & {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return noexcept_get<unexpected<E>>().error();
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return const E&& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr const E&& error() const&& {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(noexcept_get<unexpected<E>>().error());
    }

    /**
     * @brief Gets the unexpected error of type E or throws a
     * bad_expected_access exception if there is no error.
     *
     * @return E&& The error.
     * @throws bad_expected_access<E> If there is no error.
     */
    template <bool HasSingleError = (sizeof...(Es) == 0),
              std::enable_if_t<HasSingleError, int> = 0>
    constexpr E&& error() && {
        if (has_value() || !has_error()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        }
        return std::move(noexcept_get<unexpected<E>>().error());
    }

    /**
     * @brief Gets the unexpected error of the given type or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr const Error& error() const& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return noexcept_get<unexpected<Error>>().error();
    }

    /**
     * @brief Gets the unexpected error of the given type or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr Error& error() & {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return noexcept_get<unexpected<Error>>().error();
    }

    /**
     * @brief Gets the unexpected error of the given type or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return const Error&& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr const Error&& error() const&& {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(noexcept_get<unexpected<Error>>().error());
    }

    /**
     * @brief Gets the unexpected error of the given type or throws a
     * bad_expected_access exception if there is no error.
     *
     * @tparam Error The type of the error.
     * @return Error&& The error.
     * @throws bad_expected_access<Error> If there is no error.
     */
    template <typename Error>
    constexpr Error&& error() && {
        if (has_value() || !has_error<Error>()) {
            throw_bad_expected_access();
        }
        return std::move(noexcept_get<unexpected<Error>>().error());
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
        static_assert(
            std::is_invocable_v<F> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
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
        static_assert(
            std::is_invocable_v<F> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
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
        static_assert(
            std::is_invocable_v<F> &&
                detail::are_all_types_included_v<error_types,
                                                 typename RetType::error_types>,
            "Argument type should be same type of expected value type and the "
            "function should include all unexpected types from current "
            "expected object.");
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<void, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<void, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        using RetType = std::invoke_result_t<F, Arg>;
        using NewExpected = std::decay_t<RetType>;
        using NewExpectedType = typename NewExpected::value_type;
        using NewErrorType = typename NewExpected::error_type;
        using NewErrorTypes = typename NewExpected::error_types;

        static_assert(std::is_same_v<void, NewExpectedType> &&
                          (detail::is_one_of_v<Arg, E, Es...> ||
                           std::is_same_v<E, NewErrorType> ||
                           std::is_same_v<error_types, NewErrorTypes>),
                      "The functor must return an expected type with the same "
                      "value type.");

        if (!has_value() && has_error<Arg>()) {
            return f(noexcept_get<unexpected<Arg>>().error());
        } else {
            return transform_unexpected<Arg, NewExpected>(NewErrorTypes{});
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<void, Arg>,
            "Argument type should be same type of expected value type.");
        if (has_value()) {
            return Result(f());
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<void, Arg>,
            "Argument type should be same type of expected value type.");
        if (has_value()) {
            return Result(f());
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
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(
            std::is_same_v<void, Arg>,
            "Argument type should be same type of expected value type.");
        if (has_value()) {
            return Result(f());
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) & noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(
                unexpected(f(noexcept_get<unexpected<Arg>>().error())));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) const& noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(
                unexpected(f(noexcept_get<unexpected<Arg>>().error())));
        }
        return *this;
    }

    /**
     * @brief Calls the provided function with the error if present and of the
     * matching type, and returns a new expected object with the transformed
     * error.
     *
     * @tparam F The type of the function to call.
     * @param f The function to call with the error.
     * @return A new expected object with the transformed error or the original
     * expected object.
     */
    template <typename F>
    constexpr auto transform_error(F&& f) && noexcept(
        std::is_nothrow_invocable<F, detail::first_argument<F>>::value) {
        using Result = expected<void, E, Es...>;
        using Arg = detail::remove_cvref_t<detail::first_argument<F>>;
        static_assert(detail::is_one_of_v<Arg, E, Es...>,
                      "Argument type should be same type from the one of "
                      "unexpected types.");

        if (!has_value() && has_error<Arg>()) {
            return Result(unexpected(
                f(std::move(noexcept_get<unexpected<Arg>>().error()))));
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
    template <typename Type>
    Type& noexcept_get() {
        return *std::get_if<Type>(&result_);
    }

    template <typename Exp>
    Exp transform_expected() {
        return std::visit(
            [](auto&&) -> Exp {
                if constexpr (std::is_same_v<typename Exp::value_type, void>) {
                    return typename Exp::value_type{};
                } else {
                    return typename Exp::value_type{};
                }
            },
            result_);
    }

    // Helper to transform unexpected types
    template <typename OldE, typename Exp, typename... NewEs>
    Exp transform_unexpected(detail::typelist<NewEs...>) {
        return std::visit(
            [](auto&& arg) -> Exp {
                using ArgType = std::decay_t<decltype(arg)>;
                if constexpr (!std::is_same_v<ArgType, unexpected<OldE>>) {
                    return arg;
                } else {
                    using NewE = detail::find_new_error_type_t<
                        detail::typelist<E, Es...>,
                        detail::typelist<typename Exp::error_type, NewEs...>>;
                    return unexpected(NewE{});
                    // static_assert(detail::always_false<ArgType>::value,
                    // "Unhandled type in transform_unexpected");
                }
            },
            result_);
    }

    // Helper function to throw a bad_expected_access with the correct error
    // type
    void throw_bad_expected_access() {
        if (has_value()) {
            THROW_EXCEPTION(bad_expected_access<void>());
        } else if (has_error<E>()) {
            THROW_EXCEPTION(bad_expected_access<E>(error<E>()));
        } else {
            std::visit(
                [](auto&& arg) -> bad_expected_access<void> {
                    using ArgType = std::decay_t<decltype(arg)>;
                    if constexpr (!std::is_same_v<ArgType, std::monostate> &&
                                  !std::is_same_v<ArgType, E>) {
                        THROW_EXCEPTION(
                            bad_expected_access<typename ArgType::value_type>(
                                arg.error()));
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

}  // namespace mgutility

#endif  // MGUTILITY_EXPECTED_HPP
