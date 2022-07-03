#ifndef MY_UTILITY_TUPLE_H
#define MY_UTILITY_TUPLE_H

#include <concepts>
#include <compare>
#include <utility>
#include <array>
#include <memory>

namespace myutil
{
    // forward declaration
    template<class... Types>
    class tuple;

    // forward declaration
    template<std::size_t I, class Tuple>
    class tuple_element;

    template<std::size_t I, class Tuple>
    using tuple_element_t = typename tuple_element<I, Tuple>::type;

    template<typename Tuple>
    struct tuple_size;

    template<typename... Types>
    struct tuple_size<tuple<Types...>> : std::integral_constant<std::size_t, sizeof...(Types)> {};

    template<typename Tuple>
    inline constexpr std::size_t tuple_size_v = tuple_size<Tuple>::value;

    namespace details
    {
        template<typename... T>
        struct first { using type = first; }; // only when sizeof...(T) == 0, always mismatch

        template<typename T, typename... U>
        struct first<T, U...> { using type = T; };

        template<typename... Ts>
        using first_t = typename first<Ts...>::type;

        template<class T, template<class...> class TMP>
        inline constexpr bool is_instant_v = false;
        
        template<template<class...> class TMP, class... Ts>
        inline constexpr bool is_instant_v<TMP<Ts...>, TMP> = true;

        template<class... Ts>
        struct Pack {};

        template<typename T>
        void test_implicit(T t) {}

        template<typename T>
        concept copy_list_initializable = requires { test_implicit<T>({}); };

        template<std::size_t Index, class Type>
        struct tuple_element_unit
        {
            using type = Type;
            constexpr static std::size_t index = Index;
            [[no_unique_address]] Type element{};
            tuple_element_unit(auto&&... args) : element(decltype(args)(args)...) {}
        };

        template<typename index_sequence_t, class... Ts>
        struct tuple_base;

        template<std::size_t... Is, class... Ts>
        struct tuple_base<std::index_sequence<Is...>, Ts...> : tuple_element_unit<Is, Ts>...
        {
            tuple_base() = default;
            tuple_base(const Ts&... args) requires(sizeof...(Ts) > 0) : tuple_element_unit<Is, Ts>{args}... {}
            template<class... Us>
            tuple_base(Us&&... args) : tuple_element_unit<Is, Ts>{ std::forward<Us>(args) }... {}
        };

        template<std::size_t Is, class Ts>
        tuple_element_unit<Is, Ts>& get_tuple_element_unit(tuple_element_unit<Is, Ts>& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<std::size_t Is, class Ts>
        tuple_element_unit<Is, Ts>&& get_tuple_element_unit(tuple_element_unit<Is, Ts>&& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<std::size_t Is, class Ts>
        const tuple_element_unit<Is, Ts>& get_tuple_element_unit(const tuple_element_unit<Is, Ts>& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<std::size_t Is, class Ts>
        const tuple_element_unit<Is, Ts>&& get_tuple_element_unit(const tuple_element_unit<Is, Ts>&& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<class Ts, std::size_t Is>
        tuple_element_unit<Is, Ts>& get_tuple_element_unit(tuple_element_unit<Is, Ts>& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<class Ts, std::size_t Is>
        tuple_element_unit<Is, Ts>&& get_tuple_element_unit(tuple_element_unit<Is, Ts>&& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<class Ts, std::size_t Is>
        const tuple_element_unit<Is, Ts>& get_tuple_element_unit(const tuple_element_unit<Is, Ts>& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<class Ts, std::size_t Is>
        const tuple_element_unit<Is, Ts>&& get_tuple_element_unit(const tuple_element_unit<Is, Ts>&& element_unit) {
            return decltype(element_unit)(element_unit);
        }

        template<typename T, typename... Us>
        struct test_fetch
        {
            static_assert((std::is_same_v<T, Us> + ...) == 1, "Ambiguous type matching.");
            using type = T;
        };

        template<class B>
        concept boolean_testable_impl =
            std::convertible_to<B, bool>;

        template<class B>
        concept boolean_testable =
            boolean_testable_impl<B> &&
            requires (B&& b) {
                { !std::forward<B>(b) } -> boolean_testable_impl;
            };

        template<typename T, typename U>
        requires std::three_way_comparable_with<T, U>
        || (requires (const T& t, const U& u) { { t < u } -> boolean_testable; })
        constexpr auto synth_three_way(const T& lhs, const U& rhs) {
            if constexpr (std::three_way_comparable_with<T, U>) {
                return lhs <=> rhs;
            }
            else {
                return lhs < rhs ? std::weak_ordering::less :
                    rhs < lhs ? std::weak_ordering::greater :
                    std::weak_ordering::equivalent;
            }
        }

        struct ignore_t
        {
            template <typename T>
            constexpr void operator=(T&&) const noexcept {}
        };

        template<typename... T>
        inline constexpr bool always_false_v = false;

        template<class Tuple1, class Tuple2>
        struct concat_impl {
            static_assert(always_false_v<Tuple1, Tuple2>, "Unsupported tuple_cat arguments.");
        };

        template<class... Ts, class... Us>
        struct concat_impl<tuple<Ts...>, tuple<Us...>> {
            using type = tuple<Ts..., Us...>;
        };

        template<class... Tuples>
        struct concat;

        template<class Tuple1, class Tuple2, class... Rest>
        struct concat<Tuple1, Tuple2, Rest...>{
            using type = typename concat<typename concat_impl<Tuple1, Tuple2>::type, Rest...>::type;
        };

        template<class Tuple>
        struct concat<Tuple> {
            using type = Tuple;
        };

        template<>
        struct concat<> {
            using type = tuple<>;
        };

        template<class... Seq>
        struct sequence_cat;

        template<std::size_t... I, std::size_t... J, class... Rest>
        struct sequence_cat<std::index_sequence<I...>, std::index_sequence<J...>, Rest...> {
            using type = typename sequence_cat<std::index_sequence<I..., J...>, Rest...>::type;
        };

        template<std::size_t... I, std::size_t... J>
        struct sequence_cat<std::index_sequence<I...>, std::index_sequence<J...>> {
            using type = std::index_sequence<I..., J...>;
        };

        template<typename Seq>
        struct sequence_cat<Seq> {
            using type = Seq;
        };

        template<>
        struct sequence_cat<> {
            using type = std::index_sequence<>;
        };

        template<std::size_t To, typename Seq>
        struct transform_to_same;

        template<std::size_t To, std::size_t... I>
        struct transform_to_same<To, std::index_sequence<I...>> {
            using type = std::index_sequence<((void)I, To)...>;
        };

        template<class... Tuples>
        consteval std::array<std::size_t, (tuple_size_v<Tuples> + ...)> outer_index() {
            return [] <std::size_t... I>(std::index_sequence<I...>) {
                return [] <std::size_t... J>(std::index_sequence<J...>) {
                    return std::to_array<std::size_t>({J...});
                }(typename sequence_cat<
                    typename transform_to_same<I, std::make_index_sequence<tuple_size_v<Tuples>>>::type...
                >::type{});
            }(std::make_index_sequence<sizeof...(Tuples)>());
        }

        template<class... Tuples>
        consteval std::array<std::size_t, (tuple_size_v<Tuples> + ...)> inner_index() {
            return [] <std::size_t... I>(std::index_sequence<I...>) {
                return std::to_array<std::size_t>({I...});
            }(typename sequence_cat<std::make_index_sequence<tuple_size_v<Tuples>>...>::type{});
        }

        template<class Tuple>
        inline constexpr bool is_tuple_like_v = false;

        template<class... Types>
        inline constexpr bool is_tuple_like_v<myutil::tuple<Types...>> = true;

        template<std::size_t N, typename Pack1, typename Pack2>
        inline constexpr bool transform_test = true;

        template<class... Types, class... UTypes>
        inline constexpr bool transform_test<1, tuple<Types...>, tuple<UTypes...>&> =
            not (std::is_convertible_v<tuple<UTypes...>&, Types...>
            || std::is_constructible_v<Types..., tuple<UTypes...>&>
            || std::is_same_v<Types..., UTypes...>);

        template<class... Types, class... UTypes>
        inline constexpr bool transform_test<1, tuple<Types...>, const tuple<UTypes...>&> =
            not (std::is_convertible_v<const tuple<UTypes...>&, Types...>
            || std::is_constructible_v<Types..., const tuple<UTypes...>&>
            || std::is_same_v<Types..., UTypes...>);

        template<class... Types, class... UTypes>
        inline constexpr bool transform_test<1, tuple<Types...>, tuple<UTypes...>&&> =
            not (std::is_convertible_v<tuple<UTypes...>, Types...>
            || std::is_constructible_v<Types..., tuple<UTypes...>>
            || std::is_same_v<Types..., UTypes...>);

        template<class... Types, class... UTypes>
        inline constexpr bool transform_test<1, tuple<Types...>, const tuple<UTypes...>&&> =
            not (std::is_convertible_v<const tuple<UTypes...>, Types...>
            || std::is_constructible_v<Types..., const tuple<UTypes...>>
            || std::is_same_v<Types..., UTypes...>);

    } // namespace myutil::details

    template<std::size_t I, class... Types>
    struct tuple_element<I, myutil::tuple<Types...>>
    {
        static_assert(I < sizeof...(Types), "Index out of bound.");
        using type = typename std::remove_reference_t<decltype(details::get_tuple_element_unit<I>(
            std::declval<details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>&>()))>::type;
    };

    template<std::size_t I, class... Types>
    constexpr typename tuple_element<I, myutil::tuple<Types...>>::type&
        get(myutil::tuple<Types...>& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>&;
        return details::get_tuple_element_unit<I>((Base)t).element;
    }

    template<std::size_t I, class... Types>
    constexpr typename tuple_element<I, myutil::tuple<Types...>>::type&&
        get(myutil::tuple<Types...>&& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>&&;
        using RtType = typename tuple_element<I, myutil::tuple<Types...>>::type&&;
        return static_cast<RtType>(details::get_tuple_element_unit<I>((Base)t).element);
    }

    template<std::size_t I, class... Types>
    constexpr typename tuple_element<I, myutil::tuple<Types...>>::type const&
        get(myutil::tuple<Types...> const& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...> const&;
        using RtType = typename tuple_element<I, myutil::tuple<Types...>>::type const&;
        return static_cast<RtType>(details::get_tuple_element_unit<I>((Base)t).element);
    }

    template<std::size_t I, class... Types>
    constexpr typename tuple_element<I, myutil::tuple<Types...>>::type const&&
        get(myutil::tuple<Types...> const&& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...> const&&;
        using RtType = typename tuple_element<I, myutil::tuple<Types...>>::type const&&;
        return static_cast<RtType>(details::get_tuple_element_unit<I>((Base)t).element);
    }

    template<typename T, class... Types>
    constexpr typename details::test_fetch<T, Types...>::type&
        get(myutil::tuple<Types...>& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>&;
        using RtType = T&;
        return static_cast<RtType>(details::get_tuple_element_unit<T>((Base)t).element);
    }

    template<typename T, class... Types>
    constexpr typename details::test_fetch<T, Types...>::type&&
        get(myutil::tuple<Types...>&& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>&&;
        using RtType = T&&;
        return static_cast<RtType>(details::get_tuple_element_unit<T>((Base)t).element);
    }

    template<typename T, class... Types>
    constexpr typename details::test_fetch<T, Types...>::type const&
        get(myutil::tuple<Types...> const& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...> const&;
        using RtType = const T&;
        return static_cast<RtType>(details::get_tuple_element_unit<T>((Base)t).element);
    }

    template<typename T, class... Types>
    constexpr typename details::test_fetch<T, Types...>::type const&&
        get(myutil::tuple<Types...> const&& t) noexcept {
        using Base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...> const&&;
        using RtType = const T&&;
        return static_cast<RtType>(details::get_tuple_element_unit<T>((Base)t).element);
    }

    template<class... Types>
    class tuple : private details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>
    {
        using base = details::tuple_base<std::make_index_sequence<sizeof...(Types)>, Types...>;
    public:
        constexpr explicit(not (details::copy_list_initializable<Types> && ...))
        tuple() requires (std::is_default_constructible_v<Types> && ...) = default;

        constexpr explicit(not (std::is_convertible_v<const Types&, Types> && ...))
        tuple(const Types&... args) requires (sizeof...(Types) >= 1) && (std::is_copy_constructible_v<Types> && ...)
            : base(args...) {}

        template<class... UTypes> requires
            (sizeof...(UTypes) == sizeof...(Types) && sizeof...(Types) >= 1)
            && (std::is_constructible_v<Types, UTypes> && ...)
            && ( (sizeof...(Types) > 3)
                 || (sizeof...(Types) == 1 && not details::is_instant_v<std::remove_cvref_t<details::first_t<UTypes...>>, tuple>)
                 || (
                      (sizeof...(Types) == 2 || sizeof...(Types) == 3)
                      && (not std::is_same_v<std::remove_cvref_t<details::first_t<UTypes...>>, std::allocator_arg_t>
                         || std::is_same_v<details::first_t<Types...>, std::allocator_arg_t>
                      )
                 )
            )
        constexpr explicit(not (std::is_convertible_v<UTypes, Types> && ...))
        tuple(UTypes&&... args) : base(std::forward<UTypes>(args)...) {}

        template<class... UTypes> requires
            (sizeof...(UTypes) == sizeof...(Types))
            && (std::is_constructible_v<Types, UTypes&> && ...)
            && (details::transform_test<sizeof...(Types), tuple<Types...>, tuple<UTypes...>&>)
        constexpr explicit(not (std::is_convertible_v<UTypes&, Types> && ...))
        tuple(tuple<UTypes...>& other)
            : base([&other]<std::size_t... Is>(std::index_sequence<Is...>) {
            return base{ get<Is>(other)... };
        }(std::make_index_sequence<sizeof...(UTypes)>())) {}

        template<class... UTypes> requires
            (sizeof...(UTypes) == sizeof...(Types))
            && (std::is_constructible_v<Types, const UTypes&> && ...)
            && (details::transform_test<sizeof...(Types), tuple<Types...>, const tuple<UTypes...>&>)
        constexpr explicit(not (std::is_convertible_v<const UTypes&, Types> && ...))
        tuple(const tuple<UTypes...>& other)
            : base([&other]<std::size_t... Is>(std::index_sequence<Is...>) {
            return base{ get<Is>(other)... };
        }(std::make_index_sequence<sizeof...(UTypes)>())) {}

        template<class... UTypes> requires
            (sizeof...(UTypes) == sizeof...(Types))
            && (std::is_constructible_v<Types, UTypes> && ...)
            && (details::transform_test<sizeof...(Types), tuple<Types...>, tuple<UTypes...>&&>)
        constexpr explicit(not (std::is_convertible_v<UTypes, Types> && ...))
        tuple(tuple<UTypes...>&& other)
            : base([&other]<std::size_t... Is>(std::index_sequence<Is...>) {
            return base{ get<Is>(other)... };
        }(std::make_index_sequence<sizeof...(UTypes)>())) {}

        template<class... UTypes> requires
            (sizeof...(UTypes) == sizeof...(Types))
            && (std::is_constructible_v<Types, const UTypes> && ...)
            && (details::transform_test<sizeof...(Types), tuple<Types...>, const tuple<UTypes...>&&>)
        constexpr explicit(not (std::is_convertible_v<const UTypes, Types> && ...))
        tuple(const tuple<UTypes...>&& other)
            : base([&other]<std::size_t... Is>(std::index_sequence<Is...>) {
            return base{ get<Is>(other)... };
        }(std::make_index_sequence<sizeof...(UTypes)>())) {}

        tuple(const tuple&) = default;
        tuple(tuple&&) = default;

        constexpr tuple& operator=(const tuple&) requires (std::is_copy_assignable_v<Types> && ...) = default;
        constexpr tuple& operator=(const tuple&) requires (not (std::is_copy_assignable_v<Types> && ...)) = delete;

        constexpr const tuple& operator=(const tuple& other) const
            requires (std::is_copy_assignable_v<const Types> && ...) {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = get<I>(other)), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }

        constexpr tuple& operator=(tuple&&) noexcept((std::is_nothrow_move_assignable_v<Types> && ...))
            requires (std::is_move_assignable_v<Types> && ...) = default;

        constexpr const tuple& operator=(tuple&& other) const
            requires (std::is_move_assignable_v<const Types> && ...) {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = std::forward<Types>(get<I>(other))), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }

        template<class... UTypes>
        requires (sizeof...(Types) == sizeof...(UTypes)) && (std::is_assignable_v<Types&, const UTypes&> && ...)
        constexpr tuple& operator=(const tuple<UTypes...>& other) {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = get<I>(other)), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }

        template<class... UTypes>
        requires (sizeof...(Types) == sizeof...(UTypes)) && (std::is_assignable_v<const Types&, const UTypes&> && ...)
        constexpr const tuple& operator=(const tuple<UTypes...>& other) const {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = get<I>(other)), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }

        template<class... UTypes>
        requires (sizeof...(Types) == sizeof...(UTypes)) && (std::is_assignable_v<Types&, UTypes> && ...)
        constexpr tuple& operator=(tuple<UTypes...>&& other) {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = std::forward<UTypes>(get<I>(other))), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }

        template<class... UTypes>
        requires (sizeof...(Types) == sizeof...(UTypes)) && (std::is_assignable_v<const Types&, UTypes> && ...)
        constexpr const tuple& operator=(tuple<UTypes...>&& other) const {
            [&, this] <std::size_t... I>(std::index_sequence<I...>) {
                ((get<I>(*this) = std::forward<UTypes>(get<I>(other))), ...);
            }(std::make_index_sequence<sizeof...(Types)>());
            return *this;
        }
    };

    template<class... UTypes>
    tuple(UTypes...)->tuple<UTypes...>;

    template<class... TTypes, class... UTypes>
    requires (sizeof...(TTypes) == sizeof...(UTypes)) && (sizeof...(TTypes) > 0)
    constexpr std::common_comparison_category_t<decltype(std::declval<TTypes>() <=> std::declval<UTypes>())...>
    operator<=>(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs) {
        using common_type = std::common_comparison_category_t<decltype(std::declval<TTypes>() <=> std::declval<UTypes>())...>;
        common_type comp_result = common_type::equivalent;
        [&] <std::size_t... I>(std::index_sequence<I...>) {
            ((comp_result = details::synth_three_way(get<I>(lhs), get<I>(rhs)), comp_result != 0) || ...);
        }(std::make_index_sequence<sizeof...(TTypes)>());
        return comp_result;
    }

    constexpr std::strong_ordering operator<=>(const tuple<>& lhs, const tuple<>& rhs) {
        return std::strong_ordering::equal;
    }

    template<class... TTypes, class... UTypes>
    requires (sizeof...(TTypes) == sizeof...(UTypes))
    constexpr bool operator==(const tuple<TTypes...>& lhs, const tuple<UTypes...>& rhs) {
        return [&] <std::size_t... I>(std::index_sequence<I...>) {
            return ((get<I>(lhs) == get<I>(rhs)) && ...);
        }(std::make_index_sequence<sizeof...(TTypes)>());
    }

    inline constexpr details::ignore_t ignore = {};

    template<class... Types>
    constexpr tuple<std::unwrap_ref_decay_t<Types>...> make_tuple(Types&&... args) {
        return tuple<std::unwrap_ref_decay_t<Types>...>(std::forward<Types>(args)...);
    }

    template<class... Types>
    constexpr tuple<Types&...> tie(Types&... args) noexcept {
        return { args... };
    }

    template<class... Types>
    constexpr tuple<Types&&...> forward_as_tuple(Types&&... args) noexcept {
        return tuple<Types&&...>{ std::forward<Types>(args)... };
    }

    template<class... Tuples>
    constexpr typename details::concat<std::remove_cvref_t<Tuples>...>::type tuple_cat(Tuples&&... args) {
        if constexpr (sizeof...(Tuples) == 0) {
            return tuple<>{};
        }
        else {
            constexpr std::array outer_index = details::outer_index<std::remove_cvref_t<Tuples>...>();
            constexpr std::array inner_index = details::inner_index<std::remove_cvref_t<Tuples>...>();
            auto tupletuple = forward_as_tuple(std::forward<Tuples>(args)...);
            // gcc and clang could not use constexpr var without capture right now
            return [&] <std::size_t... I>(std::index_sequence<I...>) {
                return typename details::concat<std::remove_cvref_t<Tuples>...>::type{
                    get<inner_index[I]>(
                        std::forward<tuple_element_t<outer_index[I], decltype(tupletuple)>>(get<outer_index[I]>(tupletuple))
                    )...
                };
            }(std::make_index_sequence<(tuple_size_v<std::remove_cvref_t<Tuples>> + ...)>());
        }
    }
} // namespace myutil

template<class... Ts, class... Us, template<class> class TQual, template<class> class UQual>
requires requires {
    typename myutil::tuple<std::common_reference_t<TQual<Ts>, UQual<Us>>...>;
}
struct std::basic_common_reference<myutil::tuple<Ts...>, myutil::tuple<Us...>, TQual, UQual> {
    using type = myutil::tuple<std::common_reference_t<TQual<Ts>, UQual<Us>>...>;
};

template<class... Ts, class... Us>
requires requires { typename myutil::tuple<std::common_type_t<Ts, Us>...>; }
struct std::common_type<myutil::tuple<Ts...>, myutil::tuple<Us...>> {
    using type = myutil::tuple<std::common_type_t<Ts, Us>...>;
};

#endif // !MY_UTILITY_TUPLE_H
