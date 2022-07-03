#ifndef MY_UTILITY_ALLOC_UNIQUE_H
#define MY_UTILITY_ALLOC_UNIQUE_H

#include <memory>

namespace myutil {

    namespace details {

        template<typename Alloc, typename T>
        using rebind = typename std::allocator_traits<std::decay_t<Alloc>>::template rebind_alloc<T>;

        template<typename T>
        inline constexpr bool always_false = false;

        template<typename Alloc, typename T, std::size_t N>
        inline void destruction_recur(const Alloc& alloc, T (&p)[N]) {
            using value_type = typename std::allocator_traits<Alloc>::value_type;
            for (T& elem : arr) {
                if constexpr (std::is_array_v<value_type>) {
                    destruction_recur(alloc, elem);
                } else {
                    std::allocator_traits<Alloc>::destroy(a, &elem);
                }
            }
        }

        template<typename Alloc, typename T1, typename T2, std::size_t N>
        inline void construction_recur(const Alloc& alloc, T1 (&p)[N], T2 (&source)[N]) {
            for (std::size_t index = 0; index < N; ++index) {
                if constexpr (std::is_array_v<T1>) {
                    try {
                        construction_recur(alloc, p[index], source[index]);
                    } catch (...) {
                        // The obj caused exception will not get destroyed here
                        for (std::size_t rindex = 0; rindex < index; ++rindex) {
                            destruction_recur(alloc, p[index - rindex - 1]);
                        }
                        throw;
                    }
                } else {
                    try {
                        std::allocator_traits<Alloc>::construct(alloc, p + index, source[index]);
                    } catch (...) {
                        // The obj caused exception will not get destroyed here
                        for (std::size_t rindex = 0; rindex < index; ++rindex) {
                            std::allocator_traits<Alloc>::destroy(alloc, p + (index - rindex - 1));
                        }
                        throw;
                    }
                }
            }
        }

        template<typename Alloc, typename T, std::size_t N>
        inline void default_construct_recur(const Alloc& alloc, T(&p)[N]) {
            for (std::size_t index = 0; index < N; ++index) {
                if constexpr (std::is_array_v<T>) {
                    try {
                        default_construct_recur(alloc, p[index]);
                    } catch (...) {
                        // The obj caused exception will not get destroyed here
                        for (std::size_t rindex = 0; rindex < index; ++rindex) {
                            destruction_recur(alloc, p[index - rindex - 1]);
                        }
                        throw;
                    }
                } else {
                    try {
                        std::allocator_traits<Alloc>::construct(alloc, p + index);
                    } catch (...) {
                        // The obj caused exception will not get destroyed here
                        for (std::size_t rindex = 0; rindex < index; ++rindex) {
                            std::allocator_traits<Alloc>::destroy(alloc, p + (index - rindex - 1));
                        }
                        throw;
                    }
                }
            }
        }

        template<typename Alloc, std::size_t N = 1>
        class deleter_for_allocator
        {
            Alloc allocator;
        public:
            template<class OtherAlloc>
            explicit deleter_for_allocator(OtherAlloc&& other) : allocator(std::forward<OtherAlloc>(other)) {}

            deleter_for_allocator(const deleter_for_allocator&) = default;
            deleter_for_allocator(deleter_for_allocator&&) = default;

            void operator() (auto* p) const {
                for (std::size_t index = 0; index < N; ++index) {
                    destruction_recur(this->allocator, *(p + (N - 1) - index));
                }
                std::allocator_traits<Alloc>::deallocate(this->allocator,
                    reinterpret_cast<typename std::allocator_traits<Alloc>::value_type*>(p), N);
            }
        };

        template<typename Alloc>
        class dynarr_deleter_for_allocator
        {
            Alloc allocator;
            std::size_t size;
        public:
            template<class OtherAlloc>
            explicit dynarr_deleter_for_allocator(OtherAlloc&& other, std::size_t size) 
                : allocator(std::forward<OtherAlloc>(other)), size(size) {}

            dynarr_deleter_for_allocator(const dynarr_deleter_for_allocator&) = default;
            dynarr_deleter_for_allocator(dynarr_deleter_for_allocator&&) = default;

            void operator() (auto* p) const {
                for (std::size_t index = 0; index < this->size; ++index) {
                    destruction_recur(this->allocator, *(p + (this->size - 1) - index));
                }
                std::allocator_traits<Alloc>::deallocate(this->allocator,
                    reinterpret_cast<typename std::allocator_traits<Alloc>::value_type*>(p), N);
            }
        };

        template<typename T>
        struct allocate_unique_fn
        {
            static_assert(std::is_object_v<T>);

            template<class Alloc, class... Args>
            std::unique_ptr<T, deleter_for_allocator<rebind<Alloc, T>>>
                operator() (const Alloc& alloc, Args&&... args) const {
                using alloc_t = rebind<Alloc, T>;
                alloc_t rebind_alloc(alloc);
                auto* p = std::allocator_traits<alloc_t>::allocate(rebind_alloc, 1);
                try {
                    std::allocator_traits<alloc_t>::construct(p, std::forward<Args>(args)...);
                } catch (...) {
                    std::allocator_traits<alloc_t>::deallocate(rebind_alloc, p, 1);
                    throw;
                }
                return std::unique_ptr<T, deleter_for_allocator<alloc_t>>{
                    p, deleter_for_allocator<alloc_t>{ std::move(rebind_alloc) }
                };
            }
        };

        template<typename T, std::size_t N>
        struct allocate_unique_fn<T[N]>
        {
            static_assert(std::is_object_v<T>);

            template<class Alloc>
            std::unique_ptr<T, deleter_for_allocator<rebind<Alloc, std::remove_cv_t<std::remove_all_extents_t<T>>>, N>>
                operator() (const Alloc& alloc, const std::remove_extent_t<T[N]>& source) const {
                using alloc_t = rebind<Alloc, std::remove_cv_t<std::remove_all_extents_t<T>>>;
                alloc_t rebind_alloc(alloc);
                auto* p = std::allocator_traits<alloc_t>::allocate(rebind_alloc, N);
                auto& alias = *reinterpret_cast<T(*)[N]>(p);
                try {
                    construction_recur(rebind_alloc, alias, source);
                } catch (...) {
                    std::allocator_traits<alloc_t>::deallocate(rebind_alloc, p, N);
                    throw;
                }
                return std::unique_ptr<T[N], deleter_for_allocator<alloc_t>>{
                    p, deleter_for_allocator<alloc_t>{ std::move(rebind_alloc) }
                };
            }
        };

        template<typename T>
        struct allocate_unique_fn<T[]>
        {
            static_assert(std::is_object_v<T>);

            template<class Alloc>
            std::unique_ptr<T, dynarr_deleter_for_allocator<rebind<Alloc, std::remove_cv_t<std::remove_all_extents_t<T>>>>>
                operator() (const Alloc& alloc, std::size_t size) const {
                using alloc_t = rebind<Alloc, std::remove_cv_t<std::remove_all_extents_t<T>>>;
                alloc_t rebind_alloc(alloc);
                auto* p = std::allocator_traits<alloc_t>::allocate(rebind_alloc, size);
                try {
                    construction_recur(rebind_alloc, alias, source);
                } catch (...) {
                    destruction_recur(rebind_alloc, alias);
                    std::allocator_traits<alloc_t>::deallocate(rebind_alloc, p, N);
                    throw;
                }
                return std::unique_ptr<T[N], deleter_for_allocator<alloc_t>>{
                    p, deleter_for_allocator<alloc_t>{ std::move(rebind_alloc) }
                };
            }
        };

        template<typename T>
        struct allocate_unique_for_overwrite_fn
        {
            static_assert(std::is_object_v<T>);
            template<class Alloc>
            requires (not std::is_array_v<T>)
            std::unique_ptr<T, deleter_for_allocator<rebind<Alloc, T>>>
                operator() (const Alloc& alloc) const {

            }
        };

    } // namespace myutil::details

    template<typename T>
    inline constexpr details::allocate_unique_fn<T> allocate_unique = {};

    template<typename T>
    inline constexpr details::allocate_unique_for_overwrite_fn<T> allocate_unique_for_overwrite = {};

} // namespace myutil

#endif // !MY_UTILITY_ALLOC_UNIQUE_H
