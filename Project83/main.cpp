#include <fmt/core.h>
#include "MyTuple.h"

struct Any
{
    explicit Any(auto&&) {}
};

int main()
{
    myutil::tuple<int, double, long> t0{ 1, 3, 3 };
    myutil::tuple<int, double, int> t1{ 1, 2, 4 };
    auto result = myutil::tuple_cat(t0, myutil::tuple{ 5 }, t1, myutil::tuple<>{});
    auto r2 = myutil::tuple_cat(t0);
}
