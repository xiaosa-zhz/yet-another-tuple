#include <fmt/core.h>
#include "MyTuple.h"

int main()
{
    myutil::tuple<int, double, long> t0{ 1, 2, 3 };
    myutil::tuple<int, double, int> t1{ 1, 2, 3 };
    const myutil::tuple<int&, double&, int&> ref = t1;
    int n = 4;
    ref = myutil::make_tuple(5, 6, 7);
    auto result = myutil::tuple_cat(t0, myutil::tuple<int&>{ n }, std::as_const(t1), myutil::tuple<>{});
    fmt::print("({}, {}, {}, {}, {}, {}, {})",
        myutil::get<0>(result),
        myutil::get<1>(result),
        myutil::get<2>(result),
        myutil::get<3>(result),
        myutil::get<4>(result),
        myutil::get<5>(result),
        myutil::get<6>(result)
    );
}
