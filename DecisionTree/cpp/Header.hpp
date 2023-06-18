#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <iomanip>
#include <array>
#include <vector>
#include <map>

const int F = 4;
const int L = 3;

typedef std::array<double, F> Features;
typedef int Label;
struct Point
{
    Features feature;
    Label label;
};
struct Literal
{
    int feature;
    double value;
};

typedef std::vector<Point> Data;

struct Node;

typedef std::array<Node *, 2> Children;

struct Node
{
    Children child = {nullptr};
    Label _label;
    Literal literal = {0, 0};

    Node(void);
    Node(Label label);
    ~Node(void);
};

void get_data(Data &data, const std::string filename);