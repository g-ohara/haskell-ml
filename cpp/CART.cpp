#include <iostream>
#include <array>
#include <vector>

const int F = 2;

typedef std::array<bool, F> Features;
typedef bool Label;
struct Point
{
    Features feature;
    Label label;
};
typedef std::vector<Point> Data;

struct Node;

typedef std::array<Node*, 2> Children;

struct Node
{
    Children child = {nullptr};
    Label _label;
    int literal = 0;

    Node(void);
    Node(Label label);
    ~Node(void);
};

Node::Node(void)
{
    for (Node* trg: child)
        trg = nullptr;
    _label = 0;
}

Node::Node(Label label) : Node{}
{
    _label = label;
}

Node::~Node(void)
{
    for (Node* trg: child)
        if (trg != nullptr) delete trg;
}

bool homogeneous(Data D)
{
    int true_point_num = 0;
    for (const Point& trg: D)
        if(trg.label)
            ++true_point_num;
    
    return (true_point_num == 0 || true_point_num == (int)D.size());
}

Node* label(Data D)
{
    return new Node{D.at(0).label};
}

double Imp(Data D)
{
    int true_point_num = 0;
    for (const Point& trg: D)
        if(trg.label)
            ++true_point_num;
    
    double p = (double)true_point_num / (double)D.size();
    return 2.0 * p * (1.0 - p);
}

double Imp(Data D_[2])
{
    double gini = 0;
    int D_size = D_[0].size() + D_[1].size();
    for (int i = 0; i <= 1; ++i)
        gini += (double)D_[i].size() * Imp(D_[i]) / (double)D_size;
    
    return gini;
}

void split_data(Data D_[2], const Data& D, int f)
{
    for (const Point& trg: D)
        D_[trg.feature.at(f)].push_back(trg);
}

int best_split(Data D, int F)
{
    int f_best = 0;
    double I_min = 1.0;
    for (int f = 0; f < F; ++f)
    {
        Data D_[2];
        split_data(D_, D, f);
        double imp = Imp(D_);
        if (imp < I_min)
        {
            I_min = imp;
            f_best = f;
        }
    }
    return f_best;
}

Node* grow_tree(Data D, int F)
{
    // 1
    if (homogeneous(D)) return label(D);

    Node* tree = new Node;

    // 2
    int S = best_split(D, F);
    tree->literal = S;
    
    // 3. Sに含まれるリテラルに従ってDを部分集合D_iに分割する;
    Data D_[2];
    split_data(D_, D, S);

    // 4 - 6.
    for (int i = 0; i <= 1; ++i)
        if (D_[i].empty())
            tree->child[i] = label(D);
        else
            tree->child[i] = grow_tree(D_[i], F);

    return tree; // 根ノードがSでラベル付けされ, 子がT_iである木
}

void display_shape(int depth)
{
    std::cout << "|";
    for (int i = 0; i < depth; ++i)
        std::cout << "   |";
    for (int i = 0; i < 3; ++i)
        std::cout << "-";
    std::cout << " ";
}

void display(const Node* node, int depth = 0)
{
    bool leaf = true;
    for (const Node* trg : node->child)
        if (trg != nullptr)
            leaf = false;


    if (leaf)
    {
        display_shape(depth);
        std::cout << "class: " << node->_label << std::endl;
    }
    else
    {
        for (int i = 0; i < 2; ++i)
        {
            display_shape(depth);
            std::cout << "Feature[" << node->literal << "] == " << i << std::endl;
            const Node* trg = node->child[i];
            if (trg != nullptr)
                display(trg, depth + 1);
        }
    }
}

int main(void)
{
    Data D;
    D.push_back(Point{Features{0,0},0});
    D.push_back(Point{Features{0,1},1});
    D.push_back(Point{Features{1,0},1});
    D.push_back(Point{Features{1,1},0});

    display(grow_tree(D, F));
    return 0;
}
