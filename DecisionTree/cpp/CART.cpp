#include "Header.hpp"

Node::Node(void)
{
    for (Node *trg : child)
        trg = nullptr;
    _label = 0;
}

Node::Node(Label label) : Node{}
{
    _label = label;
}

Node::~Node(void)
{
    for (Node *trg : child)
        if (trg != nullptr)
            delete trg;
}

bool homogeneous(Data D, int depth, int max_depth)
{
    return depth == max_depth;
    // int true_point_num = 0;
    // for (const Point &trg : D)
    //     if (trg.label)
    //         ++true_point_num;

    // return (true_point_num == 0 || true_point_num == (int)D.size());
}

Node *label(Data D)
{
    std::array<int, L> label_cnt = {0};
    for (const Point &trg : D)
        ++label_cnt.at(trg.label);

    int major_label = 0;
    int major_count = 0;
    for (int i = 0; i < L; ++i)
    {
        if (label_cnt.at(i) > major_count)
        {
            major_label = i;
            major_count = label_cnt.at(i);
        }
    }
    return new Node{major_label};
}

double Imp(Data D)
{
    std::array<int, L> label_cnt = {0};
    for (const Point &trg : D)
        ++label_cnt.at(trg.label);

    double gini = 1.0;
    for (int cnt : label_cnt)
    {
        double p = (double)cnt / (double)D.size();
        gini -= p * p;
    }
    return gini;
}

double Imp(Data D_[2])
{
    double gini = 0;
    int D_size = D_[0].size() + D_[1].size();
    for (int i = 0; i <= 1; ++i)
        gini += (double)D_[i].size() * Imp(D_[i]) / (double)D_size;

    return gini;
}

void split_data(Data D_[2], const Data &D, Literal S)
{
    for (const Point &trg : D)
        D_[(trg.feature.at(S.feature) < S.value ? 0 : 1)].push_back(trg);
}

Literal best_split(Data D, int F)
{
    int f_best = 0;
    double val_best = 0;
    double I_min = 1.0;
    for (int f = 0; f < F; ++f)
    {
        for (const Point &point : D)
        {
            Data D_[2];
            split_data(D_, D, {f, point.feature.at(f)});
            double imp = Imp(D_);
            if (imp < I_min)
            {
                I_min = imp;
                f_best = f;
                val_best = point.feature.at(f);
            }
        }
    }
    return {f_best, val_best};
}

Node *grow_tree(Data D, int F, int depth, int max_depth)
{
    // 1
    if (homogeneous(D, depth, max_depth))
        return label(D);

    Node *tree = new Node;

    // 2
    Literal S = best_split(D, F);
    tree->literal = S;

    // 3. Sに含まれるリテラルに従ってDを部分集合D_iに分割する;
    Data D_[2];
    split_data(D_, D, S);

    // 4 - 6.
    for (int i = 0; i <= 1; ++i)
        if (D_[i].empty())
            tree->child[i] = label(D);
        else
            tree->child[i] = grow_tree(D_[i], F, depth + 1, max_depth);

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

void display(const Node *node, int depth = 0)
{
    bool leaf = true;
    for (const Node *trg : node->child)
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
            std::cout << "Feature[" << node->literal.feature << "]"
                      << (i == 0 ? " < " : " >= ")
                      << node->literal.value << std::endl;
            const Node *trg = node->child[i];
            if (trg != nullptr)
                display(trg, depth + 1);
        }
    }
}

void output_data(const Data &data)
{
    for (const Point &point : data)
    {
        for (double feat : point.feature)
            std::cout << std::setprecision(1) << feat << " ";
        std::cout << "| " << point.label << std::endl;
    }
}

int main(void)
{
    /*
    // XOR data set
    Data D;
    D.push_back(Point{Features{0, 0}, 0});
    D.push_back(Point{Features{0, 1}, 1});
    D.push_back(Point{Features{1, 0}, 1});
    D.push_back(Point{Features{1, 1}, 0});
    */
    std::cout << std::fixed;
    Data D;
    get_data(D, "../data/iris/iris.data");
    output_data(D);
    display(grow_tree(D, F, 0, 5));
    return 0;
}
