#include "Header.hpp"

typedef std::vector<std::string> RawPoint;
typedef std::vector<RawPoint> RawData;

void read_csv(RawData &data_set, std::string filename)
{
    std::string str_buf;
    std::string str_conma_buf;

    std::ifstream ifs(filename);

    while (std::getline(ifs, str_buf))
    {
        std::istringstream i_stream(str_buf);
        RawPoint point;
        while (getline(i_stream, str_conma_buf, ','))
            point.push_back(str_conma_buf);
        data_set.push_back(point);
    }
}

const std::map<std::string, int> label_map{
    {"Iris-setosa", 0},
    {"Iris-versicolor", 1},
    {"Iris-virginica", 2}};

void process_data(Data &data, const RawData &raw_data)
{
    for (const RawPoint &raw_point : raw_data)
    {
        if (raw_point.size() < F + 1)
            break;

        Point point;
        for (int i = 0; i < F; ++i)
            point.feature.at(i) = std::stod(raw_point.at(i));

        point.label = label_map.at(raw_point.at(F));
        data.push_back(point);
    }
}

void get_data(Data &data, const std::string filename)
{
    RawData raw_data;
    read_csv(raw_data, filename);
    process_data(data, raw_data);
}