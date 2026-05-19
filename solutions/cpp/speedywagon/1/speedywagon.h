#pragma once

#include <string>
#include <vector>

namespace speedywagon {

using namespace std;

struct pillar_men_sensor {
    int activity;
    string location;
    vector<int> data;
};

int uv_light_heuristic(vector<int> const* data_array);

bool connection_check(pillar_men_sensor const*);

int activity_counter(pillar_men_sensor const* sensor_array, size_t array_size);

bool alarm_control(pillar_men_sensor const*);

bool uv_alarm(pillar_men_sensor const*);

}  // namespace speedywagon
