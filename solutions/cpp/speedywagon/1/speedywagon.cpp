#include "speedywagon.h"

namespace speedywagon {

// Enter your code below:

// Please don't change the interface of the uv_light_heuristic function
int uv_light_heuristic(std::vector<int> const* data_array) {
    double avg{};
    for (auto element : *data_array) {
        avg += element;
    }
    avg /= data_array->size();
    int uv_index{};
    for (auto element : *data_array) {
        if (element > avg) {
            ++uv_index;
        }
    }
    return uv_index;
}

bool connection_check(pillar_men_sensor const* sensor) { return sensor; }

int activity_counter(pillar_men_sensor const* sensor_array, size_t array_size) {
    int total_activity = 0;
    for (size_t i = 0; i < array_size; ++i) {
        total_activity += sensor_array[i].activity;
    }
    return total_activity;
}

bool alarm_control(pillar_men_sensor const* sensor) {
    return sensor and sensor->activity > 0;
}

bool uv_alarm(pillar_men_sensor const* sensor) {
    return sensor and uv_light_heuristic(&sensor->data) > sensor->activity;
}

}  // namespace speedywagon
