#include "robot_simulator.h"

#include <assert.h>

robot_status_t robot_create(robot_direction_t direction, int x, int y) {
    return (robot_status_t){direction, {x, y}};
}

void robot_advance(robot_status_t* robot);
void robot_advance(robot_status_t* robot) {
    switch (robot->direction) {
        case DIRECTION_NORTH:
            robot->position.y++;
            break;

        case DIRECTION_EAST:
            robot->position.x++;
            break;

        case DIRECTION_SOUTH:
            robot->position.y--;
            break;

        case DIRECTION_WEST:
            robot->position.x--;
            break;

        default:
            assert(0);
    }
}

void robot_move(robot_status_t* robot, const char* commands) {
    for (const char* command = commands; *command; ++command) {
        switch (*command) {
            case 'A':
                robot_advance(robot);
                break;

            case 'L':
                robot->direction = (robot->direction + 3) % DIRECTION_MAX;
                break;

            case 'R':
                robot->direction = (robot->direction + 1) % DIRECTION_MAX;
                break;

            default:
                assert(0);
        }
    }
}
