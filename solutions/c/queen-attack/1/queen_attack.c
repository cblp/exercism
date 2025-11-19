#include "queen_attack.h"

bool position_eq(position_t a, position_t b) {
    return a.row == b.row && a.column == b.column;
}

bool position_is_valid(position_t a) { return a.row < 8 && a.column < 8; }

attack_status_t can_attack(position_t queen_1, position_t queen_2) {
    return !position_is_valid(queen_1) || !position_is_valid(queen_2) ||
                   position_eq(queen_1, queen_2)
               ? INVALID_POSITION
           : queen_1.row == queen_2.row || queen_1.column == queen_2.column ||
                   queen_1.row + queen_1.column ==
                       queen_2.row + queen_2.column ||
                   queen_1.row - queen_1.column == queen_2.row - queen_2.column
               ? CAN_ATTACK
               : CAN_NOT_ATTACK;
}
