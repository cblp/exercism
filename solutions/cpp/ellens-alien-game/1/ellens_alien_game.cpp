namespace targets {

const int START_HEALTH = 3;

class Alien {
   public:
    int x_coordinate, y_coordinate;

   private:
    int health;

   public:
    Alien(const int x_coordinate, const int y_coordinate)
        : x_coordinate(x_coordinate),
          y_coordinate(y_coordinate),
          health(START_HEALTH) {}

    bool collision_detection(const Alien other) const {
        return x_coordinate == other.x_coordinate
           and y_coordinate == other.y_coordinate;
    }

    int get_health() const { return health; }

    bool hit() {
        if (is_alive()) {
            health--;
            return true;
        }
        return false;
    }

    bool is_alive() const { return health > 0; }

    bool teleport(const int x, const int y) {
        x_coordinate = x;
        y_coordinate = y;
        return true;
    }
};
}  // namespace targets
