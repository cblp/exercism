#pragma once

namespace space_age {

struct space_age {
    long _seconds;

    space_age(long seconds) : _seconds(seconds) {}

    long seconds() const { return _seconds; }

    double on_earth() const { return _seconds / 31'557'600.0; }
    double on_jupiter() const { return on_earth() / 11.862615; }
    double on_mars() const { return on_earth() / 1.8808158; }
    double on_mercury() const { return on_earth() / 0.2408467; }
    double on_neptune() const { return on_earth() / 164.79132; }
    double on_saturn() const { return on_earth() / 29.447498; }
    double on_uranus() const { return on_earth() / 84.016846; }
    double on_venus() const { return on_earth() / 0.61519726; }
};

}  // namespace space_age
