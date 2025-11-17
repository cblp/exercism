class SpaceAge:
    seconds: int

    def __init__(self, seconds: int):
        self.seconds = seconds

    def _on(self, orbital_period_in_Earth_years: float) -> float:
        return round(
            self.seconds / 31_557_600 / orbital_period_in_Earth_years, 2
        )

    def on_earth(self) -> float:
        return self._on(1)

    def on_mercury(self) -> float:
        return self._on(0.2408467)

    def on_venus(self) -> float:
        return self._on(0.61519726)

    def on_mars(self) -> float:
        return self._on(1.8808158)

    def on_jupiter(self) -> float:
        return self._on(11.862615)

    def on_saturn(self) -> float:
        return self._on(29.447498)

    def on_uranus(self) -> float:
        return self._on(84.016846)

    def on_neptune(self) -> float:
        return self._on(164.79132)
