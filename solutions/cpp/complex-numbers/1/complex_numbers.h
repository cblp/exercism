#pragma once

#include <cmath>

namespace complex_numbers {

struct Complex {
    double re, im;

    Complex(double re, double im = 0) : re(re), im(im) {}

    double real() const { return re; }

    double imag() const { return im; }

    double abs() const { return sqrt(re * re + im * im); }

    Complex conj() const { return {re, -im}; }

    Complex exp() const {
        const double f = ::exp(re);
        return {f * cos(im), f * sin(im)};
    }

    Complex operator+(Complex c) const { return {re + c.re, im + c.im}; }

    friend Complex operator+(double r, Complex c) { return Complex(r) + c; }

    Complex operator-(Complex c) const { return {re - c.re, im - c.im}; }

    friend Complex operator-(double r, Complex c) { return Complex(r) - c; }

    Complex operator*(Complex c) const {
        return {re * c.re - im * c.im, re * c.im + im * c.re};
    }

    friend Complex operator*(double r, Complex c) { return Complex(r) * c; }

    Complex operator/(Complex c) const {
        const double d = c.re * c.re + c.im * c.im;
        return {(re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d};
    }

    friend Complex operator/(double r, Complex c) { return Complex(r) / c; }
};

}  // namespace complex_numbers
