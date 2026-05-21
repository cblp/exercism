#pragma once

#include <stdexcept>
#include <vector>

namespace circular_buffer {

using namespace std;

template <typename A>
class circular_buffer {
   private:
    vector<A> _buffer{};
    size_t _reader{};
    size_t _writer{};

   public:
    circular_buffer(size_t capacity) : _buffer(capacity + 1) {}

    bool is_empty() const { return _reader == _writer; }

    bool is_full() const { return _reader == (_writer + 1) % _buffer.size(); }

    void clear() { _writer = _reader; }

    A read() {
        if (is_empty()) {
            throw domain_error{"empty"};
        }
        const auto value = _buffer[_reader];
        _reader = (_reader + 1) % _buffer.size();
        return value;
    }

    void write(A value) {
        if (is_full()) {
            throw domain_error{"full"};
        }
        _buffer[_writer] = value;
        _writer = (_writer + 1) % _buffer.size();
    }

    void overwrite(A value) {
        if (is_full()) {
            _reader = (_reader + 1) % _buffer.size();
        }
        _buffer[_writer] = value;
        _writer = (_writer + 1) % _buffer.size();
    }
};

}  // namespace circular_buffer
