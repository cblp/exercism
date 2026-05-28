#pragma once

#include <atomic>
#include <stdexcept>

namespace Bankaccount {

using namespace std;

class Bankaccount {
   private:
    atomic<int> _balance{0};
    bool _is_open = false;

   public:
    void open() {
        if (_is_open) throw runtime_error{""};
        _is_open = true;
    }

    void close() {
        if (not _is_open) throw runtime_error{""};
        _is_open = false;
        _balance = 0;
    }

    int balance() const {
        if (not _is_open) throw runtime_error{""};
        return _balance;
    }

    void deposit(int d) {
        if (d < 0 or not _is_open) throw runtime_error{""};
        _balance += d;
    }

    void withdraw(int d) {
        if (d < 0 or not _is_open or d > _balance) throw runtime_error{""};
        _balance -= d;
    }
};  // class Bankaccount

}  // namespace Bankaccount
