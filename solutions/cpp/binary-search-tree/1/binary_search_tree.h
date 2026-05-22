#pragma once

#include <memory>
#include <string>

namespace binary_search_tree {

using namespace std;

[[noreturn]] inline void todo(string file, unsigned line, string function) {
    throw runtime_error{
        file + ":" + to_string(line) + ": " + function + " not implemented"};
}

#define TODO() todo(__FILE__, __LINE__, __FUNCTION__)

template <typename A>
struct binary_tree {
   private:
    A value;
    binary_tree const* parent;
    bool is_right;
    unique_ptr<binary_tree> sub_left, sub_right;

   public:
    binary_tree(A value) : value(value), parent(nullptr) {}

    binary_tree(A value, binary_tree const* parent, bool is_right)
        : value(value), parent(parent), is_right(is_right) {}

    void insert(A new_value) {
        if (new_value <= value) {
            if (sub_left) {
                sub_left->insert(new_value);
            } else {
                sub_left = make_unique<binary_tree>(new_value, this, false);
            }
        } else {
            if (sub_right) {
                sub_right->insert(new_value);
            } else {
                sub_right = make_unique<binary_tree>(new_value, this, true);
            }
        }
    }

    A data() const { return value; }

    unique_ptr<binary_tree> const& left() const { return sub_left; }

    unique_ptr<binary_tree> const& right() const { return sub_right; }

    struct iterator {
        binary_tree const* node;

        bool operator!=(iterator const& other) const {
            return node != other.node;
        }

        void operator++() {
            if (not node) return;
            if (node->sub_right) {
                node = node->sub_right.get();
                while (node->sub_left) {
                    node = node->sub_left.get();
                }
            } else {
                while (true) {
                    if (node->is_right) {
                        node = node->parent;
                    } else {
                        node = node->parent;
                        break;
                    }
                }
            }
        }

        A const& operator*() { return node->value; }
    };

    iterator begin() const {
        binary_tree const* node = this;
        while (node->sub_left) {
            node = node->sub_left.get();
        }
        return {node};
    }

    iterator end() const { return {nullptr}; }
};

}  // namespace binary_search_tree
