#pragma once

#include <memory>

namespace binary_search_tree {

using namespace std;

template <typename A>
struct binary_tree {
   private:
    A value;
    binary_tree const* parent{nullptr};
    unique_ptr<binary_tree> sub_left, sub_right;

   public:
    binary_tree(A value, binary_tree const* parent = nullptr)
        : value(value), parent(parent) {}

    void insert(A new_value) {
        if (new_value <= value) {
            if (sub_left) {
                sub_left->insert(new_value);
            } else {
                sub_left = make_unique<binary_tree>(new_value, this);
            }
        } else {
            if (sub_right) {
                sub_right->insert(new_value);
            } else {
                sub_right = make_unique<binary_tree>(new_value, this);
            }
        }
    }

    A const& data() const { return value; }

    unique_ptr<binary_tree> const& left() const { return sub_left; }

    unique_ptr<binary_tree> const& right() const { return sub_right; }

    struct iterator {
        binary_tree const* node;

        bool operator!=(iterator const& other) const {
            return node != other.node;
        }

        void operator++() {
            if (not node) {
                return;
            }
            if (node->sub_right) {
                node = node->sub_right.get();
                while (node->sub_left) {
                    node = node->sub_left.get();
                }
            } else {
                while (node->parent and node == node->parent->sub_right.get()) {
                    node = node->parent;
                }
                node = node->parent;
            }
        }

        A const& operator*() const { return node->value; }
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
