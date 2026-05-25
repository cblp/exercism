#pragma once

#include <cstddef>

namespace linked_list {

template <typename A>
struct Node {
    A value;
    Node* next;
    Node* prev;

    Node(A value) : value(value), next(nullptr), prev(nullptr) {}
};

template <typename A>
class List {
   private:
    Node<A>* head;
    Node<A>* last;

   public:
    List() : head(nullptr), last(nullptr) {}

    size_t count() const {
        size_t count = 0;
        Node<A>* node = head;
        while (node) {
            count++;
            node = node->next;
        }
        return count;
    }

    void push(A value) {
        Node<A>* node = new Node<A>(value);
        if (last) {
            last->next = node;
            node->prev = last;
        }
        last = node;
        if (head == nullptr) {
            head = node;
        }
    }

    void unshift(A value) {
        Node<A>* node = new Node<A>(value);
        if (head) {
            head->prev = node;
            node->next = head;
        }
        head = node;
        if (last == nullptr) {
            last = node;
        }
    }

    A pop() {
        if (last == nullptr) {
            throw "List is empty";
        }
        A value = last->value;
        Node<A>* prev = last->prev;
        delete last;
        last = prev;
        if (last) {
            last->next = nullptr;
        } else {
            head = nullptr;
        }
        return value;
    }

    A shift() {
        if (head == nullptr) {
            throw "List is empty";
        }
        A value = head->value;
        Node<A>* next = head->next;
        delete head;
        head = next;
        if (head) {
            head->prev = nullptr;
        } else {
            last = nullptr;
        }
        return value;
    }

    void erase(A value) {
        Node<A>* node = head;
        while (node) {
            if (node->value == value) {
                if (node->prev) {
                    node->prev->next = node->next;
                } else {
                    head = node->next;
                }
                if (node->next) {
                    node->next->prev = node->prev;
                } else {
                    last = node->prev;
                }
                delete node;
                return;
            }
            node = node->next;
        }
    }
};

}  // namespace linked_list
