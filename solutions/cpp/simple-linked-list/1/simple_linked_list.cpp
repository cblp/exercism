#include "simple_linked_list.h"

#include <stdexcept>

namespace simple_linked_list {

using namespace std;

size_t List::size() const { return current_size; }

void List::push(int data) {
    head = new Element{data, head};
    current_size++;
}

int List::pop() {
    if (current_size == 0) {
        throw out_of_range("Cannot pop from an empty list");
    }

    auto data = head->data;
    auto old_head = head;
    head = old_head->next;
    delete old_head;
    current_size--;
    return data;
}

void List::reverse() {
    Element* prev = nullptr;
    Element* current = head;
    while (current != nullptr) {
        Element* next = current->next;
        current->next = prev;
        prev = current;
        current = next;
    }
    head = prev;
}

List::~List() {
    while (head != nullptr) {
        auto old_head = head;
        head = old_head->next;
        delete old_head;
    }
}

}  // namespace simple_linked_list
