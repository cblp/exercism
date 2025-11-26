#include "binary_search_tree.h"

#include <stdlib.h>

static node_t* new_node(int data) {
    node_t* node = calloc(1, sizeof(node_t));
    node->data = data;
    return node;
}

static void tree_add(node_t* tree, int data) {
    if (data <= tree->data) {
        if (tree->left) {
            tree_add(tree->left, data);
        } else {
            tree->left = new_node(data);
        }
    } else {
        if (tree->right) {
            tree_add(tree->right, data);
        } else {
            tree->right = new_node(data);
        }
    }
}

node_t* build_tree(int* tree_data, size_t tree_data_len) {
    if (tree_data_len == 0) {
        return NULL;
    }
    node_t* const root = new_node(*tree_data);
    for (size_t i = 1; i < tree_data_len; i++) {
        tree_add(root, tree_data[i]);
    }
    return root;
}

void free_tree(node_t* tree) {
    if (tree) {
        free_tree(tree->left);
        free_tree(tree->right);
    }
}

static size_t tree_size(node_t* tree) {
    return tree ? 1 + tree_size(tree->left) + tree_size(tree->right) : 0;
}

static size_t sorted_data_write(node_t* tree, int* r) {
    if (tree) {
        size_t const left_size = sorted_data_write(tree->left, r);
        r[left_size] = tree->data;
        size_t const right_size =
            sorted_data_write(tree->right, r + 1 + left_size);
        return left_size + 1 + right_size;
    } else {
        return 0;
    }
}

int* sorted_data(node_t* tree) {
    if (!tree) {
        return NULL;
    }
    size_t const size = tree_size(tree);
    int* const r = malloc(size * sizeof(int));
    sorted_data_write(tree, r);
    return r;
}
