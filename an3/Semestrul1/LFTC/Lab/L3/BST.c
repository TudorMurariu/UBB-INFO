#include <stdio.h>
#include <stdlib.h>

struct TreeNode {
    int key;
    struct TreeNode* left;
    struct TreeNode* right;
};

struct TreeNode* initializeTree() {
    return NULL; // an empty tree has a NULL root
}

struct TreeNode* createTreeNode(int key) {
    struct TreeNode* newNode = (struct TreeNode*)malloc(sizeof(struct TreeNode));
    newNode->key = key;
    newNode->left = NULL;
    newNode->right = NULL;
    return newNode;
}

struct TreeNode* insertRecursive(struct TreeNode* root, int key) {
    if (root == NULL) {
        return createTreeNode(key);
    }

    if (key < root->key) {
        root->left = insertRecursive(root->left, key);
    } else {
        root->right = insertRecursive(root->right, key);
    }

    return root;
}

int countNodes(struct TreeNode* root) {
    if (root == NULL) {
        return 0;
    }

    return 1 + countNodes(root->left) + countNodes(root->right);
}

int getIndexRecursive(struct TreeNode* root, int key, int index) {
    if (root == NULL) {
        return -1; // sau altă valoare de semnalare a lipsei cheii
    }

    if (key == root->key) {
        return index + countNodes(root->left);
    }

    if (key < root->key) {
        return getIndexRecursive(root->left, key, index);
    }

    return getIndexRecursive(root->right, key, index + countNodes(root->left) + 1);
}

void inorderRecursivePrint(struct TreeNode* root) {
    if (root != NULL) {
        inorderRecursivePrint(root->left);
        printf("%d : %d\n", root->key, getIndexRecursive(root, root->key, 0));
        inorderRecursivePrint(root->right);
    }
}

// int main() {
//     struct TreeNode* root = NULL;

//     root = insertRecursive(root, 5);
//     root = insertRecursive(root, 3);
//     root = insertRecursive(root, 8);
//     root = insertRecursive(root, 2);
//     root = insertRecursive(root, 4);
//     root = insertRecursive(root, 7);
//     root = insertRecursive(root, 9);

//     printf("Inorder Traversal:\n");
//     inorderRecursivePrint(root);

//     return 0;
// }
