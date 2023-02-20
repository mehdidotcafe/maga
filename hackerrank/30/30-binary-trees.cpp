#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <queue>

using namespace std;

typedef struct Node{
    struct Node* left;
    struct Node* right;
    int data;
}Node;
Node* newNode(int data){
    Node* node=(Node*)malloc(sizeof(Node));
    node->left=node->right=NULL;
    node->data=data;
    return node;
}

void levelOrder(Node* root) {
  if (root == NULL) {
    return;
  }

  std::queue<Node*> q;
  q.push(root);

  while (!q.empty()) {
    Node* current = q.front();
    cout << current->data << " ";
    q.pop();

    if (current->left != NULL) {
      q.push(current->left);
    }

    if (current->right != NULL) {
      q.push(current->right);
    }
  }
}

Node* insert(Node* root,int data){
    if(root==NULL)
        return newNode(data);
    else{
        Node* cur;
        if(data<=root->data){
            cur=insert(root->left,data);
            root->left=cur;                
        }
        else{
            cur=insert(root->right,data);
            root->right=cur;
        }
        
    }
    return root;
}

int main(){
    std::queue<int> q;

    Node* root=NULL;
    int T,data;
    scanf("%d",&T);
    while(T-->0){
        scanf("%d",&data);
        root=insert(root,data);
    }
    q.push(root->data);
    levelOrder(root);

    return 0;
    
}