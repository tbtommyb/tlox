#ifndef clox_linked_list_h
#define clox_linked_list_h

#include <stddef.h>

typedef struct Node {
  void *data;
  struct Node *next;
  struct Node *prev;
} Node;

typedef struct LinkedList {
  int length;
  Node *head;
  Node *tail;
} LinkedList;

LinkedList *linkedList_allocate();
Node *linkedList_append(LinkedList *list, void *data);
void linkedList_free(LinkedList *list);

#endif
