#include "linked_list.h"
#include "memory.h"
#include <stdlib.h>

LinkedList *linkedList_allocate() {
  LinkedList *list = (LinkedList *)reallocate(NULL, 0, sizeof(LinkedList));
  list->head = NULL;
  list->tail = NULL;
  list->length = 0;

  return list;
}

Node *linkedList_append(LinkedList *list, void *data) {
  Node *node = (Node *)reallocate(NULL, 0, sizeof(Node));
  node->data = data;
  node->prev = NULL;
  node->next = NULL;

  if (list->tail == NULL) {
    list->head = list->tail = node;
    list->head->prev = NULL;
    list->length = 1;
  } else {
    node->prev = list->tail;
    list->tail->next = node;
    list->tail = node;
    list->length++;
  }

  return node;
}

void linkedList_free(LinkedList *list) { free(list); }
