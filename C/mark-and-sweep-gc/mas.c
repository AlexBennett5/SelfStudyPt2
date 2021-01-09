#include <stdio.h>

#define STACK_SZ 512
#define GC_THRESHOLD 30

enum {
  NUM, CONS
};

typedef struct {
  unsigned char mark;
  int type;
  struct obj_t *next;

  union {
    int numval;

    struct {
      obj_t *car;
      obj_t *cdr;
    };
  }

} obj_t;

typedef struct {
  obj_t* head;
  obj_t* stack[STACK_SZ];
  int size;
  int gc_size;
  int gc_threshold;
} VM;

VM* init_VM() {
  VM* vm = malloc(sizeof(VM));
  vm->head = NULL;
  vm->size = 0;

  vm->gc_size = 0;
  vm->gc_threshold = GC_THRESHOLD;
  return vm;
}

void push (VM* vm, obj_t* obj) {
  assert(vm->size < STACK_SZ, "ERROR: stack overflow");
  vm->stack[vm->size++] = obj;
}

obj_t* pop(VM* vm) {
  assert(vm->size > 0, "ERROR: stack underflow");
  return vm->stack[--vm->size];
}

obj_t* alloc(VM* vm) {
  if (vm->gc_size == vm->gc_threshold) gc(vm);

  obj_t* newobj = malloc(sizeof(obj_t));
  newobj->mark = 0;
  newobj->next = vm->head;
  vm->head = newobj;
  vm->gc_size++;
  return newobj;
}

obj_t* push_num(VM* vm, int numval) {
  obj_t* numobj = alloc(vm);
  numobj->type = NUM;
  numobj->numval = numval;
  push(vm, numobj);
}

obj_t* push_cons(VM* vm) {
  obj_t* consobj = alloc(vm);
  consobj->type = CONS;
  consobj->cdr = pop(vm);
  consobj->car = pop(vm);
  push(vm, consobj);
  return consobj;
} 

void mark(obj_t* obj) {
  if (obj->mark) return;
  
  obj->mark = 1;

  if (obj->type == CONS) {
    mark(obj->car);
    mark(obj->cdr);
  }
}

void markall(VM* vm) {
  for (int i = 0; i < vm->size; i++)
    mark(vm->stack[i]);
}

void sweep(VM* vm) {
  obj_t** obj = &vm->head;
  while(*obj) {
    if (!(*obj)->mark) {
      obj_t* unreached = *obj;
      *obj = unreached->next;
      free(unreached);
      vm->gc_size--; 
    } else {
      (*obj)->mark = 0;
      obj = &(*obj)->next;
    }
  }
}

void gc(VM* vm) {
  markall(vm);
  sweep(vm);
  vm->gc_threshold = vm->gc_size * 2;
}


