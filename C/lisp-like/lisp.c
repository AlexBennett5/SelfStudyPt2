

////////////////////////////
// Types
////////////////////////////

enum type = {
  NUM_T,
  LIST_T,
  FUNC_T,
  SYMBOL_T,
  BOOL_T,
  FORWARD_T
};

struct obj_t;
struct heap_t;
typedef struct obj_t *(primitive)(heap_t *heap, struct obj_t **env, struct obj_t **args);

typedef struct obj_t = {
  int type;
  int size;

  union {
    int val;

    struct {
      struct obj_t *car;
      struct obj_t *cdr;
    };

    struct {
      struct obj_t *params;
      struct obj_t *body;
      struct obj_t *env;
    };

    char *symbol;

    obj_t *forward;
  }
};


////////////////////////////
// Memory Alloc/GC
////////////////////////////

typedef struct heap_t {
  obj_t *alloc_ptr;
  obj_t *to, *from;
  unsigned long semispace_size;
} heap_t;

heap_t *make_heap(unsigned long semispace_size) {

  heap_t *heap = malloc(sizeof(heap_t));
  heap->from = malloc(semispace_size);
  heap->alloc_ptr = heap->from;
  heap->semispace_size = semispace_size;
  heap->to = heap->from + heap->semispace_size;
  return heap;
}

obj_t *alloc(heap_t *heap, size_t size) {

  if (heap->alloc_ptr + size > heap->from + heap->semispace_size)
    gc_collect(heap);

  if (heap->alloc_ptr + size > heap->from + heap->semispace_size) {
    printf("Insufficient memory\n");
    exit(1);
  }

  obj_t *newobj = heap->alloc_ptr;
  heap->alloc_ptr += size;
  return newobj;
} 

obj_t *forward_obj(heap_t *heap, obj_t *source) {
  obj_t *dest;

  if (source->type = FORWARD_T) {
    return source->forward;
  } else {
    heap->alloc_ptr += source->size;
    memcpy(dest, source, source->size);

    source->type = FORWARD_T;
    source->forward = dest;
    return dest;
  }
}

void forward_root(heap_t *heap) {}

NUM_T,
  LIST_T,
  FUNC_T,
  SYMBOL_T,
  BOOL_T,
  FORWARD_T

void gc_collect(heap_t *heap) {
  
  obj_t *scan = heap->alloc_ptr = heap->to;

  obj_t *temp = heap->from;
  heap->from = heap->to;
  heap->to = temp;

  forward_root(heap);
  
  while(scan < heap->alloc_ptr) {
    switch(scan->type) {
      case NUM_T:
      case SYMBOL_T:
      case BOOL_T:
        break;
      case LIST_T:
        forward_obj(scan->car);
        forward_obj(scan->cdr);
        break;
      case FUNC_T:
        forward_obj(scan->params);
        forward_obj(scan->body);
        forward_obj(scan->env);
        break;
      default:
        printf("Invalid type\n");
        exit(1);
        break;
    }
    scan += scan->size;
  }
}

