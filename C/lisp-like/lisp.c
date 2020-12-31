

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

void initialize_root(heap_t *heap) {}

void forward_root(heap_t *heap) {}

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

////////////////////////////
// Constructor
////////////////////////////

static obj_t *make_int(heap_t *heap, int val) {
  obj_t *newint = alloc(heap, sizeof(int));
  newint->val = val;
  return newint;
}

static obj_t *make_cons(heap_t *heap, obj_t *car, obj_t *cdr) {
  obj_t *newcons = alloc(heap, sizeof(obj_t *) * 2);
  newcons->car = car;
  newcons->cdr = cdr;
  return newcons;
}

static obj_t *make_func(heap_t *heap, obj_t *params, obj_t *body, obj_t *env) {
  obj_t *newfunc = alloc(heap, sizeof(obj_t *) * 3);
  newfunc->params = params;
  newfunc->body = body;
  newfunc->env = env;
  return newfunc;
}

static obj_t *make_symbol(heap_t *heap, char* str) {
  obj_t *newsym = alloc(heap, strlen(str));
  newsym->str;
  return newsym;
}


////////////////////////////
// Primitives
////////////////////////////





