package com.abennett.app;

public class VirtualMachine {

  int MAX_HEAP = 66536;
  int MAX_STACK = 512;
  int INITIAL_GC_THRESHOLD = 8;

  private Object heap;
  private Object[] stack;
  private int heapSize;
  private int stackSize;
  private int gcThreshold;

  public VirtualMachine() {
    this.heap = null;
    this.heapSize = 0;
    this.stack = new Object[MAX_STACK];
    this.stackSize = 0;
    this.gcThreshold = INITIAL_GC_THRESHOLD;
  }

  public int getHeapSize() {
    return this.heapSize;
  }

  public void push(Object obj) {
    assert this.stackSize < MAX_STACK : "ERROR: Stack overflow";
    this.stack[this.stackSize++] = obj;
  }

  public Object pop() {
    assert this.stackSize > 0 : "ERROR: Stack underflow";
    return this.stack[--this.stackSize];
  }

  public Object alloc(ObjectType type) {
    if (this.heapSize == this.gcThreshold)
      garbageCollect();
  
    Object next = this.heap;
    Object ret = new Object(type, next);
    this.heap = ret; 
    this.heapSize++;
    return ret;
  }

  public Object pushNum(int numval) {
    Object ret = alloc(ObjectType.NUM);
    ret.setNumval(numval);
    push(ret);
    return ret;
  }

  public Object pushCons() {
    Object ret = alloc(ObjectType.CONS);
    Object cdr = pop();
    Object car = pop();
    ret.setCons(car, cdr);
    push(ret);
    return ret;
  }

  public void mark(Object obj) {
    if (obj.getMark()) return;

    obj.setMark(true);
  
    if (obj.getType() == ObjectType.CONS) {
      mark(obj.getCar());
      mark(obj.getCdr());  
    }
  }

  public void markAll() {
    for (int i = 0; i < this.stackSize; i++)
      mark(this.stack[i]);
  }

  public void sweep() {
    
    Object obj = heap;

    while (obj != null) {
      if (obj.getMark()) {
        obj.setMark(false);
        obj = obj.getNext();
      } else {
        Object unreached = obj;
        obj = obj.getNext();
        unreached = null;
        this.heapSize--;
      } 

    }
  }
    
  public void garbageCollect() {
    markAll();
    sweep();
    this.gcThreshold = this.heapSize * 2;
  }

}
