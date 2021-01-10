package com.abennett.app;

public class Object {

  private boolean mark;
  private ObjectType type;
  private Object next;

  private int numval;
  private Object car;
  private Object cdr;

  public Object(ObjectType type, Object next) {
    this.mark = false;
    this.type = type;
    this.next = next;
  }

  public ObjectType getType() {
    return this.type;
  }

  public boolean getMark() {
    return this.mark;
  }

  public void setMark(boolean bool) {
    this.mark = bool;
  }

  public Object getNext() {
    return this.next;
  }

  public void setNumval(int numval) {
    this.numval = numval;
  }

  public Object getCar() {
    return this.car;
  }

  public Object getCdr() {
    return this.cdr;
  }

  public void setCar(Object car) {
    this.car = car;
  }

  public void setCdr(Object cdr) {
    this.cdr = cdr;
  }

  public void setCons(Object car, Object cdr) {
    this.car = car;
    this.cdr = cdr;
  }
}
