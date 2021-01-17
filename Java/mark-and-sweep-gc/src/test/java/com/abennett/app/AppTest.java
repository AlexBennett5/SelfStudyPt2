package com.abennett.app;

import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class AppTest {
    
  @Test
  public void itemsOnStackConserved() {
    VirtualMachine vm = new VirtualMachine();
    vm.pushNum(1);
    vm.pushNum(2);
    vm.garbageCollect();
    assertEquals(vm.getHeapSize(), 2);
  }

  @Test
  public void unreachableAreCollected() {
    VirtualMachine vm = new VirtualMachine();
    vm.pushNum(1);
    vm.pushNum(2);
    vm.pop();
    vm.pop();
    vm.garbageCollect();
    assertEquals(vm.getHeapSize(), 0);
  }

  @Test
  public void reachNestedObjects() {
    VirtualMachine vm = new VirtualMachine();
    vm.pushNum(1);
    vm.pushNum(2);
    vm.pushCons();
    vm.pushNum(3);
    vm.pushNum(4);
    vm.pushCons();
    vm.pushCons();
    vm.garbageCollect();
    assertEquals(vm.getHeapSize(), 7);
  }

  @Test
  public void handleCycles() {
    VirtualMachine vm = new VirtualMachine();
    vm.pushNum(1);
    vm.pushNum(2);
    Object a = vm.pushCons();
    vm.pushNum(3);
    vm.pushNum(4);
    Object b = vm.pushCons();

    a.setCdr(b);
    b.setCdr(a);

    vm.garbageCollect();
    assertEquals(vm.getHeapSize(), 4);
  }

}
