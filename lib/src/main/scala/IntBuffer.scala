package trigs

import java.util.Arrays

/** Growable array of unboxed ints */
private[trigs] final class IntBuffer(initialCapacity: Int = 4):
  private var data = new Array[Int](initialCapacity.max(1))
  private var size = 0

  def length: Int        = size
  def isEmpty: Boolean   = size == 0
  def apply(i: Int): Int = data(i)
  def last: Int          = data(size - 1)

  def +=(value: Int): Unit =
    if size == data.length then data = Arrays.copyOf(data, size * 2)
    data(size) = value
    size += 1

  def toArray: Array[Int] = Arrays.copyOf(data, size)
end IntBuffer
