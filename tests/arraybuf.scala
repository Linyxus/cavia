struct StringBuffer:
  var capacity: i32 
  var size: i32
  var data: array[char]^

def newBuffer(capacity: i32): StringBuffer^ =
  StringBuffer(capacity, 0, newArray[char](capacity, '\0'))

def copyArray[T](src: array[T]^, target: array[T]^, len: i32): Unit =
  def loop(cur: i32): Unit =
    if cur < len then
      target(cur) = src(cur)
      loop(cur + 1)
  loop(0)

def growBuffer(buf: StringBuffer^, delta: i32): Unit =
  val newSize = buf.capacity + delta
  val newData = newArray[char](newSize, '\0')
  copyArray(buf.data, newData, buf.capacity)
  buf.capacity = newSize
  buf.data = #unsafeAsPure(newData)

def appendChar(buf: StringBuffer^, ch: char): Unit =
  if buf.size == buf.capacity then
    growBuffer(buf, 32)
  buf.data(buf.size) = ch
  buf.size = buf.size + 1

def appendStr(buf: StringBuffer^, s: String^): Unit =
  def loop(cur: i32): Unit =
    if cur < s.size then
      appendChar(buf, s(cur))
      loop(cur + 1)
  loop(0)

def getResult(consume buf: StringBuffer^): String^ =
  buf.data

def putStrLn(str: String^): Unit =
  def loop(cur: i32): Unit =
    if str(cur) != '\0' then
      #putchar(str(cur))
      loop(cur + 1)
  loop(0)
  #putchar('\n')

def main(): Unit =
  val buf = newBuffer(2)
  val f = () => appendStr(buf, "hello, world")
  f()
  f()
  putStrLn(getResult(buf))
  //appendStr(buf, "!!! Scala")
  f()

