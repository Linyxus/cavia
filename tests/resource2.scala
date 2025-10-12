struct Resource(var data: i32)
struct ResourcePair(a: Resource^, b: Resource^)

def drop(consume x: Resource^): Unit = ()

def use(x: Resource^): Unit =
  #i32println(x.data)

def usePair(x: ResourcePair^): Unit =
  use(x.a)
  use(x.b)

def main(): Unit =
  val a = Resource(42)
  val b = Resource(0)
  val p = ResourcePair(a, b)
  usePair(p)
  drop(p.a)
