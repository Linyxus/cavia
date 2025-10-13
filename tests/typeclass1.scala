struct CouldAdd[A]:
  val add: (A, A) => A
def genericAdd[A](a: A, b: A, inst: CouldAdd[A]^): A =
  inst.add(a, b)
def main(): Unit =
  val natCouldAdd = CouldAdd[i32]((a, b) => a + b)
  val res = genericAdd(10, 32, natCouldAdd)
  #i32println(res)

