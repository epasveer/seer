type
  TestObj = object
    num: int
    val: float
    str: string

proc initTestObj(num: int): TestObj =
  TestObj(num: num, val: 3.141, str: "TestObj")

proc foo(x: int): int =
  let y = x + 2
  return y * 10

proc bar(x: int): int =
  if x == 3:
    return foo(x)
  return x * 100

proc main =
  var a = 1
  a += 3
  let str = "foobar"
  var seq1 = @[0, 1, 2, 3, 4]
  a = bar(1)
  a = bar(2)
  a = bar(3)
  let tobj = initTestObj(11)

main()

# A simple Nim program.
# https://internet-of-tomohiro.netlify.app/nim/gdb.en.html
# https://forum.nim-lang.org/t/11214


