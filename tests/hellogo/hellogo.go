package main

import "fmt"

type MyStruct struct {
    x string
    i int
    f float64
}

func main() {

    fmt.Println("Hello, Go World!")

    x := "abc"
    i := 3

    fmt.Println(i)
    fmt.Println(x)

    ms := &MyStruct{
        x: "cba",
        i: 10,
        f: 11.10335,
    }

    fmt.Println(ms)
}

