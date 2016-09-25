//: Playground - noun: a place where people can play

import UIKit

var str = "Hello, playground"

func a(blah: String) -> String {
  return blah + "a"
}

["a", "b", "c"].map(a)

[1,2,3,4,5].reduce(0, combine: +)

let c = { () -> () in print("abc") }